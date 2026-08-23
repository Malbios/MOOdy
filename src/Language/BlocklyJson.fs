/// Pure `BlockStmt`/`BlockValue` (`Blocks.fs`) <-> Blockly's real workspace
/// serialization shape (`Blockly.serialization.workspaces.save/load`), for
/// the "Google Blocks mode for visual coding" card. Deliberately
/// environment-agnostic - a hand-rolled `JsonValue` type instead of any BCL
/// JSON library, so this is both `dotnet test`-able here and
/// Fable-compilable for the real browser client (same guarantee every
/// other `Language` module already has). The JS-side glue that converts a
/// real JS object to/from `JsonValue` lives in `src/Client/BlocklyEditor.fs`
/// - this module never touches `obj`/dynamic JS at all.
///
/// Covers a deliberately smaller construct set than `Blocks.fs`'s full
/// width (see that module's own doc comment for the full list) - every
/// literal kind, `Ident`, `Binary`/`Unary` (one Blockly block type per
/// family with an `OP` field, not one block type per operator), `Cond`,
/// `Assign`, `Prop`/`VerbCall`/`Call`, `ListLit`, `Index`, `Range`,
/// `FirstIndex`/`LastIndex`, `MapLit`, `If`, `While`, `ForList`, `ForRange`,
/// `Fork`, `TryFinally`, `Return`/`Break`/`Continue`, `ExprStmt`,
/// `SComment`. Still out of scope (a genuine variable-arity shape needing
/// either a real Blockly mutator or a fixed-max-with-kind-selector design,
/// not attempted yet): `TryExcept` (a variable number of `except` arms),
/// `Scatter` (a variable number of independently required/optional/rest
/// items), `Catch`'s explicit code list, and computed-name `Prop`/
/// `VerbCall`. Anything out of scope maps one-way to a
/// `moo_unsupported`/`moo_unsupported_expr` placeholder carrying a debug
/// string - not meant to ever actually reach a real workspace in practice,
/// since the eventual UI gates the toggle on `isFullyMappable` first; it
/// exists only so this module's functions stay total.
///
/// Blockly's real serialized shape has no first-class way to represent a
/// variable number of child value-inputs (an `if`/`elseif` chain, a call's
/// argument list) - real Blockly blocks with that shape (`controls_if`,
/// `lists_create_with`) use a *mutator*: a JSON `extraState` field the
/// block's own JS definition reads to decide how many named input sockets
/// to create before `inputs` gets applied. Two different encodings are
/// used here:
/// - `If`/`elseif`/`else` needs no mutator at all - an arms list `[arm1;
///   arm2; ...]` with a tail is exactly equivalent to `arm1` with `arm2;
///   ...` nested inside its own `else` slot (nested `if`/`else` behaves
///   identically to `elseif` at runtime), so multi-arm `SIf` round-trips
///   through a single fixed-shape `moo_if` block (one `COND` value input,
///   `THEN`/`ELSE` statement inputs), just nested rather than flattened -
///   an accepted reshaping, not a semantic change, the same "AST-
///   equivalent, not shape-identical" stance `Sugar.fs`/`Blocks.fs` already
///   take elsewhere.
/// - `Call`/`VerbCall`/`ListLit`'s argument lists are variable-arity in the
///   `Ast` but map to a small **fixed** number of `ARGi` sockets on the real
///   Blockly-side blocks for this slice (see `BlocklyEditor.fs`) - a
///   deliberate simplification, not a full dynamic mutator. The argument
///   count is recovered by walking `ARG0`, `ARG1`, ... until the first
///   *absent* input (left-to-right-filled, no gaps), not from a reported
///   count - a real Blockly workspace save only round-trips whatever a
///   fixed-arity block's own `saveExtraState` chooses to report, and there
///   is no reason for it to report a redundant count. `splices` (whether
///   argument `i` was `@`-prefixed) has no other field to live in, so it
///   still travels via a small `extraState.splices` array, preserved
///   through a real save/load cycle by a small registered Blockly
///   extension - see `BlocklyEditor.fs`.
module Language.BlocklyJson

open Language.Ast
open Language.Blocks

type JsonValue =
    | JVString of string
    | JVNumber of float
    | JVBool of bool
    | JVNull
    | JVArray of JsonValue list
    | JVObject of (string * JsonValue) list

let private field (name: string) (fields: (string * JsonValue) list) : JsonValue option =
    fields |> List.tryFind (fun (n, _) -> n = name) |> Option.map snd

let private asFields (v: JsonValue) : (string * JsonValue) list =
    match v with
    | JVObject fs -> fs
    | _ -> []

let private asString (v: JsonValue) : string option =
    match v with
    | JVString s -> Some s
    | _ -> None

let private asNumber (v: JsonValue) : float option =
    match v with
    | JVNumber n -> Some n
    | _ -> None

let private asBool (v: JsonValue) : bool option =
    match v with
    | JVBool b -> Some b
    | _ -> None

let private asArray (v: JsonValue) : JsonValue list =
    match v with
    | JVArray xs -> xs
    | _ -> []

let mutable private idCounter = 0
let private nextId () =
    idCounter <- idCounter + 1
    sprintf "b%d" idCounter

let private binOpName (op: BinOp) : string =
    match op with
    | Add -> "ADD"
    | Sub -> "SUB"
    | Mul -> "MUL"
    | Div -> "DIV"
    | Mod -> "MOD"
    | Pow -> "POW"
    | Eq -> "EQ"
    | NotEq -> "NEQ"
    | Lt -> "LT"
    | LtEq -> "LTE"
    | Gt -> "GT"
    | GtEq -> "GTE"
    | And -> "AND"
    | Or -> "OR"
    | In -> "IN"
    | BitAnd -> "BITAND"
    | BitOr -> "BITOR"
    | BitXor -> "BITXOR"
    | Shl -> "SHL"
    | Shr -> "SHR"

let private binOpOfName (name: string) : BinOp option =
    match name with
    | "ADD" -> Some Add
    | "SUB" -> Some Sub
    | "MUL" -> Some Mul
    | "DIV" -> Some Div
    | "MOD" -> Some Mod
    | "POW" -> Some Pow
    | "EQ" -> Some Eq
    | "NEQ" -> Some NotEq
    | "LT" -> Some Lt
    | "LTE" -> Some LtEq
    | "GT" -> Some Gt
    | "GTE" -> Some GtEq
    | "AND" -> Some And
    | "OR" -> Some Or
    | "IN" -> Some In
    | "BITAND" -> Some BitAnd
    | "BITOR" -> Some BitOr
    | "BITXOR" -> Some BitXor
    | "SHL" -> Some Shl
    | "SHR" -> Some Shr
    | _ -> None

let private unOpName (op: UnOp) : string =
    match op with
    | Neg -> "NEG"
    | Not -> "NOT"
    | BitNot -> "BITNOT"

let private unOpOfName (name: string) : UnOp option =
    match name with
    | "NEG" -> Some Neg
    | "NOT" -> Some Not
    | "BITNOT" -> Some BitNot
    | _ -> None

/// Builds one block's own JSON object (not yet wrapped in a `{block:
/// ...}` input holder, and not yet given a `next` chain - callers add
/// those as needed).
let private makeBlock
    (blockType: string)
    (fields: (string * JsonValue) list)
    (inputs: (string * JsonValue) list)
    (extraState: (string * JsonValue) list)
    : (string * JsonValue) list =
    [ "type", JVString blockType; "id", JVString(nextId ()) ]
    @ (if fields.IsEmpty then [] else [ "fields", JVObject fields ])
    @ (if inputs.IsEmpty then [] else [ "inputs", JVObject inputs ])
    @ (if extraState.IsEmpty then [] else [ "extraState", JVObject extraState ])

let private valueSlot (v: JsonValue) : JsonValue = JVObject [ "block", v ]

let rec private argsShape (args: BlockArg list) : (string * JsonValue) list * (string * JsonValue) list =
    let argValue =
        function
        | AArg v -> v
        | ASplice v -> v

    let isSplice =
        function
        | AArg _ -> false
        | ASplice _ -> true

    let inputs = args |> List.mapi (fun i a -> sprintf "ARG%d" i, valueSlot (valueToJson (argValue a)))

    // No "count" field - the real Blockly-side blocks have a fixed number
    // of ARGi sockets (see BlocklyEditor.fs), and the argument count is
    // recovered from which ones are actually connected (`jsonToValue`'s
    // `args()`, below) - a real Blockly workspace save only round-trips
    // whatever a block's own `saveExtraState` reports, and a fixed-arity
    // block has no reason to report a redundant count. `splices` still
    // needs a home here (no other field could carry "was this one `@`-
    // prefixed"), preserved via a small registered extension on the JS side.
    let extraState = [ "splices", JVArray(args |> List.map (isSplice >> JVBool)) ]

    inputs, extraState

/// `BlockValue` -> Blockly's real per-block JSON shape, total (anything
/// outside this slice's covered subset becomes a `moo_unsupported_expr`
/// debug placeholder - see the module's own doc comment).
and valueToJson (value: BlockValue) : JsonValue =
    let obj blockType fields inputs extraState = JVObject(makeBlock blockType fields inputs extraState)
    let v1 name value = [ name, valueSlot (valueToJson value) ]

    match value with
    | VIntLit n -> obj "moo_int" [ "NUM", JVNumber(float n) ] [] []
    | VFloatLit f -> obj "moo_float" [ "NUM", JVNumber f ] [] []
    | VStrLit s -> obj "moo_string" [ "TEXT", JVString s ] [] []
    | VObjLit n -> obj "moo_obj" [ "NUM", JVNumber(float n) ] [] []
    | VErrLit e -> obj "moo_err" [ "CODE", JVString e ] [] []
    | VBoolLit b -> obj "moo_bool" [ "BOOL", JVString(if b then "TRUE" else "FALSE") ] [] []
    | VIdent name -> obj "moo_ident" [ "NAME", JVString name ] [] []
    | VBinary(op, l, r) -> obj "moo_binary" [ "OP", JVString(binOpName op) ] (v1 "LEFT" l @ v1 "RIGHT" r) []
    | VUnary(op, e) -> obj "moo_unary" [ "OP", JVString(unOpName op) ] (v1 "VALUE" e) []
    | VCond(c, t, f) -> obj "moo_cond" [] (v1 "COND" c @ v1 "THEN" t @ v1 "ELSE" f) []
    | VAssign(target, value) -> obj "moo_assign" [] (v1 "TARGET" target @ v1 "VALUE" value) []
    | VProp(receiver, name) -> obj "moo_prop" [ "NAME", JVString name ] (v1 "RECEIVER" receiver) []
    | VVerbCall(receiver, name, args) ->
        let inputs, extraState = argsShape args
        obj "moo_verbcall" [ "NAME", JVString name ] (v1 "RECEIVER" receiver @ inputs) extraState
    | VCall(name, args) ->
        let inputs, extraState = argsShape args
        obj "moo_call" [ "NAME", JVString name ] inputs extraState
    | VListLit args ->
        let inputs, extraState = argsShape args
        obj "moo_list" [] inputs extraState
    | VIndex(e, i) -> obj "moo_index" [] (v1 "VALUE" e @ v1 "INDEX" i) []
    | VRange(lo, hi) -> obj "moo_range" [] (v1 "LO" lo @ v1 "HI" hi) []
    | VFirstIndex -> obj "moo_firstindex" [] [] []
    | VLastIndex -> obj "moo_lastindex" [] [] []
    | VMapLit pairs ->
        let inputs =
            pairs |> List.mapi (fun i (k, v) -> v1 (sprintf "KEY%d" i) k @ v1 (sprintf "VAL%d" i) v) |> List.concat

        obj "moo_map" [] inputs []
    | VScatter _
    | VCatch _
    | VUnsupported _ -> obj "moo_unsupported_expr" [ "DEBUG", JVString(sprintf "%A" value) ] [] []

/// The reverse of `valueToJson` for the covered subset - `None` for a
/// block type this slice doesn't map (including `moo_unsupported_expr`,
/// which is one-way only: a defensive fallback `valueToJson` produces,
/// never something this function is expected to reconstruct).
let rec jsonToValue (json: JsonValue) : BlockValue option =
    let fields = field "fields" (asFields json) |> Option.map asFields |> Option.defaultValue []
    let inputs = field "inputs" (asFields json) |> Option.map asFields |> Option.defaultValue []
    let extraState = field "extraState" (asFields json) |> Option.map asFields |> Option.defaultValue []
    let blockType = field "type" (asFields json) |> Option.bind asString

    let inputBlock (name: string) : JsonValue option =
        field name inputs |> Option.map asFields |> Option.bind (field "block")

    let value1 (name: string) : BlockValue option = inputBlock name |> Option.bind jsonToValue

    // Walks ARG0, ARG1, ... until the first *missing* input, treating that
    // as "end of arguments" - matching the real Blockly-side blocks' fixed,
    // left-to-right-filled ARGi sockets (see BlocklyEditor.fs), rather than
    // depending on a separately-reported count that a real workspace save
    // has no reason to carry (see `argsShape`'s own comment above).
    let args () : BlockArg list option =
        let splices =
            field "splices" extraState |> Option.map asArray |> Option.map (List.choose asBool) |> Option.defaultValue []

        let rec loop (i: int) (acc: BlockArg list) : BlockArg list option =
            match inputBlock (sprintf "ARG%d" i) with
            | None -> Some(List.rev acc)
            | Some blockJson ->
                match jsonToValue blockJson with
                | None -> None
                | Some v ->
                    let isSplice = splices |> List.tryItem i |> Option.defaultValue false
                    loop (i + 1) ((if isSplice then ASplice v else AArg v) :: acc)

        loop 0 []

    // Walks KEY0/VAL0, KEY1/VAL1, ... until the first missing pair - same
    // "fixed, left-to-right-filled sockets" convention as `args()` above.
    let mapPairs () : (BlockValue * BlockValue) list option =
        let rec loop (i: int) (acc: (BlockValue * BlockValue) list) : (BlockValue * BlockValue) list option =
            match inputBlock (sprintf "KEY%d" i) with
            | None -> Some(List.rev acc)
            | Some keyJson ->
                match inputBlock (sprintf "VAL%d" i), jsonToValue keyJson with
                | Some valJson, Some k ->
                    match jsonToValue valJson with
                    | Some v -> loop (i + 1) ((k, v) :: acc)
                    | None -> None
                | _ -> None

        loop 0 []

    match blockType with
    | Some "moo_int" -> field "NUM" fields |> Option.bind asNumber |> Option.map (int64 >> VIntLit)
    | Some "moo_float" -> field "NUM" fields |> Option.bind asNumber |> Option.map VFloatLit
    | Some "moo_string" -> field "TEXT" fields |> Option.bind asString |> Option.map VStrLit
    | Some "moo_obj" -> field "NUM" fields |> Option.bind asNumber |> Option.map (int64 >> VObjLit)
    | Some "moo_err" -> field "CODE" fields |> Option.bind asString |> Option.map VErrLit
    | Some "moo_bool" -> field "BOOL" fields |> Option.bind asString |> Option.map (fun b -> VBoolLit(b = "TRUE"))
    | Some "moo_ident" -> field "NAME" fields |> Option.bind asString |> Option.map VIdent
    | Some "moo_binary" ->
        match field "OP" fields |> Option.bind asString |> Option.bind binOpOfName, value1 "LEFT", value1 "RIGHT" with
        | Some op, Some l, Some r -> Some(VBinary(op, l, r))
        | _ -> None
    | Some "moo_unary" ->
        match field "OP" fields |> Option.bind asString |> Option.bind unOpOfName, value1 "VALUE" with
        | Some op, Some e -> Some(VUnary(op, e))
        | _ -> None
    | Some "moo_cond" ->
        match value1 "COND", value1 "THEN", value1 "ELSE" with
        | Some c, Some t, Some f -> Some(VCond(c, t, f))
        | _ -> None
    | Some "moo_assign" ->
        match value1 "TARGET", value1 "VALUE" with
        | Some target, Some v -> Some(VAssign(target, v))
        | _ -> None
    | Some "moo_prop" ->
        match field "NAME" fields |> Option.bind asString, value1 "RECEIVER" with
        | Some name, Some r -> Some(VProp(r, name))
        | _ -> None
    | Some "moo_verbcall" ->
        match field "NAME" fields |> Option.bind asString, value1 "RECEIVER", args () with
        | Some name, Some r, Some a -> Some(VVerbCall(r, name, a))
        | _ -> None
    | Some "moo_call" ->
        match field "NAME" fields |> Option.bind asString, args () with
        | Some name, Some a -> Some(VCall(name, a))
        | _ -> None
    | Some "moo_list" -> args () |> Option.map VListLit
    | Some "moo_index" ->
        match value1 "VALUE", value1 "INDEX" with
        | Some e, Some i -> Some(VIndex(e, i))
        | _ -> None
    | Some "moo_range" ->
        match value1 "LO", value1 "HI" with
        | Some lo, Some hi -> Some(VRange(lo, hi))
        | _ -> None
    | Some "moo_firstindex" -> Some VFirstIndex
    | Some "moo_lastindex" -> Some VLastIndex
    | Some "moo_map" -> mapPairs () |> Option.map VMapLit
    | _ -> None

/// `BlockStmt` -> Blockly's real per-block JSON shape, total, WITHOUT its
/// own `next` chain (the caller, `stmtsToJson`, wires that).
let rec private stmtToJsonFields (stmt: BlockStmt) : (string * JsonValue) list =
    let v1 name value = [ name, valueSlot (valueToJson value) ]
    let stmtSlot name body = body |> stmtsToJson |> Option.map (fun j -> [ name, JVObject [ "block", j ] ]) |> Option.defaultValue []

    match stmt with
    | SIf(arms, elsePart) ->
        match arms with
        | [] -> makeBlock "moo_unsupported" [ "DEBUG", JVString "SIf with no arms" ] [] []
        | (cond, thenBody) :: restArms ->
            let elseBody = if restArms.IsEmpty then elsePart else Some [ SIf(restArms, elsePart) ]
            makeBlock "moo_if" [] (v1 "COND" cond @ stmtSlot "THEN" thenBody @ (elseBody |> Option.map (stmtSlot "ELSE") |> Option.defaultValue [])) []
    | SWhile(name, cond, body) ->
        makeBlock "moo_while" [ "LABEL", JVString(name |> Option.defaultValue "") ] (v1 "COND" cond @ stmtSlot "BODY" body) []
    | SReturn v ->
        makeBlock "moo_return" [] (v |> Option.map (v1 "VALUE") |> Option.defaultValue []) []
    | SBreak name -> makeBlock "moo_break" [ "LABEL", JVString(name |> Option.defaultValue "") ] [] []
    | SContinue name -> makeBlock "moo_continue" [ "LABEL", JVString(name |> Option.defaultValue "") ] [] []
    | SExpr v -> makeBlock "moo_expr" [] (v1 "VALUE" v) []
    | SComment s -> makeBlock "moo_comment" [ "TEXT", JVString s ] [] []
    | SForList(var, indexVar, source, body) ->
        makeBlock
            "moo_forlist"
            [ "VAR", JVString var; "INDEXVAR", JVString(indexVar |> Option.defaultValue "") ]
            (v1 "SOURCE" source @ stmtSlot "BODY" body)
            []
    | SForRange(var, lo, hi, body) ->
        makeBlock "moo_forrange" [ "VAR", JVString var ] (v1 "LO" lo @ v1 "HI" hi @ stmtSlot "BODY" body) []
    | SFork(name, delay, body) ->
        makeBlock "moo_fork" [ "NAME", JVString(name |> Option.defaultValue "") ] (v1 "DELAY" delay @ stmtSlot "BODY" body) []
    | STryFinally(body, handler) -> makeBlock "moo_try_finally" [] (stmtSlot "BODY" body @ stmtSlot "HANDLER" handler) []
    | STryExcept _
    | SUnsupported _ -> makeBlock "moo_unsupported" [ "DEBUG", JVString(sprintf "%A" stmt) ] [] []

/// A statement sequence (a verb body, or any block's nested body) ->
/// Blockly's `next`-chained JSON shape - `None` for an empty list (Blockly
/// has no "empty statement" block; an empty slot is just absent).
and stmtsToJson (stmts: BlockStmt list) : JsonValue option =
    match stmts with
    | [] -> None
    | s :: rest ->
        let fields = stmtToJsonFields s
        let fields = fields @ (rest |> stmtsToJson |> Option.map (fun r -> [ "next", JVObject [ "block", r ] ]) |> Option.defaultValue [])
        Some(JVObject fields)

/// The reverse of `stmtsToJson` - walks one block plus its `next` chain
/// into a `BlockStmt list`. `None` if any block along the chain fails to
/// map (the covered-subset boundary - see the module's own doc comment).
let rec jsonToStmts (json: JsonValue) : BlockStmt list option =
    let fields = asFields json
    let blockType = field "type" fields |> Option.bind asString
    let inputs = field "inputs" fields |> Option.map asFields |> Option.defaultValue []
    let inputBlock (name: string) : JsonValue option = field name inputs |> Option.map asFields |> Option.bind (field "block")
    let value1 (name: string) : BlockValue option = inputBlock name |> Option.bind jsonToValue
    let body (name: string) : BlockStmt list option = inputBlock name |> Option.map jsonToStmts |> Option.defaultValue (Some [])
    let strField (name: string) : string option = field "fields" fields |> Option.map asFields |> Option.bind (field name) |> Option.bind asString
    let optStrField (name: string) : string option = match strField name with Some "" -> None | other -> other

    let this: BlockStmt option =
        match blockType with
        | Some "moo_if" ->
            match value1 "COND", body "THEN" with
            | Some cond, Some thenBody ->
                match body "ELSE" with
                | Some elseBody -> Some(SIf([ (cond, thenBody) ], if elseBody.IsEmpty then None else Some elseBody))
                | None -> None
            | _ -> None
        | Some "moo_while" ->
            match value1 "COND", body "BODY" with
            | Some cond, Some b -> Some(SWhile(optStrField "LABEL", cond, b))
            | _ -> None
        | Some "moo_return" -> Some(SReturn(value1 "VALUE"))
        | Some "moo_break" -> Some(SBreak(optStrField "LABEL"))
        | Some "moo_continue" -> Some(SContinue(optStrField "LABEL"))
        | Some "moo_expr" -> value1 "VALUE" |> Option.map SExpr
        | Some "moo_comment" -> strField "TEXT" |> Option.map SComment
        | Some "moo_forlist" ->
            match strField "VAR", value1 "SOURCE", body "BODY" with
            | Some var, Some source, Some b -> Some(SForList(var, optStrField "INDEXVAR", source, b))
            | _ -> None
        | Some "moo_forrange" ->
            match strField "VAR", value1 "LO", value1 "HI", body "BODY" with
            | Some var, Some lo, Some hi, Some b -> Some(SForRange(var, lo, hi, b))
            | _ -> None
        | Some "moo_fork" ->
            match value1 "DELAY", body "BODY" with
            | Some delay, Some b -> Some(SFork(optStrField "NAME", delay, b))
            | _ -> None
        | Some "moo_try_finally" ->
            match body "BODY", body "HANDLER" with
            | Some b, Some h -> Some(STryFinally(b, h))
            | _ -> None
        | _ -> None

    match this with
    | None -> None
    | Some s ->
        match field "next" fields |> Option.map asFields |> Option.bind (field "block") with
        | None -> Some [ s ]
        | Some nextJson -> jsonToStmts nextJson |> Option.map (fun rest -> s :: rest)

// ---------------------------------------------------------------------------
// JsonValue <-> JSON text. Needed so `Client/BlocklyEditor.fs`'s entire
// obj<->JsonValue boundary can just be `JS.JSON.stringify`/`JS.JSON.parse`
// against a plain string, rather than a hand-written recursive walk of a
// real JS object's runtime type (`Array.isArray`/`typeof` introspection) -
// keeping that unavoidably environment-specific glue as small as possible,
// and keeping the one non-trivial piece (actually parsing/serializing JSON)
// in this pure, `dotnet test`-able module instead.
// ---------------------------------------------------------------------------

let private escapeJsonChar (c: char) : string =
    match c with
    | '"' -> "\\\""
    | '\\' -> "\\\\"
    | '\n' -> "\\n"
    | '\r' -> "\\r"
    | '\t' -> "\\t"
    | c when int c < 0x20 -> sprintf "\\u%04x" (int c)
    | c -> string c

let private escapeJsonString (s: string) : string =
    "\"" + (s |> Seq.map escapeJsonChar |> String.concat "") + "\""

/// Whole numbers print without a trailing `.0` (`42`, not `42.0`) - JSON
/// (and JS) makes no int/float distinction, so either is equally valid on
/// the wire; the plain form is just more natural for the common integer
/// case (most `NUM` fields).
let private numberText (n: float) : string =
    if System.Double.IsNaN n || System.Double.IsInfinity n then
        "0"
    elif n = System.Math.Floor n && abs n < 1e15 then
        string (int64 n)
    else
        n.ToString(System.Globalization.CultureInfo.InvariantCulture)

let rec toJsonText (v: JsonValue) : string =
    match v with
    | JVString s -> escapeJsonString s
    | JVNumber n -> numberText n
    | JVBool b -> if b then "true" else "false"
    | JVNull -> "null"
    | JVArray xs -> "[" + (xs |> List.map toJsonText |> String.concat ",") + "]"
    | JVObject fs -> "{" + (fs |> List.map (fun (k, v) -> escapeJsonString k + ":" + toJsonText v) |> String.concat ",") + "}"

/// A small recursive-descent JSON parser - `None` on any malformed input
/// (including trailing garbage after a well-formed value) rather than
/// throwing, since a caller (a Blockly workspace save, in practice) should
/// treat "couldn't parse this" as just another "not representable" case.
let parseJsonText (text: string) : JsonValue option =
    let len = text.Length
    let mutable pos = 0
    let peek () = if pos < len then Some text.[pos] else None
    let advance () =
        let c = text.[pos]
        pos <- pos + 1
        c

    let skipWs () =
        while pos < len && (text.[pos] = ' ' || text.[pos] = '\t' || text.[pos] = '\n' || text.[pos] = '\r') do
            pos <- pos + 1

    let expect (c: char) : bool =
        if peek () = Some c then
            advance () |> ignore
            true
        else
            false

    let startsWith (word: string) : bool = pos + word.Length <= len && text.Substring(pos, word.Length) = word

    let rec parseValue () : JsonValue option =
        skipWs ()
        match peek () with
        | Some '{' -> parseObject ()
        | Some '[' -> parseArray ()
        | Some '"' -> parseString () |> Option.map JVString
        | Some 't' when startsWith "true" -> pos <- pos + 4; Some(JVBool true)
        | Some 'f' when startsWith "false" -> pos <- pos + 5; Some(JVBool false)
        | Some 'n' when startsWith "null" -> pos <- pos + 4; Some JVNull
        | Some c when c = '-' || System.Char.IsDigit c -> parseNumber ()
        | _ -> None

    and parseObject () : JsonValue option =
        advance () |> ignore // '{'
        skipWs ()

        if expect '}' then
            Some(JVObject [])
        else
            let fields = ResizeArray()

            let rec loop () =
                skipWs ()

                match parseString () with
                | None -> None
                | Some key ->
                    skipWs ()

                    if not (expect ':') then
                        None
                    else
                        match parseValue () with
                        | None -> None
                        | Some v ->
                            fields.Add(key, v)
                            skipWs ()

                            if expect ',' then loop ()
                            elif expect '}' then Some(JVObject(List.ofSeq fields))
                            else None

            loop ()

    and parseArray () : JsonValue option =
        advance () |> ignore // '['
        skipWs ()

        if expect ']' then
            Some(JVArray [])
        else
            let items = ResizeArray()

            let rec loop () =
                match parseValue () with
                | None -> None
                | Some v ->
                    items.Add v
                    skipWs ()

                    if expect ',' then loop ()
                    elif expect ']' then Some(JVArray(List.ofSeq items))
                    else None

            loop ()

    and parseString () : string option =
        skipWs ()

        if peek () <> Some '"' then
            None
        else
            advance () |> ignore
            let sb = System.Text.StringBuilder()

            let rec loop () =
                match peek () with
                | None -> None
                | Some '"' ->
                    advance () |> ignore
                    Some(sb.ToString())
                | Some '\\' ->
                    advance () |> ignore

                    match peek () with
                    | Some 'n' -> advance () |> ignore; sb.Append('\n') |> ignore; loop ()
                    | Some 'r' -> advance () |> ignore; sb.Append('\r') |> ignore; loop ()
                    | Some 't' -> advance () |> ignore; sb.Append('\t') |> ignore; loop ()
                    | Some 'b' -> advance () |> ignore; sb.Append('\b') |> ignore; loop ()
                    | Some 'f' -> advance () |> ignore; sb.Append('\f') |> ignore; loop ()
                    | Some '"' -> advance () |> ignore; sb.Append('"') |> ignore; loop ()
                    | Some '\\' -> advance () |> ignore; sb.Append('\\') |> ignore; loop ()
                    | Some '/' -> advance () |> ignore; sb.Append('/') |> ignore; loop ()
                    | Some 'u' when pos + 5 <= len ->
                        advance () |> ignore
                        let hex = text.Substring(pos, 4)
                        pos <- pos + 4

                        match System.Int32.TryParse(hex, System.Globalization.NumberStyles.HexNumber, System.Globalization.CultureInfo.InvariantCulture) with
                        | true, code ->
                            sb.Append(char code) |> ignore
                            loop ()
                        | false, _ -> None
                    | _ -> None
                | Some c ->
                    advance () |> ignore
                    sb.Append(c) |> ignore
                    loop ()

            loop ()

    and parseNumber () : JsonValue option =
        let start = pos
        if peek () = Some '-' then advance () |> ignore

        while pos < len && System.Char.IsDigit text.[pos] do
            pos <- pos + 1

        if peek () = Some '.' then
            advance () |> ignore

            while pos < len && System.Char.IsDigit text.[pos] do
                pos <- pos + 1

        match peek () with
        | Some 'e'
        | Some 'E' ->
            advance () |> ignore

            match peek () with
            | Some '+'
            | Some '-' -> advance () |> ignore
            | _ -> ()

            while pos < len && System.Char.IsDigit text.[pos] do
                pos <- pos + 1
        | _ -> ()

        let numText = text.Substring(start, pos - start)

        match
            System.Double.TryParse(numText, System.Globalization.NumberStyles.Float, System.Globalization.CultureInfo.InvariantCulture)
        with
        | true, n -> Some(JVNumber n)
        | false, _ -> None

    match parseValue () with
    | None -> None
    | Some v ->
        skipWs ()
        if pos = len then Some v else None

/// Whether every construct in `blocks` maps to a *real* Blockly block type
/// this module defines - not `Blocks.isFullyRepresentable` (which reflects
/// `Blocks.fs`'s own, wider coverage: `ForList`/`ForRange`/`Fork`/
/// `TryExcept`/`TryFinally`/`Scatter`/`Catch` all have real `BlockStmt`/
/// `BlockValue` cases there). This module's own JS-side block set (see
/// `Client/BlocklyEditor.fs`) is deliberately smaller for this slice, so a
/// verb `Blocks.isFullyRepresentable` calls fully representable can still
/// contain a construct with **no registered Blockly block type at all** -
/// confirmed live: loading a `moo_unsupported`/`moo_unsupported_expr`
/// placeholder (what `stmtToJsonFields`/`valueToJson`'s own fallback arms
/// produce for exactly this gap) into a real workspace throws `Invalid
/// block definition for type: moo_unsupported`, not a graceful no-op. A
/// real toggle needs to gate on *this* check, not `isFullyRepresentable`
/// alone, before ever calling `stmtsToJson`/`loadStateText`.
let rec private valueIsFullyMappable (value: BlockValue) : bool =
    match value with
    | VScatter _
    | VCatch _
    | VUnsupported _ -> false
    | VIntLit _
    | VFloatLit _
    | VStrLit _
    | VObjLit _
    | VErrLit _
    | VBoolLit _
    | VIdent _
    | VFirstIndex
    | VLastIndex -> true
    | VBinary(_, l, r) -> valueIsFullyMappable l && valueIsFullyMappable r
    | VUnary(_, e) -> valueIsFullyMappable e
    | VCond(c, t, f) -> valueIsFullyMappable c && valueIsFullyMappable t && valueIsFullyMappable f
    | VAssign(target, v) -> valueIsFullyMappable target && valueIsFullyMappable v
    | VProp(receiver, _) -> valueIsFullyMappable receiver
    | VVerbCall(receiver, _, args) -> valueIsFullyMappable receiver && args |> List.forall argIsFullyMappable
    | VCall(_, args) -> args |> List.forall argIsFullyMappable
    | VListLit args -> args |> List.forall argIsFullyMappable
    | VIndex(e, i) -> valueIsFullyMappable e && valueIsFullyMappable i
    | VRange(lo, hi) -> valueIsFullyMappable lo && valueIsFullyMappable hi
    | VMapLit pairs -> pairs |> List.forall (fun (k, v) -> valueIsFullyMappable k && valueIsFullyMappable v)

and private argIsFullyMappable (arg: BlockArg) : bool =
    match arg with
    | AArg v
    | ASplice v -> valueIsFullyMappable v

let rec private stmtIsFullyMappable (stmt: BlockStmt) : bool =
    match stmt with
    | STryExcept _
    | SUnsupported _ -> false
    | SBreak _
    | SContinue _
    | SComment _ -> true
    | SIf(arms, elsePart) ->
        (arms |> List.forall (fun (c, body) -> valueIsFullyMappable c && body |> List.forall stmtIsFullyMappable))
        && (elsePart |> Option.forall (List.forall stmtIsFullyMappable))
    | SWhile(_, cond, body) -> valueIsFullyMappable cond && body |> List.forall stmtIsFullyMappable
    | SReturn v -> v |> Option.forall valueIsFullyMappable
    | SExpr v -> valueIsFullyMappable v
    | SForList(_, _, source, body) -> valueIsFullyMappable source && body |> List.forall stmtIsFullyMappable
    | SForRange(_, lo, hi, body) -> valueIsFullyMappable lo && valueIsFullyMappable hi && body |> List.forall stmtIsFullyMappable
    | SFork(_, delay, body) -> valueIsFullyMappable delay && body |> List.forall stmtIsFullyMappable
    | STryFinally(body, handler) -> (body |> List.forall stmtIsFullyMappable) && (handler |> List.forall stmtIsFullyMappable)

/// Whether every construct in `blocks` (already converted via
/// `Blocks.astToBlocks`) has a real, registered Blockly block type -
/// see this function's own doc comment above for why this is a separate,
/// necessary check from `Blocks.isFullyRepresentable`.
let isFullyMappable (blocks: BlockStmt list) : bool = blocks |> List.forall stmtIsFullyMappable
