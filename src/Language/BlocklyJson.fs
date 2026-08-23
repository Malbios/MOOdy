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
/// width (see that module's own doc comment for the full list) - just
/// enough to prove the real Blockly pipeline end-to-end: every literal
/// kind, `Ident`, `Binary`/`Unary` (one Blockly block type per family with
/// an `OP` field, not one block type per operator), `Cond`, `Assign`,
/// `Prop`/`VerbCall`/`Call`, `ListLit`, `Index`, `If`, `While`,
/// `Return`/`Break`/`Continue`, `ExprStmt`, `SComment`. Anything else maps
/// one-way to a `moo_unsupported`/`moo_unsupported_expr` placeholder
/// carrying a debug string - not meant to ever actually reach a real
/// workspace in practice, since the eventual UI gates the toggle on
/// `Blocks.isFullyRepresentable` first; it exists only so this module's
/// functions stay total.
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
/// - `Call`/`VerbCall`/`ListLit`'s argument lists really are variable-arity
///   with no equivalent fixed-shape trick, so they use a real `extraState`
///   (`{count = N; splices = [bool list]}`) the way real Blockly mutators
///   do, with `inputs.ARG0`..`ARG(N-1)` as the actual argument sockets.
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

    let extraState =
        [ "count", JVNumber(float args.Length); "splices", JVArray(args |> List.map (isSplice >> JVBool)) ]

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
    | VMapLit _
    | VRange _
    | VFirstIndex
    | VLastIndex
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

    let args () : BlockArg list option =
        let count = field "count" extraState |> Option.bind asNumber |> Option.map int |> Option.defaultValue 0

        let splices =
            field "splices" extraState |> Option.map asArray |> Option.map (List.choose asBool) |> Option.defaultValue []

        [ 0 .. count - 1 ]
        |> List.map (fun i ->
            let isSplice = splices |> List.tryItem i |> Option.defaultValue false
            inputBlock (sprintf "ARG%d" i) |> Option.bind jsonToValue |> Option.map (fun v -> if isSplice then ASplice v else AArg v))
        |> fun opts -> if opts |> List.forall Option.isSome then Some(opts |> List.choose id) else None

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
    | SForList _
    | SForRange _
    | SFork _
    | STryExcept _
    | STryFinally _
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
            let label = field "fields" fields |> Option.map asFields |> Option.bind (field "LABEL") |> Option.bind asString
            match value1 "COND", body "BODY" with
            | Some cond, Some b -> Some(SWhile((if label = Some "" then None else label), cond, b))
            | _ -> None
        | Some "moo_return" -> Some(SReturn(value1 "VALUE"))
        | Some "moo_break" ->
            let label = field "fields" fields |> Option.map asFields |> Option.bind (field "LABEL") |> Option.bind asString
            Some(SBreak(if label = Some "" then None else label))
        | Some "moo_continue" ->
            let label = field "fields" fields |> Option.map asFields |> Option.bind (field "LABEL") |> Option.bind asString
            Some(SContinue(if label = Some "" then None else label))
        | Some "moo_expr" -> value1 "VALUE" |> Option.map SExpr
        | Some "moo_comment" ->
            field "fields" fields |> Option.map asFields |> Option.bind (field "TEXT") |> Option.bind asString |> Option.map SComment
        | _ -> None

    match this with
    | None -> None
    | Some s ->
        match field "next" fields |> Option.map asFields |> Option.bind (field "block") with
        | None -> Some [ s ]
        | Some nextJson -> jsonToStmts nextJson |> Option.map (fun rest -> s :: rest)
