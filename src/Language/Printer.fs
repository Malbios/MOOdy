/// Turns a parsed `Language.Ast.Stmt list` back into real MOOcode source
/// text - the missing half `Blocks.fs` needs to ever actually save a
/// block-edited verb (`blocksToAst` only gets back to the typed tree;
/// `set_verb_code()` needs text). Unlike `Blocks.fs`, this covers the
/// *full* `Ast` grammar, not a restricted subset: generating code is the
/// mechanically easy direction (per the Blockly card's own research), and
/// `Blocks.fs`'s own `SUnsupported`/`VUnsupported` escape hatch carries the
/// real, original `Ast` node verbatim - so even a block tree with one
/// out-of-subset construct still needs this module to render it correctly.
///
/// The `Ast` carries no parenthesis metadata, so parens have to be
/// *re-inferred* structurally: `emitExpr` is a standard precedence-aware
/// printer, given the minimum precedence tier required at each position and
/// wrapping in parens only when a child binds looser than that. The tier
/// table mirrors `Parser.fs`'s own precedence-climbing chain exactly (see
/// that module for the grammar this must stay consistent with) - tightest
/// last: `Assign` (1, right) < `Cond` (2) < `Or`/`And` sharing one tier (3,
/// left - MOO is not C-like here) < comparisons (4) < bitwise (5) < shift
/// (6) < `Add`/`Sub` (7) < `Mul`/`Div`/`Mod` (8) < `Pow` (9, right) < unary
/// prefix (10 - binds tighter than `Pow`, so `-x^2` really is `(-x)^2`) <
/// postfix/primary (11 - property/verb/index access, plus every
/// self-delimited form: literals, `Catch`, `ListLit`, `MapLit`, which never
/// need external parens regardless of position). Where the exact grammar
/// tier for a sub-position isn't pinned down by that table (scatter
/// defaults, map/list elements, range bounds), this deliberately uses the
/// loosest tier (1) - it can only ever add a harmless extra paren, never a
/// wrong one.
///
/// Matches `Sugar.fs`'s own `Result`-returning contract. The only way this
/// can fail is an `ErrorStmt` reaching it - a parse-failure placeholder,
/// not reconstructable syntax - raised internally as a private exception
/// so the recursive descent itself doesn't need `Result`-threading, and
/// caught at the `print` boundary.
///
/// "Round trip" here means AST-equivalence once re-parsed, not textual
/// identity with whatever the verb originally looked like - the same
/// stance `Sugar.fs` already established project-wide. Indentation (2
/// spaces per nesting depth) is a new, explicit convention for this
/// module, not a reuse of an existing one - `Sugar.fs` itself has none to
/// borrow (it's purely subtractive/line-preserving), though it matches
/// every hand-written fixture already used throughout this project's own
/// tests.
module Language.Printer

open System.Globalization
open Language.Ast

exception private UnprintableError of string

let private indent (depth: int) : string = String.replicate (depth * 2) " "

let private binOpTier (op: BinOp) : int =
    match op with
    | Or
    | And -> 3
    | Eq
    | NotEq
    | Lt
    | LtEq
    | Gt
    | GtEq
    | In -> 4
    | BitOr
    | BitAnd
    | BitXor -> 5
    | Shl
    | Shr -> 6
    | Add
    | Sub -> 7
    | Mul
    | Div
    | Mod -> 8
    | Pow -> 9

let private binOpText (op: BinOp) : string =
    match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Pow -> "^"
    | Eq -> "=="
    | NotEq -> "!="
    | Lt -> "<"
    | LtEq -> "<="
    | Gt -> ">"
    | GtEq -> ">="
    | And -> "&&"
    | Or -> "||"
    | In -> "in"
    | BitAnd -> "&."
    | BitOr -> "|."
    | BitXor -> "^."
    | Shl -> "<<"
    | Shr -> ">>"

let private unOpText (op: UnOp) : string =
    match op with
    | Neg -> "-"
    | Not -> "!"
    | BitNot -> "~"

/// Precedence tier for the purpose of deciding whether a *child*
/// expression needs parens at some position - see the module's own doc
/// comment for the full table this mirrors.
let private exprTier (expr: Expr) : int =
    match expr with
    | Assign _ -> 1
    | Scatter _ -> 1
    | Cond _ -> 2
    | Binary(op, _, _) -> binOpTier op
    | Unary _ -> 10
    | _ -> 11

/// `f.ToString()` already round-trips the exact double value on modern
/// .NET, but can omit any `.`/`e` for a whole number (e.g. `3` for `3.0`) -
/// which would re-lex as `TInt`, not `TFloat` (`Lexer.fs`'s number scanner
/// only sets `isFloat` on seeing a literal `.` or `e`/`E`). Force one back
/// in when that happens.
let private floatText (f: float) : string =
    let s = f.ToString(CultureInfo.InvariantCulture)

    if s.IndexOfAny([| '.'; 'e'; 'E' |]) >= 0 then
        s
    else
        s + ".0"

/// `Lexer.fs`'s string scanner unescapes `\X` to plain `X` unconditionally
/// (no `\n` => newline, etc.) and never admits a raw newline into a `TStr`
/// at all - so `"` and `\` are the only characters that need re-escaping
/// for a value that actually came from real parsed source.
let private strText (s: string) : string =
    "\"" + s.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\""

let private wrap (needParens: bool) (s: string) : string =
    if needParens then "(" + s + ")" else s

let private emitBoundName (name: BoundName) : string = name.Name

let rec private emitExpr (minPrec: int) (expr: Expr) : string =
    let text =
        match expr with
        | IntLit n -> string n
        | FloatLit f -> floatText f
        | StrLit s -> strText s
        | ObjLit n -> "#" + string n
        | ErrLit e -> e
        | Ident(name, _, _) -> name
        | FirstIndex -> "$"
        | LastIndex -> "^"
        // `StrLit name` also covers the waif-property sugar `obj.:name` -
        // `Parser.fs` already normalizes that to `Prop(obj, StrLit
        // ":name", ...)` (the stored name literally carries its own
        // leading colon), so printing `receiver + "." + name` reproduces
        // `obj.:name` for free with no separate case needed.
        | Prop(receiver, StrLit name, _, _) -> emitExpr 11 receiver + "." + name
        | Prop(receiver, nameExpr, _, _) -> emitExpr 11 receiver + ".(" + emitExpr 1 nameExpr + ")"
        | VerbCall(receiver, StrLit name, args, _, _) -> emitExpr 11 receiver + ":" + name + "(" + emitArgs args + ")"
        | VerbCall(receiver, nameExpr, args, _, _) ->
            emitExpr 11 receiver + ":(" + emitExpr 1 nameExpr + ")(" + emitArgs args + ")"
        | Call(name, args, _, _) -> name + "(" + emitArgs args + ")"
        | Index(e, i) -> emitExpr 11 e + "[" + emitExpr 1 i + "]"
        | Range(lo, hi) -> emitExpr 1 lo + ".." + emitExpr 1 hi
        | Binary(op, l, r) ->
            let tier = binOpTier op
            let lMin, rMin = if op = Pow then (tier + 1, tier) else (tier, tier + 1)
            emitExpr lMin l + " " + binOpText op + " " + emitExpr rMin r
        | Unary(op, e) -> unOpText op + emitExpr 10 e
        | Cond(c, t, f) -> emitExpr 3 c + " ? " + emitExpr 1 t + " | " + emitExpr 1 f
        | Catch(e, codes, fallback) ->
            let fallbackText =
                fallback |> Option.map (fun f -> " => " + emitExpr 1 f) |> Option.defaultValue ""

            "`" + emitExpr 1 e + " ! " + emitCodes codes + fallbackText + "'"
        | Assign(target, value) -> emitExpr 2 target + " = " + emitExpr 1 value
        | Scatter(items, value) -> "{" + emitScatterItems items + "} = " + emitExpr 1 value
        | ListLit args -> "{" + emitArgs args + "}"
        | MapLit pairs ->
            "["
            + (pairs |> List.map (fun (k, v) -> emitExpr 1 k + " -> " + emitExpr 1 v) |> String.concat ", ")
            + "]"

    wrap (exprTier expr < minPrec) text

and private emitArg (arg: Arg) : string =
    match arg with
    | Normal e -> emitExpr 1 e
    | Splice e -> "@" + emitExpr 1 e

and private emitArgs (args: Arg list) : string = args |> List.map emitArg |> String.concat ", "

and private emitCodes (codes: Codes) : string =
    match codes with
    | AnyCode -> "ANY"
    | Codes args -> emitArgs args

and private emitScatterItems (items: ScatterItem list) : string =
    items
    |> List.map (fun item ->
        match item with
        | Required name -> emitBoundName name
        | Rest name -> "@" + emitBoundName name
        | Optional(name, None) -> "?" + emitBoundName name
        | Optional(name, Some def) -> "?" + emitBoundName name + " = " + emitExpr 1 def)
    |> String.concat ", "

let rec private emitStmts (depth: int) (stmts: Stmt list) : string =
    stmts |> List.map (emitStmt depth) |> String.concat "\n"

and private emitStmt (depth: int) (stmt: Stmt) : string =
    let pad = indent depth
    let body b = emitStmts (depth + 1) b

    match stmt with
    | If(arms, elsePart) ->
        let armsText =
            arms
            |> List.mapi (fun i (cond, armBody) ->
                let kw = if i = 0 then "if" else "elseif"
                sprintf "%s%s (%s)\n%s" pad kw (emitExpr 1 cond) (body armBody))
            |> String.concat "\n"

        let elseText =
            match elsePart with
            | Some elseBody -> sprintf "\n%selse\n%s" pad (body elseBody)
            | None -> ""

        sprintf "%s%s\n%sendif" armsText elseText pad
    | ForList(var, None, src, loopBody) ->
        sprintf "%sfor %s in (%s)\n%s\n%sendfor" pad (emitBoundName var) (emitExpr 1 src) (body loopBody) pad
    | ForList(var, Some idx, src, loopBody) ->
        sprintf
            "%sfor %s, %s in (%s)\n%s\n%sendfor"
            pad
            (emitBoundName var)
            (emitBoundName idx)
            (emitExpr 1 src)
            (body loopBody)
            pad
    | ForRange(var, lo, hi, loopBody) ->
        sprintf
            "%sfor %s in [%s..%s]\n%s\n%sendfor"
            pad
            (emitBoundName var)
            (emitExpr 1 lo)
            (emitExpr 1 hi)
            (body loopBody)
            pad
    | While(name, cond, loopBody) ->
        let label = name |> Option.map (fun n -> n + " ") |> Option.defaultValue ""
        sprintf "%swhile %s(%s)\n%s\n%sendwhile" pad label (emitExpr 1 cond) (body loopBody) pad
    | Fork(name, delay, loopBody) ->
        let label = name |> Option.map (fun n -> emitBoundName n + " ") |> Option.defaultValue ""
        sprintf "%sfork %s(%s)\n%s\n%sendfork" pad label (emitExpr 1 delay) (body loopBody) pad
    | TryExcept(tryBody, arms) ->
        let armsText =
            arms
            |> List.map (fun arm ->
                let label = arm.Name |> Option.map (fun n -> emitBoundName n + " ") |> Option.defaultValue ""
                sprintf "%sexcept %s(%s)\n%s" pad label (emitCodes arm.Codes) (body arm.Body))
            |> String.concat "\n"

        sprintf "%stry\n%s\n%s\n%sendtry" pad (body tryBody) armsText pad
    | TryFinally(tryBody, handler) ->
        sprintf "%stry\n%s\n%sfinally\n%s\n%sendtry" pad (body tryBody) pad (body handler) pad
    | ExprStmt e -> sprintf "%s%s;" pad (emitExpr 1 e)
    | Return(Some e) -> sprintf "%sreturn %s;" pad (emitExpr 1 e)
    | Return None -> sprintf "%sreturn;" pad
    | Break(Some name) -> sprintf "%sbreak %s;" pad name
    | Break None -> sprintf "%sbreak;" pad
    | Continue(Some name) -> sprintf "%scontinue %s;" pad name
    | Continue None -> sprintf "%scontinue;" pad
    | ErrorStmt(msg, line, col) ->
        raise (UnprintableError(sprintf "cannot print an ErrorStmt (%s at %d:%d)" msg line col))

/// `Ast.Stmt list` -> real MOOcode source text. `Error` only when the tree
/// contains an `ErrorStmt` (see the module doc comment).
let print (stmts: Stmt list) : Result<string, string> =
    try
        Ok(emitStmts 0 stmts)
    with UnprintableError msg ->
        Error msg
