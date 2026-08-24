/// Round-trip proof for `Language.Printer`. Unlike `BlocksTests.fs` (which
/// compares hand-built `Stmt` values directly), these tests go through the
/// real pipeline both ways: real MOOcode text -> `Lexer.tokenize` ->
/// `Parser.parse` -> `Printer.print` -> re-tokenize/re-parse -> compare the
/// two `Stmt list` values. Since the printer's own line layout generally
/// differs from the original source, `Ident`/`Prop`/`VerbCall`/`Call`
/// position fields won't match between the two parses even when the trees
/// are otherwise identical - `zeroPositions` strips them before comparing,
/// same "AST-equivalence, not textual identity" stance `Sugar.fs` and
/// `BlocksTests.fs` already established.
///
/// Most cases below rely on the round trip itself to prove correct
/// parenthesization: if `Printer.fs` omitted a paren some construct
/// actually needs, the printed text would re-parse to a *different* tree
/// (different grouping/associativity), which the equality assertion
/// catches directly - no manual text inspection needed to prove a
/// precedence bug exists.
module Language.Tests.PrinterTests

open Xunit
open Language.Ast

let private parseOk (text: string) : Stmt list =
    match Language.Lexer.tokenize text with
    | { Error = Some err } -> failwithf "lex error: %A" err
    | { Tokens = tokens; Error = None } -> Language.Parser.parse tokens

let private zeroName (n: BoundName) : BoundName = { n with Line = 0; Col = 0 }

let rec private zeroExpr (expr: Expr) : Expr =
    match expr with
    | IntLit _
    | FloatLit _
    | StrLit _
    | ObjLit _
    | ErrLit _
    | FirstIndex
    | LastIndex -> expr
    | Ident(name, _, _) -> Ident(name, 0, 0)
    | Prop(o, n, _, _) -> Prop(zeroExpr o, zeroExpr n, 0, 0)
    | VerbCall(o, n, args, _, _) -> VerbCall(zeroExpr o, zeroExpr n, args |> List.map zeroArg, 0, 0)
    | Call(name, args, _, _) -> Call(name, args |> List.map zeroArg, 0, 0)
    | Index(a, b) -> Index(zeroExpr a, zeroExpr b)
    | Range(a, b) -> Range(zeroExpr a, zeroExpr b)
    | Binary(op, a, b) -> Binary(op, zeroExpr a, zeroExpr b)
    | Unary(op, a) -> Unary(op, zeroExpr a)
    | Cond(a, b, c) -> Cond(zeroExpr a, zeroExpr b, zeroExpr c)
    | Catch(a, codes, fb) -> Catch(zeroExpr a, zeroCodes codes, fb |> Option.map zeroExpr)
    | Assign(a, b) -> Assign(zeroExpr a, zeroExpr b)
    | Scatter(items, v) -> Scatter(items |> List.map zeroScatterItem, zeroExpr v)
    | ListLit args -> ListLit(args |> List.map zeroArg)
    | MapLit pairs -> MapLit(pairs |> List.map (fun (k, v) -> zeroExpr k, zeroExpr v))

and private zeroArg (arg: Arg) : Arg =
    match arg with
    | Normal e -> Normal(zeroExpr e)
    | Splice e -> Splice(zeroExpr e)

and private zeroCodes (codes: Codes) : Codes =
    match codes with
    | AnyCode -> AnyCode
    | Codes args -> Codes(args |> List.map zeroArg)

and private zeroScatterItem (item: ScatterItem) : ScatterItem =
    match item with
    | Required n -> Required(zeroName n)
    | Rest n -> Rest(zeroName n)
    | Optional(n, d) -> Optional(zeroName n, d |> Option.map zeroExpr)

let rec private zeroStmt (stmt: Stmt) : Stmt =
    match stmt with
    | If(arms, elsePart) ->
        If(arms |> List.map (fun (c, b) -> zeroExpr c, b |> List.map zeroStmt), elsePart |> Option.map (List.map zeroStmt))
    | ForList(v, i, src, b) -> ForList(zeroName v, i |> Option.map zeroName, zeroExpr src, b |> List.map zeroStmt)
    | ForRange(v, lo, hi, b) -> ForRange(zeroName v, zeroExpr lo, zeroExpr hi, b |> List.map zeroStmt)
    | While(n, c, b) -> While(n, zeroExpr c, b |> List.map zeroStmt)
    | Fork(n, d, b) -> Fork(n |> Option.map zeroName, zeroExpr d, b |> List.map zeroStmt)
    | TryExcept(b, arms) ->
        TryExcept(
            b |> List.map zeroStmt,
            arms
            |> List.map (fun a ->
                { a with
                    Name = a.Name |> Option.map zeroName
                    Codes = zeroCodes a.Codes
                    Body = a.Body |> List.map zeroStmt })
        )
    | TryFinally(b, h) -> TryFinally(b |> List.map zeroStmt, h |> List.map zeroStmt)
    | ExprStmt e -> ExprStmt(zeroExpr e)
    | Return e -> Return(e |> Option.map zeroExpr)
    | Break n -> Break n
    | Continue n -> Continue n
    | ErrorStmt(m, _, _) -> ErrorStmt(m, 0, 0)

let private zeroStmts (stmts: Stmt list) : Stmt list = stmts |> List.map zeroStmt

let private assertRoundTrips (text: string) =
    let original = parseOk text

    match Language.Printer.print original with
    | Error msg -> Assert.Fail(sprintf "print failed: %s\ninput was:\n%s" msg text)
    | Ok printed ->
        let reparsed = parseOk printed
        Assert.Equal<Stmt list>(zeroStmts original, zeroStmts reparsed)

// ---------------------------------------------------------------------------
// Precedence - the round trip itself is the proof: a missing/extra paren
// changes the reparsed tree's shape, which the equality assertion catches.
// ---------------------------------------------------------------------------

[<Fact>]
let ``binary operators at different tiers round-trip regardless of grouping`` () =
    assertRoundTrips "x = a + b * c;"
    assertRoundTrips "x = (a + b) * c;"
    assertRoundTrips "x = a || b && c;"
    assertRoundTrips "x = (a || b) && c;"

[<Fact>]
let ``pow is right-associative and binds looser than unary`` () =
    assertRoundTrips "x = 2 ^ 3 ^ 2;"
    assertRoundTrips "x = -y ^ 2;" // (-y) ^ 2
    assertRoundTrips "x = -(y ^ 2);" // unary applied to the whole pow - needs re-inserted parens

[<Fact>]
let ``a ternary nested in another ternary's condition needs parens`` () = assertRoundTrips "x = (c ? 1 | 0) ? 2 | 3;"

[<Fact>]
let ``chained assignment round-trips`` () = assertRoundTrips "x = y = 1;"

[<Fact>]
let ``the printer does not add parens it doesn't need`` () =
    let stmts = parseOk "x = -y ^ 2;"

    match Language.Printer.print stmts with
    | Error msg -> Assert.Fail(msg)
    | Ok printed -> Assert.DoesNotContain("(-y)", printed.Replace(" ", ""))

// ---------------------------------------------------------------------------
// Statement forms
// ---------------------------------------------------------------------------

[<Fact>]
let ``if-elseif-else round-trips`` () =
    assertRoundTrips "if (x == 1)\n  a = 1;\nelseif (x == 2)\n  a = 2;\nelse\n  a = 0;\nendif"

[<Fact>]
let ``both for-loop shapes round-trip`` () =
    assertRoundTrips "for x in (lst)\n  y = x;\nendfor"
    assertRoundTrips "for v, k in (map_expr)\n  y = v + k;\nendfor"
    assertRoundTrips "for x in [1..10]\n  y = x;\nendfor"

[<Fact>]
let ``labeled and unlabeled while round-trip`` () =
    assertRoundTrips "while (x < 5)\n  x = x + 1;\nendwhile"
    assertRoundTrips "while outer (x < 5)\n  break outer;\nendwhile"

[<Fact>]
let ``labeled and unlabeled fork round-trip`` () =
    assertRoundTrips "fork (1)\n  x = 1;\nendfork"
    assertRoundTrips "fork task_id (1)\n  x = task_id;\nendfork"

[<Fact>]
let ``try-except with multiple arms round-trips`` () =
    assertRoundTrips "try\n  x = 1 / 0;\nexcept e1 (E_DIV)\n  x = 0;\nexcept e2 (ANY)\n  x = -1;\nendtry"

[<Fact>]
let ``try-finally round-trips`` () = assertRoundTrips "try\n  do_something();\nfinally\n  cleanup();\nendtry"

[<Fact>]
let ``scatter-assignment with required-optional-rest round-trips`` () =
    assertRoundTrips "{a, ?b = 1, @c} = args;"

[<Fact>]
let ``catch-expression with and without a fallback round-trips`` () =
    assertRoundTrips "x = `1 / 0 ! ANY';"
    assertRoundTrips "x = `1 / 0 ! E_DIV => 0';"

[<Fact>]
let ``a realistic multi-construct verb body round-trips`` () =
    assertRoundTrips "if (x > 0)\n  for i in [1..10]\n    while (i < 5)\n      i = i + 1;\n    endwhile\n  endfor\nendif"

[<Fact>]
let ``print fails fast on an ErrorStmt rather than emitting bogus text`` () =
    match Language.Printer.print [ ErrorStmt("recovered after a parse failure", 1, 1) ] with
    | Error _ -> ()
    | Ok text -> Assert.Fail(sprintf "expected an Error, got: %s" text)

// ---------------------------------------------------------------------------
// Composition with Blocks.fs - proves the two slices work together: real
// text -> blocks -> back -> text -> re-parse yields the same AST.
// ---------------------------------------------------------------------------

[<Fact>]
let ``text through blocksToAst(astToBlocks(...)) through print still round-trips`` () =
    let text =
        "while outer (i < 10)\n  if (i % 2 == 0)\n    player.location:tell(i);\n  endif\n  i = i + 1;\nendwhile"

    let original = parseOk text
    let roundTripped = Language.Blocks.blocksToAst (Language.Blocks.astToBlocks original)

    match Language.Printer.print roundTripped with
    | Error msg -> Assert.Fail(sprintf "print failed: %s" msg)
    | Ok printed ->
        let reparsed = parseOk printed
        Assert.Equal<Stmt list>(zeroStmts original, zeroStmts reparsed)
