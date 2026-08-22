/// Round-trip proof for `Language.Blocks` - the spike for the "Google
/// Blocks mode for visual coding" card. Mirrors `SugarTests.fs`'s own
/// convention (a per-construct `[<Fact>]`, real code shapes, an explicit
/// escape-hatch case) but compares typed `Ast` values directly rather than
/// text, since `Blocks.fs` maps `Ast.Stmt`/`Ast.Expr` <-> `BlockStmt`/
/// `BlockValue`, not text <-> text.
///
/// Every fixture below is hand-constructed with every position field set to
/// `1, 1` throughout, matching the fixed placeholder `blockToExpr` itself
/// emits for any node it reconstructs (see `Blocks.fs`'s own doc comment on
/// why a block has no real source position to recover) - so plain `=`
/// equality holds exactly, with no separate position-normalizing
/// comparison helper needed.
module Language.Tests.BlocksTests

open Xunit
open Language.Ast
open Language.Blocks

let private assertRoundTrips (stmts: Stmt list) =
    Assert.Equal<Stmt list>(stmts, blocksToAst (astToBlocks stmts))

let private id_ name = Ident(name, 1, 1)

[<Fact>]
let ``every literal kind round-trips`` () =
    assertRoundTrips
        [ ExprStmt(IntLit 42L)
          ExprStmt(FloatLit 3.5)
          ExprStmt(StrLit "hello")
          ExprStmt(ObjLit 7L)
          ExprStmt(ErrLit "E_PERM") ]

[<Fact>]
let ``true/false round-trip as literal-like blocks (stated spike assumption)`` () =
    assertRoundTrips [ ExprStmt(id_ "true"); ExprStmt(id_ "false") ]

[<Fact>]
let ``a plain identifier round-trips`` () =
    assertRoundTrips [ ExprStmt(id_ "x") ]

[<Fact>]
let ``every binary operator round-trips`` () =
    let ops =
        [ Add; Sub; Mul; Div; Mod; Pow; Eq; NotEq; Lt; LtEq; Gt; GtEq; And; Or; In; BitAnd; BitOr; BitXor; Shl; Shr ]

    assertRoundTrips [ for op in ops -> ExprStmt(Binary(op, IntLit 1L, IntLit 2L)) ]

[<Fact>]
let ``every unary operator round-trips`` () =
    assertRoundTrips [ for op in [ Neg; Not; BitNot ] -> ExprStmt(Unary(op, id_ "x")) ]

[<Fact>]
let ``a ternary round-trips`` () =
    assertRoundTrips [ ExprStmt(Cond(id_ "x", IntLit 1L, IntLit 0L)) ]

[<Fact>]
let ``plain assignment round-trips`` () =
    assertRoundTrips [ ExprStmt(Assign(id_ "x", IntLit 1L)) ]

[<Fact>]
let ``literal-name property access round-trips`` () =
    assertRoundTrips [ ExprStmt(Prop(id_ "obj", StrLit "name", 1, 1)) ]

[<Fact>]
let ``literal-name verb call round-trips, including a splice argument`` () =
    assertRoundTrips [ ExprStmt(VerbCall(id_ "obj", StrLit "tell", [ Normal(StrLit "hi"); Splice(id_ "args") ], 1, 1)) ]

[<Fact>]
let ``a function call round-trips, including a splice argument`` () =
    assertRoundTrips [ ExprStmt(Call("tostr", [ Normal(id_ "x"); Splice(id_ "rest") ], 1, 1)) ]

[<Fact>]
let ``a list literal round-trips`` () =
    assertRoundTrips [ ExprStmt(ListLit [ Normal(IntLit 1L); Normal(IntLit 2L); Splice(id_ "more") ]) ]

[<Fact>]
let ``indexing round-trips`` () =
    assertRoundTrips [ ExprStmt(Index(id_ "lst", IntLit 1L)) ]

[<Fact>]
let ``if/elseif/else round-trips`` () =
    assertRoundTrips
        [ If(
              [ (Binary(Eq, id_ "x", IntLit 1L), [ ExprStmt(Assign(id_ "a", IntLit 1L)) ])
                (Binary(Eq, id_ "x", IntLit 2L), [ ExprStmt(Assign(id_ "a", IntLit 2L)) ]) ],
              Some [ ExprStmt(Assign(id_ "a", IntLit 0L)) ]
          ) ]

[<Fact>]
let ``while, with and without a loop label, round-trips`` () =
    assertRoundTrips
        [ While(None, id_ "cond", [ ExprStmt(Assign(id_ "x", IntLit 1L)) ])
          While(Some "outer", id_ "cond", [ Break(Some "outer") ]) ]

[<Fact>]
let ``return, with and without a value, round-trips`` () =
    assertRoundTrips [ Return(Some(id_ "x")); Return None ]

[<Fact>]
let ``break/continue, with and without a label, round-trip`` () =
    assertRoundTrips [ Break None; Break(Some "outer"); Continue None; Continue(Some "outer") ]

[<Fact>]
let ``a realistic multi-construct verb body round-trips`` () =
    // Nested if-inside-while, and a property access used as a verb-call
    // receiver - the kind of interaction a single-construct test wouldn't
    // exercise.
    assertRoundTrips
        [ While(
              Some "loop",
              Binary(Lt, id_ "i", IntLit 10L),
              [ If(
                    [ (Binary(Eq, Binary(Mod, id_ "i", IntLit 2L), IntLit 0L),
                       [ ExprStmt(VerbCall(Prop(id_ "player", StrLit "location", 1, 1), StrLit "tell", [ Normal(id_ "i") ], 1, 1)) ]) ],
                    None
                )
                ExprStmt(Assign(id_ "i", Binary(Add, id_ "i", IntLit 1L))) ]
          ) ]

[<Fact>]
let ``an out-of-subset statement (a for-loop) round-trips via the UnsupportedBlock escape hatch`` () =
    let forLoop =
        ForRange({ Name = "i"; Line = 1; Col = 1 }, IntLit 1L, IntLit 10L, [ ExprStmt(Assign(id_ "x", id_ "i")) ])

    match stmtToBlock forLoop with
    | SUnsupported s -> Assert.Equal<Stmt>(forLoop, s)
    | other -> Assert.Fail(sprintf "expected SUnsupported, got %A" other)

    assertRoundTrips [ forLoop ]

[<Fact>]
let ``an unsupported operand degrades to a leaf, not a whole-expression bailout`` () =
    // A catch-expression is out of subset - nested inside an otherwise
    // fully-supported binary expression, only that one operand should fall
    // back, proving the granular (not whole-statement) fallback design.
    let catchExpr = Catch(id_ "x", AnyCode, None)
    let stmt = ExprStmt(Binary(Add, IntLit 1L, catchExpr))

    match stmtToBlock stmt with
    | SExpr(VBinary(Add, VIntLit 1L, VUnsupported inner)) -> Assert.Equal<Expr>(catchExpr, inner)
    | other -> Assert.Fail(sprintf "expected a granular VUnsupported leaf, got %A" other)

    assertRoundTrips [ stmt ]

[<Fact>]
let ``an unsupported statement inside an if body degrades to a leaf, not the whole if`` () =
    // TryFinally is out of subset - sitting alongside a supported statement
    // in the same if-body, only that one statement line should fall back.
    let tryFinally = TryFinally([ ExprStmt(Assign(id_ "x", IntLit 1L)) ], [ ExprStmt(Call("cleanup", [], 1, 1)) ])
    let ifStmt = If([ (id_ "cond", [ ExprStmt(Assign(id_ "a", IntLit 1L)); tryFinally ]) ], None)

    match stmtToBlock ifStmt with
    | SIf([ (_, [ SExpr _; SUnsupported inner ]) ], None) -> Assert.Equal<Stmt>(tryFinally, inner)
    | other -> Assert.Fail(sprintf "expected the if's own shape with a granular SUnsupported leaf, got %A" other)

    assertRoundTrips [ ifStmt ]
