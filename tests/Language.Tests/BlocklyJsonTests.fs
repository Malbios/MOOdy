/// Round-trip proof for `Language.BlocklyJson` - `Ast -> BlockStmt ->
/// BlocklyJson -> BlockStmt -> Ast` for the deliberately smaller construct
/// set this module covers (see its own doc comment). Mirrors
/// `BlocksTests.fs`'s own convention: hand-built fixtures with every
/// position field at `1, 1`, matching the fixed placeholder `Blocks.fs`'s
/// reverse direction already emits, so plain `=` equality holds with no
/// separate normalizer.
module Language.Tests.BlocklyJsonTests

open Xunit
open Language.Ast
open Language.Blocks
open Language.BlocklyJson

let private id_ name = Ident(name, 1, 1)
let private bn name : BoundName = { Name = name; Line = 1; Col = 1 }

let private assertRoundTrips (stmts: Stmt list) =
    match stmts |> astToBlocks |> stmtsToJson with
    | None -> Assert.Fail("stmtsToJson returned None for a non-empty statement list")
    | Some json ->
        match jsonToStmts json with
        | None -> Assert.Fail("jsonToStmts failed to reconstruct a block list this slice should cover")
        | Some blocks -> Assert.Equal<Stmt list>(stmts, blocksToAst blocks)

[<Fact>]
let ``every literal kind round-trips through the full json pipeline`` () =
    assertRoundTrips
        [ ExprStmt(IntLit 42L)
          ExprStmt(FloatLit 3.5)
          ExprStmt(StrLit "hello")
          ExprStmt(ObjLit 7L)
          ExprStmt(ErrLit "E_PERM")
          ExprStmt(id_ "true")
          ExprStmt(id_ "false")
          ExprStmt(id_ "x") ]

[<Fact>]
let ``binary and unary operators round-trip`` () =
    assertRoundTrips
        [ ExprStmt(Binary(Add, IntLit 1L, IntLit 2L))
          ExprStmt(Binary(And, id_ "a", id_ "b"))
          ExprStmt(Unary(Neg, id_ "x"))
          ExprStmt(Unary(Not, id_ "y")) ]

[<Fact>]
let ``ternary and assignment round-trip`` () =
    assertRoundTrips [ ExprStmt(Cond(id_ "x", IntLit 1L, IntLit 0L)); ExprStmt(Assign(id_ "x", IntLit 1L)) ]

[<Fact>]
let ``property access, verb calls, and function calls round-trip, including splice args`` () =
    assertRoundTrips
        [ ExprStmt(Prop(id_ "obj", StrLit "name", 1, 1))
          ExprStmt(VerbCall(id_ "obj", StrLit "tell", [ Normal(StrLit "hi"); Splice(id_ "args") ], 1, 1))
          ExprStmt(Call("tostr", [ Normal(id_ "x"); Splice(id_ "rest") ], 1, 1)) ]

[<Fact>]
let ``a list literal and indexing round-trip`` () =
    assertRoundTrips
        [ ExprStmt(ListLit [ Normal(IntLit 1L); Normal(IntLit 2L); Splice(id_ "more") ])
          ExprStmt(Index(id_ "lst", IntLit 1L)) ]

[<Fact>]
let ``a multi-arm if reshapes into nested if/else through the json pipeline, but stays behaviorally equivalent`` () =
    // Documented in BlocklyJson.fs's own doc comment: Blockly's real
    // serialized shape has no elseif-chain construct, so a multi-arm SIf
    // round-trips as nested single-arm ifs (arm2 nested inside arm1's own
    // else slot) rather than flattened back to one multi-arm SIf - nested
    // if/else behaves identically to elseif at runtime, so this is an
    // accepted reshaping, not a correctness bug (same "AST-equivalent, not
    // shape-identical" stance Sugar.fs/Blocks.fs already take elsewhere).
    let original =
        [ If(
              [ (Binary(Eq, id_ "x", IntLit 1L), [ ExprStmt(Assign(id_ "a", IntLit 1L)) ])
                (Binary(Eq, id_ "x", IntLit 2L), [ ExprStmt(Assign(id_ "a", IntLit 2L)) ]) ],
              Some [ ExprStmt(Assign(id_ "a", IntLit 0L)) ]
          ) ]

    let expectedAfterRoundTrip =
        [ If(
              [ (Binary(Eq, id_ "x", IntLit 1L), [ ExprStmt(Assign(id_ "a", IntLit 1L)) ]) ],
              Some [ If([ (Binary(Eq, id_ "x", IntLit 2L), [ ExprStmt(Assign(id_ "a", IntLit 2L)) ]) ], Some [ ExprStmt(Assign(id_ "a", IntLit 0L)) ]) ]
          ) ]

    match original |> astToBlocks |> stmtsToJson with
    | None -> Assert.Fail("stmtsToJson returned None for a non-empty statement list")
    | Some json ->
        match jsonToStmts json with
        | None -> Assert.Fail("jsonToStmts failed to reconstruct a block list this slice should cover")
        | Some blocks -> Assert.Equal<Stmt list>(expectedAfterRoundTrip, blocksToAst blocks)

[<Fact>]
let ``a plain if with no else round-trips`` () =
    assertRoundTrips [ If([ (id_ "cond", [ ExprStmt(Assign(id_ "a", IntLit 1L)) ]) ], None) ]

[<Fact>]
let ``while, labeled and unlabeled, round-trips`` () =
    assertRoundTrips
        [ While(None, id_ "cond", [ ExprStmt(Assign(id_ "x", IntLit 1L)) ])
          While(Some "outer", id_ "cond", [ Break(Some "outer") ]) ]

[<Fact>]
let ``return, break, continue round-trip, with and without values/labels`` () =
    assertRoundTrips [ Return(Some(id_ "x")); Return None; Break None; Break(Some "outer"); Continue None; Continue(Some "outer") ]

[<Fact>]
let ``a comment statement round-trips`` () =
    assertRoundTrips [ ExprStmt(StrLit "this explains the next bit") ]

[<Fact>]
let ``a realistic multi-construct verb body round-trips`` () =
    assertRoundTrips
        [ While(
              Some "loop",
              Binary(Lt, id_ "i", IntLit 10L),
              [ ExprStmt(StrLit "print even numbers only")
                If(
                    [ (Binary(Eq, Binary(Mod, id_ "i", IntLit 2L), IntLit 0L),
                       [ ExprStmt(VerbCall(Prop(id_ "player", StrLit "location", 1, 1), StrLit "tell", [ Normal(id_ "i") ], 1, 1)) ]) ],
                    None
                )
                ExprStmt(Assign(id_ "i", Binary(Add, id_ "i", IntLit 1L))) ]
          ) ]

[<Fact>]
let ``a construct outside this slice's coverage fails the whole chain, rather than silently misconverting`` () =
    let stmts = [ ForRange(bn "i", IntLit 1L, IntLit 3L, [ ExprStmt(Assign(id_ "x", id_ "i")) ]) ]

    match stmts |> astToBlocks |> stmtsToJson with
    | None -> Assert.Fail("expected stmtsToJson to still produce a (one-way) moo_unsupported placeholder")
    | Some json ->
        match jsonToStmts json with
        | None -> ()
        | Some blocks -> Assert.Fail(sprintf "expected None, got %A" blocks)
