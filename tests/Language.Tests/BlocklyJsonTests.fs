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

/// Goes all the way through JSON *text*, not just the `JsonValue` tree -
/// the same boundary `Client/BlocklyEditor.fs` actually crosses via
/// `JS.JSON.stringify`/`JS.JSON.parse`.
let private assertRoundTrips (stmts: Stmt list) =
    match stmts |> astToBlocks |> stmtsToJson with
    | None -> Assert.Fail("stmtsToJson returned None for a non-empty statement list")
    | Some json ->
        let text = toJsonText json

        match parseJsonText text with
        | None -> Assert.Fail(sprintf "parseJsonText failed to re-parse stmtsToJson's own output: %s" text)
        | Some reparsedJson ->
            match jsonToStmts reparsedJson with
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
let ``for-in-list, both one- and two-variable forms, round-trips`` () =
    assertRoundTrips
        [ ForList(bn "v", None, id_ "collection", [ ExprStmt(Assign(id_ "x", id_ "v")) ])
          ForList(bn "v", Some(bn "k"), id_ "collection", [ ExprStmt(Assign(id_ "x", id_ "k")) ]) ]

[<Fact>]
let ``for-in-range round-trips`` () =
    assertRoundTrips [ ForRange(bn "i", IntLit 1L, IntLit 3L, [ ExprStmt(Assign(id_ "x", id_ "i")) ]) ]

[<Fact>]
let ``fork, named and unnamed, round-trips`` () =
    assertRoundTrips
        [ Fork(None, IntLit 0L, [ ExprStmt(StrLit "background work") ])
          Fork(Some(bn "task"), IntLit 5L, [ ExprStmt(Assign(id_ "x", id_ "task")) ]) ]

[<Fact>]
let ``try/finally round-trips`` () =
    assertRoundTrips [ TryFinally([ ExprStmt(Assign(id_ "x", IntLit 1L)) ], [ ExprStmt(Assign(id_ "y", IntLit 2L)) ]) ]

[<Fact>]
let ``range, first/last index, and a map literal round-trip`` () =
    assertRoundTrips
        [ ExprStmt(Range(IntLit 1L, IntLit 5L))
          ExprStmt(FirstIndex)
          ExprStmt(LastIndex)
          ExprStmt(MapLit [ (StrLit "a", IntLit 1L); (StrLit "b", IntLit 2L) ]) ]

[<Fact>]
let ``try/except, with one arm and with multiple arms (one ANY, one explicit codes with a splice), round-trips`` () =
    assertRoundTrips
        [ TryExcept(
              [ ExprStmt(Assign(id_ "x", Binary(Div, IntLit 1L, IntLit 0L))) ],
              [ { Name = Some(bn "e"); Codes = AnyCode; Body = [ ExprStmt(Assign(id_ "x", IntLit -1L)) ] } ]
          )
          TryExcept(
              [ ExprStmt(Assign(id_ "x", IntLit 1L)) ],
              [ { Name = None
                  Codes = Codes [ Normal(id_ "E_DIV"); Splice(id_ "more") ]
                  Body = [ ExprStmt(Assign(id_ "x", IntLit 0L)) ] }
                { Name = Some(bn "e2"); Codes = AnyCode; Body = [ ExprStmt(Assign(id_ "x", IntLit -1L)) ] } ]
          ) ]

[<Fact>]
let ``catch, with the bare ANY form and with an explicit code list (including a splice), round-trips`` () =
    assertRoundTrips
        [ ExprStmt(Catch(Binary(Div, IntLit 1L, IntLit 0L), AnyCode, Some(IntLit 0L)))
          ExprStmt(Catch(Binary(Div, IntLit 1L, IntLit 0L), Codes [ Normal(id_ "E_DIV"); Splice(id_ "rest") ], None)) ]

[<Fact>]
let ``scatter-assignment, mixing required/optional (with and without a default)/rest, round-trips`` () =
    assertRoundTrips
        [ ExprStmt(
              Scatter([ Required(bn "a"); Optional(bn "b", Some(IntLit 1L)); Optional(bn "c", None); Rest(bn "d") ], id_ "args")
          ) ]

[<Fact>]
let ``a construct outside this slice's coverage fails the whole chain, rather than silently misconverting`` () =
    // Computed-name Prop (`obj.(expr)`, as opposed to the literal-name
    // `obj.prop` form) is the one remaining permanently out-of-scope
    // construct after this slice - see Blocks.fs:191-192, where it falls
    // through to the VUnsupported escape hatch (Blocks.fs's own, not just
    // this module's).
    let stmts = [ ExprStmt(Prop(id_ "obj", id_ "name", 1, 1)) ]

    match stmts |> astToBlocks |> stmtsToJson with
    | None -> Assert.Fail("expected stmtsToJson to still produce a (one-way) moo_unsupported placeholder")
    | Some json ->
        match jsonToStmts json with
        | None -> ()
        | Some blocks -> Assert.Fail(sprintf "expected None, got %A" blocks)

// ---------------------------------------------------------------------------
// JsonValue <-> JSON text - the boundary `Client/BlocklyEditor.fs` actually
// crosses via `JS.JSON.stringify`/`JS.JSON.parse`.
// ---------------------------------------------------------------------------

let private assertJsonTextRoundTrips (v: JsonValue) =
    let text = toJsonText v

    match parseJsonText text with
    | None -> Assert.Fail(sprintf "parseJsonText failed on: %s" text)
    | Some v2 -> Assert.Equal<JsonValue>(v, v2)

[<Fact>]
let ``every JsonValue case round-trips through text`` () =
    assertJsonTextRoundTrips (JVString "hello")
    assertJsonTextRoundTrips (JVNumber 42.0)
    assertJsonTextRoundTrips (JVNumber -3.5)
    assertJsonTextRoundTrips (JVBool true)
    assertJsonTextRoundTrips (JVBool false)
    assertJsonTextRoundTrips JVNull
    assertJsonTextRoundTrips (JVArray [ JVNumber 1.0; JVNumber 2.0; JVNull ])
    assertJsonTextRoundTrips (JVObject [ "a", JVNumber 1.0; "b", JVArray [ JVString "x"; JVBool true ] ])

[<Fact>]
let ``strings needing escaping (quotes, backslashes, control chars, unicode) round-trip through text`` () =
    assertJsonTextRoundTrips (JVString "a \"quoted\" \\backslash\\ line\nbreak\ttab")
    assertJsonTextRoundTrips (JVString "unicode: éü中")
    assertJsonTextRoundTrips (JVString "")

[<Fact>]
let ``a deeply nested object/array structure round-trips through text`` () =
    assertJsonTextRoundTrips (
        JVObject
            [ "type", JVString "moo_if"
              "id", JVString "b1"
              "inputs",
              JVObject
                  [ "COND", JVObject [ "block", JVObject [ "type", JVString "moo_ident"; "fields", JVObject [ "NAME", JVString "x" ] ] ] ]
              "extraState", JVObject [ "count", JVNumber 2.0; "splices", JVArray [ JVBool false; JVBool true ] ] ]
    )

[<Fact>]
let ``malformed json text fails cleanly rather than throwing`` () =
    Assert.True(parseJsonText "{not valid" |> Option.isNone)
    Assert.True(parseJsonText "" |> Option.isNone)
    Assert.True(parseJsonText "{\"a\": 1} trailing garbage" |> Option.isNone)

[<Fact>]
let ``call arguments are recovered by ARGi presence, not a reported count - what a real fixed-arity Blockly block actually saves`` () =
    // No "count" field at all here, on purpose - a real Blockly-side
    // moo_call block (fixed ARG0..ARG3 sockets, see BlocklyJson.fs's own
    // updated doc comment) has no reason to report one; only "splices"
    // travels via extraState, for whichever ARGi slots are actually
    // connected.
    let json =
        JVObject
            [ "type", JVString "moo_call"
              "id", JVString "b1"
              "fields", JVObject [ "NAME", JVString "tostr" ]
              "inputs",
              JVObject
                  [ "ARG0", JVObject [ "block", JVObject [ "type", JVString "moo_int"; "fields", JVObject [ "NUM", JVNumber 1.0 ] ] ] ]
              "extraState", JVObject [ "splices", JVArray [ JVBool false ] ] ]

    match jsonToValue json with
    | Some(VCall("tostr", [ AArg(VIntLit 1L) ])) -> ()
    | other -> Assert.Fail(sprintf "expected a single-argument VCall, got %A" other)

// ---------------------------------------------------------------------------
// isFullyMappable - a real gate is required before ever calling
// stmtsToJson/loadStateText: Blocks.isFullyRepresentable alone isn't enough,
// since it reflects Blocks.fs's own wider coverage (for/fork/try/scatter/
// catch all have real BlockStmt cases there), not this module's smaller,
// real-Blockly-block-backed subset. Confirmed live (see BlocklyEditor.fs's
// own doc comment) that skipping this check lets an out-of-subset
// construct reach a real Blockly workspace and throw "Invalid block
// definition for type: moo_unsupported" instead of failing gracefully.
// ---------------------------------------------------------------------------

[<Fact>]
let ``isFullyMappable is true for every construct this module actually maps to a real block`` () =
    let blocks =
        [ While(
              Some "loop",
              Binary(Lt, id_ "i", IntLit 10L),
              [ If([ (Binary(Eq, id_ "i", IntLit 0L), [ ExprStmt(Assign(id_ "x", IntLit 1L)) ]) ], None)
                ExprStmt(StrLit "a comment")
                ExprStmt(Call("tostr", [ Normal(id_ "i") ], 1, 1))
                ForList(bn "v", None, id_ "collection", [ ExprStmt(Assign(id_ "x", id_ "v")) ])
                ForRange(bn "i", IntLit 1L, IntLit 3L, [ ExprStmt(Assign(id_ "x", id_ "i")) ])
                Fork(Some(bn "task"), IntLit 5L, [ ExprStmt(Assign(id_ "x", IntLit 1L)) ])
                TryFinally([ ExprStmt(Assign(id_ "x", IntLit 1L)) ], [ ExprStmt(Assign(id_ "y", IntLit 2L)) ])
                TryExcept(
                    [ ExprStmt(Assign(id_ "x", IntLit 1L)) ],
                    [ { Name = None
                        Codes = Codes [ Normal(id_ "E_DIV") ]
                        Body = [ ExprStmt(Assign(id_ "x", IntLit 0L)) ] } ]
                )
                ExprStmt(Range(IntLit 1L, IntLit 5L))
                ExprStmt(FirstIndex)
                ExprStmt(LastIndex)
                ExprStmt(MapLit [ (StrLit "a", IntLit 1L) ])
                ExprStmt(Catch(IntLit 1L, Codes [ Normal(id_ "E_DIV") ], Some(IntLit 0L)))
                ExprStmt(Scatter([ Required(bn "a"); Optional(bn "b", Some(IntLit 1L)); Rest(bn "c") ], id_ "args")) ]
          ) ]
        |> astToBlocks

    Assert.True(isFullyMappable blocks)

[<Fact>]
let ``isFullyMappable is false for the one remaining permanently out-of-scope construct (computed-name Prop/VerbCall) - Blocks.isFullyRepresentable agrees, since it's Blocks.fs's own escape hatch too`` () =
    let computedProp = ExprStmt(Prop(id_ "obj", id_ "name", 1, 1))

    Assert.False(isFullyRepresentable [ computedProp ])
    Assert.False(isFullyMappable (astToBlocks [ computedProp ]))

[<Fact>]
let ``isFullyMappable is false when the unmappable construct is nested arbitrarily deep`` () =
    let stmts =
        [ While(
              None,
              id_ "cond",
              [ If([ (id_ "cond2", [ ExprStmt(Binary(Add, IntLit 1L, Prop(id_ "obj", id_ "name", 1, 1))) ]) ], None) ]
          ) ]

    Assert.False(isFullyMappable (astToBlocks stmts))
