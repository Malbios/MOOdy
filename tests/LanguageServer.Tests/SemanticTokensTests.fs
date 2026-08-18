/// Unit tests for `Handlers.classifySemanticToken` - the pure classification
/// step of the SemanticTokensProvider feature, deliberately separated from
/// `computeSemanticTokens`'s own live `bridge.ResolveVerbDispatch`/
/// `GetBuiltins` calls so it's testable with plain hand-built inputs
/// (`liveBuiltins`/`resolvedVerbCalls` maps), same reasoning as
/// `VerbMetricsTests.fs`/`MoocodeDocsTests.fs`'s fixture-graph style.
module LanguageServer.Tests.SemanticTokensTests

open Xunit
open Language.Ast
open Metadata.Schema
open LanguageServer.Handlers
open LanguageServer.AstQuery

let private graphOf (systemObjectProperties: (string * ObjRef) list) : Graph =
    { Objects = Map.empty
      SystemObjectProperties = Map.ofList systemObjectProperties
      Builtins = Map.empty }

let private refAt (line: int) (col: int) (length: int) (r: Reference) : FoundReference =
    { Line = line; Col = col; Length = length; Ref = r }

let private classify
    (liveBuiltins: (string * BuiltinFunc) list)
    (resolvedVerbCalls: ((ObjRef * string) * bool) list)
    (r: FoundReference)
    : SemanticTokenEntry option =
    classifySemanticToken (graphOf []) 1L (Map.ofList liveBuiltins) (Map.ofList resolvedVerbCalls) r

// Fixture helpers for the single-candidate-fallback test below, mirroring
// `MoocodeDocsTests.fs`'s own local object/verb builders - `graphOf` above
// always hardcodes `Objects = Map.empty`, which can't exercise
// `findAllDefiningObjects`'s corpus-wide scan.
let private verbMeta (index: int) (name: string) : VerbMeta =
    { Index = index
      Names = [ name ]
      Owner = 2L
      Perms = "rxd"
      Dobj = "this"
      Prep = "none"
      Iobj = "this" }

let private verbNode (definedOn: ObjRef) (meta: VerbMeta) : VerbNode =
    { Meta = meta
      DefinedOn = definedOn
      SourcePath = None
      Ast = None
      DiagnosticCount = 0
      Tokens = None }

let private objNode (num: ObjRef) (verbs: VerbNode list) : ObjectNode =
    { Num = num
      Name = None
      LiveName = None
      Parents = []
      Children = []
      Verbs = verbs
      Owner = None
      Flags = None
      Properties = []
      Aliases = [] }

let private graphWithObjects (objects: ObjectNode list) : Graph =
    { Objects = objects |> List.map (fun o -> o.Num, o) |> Map.ofList
      SystemObjectProperties = Map.empty
      Builtins = Map.empty }

let private classifyWithGraph
    (graph: Graph)
    (resolvedVerbCalls: ((ObjRef * string) * bool) list)
    (r: FoundReference)
    : SemanticTokenEntry option =
    classifySemanticToken graph 1L Map.empty (Map.ofList resolvedVerbCalls) r

let private fn (name: string) : BuiltinFunc =
    { Name = name
      MinArgs = 0
      MaxArgs = 0
      ArgTypes = []
      ParamNames = None
      Description = None }

[<Fact>]
let ``a plain local variable classifies as variable with no modifiers`` () =
    let entry = classify [] [] (refAt 3 5 1 (RefIdent "x"))
    Assert.Equal(Some { Line = 2; StartChar = 4; Length = 1; TokenType = "variable"; TokenModifiers = [||] }, entry)

[<Fact>]
let ``an implicit verb-call variable classifies as variable with the defaultLibrary modifier`` () =
    let entry = classify [] [] (refAt 1 1 4 (RefIdent "this"))
    Assert.Equal(Some [| "defaultLibrary" |], entry |> Option.map (fun e -> e.TokenModifiers))
    Assert.Equal(Some "variable", entry |> Option.map (fun e -> e.TokenType))

[<Fact>]
let ``a type-tag constant classifies as variable with the defaultLibrary modifier`` () =
    let entry = classify [] [] (refAt 1 1 3 (RefIdent "OBJ"))
    Assert.Equal(Some [| "defaultLibrary" |], entry |> Option.map (fun e -> e.TokenModifiers))

[<Fact>]
let ``true/false classify as variable with the defaultLibrary modifier`` () =
    let trueEntry = classify [] [] (refAt 1 1 4 (RefIdent "true"))
    let falseEntry = classify [] [] (refAt 1 1 5 (RefIdent "false"))
    Assert.Equal(Some [| "defaultLibrary" |], trueEntry |> Option.map (fun e -> e.TokenModifiers))
    Assert.Equal(Some [| "defaultLibrary" |], falseEntry |> Option.map (fun e -> e.TokenModifiers))

[<Fact>]
let ``a call to a known builtin classifies as function with the defaultLibrary modifier`` () =
    let entry = classify [ "notify", fn "notify" ] [] (refAt 1 1 6 (RefCall("notify", [])))
    Assert.Equal(Some "function", entry |> Option.map (fun e -> e.TokenType))
    Assert.Equal(Some [| "defaultLibrary" |], entry |> Option.map (fun e -> e.TokenModifiers))

[<Fact>]
let ``a call to an unknown function name classifies as function with no modifiers`` () =
    let entry = classify [] [] (refAt 1 1 5 (RefCall("nosuch", [])))
    Assert.Equal(Some "function", entry |> Option.map (fun e -> e.TokenType))
    Assert.Equal(Some[||], entry |> Option.map (fun e -> e.TokenModifiers))

[<Fact>]
let ``a resolved verb call classifies as method with no modifiers`` () =
    let entry = classify [] [ (2L, "target"), true ] (refAt 1 1 6 (RefVerbCall(ObjLit 2L, StrLit "target", [])))
    Assert.Equal(Some "method", entry |> Option.map (fun e -> e.TokenType))
    Assert.Equal(Some[||], entry |> Option.map (fun e -> e.TokenModifiers))

[<Fact>]
let ``a verb call with a known starting object but confirmed-failed dispatch classifies as method with the broken modifier`` () =
    let entry = classify [] [ (2L, "target"), false ] (refAt 1 1 6 (RefVerbCall(ObjLit 2L, StrLit "target", [])))
    Assert.Equal(Some [| "broken" |], entry |> Option.map (fun e -> e.TokenModifiers))

[<Fact>]
let ``a single-candidate verb call with confirmed-failed dispatch also classifies as broken, not unresolved`` () =
    let graph = graphWithObjects [ objNode 1L [ verbNode 1L (verbMeta 1 "name") ] ]
    let entry = classifyWithGraph graph [ (1L, "name"), false ] (refAt 1 1 4 (RefVerbCall(Ident("o", 1, 1), StrLit "name", [])))
    Assert.Equal(Some [| "broken" |], entry |> Option.map (fun e -> e.TokenModifiers))

[<Fact>]
let ``a verb call with an unresolvable receiver classifies as method with the unresolved modifier`` () =
    let entry = classify [] [] (refAt 1 1 6 (RefVerbCall(Ident("player", 1, 1), StrLit "tell", [])))
    Assert.Equal(Some "method", entry |> Option.map (fun e -> e.TokenType))
    Assert.Equal(Some [| "unresolved" |], entry |> Option.map (fun e -> e.TokenModifiers))

[<Fact>]
let ``a verb call with an unresolvable receiver but exactly one object defining the verb classifies as resolved`` () =
    let graph = graphWithObjects [ objNode 1L [ verbNode 1L (verbMeta 1 "name") ] ]
    let entry = classifyWithGraph graph [ (1L, "name"), true ] (refAt 1 1 4 (RefVerbCall(Ident("o", 1, 1), StrLit "name", [])))
    Assert.Equal(Some "method", entry |> Option.map (fun e -> e.TokenType))
    Assert.Equal(Some[||], entry |> Option.map (fun e -> e.TokenModifiers))

[<Fact>]
let ``a verb call with an unresolvable receiver and genuinely ambiguous candidates stays unresolved`` () =
    let graph = graphWithObjects [ objNode 1L [ verbNode 1L (verbMeta 1 "name") ]; objNode 2L [ verbNode 2L (verbMeta 1 "name") ] ]
    let entry = classifyWithGraph graph [] (refAt 1 1 4 (RefVerbCall(Ident("o", 1, 1), StrLit "name", [])))
    Assert.Equal(Some [| "unresolved" |], entry |> Option.map (fun e -> e.TokenModifiers))

[<Fact>]
let ``$foo(args) call-sugar classifies as property with the corponym modifier, not method`` () =
    let entry = classify [] [] (refAt 1 2 12 (RefVerbCall(ObjLit 0L, StrLit "my_verb_call", [])))
    Assert.Equal(Some "property", entry |> Option.map (fun e -> e.TokenType))
    Assert.Equal(Some [| "corponym" |], entry |> Option.map (fun e -> e.TokenModifiers))

[<Fact>]
let ``a property access classifies as property with no modifiers`` () =
    let entry = classify [] [] (refAt 1 1 3 (RefProp(Ident("this", 1, 1), StrLit "foo")))
    Assert.Equal(Some "property", entry |> Option.map (fun e -> e.TokenType))
    Assert.Equal(Some[||], entry |> Option.map (fun e -> e.TokenModifiers))

[<Fact>]
let ``$foo property-sugar classifies as property with the corponym modifier`` () =
    let entry = classify [] [] (refAt 1 2 12 (RefProp(ObjLit 0L, StrLit "string_utils")))
    Assert.Equal(Some "property", entry |> Option.map (fun e -> e.TokenType))
    Assert.Equal(Some [| "corponym" |], entry |> Option.map (fun e -> e.TokenModifiers))

[<Fact>]
let ``a computed property name has nothing to classify`` () =
    let entry = classify [] [] (refAt 1 1 1 (RefProp(Ident("this", 1, 1), Ident("dynamic", 1, 6))))
    Assert.Equal(None, entry)

[<Fact>]
let ``a computed verb-call name has nothing to classify`` () =
    let entry = classify [] [] (refAt 1 1 1 (RefVerbCall(ObjLit 2L, Ident("dynamic", 1, 6), [])))
    Assert.Equal(None, entry)
