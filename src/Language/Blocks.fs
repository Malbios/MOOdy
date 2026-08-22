/// Spike for the "Google Blocks mode for visual coding" card (MOOdy
/// Development board): a MOOcode `Ast` <-> block-tree mapping, proving out
/// the direction the card's own research flagged as genuinely uncertain -
/// parsing arbitrary, existing, hand-written verb code back into blocks,
/// not generating code forward from them (that half is comparatively
/// mechanical - see `Printer.fs`, the `Ast -> text` generator this module's
/// output eventually feeds).
///
/// Deliberately not wired to the real Blockly JS library or its wire JSON
/// format yet - `BlockStmt`/`BlockValue` below are this spike's own
/// intermediate shape. Matching Blockly's actual block/field/connection
/// JSON is a separate, mechanical concern for once this mapping itself is
/// proven - not something to design blind against here.
///
/// Covers the full `Ast.Stmt`/`Ast.Expr` grammar except two shapes that
/// stay on the `SUnsupported`/`VUnsupported` escape hatch deliberately:
/// computed-name `Prop`/`VerbCall` (`obj.(expr)`/`obj:(expr)(...)` - the
/// research doc's own reasoning stands: "invisible to static analysis
/// already, zero upside to a second block variant for it") and
/// `ErrorStmt` (a parse-failure placeholder, not a reconstructable
/// construct at all - `Printer.fs` takes the same stance).
///
/// Both directions are total functions, never throwing: anything that
/// still lands on the escape hatch carries the original `Ast` node
/// verbatim, the same "degrade a piece, don't refuse the whole tree"
/// instinct `Ast.ErrorStmt` already uses for a parse failure. This is
/// deliberately granular, not whole-statement: a `Binary` expression with
/// one unsupported operand still renders as a real `VBinary` block with a
/// `VUnsupported` leaf plugged into just that one socket, and an `If` with
/// one unsupported statement in its body still renders as a real `SIf`
/// block with just that one body line as a raw leaf - not the whole
/// containing construct collapsing to unsupported.
module Language.Blocks

open Language.Ast

/// One value-producing ("reporter") block. Mirrors `Ast.Expr` almost
/// completely - see the module's own doc comment for the two remaining
/// gaps. `VBoolLit` is a stated spike assumption, not a silent one:
/// `true`/`false` are real reassignable variables in MOOcode (see
/// `Ident`), not literals, but modeling them as literal-like blocks
/// (research doc's option (a)) is more ergonomic than always emitting a
/// bare `1`/`0` - revisit if a real verb in a later round-trip corpus ever
/// reassigns them.
///
/// None of these carry the `line`/`col` position `Ast.fs` attaches to
/// `Ident`/`Prop`/`VerbCall`/`Call`/bound names - a position identifies a
/// token in *text*, which a block sitting on a visual canvas doesn't have.
/// Round-tripping through this spike only proves structural (AST-shape)
/// equivalence, not source-position preservation - `blockToExpr`/
/// `blockToStmt` below synthesize a fixed placeholder position for any
/// node they reconstruct, matching `Printer.fs`'s own stance that "round
/// trip" means AST-equivalence, not textual (or, here, positional)
/// identity, the same stance `Sugar.fs` already established project-wide.
type BlockValue =
    | VIntLit of int64
    | VFloatLit of float
    | VStrLit of string
    | VObjLit of int64
    | VErrLit of string
    | VBoolLit of bool
    | VIdent of string
    | VBinary of BinOp * BlockValue * BlockValue
    | VUnary of UnOp * BlockValue
    | VCond of BlockValue * BlockValue * BlockValue
    | VAssign of BlockValue * BlockValue
    /// Literal-name property access only (`obj.prop`/`obj.:waifprop`) - the
    /// computed form (`obj.(expr)`) stays on the escape hatch, see the
    /// module's own doc comment.
    | VProp of receiver: BlockValue * name: string
    /// Literal-name verb calls only, same reasoning as `VProp`.
    | VVerbCall of receiver: BlockValue * name: string * args: BlockArg list
    | VCall of name: string * args: BlockArg list
    | VListLit of BlockArg list
    | VMapLit of (BlockValue * BlockValue) list
    | VIndex of BlockValue * BlockValue
    /// `a..b` as an index/slice bound (`list[a..b]`) - not a for-loop's own
    /// `[lo..hi]` bound, which never constructs a `Range` node at all
    /// (confirmed in `Parser.fs`'s `parseFor`: `ForRange`'s `lo`/`hi` are
    /// plain `Expr` fields), so that case already round-trips via ordinary
    /// `exprToBlock`/`blockToExpr` with no dedicated case needed.
    | VRange of BlockValue * BlockValue
    /// `^` / `$` used bare inside index/range brackets - legal only in
    /// that context, same as `Ast.FirstIndex`/`Ast.LastIndex`.
    | VFirstIndex
    | VLastIndex
    | VScatter of BlockScatterItem list * BlockValue
    /// `` `expr ! codes => fallback' `` - `fallback` is `None` when
    /// omitted, matching `Ast.Catch` exactly.
    | VCatch of tryValue: BlockValue * codes: BlockCodes * fallback: BlockValue option
    /// The escape hatch - computed-name `Prop`/`VerbCall` only (see the
    /// module's own doc comment) - carrying the real `Expr` verbatim so
    /// `blockToExpr` can still reproduce it exactly.
    | VUnsupported of Expr

/// A call/list argument - mirrors `Ast.Arg` exactly.
and BlockArg =
    | AArg of BlockValue
    | ASplice of BlockValue

/// Mirrors `Ast.ScatterItem`, dropping `BoundName`'s position fields to a
/// bare `string` (same as every other bound name in this module).
and BlockScatterItem =
    | BRequired of string
    | BOptional of string * BlockValue option
    | BRest of string

/// Mirrors `Ast.Codes` - either the bare `ANY` keyword or an arbitrary,
/// possibly-splicing argument list.
and BlockCodes =
    | BAnyCode
    | BCodes of BlockArg list

/// One statement ("action"/"stack") block. Mirrors `Ast.Stmt` except
/// `ErrorStmt` (see the module's own doc comment).
and BlockStmt =
    | SIf of arms: (BlockValue * BlockStmt list) list * elsePart: BlockStmt list option
    /// `for var in (source)` (`indexVar = None`) or `for var, indexVar in
    /// (source)` - value first, index/key second, matching `Ast.ForList`'s
    /// own field order exactly.
    | SForList of var: string * indexVar: string option * source: BlockValue * body: BlockStmt list
    | SForRange of var: string * lo: BlockValue * hi: BlockValue * body: BlockStmt list
    | SWhile of name: string option * cond: BlockValue * body: BlockStmt list
    | SFork of name: string option * delay: BlockValue * body: BlockStmt list
    | SReturn of BlockValue option
    | SBreak of string option
    | SContinue of string option
    | SExpr of BlockValue
    | STryExcept of body: BlockStmt list * arms: BlockExceptArm list
    | STryFinally of body: BlockStmt list * handler: BlockStmt list
    /// The escape hatch - `ErrorStmt` only (see the module's own doc
    /// comment), carrying the original `Stmt` verbatim.
    | SUnsupported of Stmt

/// Mirrors `Ast.ExceptArm`, dropping `Name`'s position field.
and BlockExceptArm =
    { Name: string option
      Codes: BlockCodes
      Body: BlockStmt list }

let private argToBlock (toBlock: Expr -> BlockValue) (arg: Arg) : BlockArg =
    match arg with
    | Normal e -> AArg(toBlock e)
    | Splice e -> ASplice(toBlock e)

let private argToExpr (toExpr: BlockValue -> Expr) (arg: BlockArg) : Arg =
    match arg with
    | AArg v -> Normal(toExpr v)
    | ASplice v -> Splice(toExpr v)

/// `Ast.Expr` -> `BlockValue`, total - see the module's own doc comment for
/// the granular-fallback design (an unsupported operand becomes a leaf, not
/// a whole-expression bailout).
let rec exprToBlock (expr: Expr) : BlockValue =
    match expr with
    | IntLit n -> VIntLit n
    | FloatLit f -> VFloatLit f
    | StrLit s -> VStrLit s
    | ObjLit n -> VObjLit n
    | ErrLit e -> VErrLit e
    | Ident("true", _, _) -> VBoolLit true
    | Ident("false", _, _) -> VBoolLit false
    | Ident(name, _, _) -> VIdent name
    | Binary(op, l, r) -> VBinary(op, exprToBlock l, exprToBlock r)
    | Unary(op, e) -> VUnary(op, exprToBlock e)
    | Cond(c, t, f) -> VCond(exprToBlock c, exprToBlock t, exprToBlock f)
    | Assign(target, value) -> VAssign(exprToBlock target, exprToBlock value)
    | Prop(receiver, StrLit name, _, _) -> VProp(exprToBlock receiver, name)
    | VerbCall(receiver, StrLit name, args, _, _) -> VVerbCall(exprToBlock receiver, name, args |> List.map (argToBlock exprToBlock))
    | Call(name, args, _, _) -> VCall(name, args |> List.map (argToBlock exprToBlock))
    | ListLit args -> VListLit(args |> List.map (argToBlock exprToBlock))
    | MapLit pairs -> VMapLit(pairs |> List.map (fun (k, v) -> exprToBlock k, exprToBlock v))
    | Index(e, i) -> VIndex(exprToBlock e, exprToBlock i)
    | Range(lo, hi) -> VRange(exprToBlock lo, exprToBlock hi)
    | FirstIndex -> VFirstIndex
    | LastIndex -> VLastIndex
    | Scatter(items, value) -> VScatter(items |> List.map scatterItemToBlock, exprToBlock value)
    | Catch(e, codes, fallback) -> VCatch(exprToBlock e, codesToBlock codes, fallback |> Option.map exprToBlock)
    | Prop _
    | VerbCall _ -> VUnsupported expr

and private scatterItemToBlock (item: ScatterItem) : BlockScatterItem =
    match item with
    | Required n -> BRequired n.Name
    | Rest n -> BRest n.Name
    | Optional(n, d) -> BOptional(n.Name, d |> Option.map exprToBlock)

and private codesToBlock (codes: Codes) : BlockCodes =
    match codes with
    | AnyCode -> BAnyCode
    | Codes args -> BCodes(args |> List.map (argToBlock exprToBlock))

/// `BlockValue` -> `Ast.Expr`, total - the reverse of `exprToBlock`.
/// Reconstructed `Ident`/`Prop`/`VerbCall`/`Call`/bound-name nodes get a
/// fixed `1, 1` placeholder position (see the module's own doc comment on
/// why a real position can't be recovered, and isn't the point of this
/// spike).
let rec blockToExpr (block: BlockValue) : Expr =
    match block with
    | VIntLit n -> IntLit n
    | VFloatLit f -> FloatLit f
    | VStrLit s -> StrLit s
    | VObjLit n -> ObjLit n
    | VErrLit e -> ErrLit e
    | VBoolLit true -> Ident("true", 1, 1)
    | VBoolLit false -> Ident("false", 1, 1)
    | VIdent name -> Ident(name, 1, 1)
    | VBinary(op, l, r) -> Binary(op, blockToExpr l, blockToExpr r)
    | VUnary(op, e) -> Unary(op, blockToExpr e)
    | VCond(c, t, f) -> Cond(blockToExpr c, blockToExpr t, blockToExpr f)
    | VAssign(target, value) -> Assign(blockToExpr target, blockToExpr value)
    | VProp(receiver, name) -> Prop(blockToExpr receiver, StrLit name, 1, 1)
    | VVerbCall(receiver, name, args) -> VerbCall(blockToExpr receiver, StrLit name, args |> List.map (argToExpr blockToExpr), 1, 1)
    | VCall(name, args) -> Call(name, args |> List.map (argToExpr blockToExpr), 1, 1)
    | VListLit args -> ListLit(args |> List.map (argToExpr blockToExpr))
    | VMapLit pairs -> MapLit(pairs |> List.map (fun (k, v) -> blockToExpr k, blockToExpr v))
    | VIndex(e, i) -> Index(blockToExpr e, blockToExpr i)
    | VRange(lo, hi) -> Range(blockToExpr lo, blockToExpr hi)
    | VFirstIndex -> FirstIndex
    | VLastIndex -> LastIndex
    | VScatter(items, value) -> Scatter(items |> List.map blockToScatterItem, blockToExpr value)
    | VCatch(e, codes, fallback) -> Catch(blockToExpr e, blockToCodes codes, fallback |> Option.map blockToExpr)
    | VUnsupported expr -> expr

and private blockToScatterItem (item: BlockScatterItem) : ScatterItem =
    match item with
    | BRequired name -> Required { Name = name; Line = 1; Col = 1 }
    | BRest name -> Rest { Name = name; Line = 1; Col = 1 }
    | BOptional(name, d) -> Optional({ Name = name; Line = 1; Col = 1 }, d |> Option.map blockToExpr)

and private blockToCodes (codes: BlockCodes) : Codes =
    match codes with
    | BAnyCode -> AnyCode
    | BCodes args -> Codes(args |> List.map (argToExpr blockToExpr))

/// `Ast.Stmt` -> `BlockStmt`, total.
let rec stmtToBlock (stmt: Stmt) : BlockStmt =
    match stmt with
    | If(arms, elsePart) -> SIf(arms |> List.map (fun (c, body) -> exprToBlock c, body |> List.map stmtToBlock), elsePart |> Option.map (List.map stmtToBlock))
    | ForList(var, indexVar, source, body) ->
        SForList(var.Name, indexVar |> Option.map (fun v -> v.Name), exprToBlock source, body |> List.map stmtToBlock)
    | ForRange(var, lo, hi, body) -> SForRange(var.Name, exprToBlock lo, exprToBlock hi, body |> List.map stmtToBlock)
    | While(name, cond, body) -> SWhile(name, exprToBlock cond, body |> List.map stmtToBlock)
    | Fork(name, delay, body) -> SFork(name |> Option.map (fun n -> n.Name), exprToBlock delay, body |> List.map stmtToBlock)
    | Return e -> SReturn(e |> Option.map exprToBlock)
    | Break name -> SBreak name
    | Continue name -> SContinue name
    | ExprStmt e -> SExpr(exprToBlock e)
    | TryExcept(body, arms) -> STryExcept(body |> List.map stmtToBlock, arms |> List.map exceptArmToBlock)
    | TryFinally(body, handler) -> STryFinally(body |> List.map stmtToBlock, handler |> List.map stmtToBlock)
    | ErrorStmt _ -> SUnsupported stmt

and private exceptArmToBlock (arm: ExceptArm) : BlockExceptArm =
    { Name = arm.Name |> Option.map (fun n -> n.Name)
      Codes = codesToBlock arm.Codes
      Body = arm.Body |> List.map stmtToBlock }

/// `BlockStmt` -> `Ast.Stmt`, total - the reverse of `stmtToBlock`.
let rec blockToStmt (block: BlockStmt) : Stmt =
    match block with
    | SIf(arms, elsePart) -> If(arms |> List.map (fun (c, body) -> blockToExpr c, body |> List.map blockToStmt), elsePart |> Option.map (List.map blockToStmt))
    | SForList(var, indexVar, source, body) ->
        ForList(
            { Name = var; Line = 1; Col = 1 },
            indexVar |> Option.map (fun v -> { Name = v; Line = 1; Col = 1 }),
            blockToExpr source,
            body |> List.map blockToStmt
        )
    | SForRange(var, lo, hi, body) -> ForRange({ Name = var; Line = 1; Col = 1 }, blockToExpr lo, blockToExpr hi, body |> List.map blockToStmt)
    | SWhile(name, cond, body) -> While(name, blockToExpr cond, body |> List.map blockToStmt)
    | SFork(name, delay, body) -> Fork(name |> Option.map (fun n -> { Name = n; Line = 1; Col = 1 }), blockToExpr delay, body |> List.map blockToStmt)
    | SReturn v -> Return(v |> Option.map blockToExpr)
    | SBreak name -> Break name
    | SContinue name -> Continue name
    | SExpr v -> ExprStmt(blockToExpr v)
    | STryExcept(body, arms) -> TryExcept(body |> List.map blockToStmt, arms |> List.map blockToExceptArm)
    | STryFinally(body, handler) -> TryFinally(body |> List.map blockToStmt, handler |> List.map blockToStmt)
    | SUnsupported stmt -> stmt

and private blockToExceptArm (arm: BlockExceptArm) : ExceptArm =
    { Name = arm.Name |> Option.map (fun n -> { Name = n; Line = 1; Col = 1 })
      Codes = blockToCodes arm.Codes
      Body = arm.Body |> List.map blockToStmt }

/// The two directions the card's own research flagged as needing a spike -
/// `astToBlocks` is the genuinely uncertain one (existing code -> blocks),
/// `blocksToAst` is what a real Blockly generator would eventually feed
/// back (blocks -> code, via `Printer.fs`), needed here only to prove the
/// round trip.
let astToBlocks (stmts: Stmt list) : BlockStmt list = stmts |> List.map stmtToBlock
let blocksToAst (blocks: BlockStmt list) : Stmt list = blocks |> List.map blockToStmt

/// Whether a `BlockValue`/`BlockStmt` (or, transitively, everything under
/// it) is real block structure with no `VUnsupported`/`SUnsupported` leaf
/// anywhere - what a real toggle would call to decide whether switching a
/// verb to block view is safe. This only answers "would it be lossy" -
/// what to actually do when it would (warn, refuse, silently fall back to
/// text) is still an open product question, see the card's own notes.
let rec private valueIsFullyRepresentable (value: BlockValue) : bool =
    match value with
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
    | VBinary(_, l, r) -> valueIsFullyRepresentable l && valueIsFullyRepresentable r
    | VUnary(_, e) -> valueIsFullyRepresentable e
    | VCond(c, t, f) -> valueIsFullyRepresentable c && valueIsFullyRepresentable t && valueIsFullyRepresentable f
    | VAssign(target, v) -> valueIsFullyRepresentable target && valueIsFullyRepresentable v
    | VProp(receiver, _) -> valueIsFullyRepresentable receiver
    | VVerbCall(receiver, _, args) -> valueIsFullyRepresentable receiver && args |> List.forall argIsFullyRepresentable
    | VCall(_, args) -> args |> List.forall argIsFullyRepresentable
    | VListLit args -> args |> List.forall argIsFullyRepresentable
    | VMapLit pairs -> pairs |> List.forall (fun (k, v) -> valueIsFullyRepresentable k && valueIsFullyRepresentable v)
    | VIndex(e, i) -> valueIsFullyRepresentable e && valueIsFullyRepresentable i
    | VRange(lo, hi) -> valueIsFullyRepresentable lo && valueIsFullyRepresentable hi
    | VScatter(items, v) -> (items |> List.forall scatterItemIsFullyRepresentable) && valueIsFullyRepresentable v
    | VCatch(e, codes, fallback) ->
        valueIsFullyRepresentable e && codesIsFullyRepresentable codes && (fallback |> Option.forall valueIsFullyRepresentable)

and private argIsFullyRepresentable (arg: BlockArg) : bool =
    match arg with
    | AArg v
    | ASplice v -> valueIsFullyRepresentable v

and private codesIsFullyRepresentable (codes: BlockCodes) : bool =
    match codes with
    | BAnyCode -> true
    | BCodes args -> args |> List.forall argIsFullyRepresentable

and private scatterItemIsFullyRepresentable (item: BlockScatterItem) : bool =
    match item with
    | BRequired _
    | BRest _ -> true
    | BOptional(_, d) -> d |> Option.forall valueIsFullyRepresentable

let rec private stmtIsFullyRepresentable (stmt: BlockStmt) : bool =
    match stmt with
    | SUnsupported _ -> false
    | SIf(arms, elsePart) ->
        (arms |> List.forall (fun (c, body) -> valueIsFullyRepresentable c && body |> List.forall stmtIsFullyRepresentable))
        && (elsePart |> Option.forall (List.forall stmtIsFullyRepresentable))
    | SForList(_, _, source, body) -> valueIsFullyRepresentable source && body |> List.forall stmtIsFullyRepresentable
    | SForRange(_, lo, hi, body) ->
        valueIsFullyRepresentable lo && valueIsFullyRepresentable hi && body |> List.forall stmtIsFullyRepresentable
    | SWhile(_, cond, body) -> valueIsFullyRepresentable cond && body |> List.forall stmtIsFullyRepresentable
    | SFork(_, delay, body) -> valueIsFullyRepresentable delay && body |> List.forall stmtIsFullyRepresentable
    | SReturn v -> v |> Option.forall valueIsFullyRepresentable
    | SBreak _
    | SContinue _ -> true
    | SExpr v -> valueIsFullyRepresentable v
    | STryExcept(body, arms) ->
        (body |> List.forall stmtIsFullyRepresentable)
        && (arms |> List.forall (fun arm -> codesIsFullyRepresentable arm.Codes && arm.Body |> List.forall stmtIsFullyRepresentable))
    | STryFinally(body, handler) -> (body |> List.forall stmtIsFullyRepresentable) && (handler |> List.forall stmtIsFullyRepresentable)

/// Whether every construct in `stmts` maps to a real block - no
/// `VUnsupported`/`SUnsupported` anywhere in the tree `astToBlocks` would
/// produce for it.
let isFullyRepresentable (stmts: Stmt list) : bool = stmts |> astToBlocks |> List.forall stmtIsFullyRepresentable
