/// Spike for the "Google Blocks mode for visual coding" card (MOOdy
/// Development board): a MOOcode `Ast` <-> block-tree mapping, proving out
/// the direction the card's own research flagged as genuinely uncertain -
/// parsing arbitrary, existing, hand-written verb code back into blocks,
/// not generating code forward from them (that half is comparatively
/// mechanical once a real Blockly toolbox exists).
///
/// Deliberately not wired to the real Blockly JS library or its wire JSON
/// format yet - `BlockStmt`/`BlockValue` below are this spike's own
/// intermediate shape, covering a representative subset of `Ast.Stmt`/
/// `Ast.Expr` (see the card's own research doc, "Round-trip fidelity"
/// section, for the full construct-by-construct scoring this subset draws
/// from). Matching Blockly's actual block/field/connection JSON is a
/// separate, mechanical concern for once this mapping itself is proven -
/// not something to design blind against here.
///
/// Both directions are total functions, never throwing: anything outside
/// the covered subset becomes an explicit `SUnsupported`/`VUnsupported` leaf
/// carrying the original `Ast` node verbatim, the same "degrade a piece,
/// don't refuse the whole tree" instinct `Ast.ErrorStmt` already uses for a
/// parse failure. This is deliberately granular, not whole-statement: a
/// `Binary` expression with one unsupported operand still renders as a real
/// `VBinary` block with a `VUnsupported` leaf plugged into just that one
/// socket, and an `If` with one unsupported statement in its body still
/// renders as a real `SIf` block with just that one body line as a raw
/// leaf - not the whole containing construct collapsing to unsupported.
module Language.Blocks

open Language.Ast

/// One value-producing ("reporter") block. Mirrors `Ast.Expr`'s covered
/// subset one-to-one. `VBoolLit` is a stated spike assumption, not a
/// silent one: `true`/`false` are real reassignable variables in MOOcode
/// (see `Ident`), not literals, but modeling them as literal-like blocks
/// (research doc's option (a)) is more ergonomic than always emitting a
/// bare `1`/`0` - revisit if a real verb in a later round-trip corpus ever
/// reassigns them, per the research doc's own open question.
///
/// `VIdent`/`VProp`/`VVerbCall`/`VCall` deliberately do NOT carry the
/// `line`/`col` position `Ast.fs` attaches to their real counterparts - a
/// position identifies a token in *text*, which a block sitting on a
/// visual canvas doesn't have. Round-tripping through this spike is only
/// meant to prove structural (AST-shape) equivalence, not source-position
/// preservation - `blockToStmt`/`blockToExpr` below synthesize a fixed
/// placeholder position for any node they reconstruct, and
/// `BlocksTests.fs`'s own comparison helper normalizes positions away
/// before asserting equality, exactly mirroring the stance `Sugar.fs`
/// already established project-wide: round trip means AST-equivalence, not
/// textual (or, here, positional) identity.
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
    /// computed form (`obj.(expr)`) is out of scope for this spike (the
    /// research doc: "invisible to static analysis already, a block editor
    /// inherits that exact ceiling, not a regression" - zero upside to
    /// building a second block variant for it yet).
    | VProp of receiver: BlockValue * name: string
    /// Literal-name verb calls only, same reasoning as `VProp`.
    | VVerbCall of receiver: BlockValue * name: string * args: BlockArg list
    | VCall of name: string * args: BlockArg list
    | VListLit of BlockArg list
    | VIndex of BlockValue * BlockValue
    /// The escape hatch - anything this spike's subset doesn't cover
    /// (`Range` used as a for-bound, `Catch`, `Scatter`, computed
    /// `Prop`/`VerbCall`, ...), carrying the real `Expr` verbatim so
    /// `blockToExpr` can still reproduce it exactly.
    | VUnsupported of Expr

/// A call/list argument - mirrors `Ast.Arg` exactly.
and BlockArg =
    | AArg of BlockValue
    | ASplice of BlockValue

/// One statement ("action"/"stack") block. Mirrors `Ast.Stmt`'s covered
/// subset. `SUnsupported` is the same escape hatch as `VUnsupported`, for
/// `ForList`/`ForRange`/`Fork`/`TryExcept`/`TryFinally`/`ErrorStmt` and any
/// other statement shape outside this spike's chosen subset.
type BlockStmt =
    | SIf of arms: (BlockValue * BlockStmt list) list * elsePart: BlockStmt list option
    | SWhile of name: string option * cond: BlockValue * body: BlockStmt list
    | SReturn of BlockValue option
    | SBreak of string option
    | SContinue of string option
    | SExpr of BlockValue
    | SUnsupported of Stmt

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
    | Index(e, i) -> VIndex(exprToBlock e, exprToBlock i)
    | FirstIndex
    | LastIndex
    | Prop _
    | VerbCall _
    | Range _
    | Catch _
    | Scatter _
    | MapLit _ -> VUnsupported expr

/// `BlockValue` -> `Ast.Expr`, total - the reverse of `exprToBlock`.
/// Reconstructed `Ident`/`Prop`/`VerbCall`/`Call` nodes get a fixed `1, 1`
/// placeholder position (see the module's own doc comment on why a real
/// position can't be recovered, and isn't the point of this spike).
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
    | VIndex(e, i) -> Index(blockToExpr e, blockToExpr i)
    | VUnsupported expr -> expr

/// `Ast.Stmt` -> `BlockStmt`, total.
let rec stmtToBlock (stmt: Stmt) : BlockStmt =
    match stmt with
    | If(arms, elsePart) -> SIf(arms |> List.map (fun (c, body) -> exprToBlock c, body |> List.map stmtToBlock), elsePart |> Option.map (List.map stmtToBlock))
    | While(name, cond, body) -> SWhile(name, exprToBlock cond, body |> List.map stmtToBlock)
    | Return e -> SReturn(e |> Option.map exprToBlock)
    | Break name -> SBreak name
    | Continue name -> SContinue name
    | ExprStmt e -> SExpr(exprToBlock e)
    | ForList _
    | ForRange _
    | Fork _
    | TryExcept _
    | TryFinally _
    | ErrorStmt _ -> SUnsupported stmt

/// `BlockStmt` -> `Ast.Stmt`, total - the reverse of `stmtToBlock`.
let rec blockToStmt (block: BlockStmt) : Stmt =
    match block with
    | SIf(arms, elsePart) -> If(arms |> List.map (fun (c, body) -> blockToExpr c, body |> List.map blockToStmt), elsePart |> Option.map (List.map blockToStmt))
    | SWhile(name, cond, body) -> While(name, blockToExpr cond, body |> List.map blockToStmt)
    | SReturn v -> Return(v |> Option.map blockToExpr)
    | SBreak name -> Break name
    | SContinue name -> Continue name
    | SExpr v -> ExprStmt(blockToExpr v)
    | SUnsupported stmt -> stmt

/// The two directions the card's own research flagged as needing a spike -
/// `astToBlocks` is the genuinely uncertain one (existing code -> blocks),
/// `blocksToAst` is what a real Blockly generator would eventually feed
/// back (blocks -> code), needed here only to prove the round trip.
let astToBlocks (stmts: Stmt list) : BlockStmt list = stmts |> List.map stmtToBlock
let blocksToAst (blocks: BlockStmt list) : Stmt list = blocks |> List.map blockToStmt
