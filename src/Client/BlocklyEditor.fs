/// Thin interop over the `blockly` npm package, mirroring `Monaco.fs`'s own
/// house style (untyped `importAll` + `emitJsExpr`/`emitJsStatement` for
/// bulk static JS data, dynamic `?` access for anything one-off, no
/// `[<Import>]` attributes). This is the JS-side half of the Blockly visual
/// editor spike - `Language.BlocklyJson` (pure F#) owns the actual
/// `BlockStmt`/`BlockValue` <-> serialized-shape mapping; this module only
/// mounts/tears down a real Blockly workspace and crosses the `obj`/text
/// boundary via `JS.JSON.stringify`/`JS.JSON.parse` against
/// `BlocklyJson.toJsonText`/`parseJsonText` - no hand-written recursive walk
/// of a real JS object's runtime type anywhere here.
///
/// Block set covers exactly `BlocklyJson.fs`'s own covered subset (see that
/// module's doc comment) - one Blockly block type per `BlockValue`/
/// `BlockStmt` case, `moo_binary`/`moo_unary` sharing one block type per
/// family with an `OP` dropdown (not one block type per operator, matching
/// how Blockly's own `math_arithmetic`/`logic_compare` already work).
///
/// `moo_call`/`moo_verbcall`/`moo_list` (the three variable-arity
/// constructs) get a small **fixed** number of `ARGi` sockets rather than a
/// full drag-based mutator - a deliberate first-slice simplification (see
/// `BlocklyJson.fs`'s own doc comment on why this is still correct: the
/// argument count is recovered from which sockets are actually connected,
/// not from a reported count). `moo_call_extra_state` is a tiny registered
/// extension (not a full mutator) that only exists so `splices` (was
/// argument `i` written with `@`) survives a real save/load cycle - nothing
/// in this slice's toolbox actually constructs a splice argument yet, but a
/// verb round-tripped in from real text might already have one.
module Client.BlocklyEditor

open Fable.Core
open Fable.Core.JsInterop

let private blockly: obj = importAll "blockly"

/// Every block this slice covers, JSON-format (`Blockly.common.
/// defineBlocksWithJsonArray`) - static data, no F# values spliced in, same
/// "written as a raw JS literal, not built through generic interop
/// helpers" call `Monaco.fs`'s own Monarch grammar already makes for the
/// same reason. Field/input names match `BlocklyJson.fs`'s own
/// `fields`/`inputs` keys exactly (`NUM`/`TEXT`/`CODE`/`NAME`/`OP`/`LABEL`,
/// `LEFT`/`RIGHT`/`VALUE`/`COND`/`THEN`/`ELSE`/`TARGET`/`RECEIVER`/`INDEX`/
/// `BODY`/`ARG0..ARG3`) - this module and that one must never drift apart
/// on those spellings.
/// `moo_call`/`moo_verbcall`/`moo_list` each get exactly 4 fixed `ARGi`
/// argument sockets (`ARG0`..`ARG3`) - this constant documents that number
/// everywhere it matters (`BlocklyJson.fs` doesn't hardcode it at all,
/// since it just walks `ARG0`, `ARG1`, ... until the first absent input,
/// so it never needs to know the JS-side maximum). The block JSON below is
/// one fully static literal, deliberately not built by splicing computed
/// F# strings into the `emitJsStatement` template below (that macro's `$0`/
/// `$1` placeholders splice in *values*, not raw JS source text - the
/// literal template itself must be static, matching `Monaco.fs`'s own
/// Monarch-grammar precedent exactly: "written as a raw JS object literal
/// ... this is static data with no F# values spliced in").
let private maxCallArity = 4

/// The `moo_call_extra_state` extension function itself - static data (a
/// function *value*, never called from here), matching the same
/// "`emitJsExpr` for an inert JS blob, a separate dynamic call elsewhere to
/// actually register it" split `registerMoocodeLanguage` uses for
/// `moocodeLanguage`/`moocodeTheme` below. This is the fix for a real bug
/// hit live: an earlier version of this file called `Blockly.Extensions.
/// register(...)`/`Blockly.common.defineBlocksWithJsonArray(...)` directly
/// inside a bare `emitJsStatement`, referencing a global `Blockly` that
/// doesn't exist - `importAll "blockly"` binds the module namespace to this
/// file's own `blockly` local, it never touches `window.Blockly` - so this
/// threw `Cannot read properties of undefined (reading 'register')` on
/// every page load. Registration now happens through `blockly?...`
/// dynamic calls in `register()`, same as everything else in this file.
let private extraStateExtensionFn: obj =
    emitJsExpr
        ()
        """(function() {
        this.spliceFlags_ = [false, false, false, false];
        this.saveExtraState = function() {
            return {'splices': this.spliceFlags_};
        };
        this.loadExtraState = function(state) {
            this.spliceFlags_ = (state && state['splices']) || [false, false, false, false];
        };
    })"""

let private blockDefinitions: obj =
    emitJsExpr
        ()
        """[
        {"type": "moo_int", "message0": "%1", "args0": [{"type": "field_number", "name": "NUM", "value": 0, "precision": 1}], "output": null, "style": "math_blocks"},
        {"type": "moo_float", "message0": "%1", "args0": [{"type": "field_number", "name": "NUM", "value": 0}], "output": null, "style": "math_blocks"},
        {"type": "moo_string", "message0": "\"%1\"", "args0": [{"type": "field_input", "name": "TEXT", "text": ""}], "output": null, "style": "text_blocks"},
        {"type": "moo_obj", "message0": "#%1", "args0": [{"type": "field_number", "name": "NUM", "value": 0, "precision": 1}], "output": null, "style": "math_blocks"},
        {"type": "moo_err", "message0": "%1", "args0": [{"type": "field_input", "name": "CODE", "text": "E_NONE"}], "output": null, "style": "logic_blocks"},
        {"type": "moo_bool", "message0": "%1", "args0": [{"type": "field_dropdown", "name": "BOOL", "options": [["true", "TRUE"], ["false", "FALSE"]]}], "output": null, "style": "logic_blocks"},
        {"type": "moo_ident", "message0": "%1", "args0": [{"type": "field_input", "name": "NAME", "text": "x"}], "output": null, "style": "variable_blocks"},
        {"type": "moo_binary", "message0": "%1 %2 %3", "args0": [
            {"type": "input_value", "name": "LEFT"},
            {"type": "field_dropdown", "name": "OP", "options": [
                ["+", "ADD"], ["-", "SUB"], ["*", "MUL"], ["/", "DIV"], ["%", "MOD"], ["^", "POW"],
                ["==", "EQ"], ["!=", "NEQ"], ["<", "LT"], ["<=", "LTE"], [">", "GT"], [">=", "GTE"],
                ["&&", "AND"], ["||", "OR"], ["in", "IN"],
                ["&.", "BITAND"], ["|.", "BITOR"], ["^.", "BITXOR"], ["<<", "SHL"], [">>", "SHR"]
            ]},
            {"type": "input_value", "name": "RIGHT"}
        ], "inputsInline": true, "output": null, "style": "math_blocks"},
        {"type": "moo_unary", "message0": "%1 %2", "args0": [
            {"type": "field_dropdown", "name": "OP", "options": [["-", "NEG"], ["!", "NOT"], ["~", "BITNOT"]]},
            {"type": "input_value", "name": "VALUE"}
        ], "inputsInline": true, "output": null, "style": "math_blocks"},
        {"type": "moo_cond", "message0": "if %1 then %2 else %3", "args0": [
            {"type": "input_value", "name": "COND"}, {"type": "input_value", "name": "THEN"}, {"type": "input_value", "name": "ELSE"}
        ], "output": null, "style": "logic_blocks"},
        {"type": "moo_assign", "message0": "%1 = %2", "args0": [
            {"type": "input_value", "name": "TARGET"}, {"type": "input_value", "name": "VALUE"}
        ], "inputsInline": true, "output": null, "style": "variable_blocks"},
        {"type": "moo_prop", "message0": "%1 . %2", "args0": [
            {"type": "input_value", "name": "RECEIVER"}, {"type": "field_input", "name": "NAME", "text": "prop"}
        ], "inputsInline": true, "output": null, "style": "variable_blocks"},
        {"type": "moo_index", "message0": "%1 [ %2 ]", "args0": [
            {"type": "input_value", "name": "VALUE"}, {"type": "input_value", "name": "INDEX"}
        ], "inputsInline": true, "output": null, "style": "list_blocks"},
        {"type": "moo_verbcall", "message0": "%1 : %2 (", "args0": [
            {"type": "input_value", "name": "RECEIVER"}, {"type": "field_input", "name": "NAME", "text": "verb"}
        ], "message1": "%1 %2 %3 %4", "args1": [
            {"type": "input_value", "name": "ARG0"}, {"type": "input_value", "name": "ARG1"},
            {"type": "input_value", "name": "ARG2"}, {"type": "input_value", "name": "ARG3"}
        ], "message2": ")",
        "extensions": ["moo_call_extra_state"], "inputsInline": true, "output": null, "style": "procedure_blocks"},
        {"type": "moo_call", "message0": "%1 (", "args0": [
            {"type": "field_input", "name": "NAME", "text": "func"}
        ], "message1": "%1 %2 %3 %4", "args1": [
            {"type": "input_value", "name": "ARG0"}, {"type": "input_value", "name": "ARG1"},
            {"type": "input_value", "name": "ARG2"}, {"type": "input_value", "name": "ARG3"}
        ], "message2": ")",
        "extensions": ["moo_call_extra_state"], "inputsInline": true, "output": null, "style": "procedure_blocks"},
        {"type": "moo_list", "message0": "{ %1 %2 %3 %4 }", "args0": [
            {"type": "input_value", "name": "ARG0"}, {"type": "input_value", "name": "ARG1"},
            {"type": "input_value", "name": "ARG2"}, {"type": "input_value", "name": "ARG3"}
        ], "extensions": ["moo_call_extra_state"], "inputsInline": true, "output": null, "style": "list_blocks"},
        {"type": "moo_if", "message0": "if %1", "args0": [{"type": "input_value", "name": "COND"}],
        "message1": "then %1", "args1": [{"type": "input_statement", "name": "THEN"}],
        "message2": "else %1", "args2": [{"type": "input_statement", "name": "ELSE"}],
        "previousStatement": null, "nextStatement": null, "style": "logic_blocks"},
        {"type": "moo_while", "message0": "while [%1] %2", "args0": [
            {"type": "field_input", "name": "LABEL", "text": ""}, {"type": "input_value", "name": "COND"}
        ], "message1": "do %1", "args1": [{"type": "input_statement", "name": "BODY"}],
        "previousStatement": null, "nextStatement": null, "style": "loop_blocks"},
        {"type": "moo_return", "message0": "return %1", "args0": [{"type": "input_value", "name": "VALUE"}],
        "previousStatement": null, "nextStatement": null, "style": "loop_blocks"},
        {"type": "moo_break", "message0": "break [%1]", "args0": [{"type": "field_input", "name": "LABEL", "text": ""}],
        "previousStatement": null, "nextStatement": null, "style": "loop_blocks"},
        {"type": "moo_continue", "message0": "continue [%1]", "args0": [{"type": "field_input", "name": "LABEL", "text": ""}],
        "previousStatement": null, "nextStatement": null, "style": "loop_blocks"},
        {"type": "moo_expr", "message0": "%1 ;", "args0": [{"type": "input_value", "name": "VALUE"}],
        "previousStatement": null, "nextStatement": null, "style": "variable_blocks"},
        {"type": "moo_comment", "message0": "# %1", "args0": [{"type": "field_input", "name": "TEXT", "text": ""}],
        "previousStatement": null, "nextStatement": null, "style": "text_blocks"}
    ]"""

/// Registers this slice's block set and extension with Blockly - call once,
/// before mounting any workspace, mirroring `Monaco.registerMoocodeLanguage`'s
/// own "register once, create/inject as many times as needed afterward"
/// shape.
let register () : unit =
    blockly?Extensions?register ("moo_call_extra_state", extraStateExtensionFn)
    blockly?common?defineBlocksWithJsonArray (blockDefinitions)

/// One flyout-per-category toolbox listing every block above. `moo_binary`
/// is listed twice (once per category it's genuinely useful from,
/// `Logic`/`Math`) with a different default `OP` so dragging it out already
/// shows a sensible starting operator rather than always "+" - the same
/// block type either way, just a different initial field value.
let private toolbox: obj =
    emitJsExpr
        ()
        """({
        "kind": "categoryToolbox",
        "contents": [
            {"kind": "category", "name": "Logic", "colour": "210", "contents": [
                {"kind": "block", "type": "moo_bool"},
                {"kind": "block", "type": "moo_cond"},
                {"kind": "block", "type": "moo_if"},
                {"kind": "block", "type": "moo_binary", "fields": {"OP": "EQ"}},
                {"kind": "block", "type": "moo_unary", "fields": {"OP": "NOT"}}
            ]},
            {"kind": "category", "name": "Loops", "colour": "120", "contents": [
                {"kind": "block", "type": "moo_while"},
                {"kind": "block", "type": "moo_break"},
                {"kind": "block", "type": "moo_continue"},
                {"kind": "block", "type": "moo_return"}
            ]},
            {"kind": "category", "name": "Math", "colour": "230", "contents": [
                {"kind": "block", "type": "moo_int"},
                {"kind": "block", "type": "moo_float"},
                {"kind": "block", "type": "moo_binary", "fields": {"OP": "ADD"}},
                {"kind": "block", "type": "moo_unary", "fields": {"OP": "NEG"}}
            ]},
            {"kind": "category", "name": "Text", "colour": "160", "contents": [
                {"kind": "block", "type": "moo_string"},
                {"kind": "block", "type": "moo_comment"}
            ]},
            {"kind": "category", "name": "Lists", "colour": "260", "contents": [
                {"kind": "block", "type": "moo_list"},
                {"kind": "block", "type": "moo_index"}
            ]},
            {"kind": "category", "name": "Objects", "colour": "20", "contents": [
                {"kind": "block", "type": "moo_obj"},
                {"kind": "block", "type": "moo_err"},
                {"kind": "block", "type": "moo_prop"},
                {"kind": "block", "type": "moo_verbcall"}
            ]},
            {"kind": "category", "name": "Functions", "colour": "290", "contents": [
                {"kind": "block", "type": "moo_call"}
            ]},
            {"kind": "category", "name": "Variables", "colour": "330", "contents": [
                {"kind": "block", "type": "moo_ident"},
                {"kind": "block", "type": "moo_assign"},
                {"kind": "block", "type": "moo_expr"}
            ]}
        ]
    })"""

/// Mounts a fresh Blockly workspace into `container`, returning the real
/// `WorkspaceSvg` as an untyped handle - every other function in this
/// module takes that same handle back, mirroring `Monaco.fs.create`'s own
/// shape (a bare "mount, get a handle" call, no separate registration
/// step).
let inject (container: Browser.Types.HTMLElement) : obj =
    let options =
        createObj
            [ "toolbox" ==> toolbox
              "trashcan" ==> true
              "scrollbars" ==> true
              "zoom" ==> createObj [ "controls" ==> true; "wheel" ==> true; "startScale" ==> 0.9 ]
              "grid" ==> createObj [ "spacing" ==> 20; "length" ==> 3; "colour" ==> "#3a3a3a"; "snap" ==> true ] ]

    blockly?inject (container, options)

/// The workspace's current state as JSON text (`BlocklyJson.parseJsonText`
/// on the caller's side turns this into a `JsonValue`) - `Blockly.
/// serialization.workspaces.save` already returns a plain, JSON-safe object,
/// so crossing the `obj`/text boundary is just `JS.JSON.stringify`.
///
/// `BlocklyJson.fs` only knows about one root block's own chain (a single
/// statement's JSON with a `next` link to the rest) - it never needs to
/// know about Blockly's own outer workspace-state envelope
/// (`{"blocks": {"languageVersion": ..., "blocks": [...one entry per
/// *top-level* block...]}}`, confirmed against a real serialized save).
/// Un/wrapping that envelope - the one piece of real workspace-state shape
/// this slice's toolbox always produces exactly one root block for - is
/// this file's own job, not `BlocklyJson.fs`'s. An empty workspace (the
/// user deleted every block) has no root block to report at all, so this
/// returns `""` rather than some placeholder JSON - `App.fs`'s own
/// `blocklyStateToText` treats that as "an empty verb body," matching
/// `BlocklyJson.stmtsToJson []`'s own `None` (no root block to build) on
/// the way in.
let getStateText (workspace: obj) : string =
    let state = blockly?serialization?workspaces?save (workspace)
    let blocksHolder = state?blocks

    if isNullOrUndefined blocksHolder then
        ""
    else
        let rootBlocks: obj[] = blocksHolder?blocks

        if isNullOrUndefined rootBlocks || rootBlocks.Length = 0 then
            ""
        else
            JS.JSON.stringify rootBlocks.[0]

/// Replaces the workspace's entire contents with `stateText`
/// (`BlocklyJson.toJsonText`'s own output, or `""` for an empty verb body -
/// see `getStateText`'s own comment on the envelope this wraps it in)-
/// clears first (`.load` itself only adds/updates blocks named in `state`,
/// it doesn't remove ones absent from it), matching the "Edit as list/map"
/// toggle's own "Apply overwrites wholesale" interaction, not an
/// incremental merge.
let loadStateText (workspace: obj) (stateText: string) : unit =
    workspace?clear ()

    if stateText <> "" then
        let rootBlock = JS.JSON.parse stateText
        let state = createObj [ "blocks" ==> createObj [ "languageVersion" ==> 0; "blocks" ==> [| rootBlock |] ] ]
        blockly?serialization?workspaces?load (state, workspace) |> ignore

/// Fires `listener` on every workspace mutation (block create/change/move/
/// delete, and others this slice doesn't need to distinguish - re-deriving
/// from a full `getStateText` on any change is simpler and safer than
/// patching incrementally per Blockly event type, per this project's own
/// research into Blockly's event model). Returns a disposer.
let onChange (workspace: obj) (listener: unit -> unit) : (unit -> unit) =
    let handler: obj -> unit = fun _ev -> listener ()
    workspace?addChangeListener (handler)
    fun () -> workspace?removeChangeListener (handler)

let dispose (workspace: obj) : unit = workspace?dispose ()

/// Re-measures the workspace against its container's current size - needed
/// the same way `Monaco.fs`'s own `layout()` is: a workspace mounted while
/// its container was `display:none` doesn't always pick up its real size
/// once shown again.
let resize (workspace: obj) : unit = blockly?svgResize (workspace)
