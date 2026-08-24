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
/// `moo_call`/`moo_verbcall`/`moo_list`/`moo_except_arm`/`moo_catch` (the
/// fixed-arity-with-splices constructs) get a small **fixed** number of
/// `ARGi`/`CODEi` sockets rather than a full drag-based mutator dialog - a
/// deliberate simplification (see `BlocklyJson.fs`'s own doc comment on why
/// this is still correct: the argument count is recovered from which
/// sockets are actually connected, not from a reported count).
/// `moo_call_extra_state` is a tiny registered extension that only exists
/// so `splices` (was argument/code `i` written with `@`) survives a real
/// save/load cycle - it defines `saveExtraState`/`loadExtraState` but no
/// `compose`/`decompose`, so it never shows a mutator-dialog gear icon.
/// **Must be wired up via each block's own `"mutator"` JSON field, not
/// `"extensions"`** - confirmed live: Blockly's own `Block.jsonInit`
/// applies `"extensions"` entries with `isMutator = false`, which runs a
/// strict sanity check requiring the block's own mutator-related
/// properties (`saveExtraState`/`loadExtraState`/etc.) to be *identical*
/// before and after the extension runs; since this extension's whole job
/// is to *add* `saveExtraState`/`loadExtraState` where none existed, that
/// check always fails ("mutation properties changed when applying a
/// non-mutator extension"), thrown the moment any block using it is
/// constructed (fresh from the toolbox, or loaded from a saved state) -
/// `"mutator"` applies with `isMutator = true` instead, which skips that
/// specific check.
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
/// `BODY`/`ARG0..ARG3`/`VAR`/`INDEXVAR`/`SOURCE`/`LO`/`HI`/`DELAY`/
/// `HANDLER`/`KEY0..KEY3`/`VAL0..VAL3`/`KIND`/`CODE0..CODE3`/`TRY`/
/// `FALLBACK`/`ARMS`/`ITEM0..ITEM3`/`DEFAULT`) - this module and that one
/// must never drift apart on those spellings.
///
/// `moo_except_arm` chains into `moo_try_except`'s own `ARMS` slot via a
/// *typed* `previousStatement`/`nextStatement` check (both declare
/// `"moo_except_arm"`, not `null`) - it never chains into an ordinary
/// statement body. `moo_scatter_item` is the value-side equivalent: a
/// typed `output`/`check` pair (`"moo_scatter_item"`) so it only plugs
/// into `moo_scatter`'s own `ITEMi` sockets, never a real expression
/// input. Both reuse `moo_call_extra_state` for their own fixed
/// `CODE0..CODE3` sockets' splice flags - the extension is registered
/// once and is not tied to a single block type.
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

/// A non-blocking `onchange`-driven warning, not a hard connection-type
/// refusal - confirmed live (this project's own research) that
/// `Blockly.serialization.workspaces.load` throws on a mismatched-type
/// connection during deserialization, so retrofitting `"output"`/`"check"`
/// typing onto `moo_obj`/`moo_err` to make Blockly outright refuse the
/// connection would risk breaking the load of any pre-existing saved verb
/// that already has this pattern. A `setWarningText` icon achieves the
/// same "surface the gotcha" goal without that risk: MOOcode's own real,
/// documented footgun is that an OBJ/ERR value tests false in a boolean
/// context even when it's a valid object/error - `#123 ? "y" | "n"` always
/// picks "n" - a mistake raw text and the LSP don't currently catch either.
/// `onchange`/`getInputTargetBlock`/`setWarningText` are real, confirmed
/// `Block` APIs (`core/block.d.ts`); this extension only sets `onchange`
/// (no `saveExtraState`/`loadExtraState`/`compose`/`decompose`), so per
/// this file's own doc comment on `"extensions"` vs `"mutator"` above, it's
/// safe as a plain `"extensions"` entry - confirmed the sanity check
/// Blockly runs on non-mutator extensions only inspects those four
/// mutation-related properties, never `onchange`.
let private booleanContextWarningExtensionFn: obj =
    emitJsExpr
        ()
        """(function() {
        this.setOnChange(function() {
            var cond = this.getInputTargetBlock('COND');
            var isAlwaysFalse = cond && (cond.type === 'moo_obj' || cond.type === 'moo_err');
            this.setWarningText(isAlwaysFalse
                ? 'An object or error value is always false in a boolean context in MOOcode, even a valid object reference - this condition will never be true.'
                : null);
        });
    })"""

/// A plain mutable JS object (not an F# value - `emitJsExpr`'s `$0`
/// splices in *values*, so this is the shared handle both
/// `callNameValidatorExtensionFn`'s closures and `setKnownBuiltins` below
/// read/write) holding the live builtins-name list. Starts empty; `moo_call`
/// blocks constructed before the first successful fetch simply see no
/// warning at all (the validator below explicitly no-ops while this is
/// still empty), rather than every name looking "unknown" before the real
/// list has loaded.
let private knownBuiltinsHolder: obj = emitJsExpr () "({ names: [] })"

/// Sets the live builtins-name list every `moo_call` block's own NAME
/// warning check reads - called from `App.fs` once the same live builtins
/// cache the MOOcode docs panel already uses (`LspClient.
/// getMoocodeDocsAsync`) has loaded. `BlocklyEditor.fs` has no dependency
/// on `LspClient`/the LSP connection itself - `App.fs`'s `BlocklyToggle`
/// module (which already owns both) is the one that fetches and calls
/// this, keeping this module's own dependency surface unchanged.
///
/// Also re-checks every `moo_call` block already sitting in every live
/// workspace (`Workspace.getAll()`/`getBlocksByType` - real, confirmed
/// APIs), not just the holder itself - confirmed live this session that
/// this is necessary, not defensive-only: the async fetch this feeds
/// resolves well after a workspace loaded from existing text has already
/// created its blocks (each already fired its own one-time `onchange` with
/// an empty list, correctly finding nothing unknown yet), and `onchange`
/// never fires again on its own just because the list changed later -
/// without this, any verb converted to blocks before the first fetch
/// finished would silently never get checked at all.
let setKnownBuiltins (names: string[]) : unit =
    knownBuiltinsHolder?names <- names
    let workspaces: obj[] = blockly?Workspace?getAll ()

    for ws in workspaces do
        let callBlocks: obj[] = ws?getBlocksByType ("moo_call", false)

        for b in callBlocks do
            let name: string = b?getFieldValue ("NAME")
            let isUnknown = names.Length > 0 && not (names |> Array.contains name)

            if isUnknown then
                b?setWarningText (sprintf "\"%s\" is not a known MOO builtin function." name)
            else
                b?setWarningText (null: string)

/// `moo_call`'s NAME field is the one place raw-text `IDENT(args)` calls
/// live in this language - always a builtin, never a user verb (those go
/// through `moo_verbcall`'s `receiver:name(args)` instead) - so validating
/// live against the real builtins list is meaningful, not just a random
/// guess. Warn-only, never rejects - a not-yet-real builtin name (a typo,
/// or one this project's own parser would still happily accept per its own
/// lenient stance) never blocks editing, just flags it.
///
/// Deliberately `onchange`-driven (matching `booleanContextWarningExtensionFn`
/// exactly), NOT `Field.setValidator` - confirmed live this session that a
/// validator only fires on a *live user edit* of the field, never during
/// deserialization (`Blockly.serialization.workspaces.load`, what every
/// verb converted straight from existing text goes through) - so a verb
/// round-tripped in already containing an unknown builtin name silently
/// showed no warning at all until the field was manually re-edited, the
/// exact opposite of what this feature is for. `onchange` fires on block
/// creation (including from deserialization) as well as later edits, so it
/// catches both. Same non-mutator-safe shape as
/// `booleanContextWarningExtensionFn` above (only sets `onchange` +
/// warning text, no mutation-related properties).
let private callNameValidatorExtensionFn: obj =
    emitJsExpr
        knownBuiltinsHolder
        """(function() {
        var holder = $0;
        this.setOnChange(function() {
            var known = holder.names;
            var name = this.getFieldValue('NAME');
            var isUnknown = known.length > 0 && known.indexOf(name) === -1;
            this.setWarningText(isUnknown ? ('"' + name + '" is not a known MOO builtin function.') : null);
        });
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
        ], "extensions": ["moo_boolean_context_warning"], "output": null, "style": "logic_blocks"},
        {"type": "moo_assign", "message0": "%1 = %2", "args0": [
            {"type": "input_value", "name": "TARGET"}, {"type": "input_value", "name": "VALUE"}
        ], "inputsInline": true, "output": null, "style": "variable_blocks"},
        {"type": "moo_prop", "message0": "%1 . %2", "args0": [
            {"type": "input_value", "name": "RECEIVER"}, {"type": "field_input", "name": "NAME", "text": "prop"}
        ], "inputsInline": true, "output": null, "style": "variable_blocks"},
        {"type": "moo_computed_prop", "message0": "%1 . ( %2 )", "args0": [
            {"type": "input_value", "name": "RECEIVER"}, {"type": "input_value", "name": "NAME"}
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
        "mutator": "moo_call_extra_state", "inputsInline": true, "output": null, "style": "procedure_blocks"},
        {"type": "moo_computed_verbcall", "message0": "%1 : ( %2 ) (", "args0": [
            {"type": "input_value", "name": "RECEIVER"}, {"type": "input_value", "name": "NAME"}
        ], "message1": "%1 %2 %3 %4", "args1": [
            {"type": "input_value", "name": "ARG0"}, {"type": "input_value", "name": "ARG1"},
            {"type": "input_value", "name": "ARG2"}, {"type": "input_value", "name": "ARG3"}
        ], "message2": ")",
        "mutator": "moo_call_extra_state", "inputsInline": true, "output": null, "style": "procedure_blocks"},
        {"type": "moo_call", "message0": "%1 (", "args0": [
            {"type": "field_input", "name": "NAME", "text": "func"}
        ], "message1": "%1 %2 %3 %4", "args1": [
            {"type": "input_value", "name": "ARG0"}, {"type": "input_value", "name": "ARG1"},
            {"type": "input_value", "name": "ARG2"}, {"type": "input_value", "name": "ARG3"}
        ], "message2": ")",
        "mutator": "moo_call_extra_state", "extensions": ["moo_call_name_validator"],
        "inputsInline": true, "output": null, "style": "procedure_blocks"},
        {"type": "moo_list", "message0": "{ %1 %2 %3 %4 }", "args0": [
            {"type": "input_value", "name": "ARG0"}, {"type": "input_value", "name": "ARG1"},
            {"type": "input_value", "name": "ARG2"}, {"type": "input_value", "name": "ARG3"}
        ], "mutator": "moo_call_extra_state", "inputsInline": true, "output": null, "style": "list_blocks"},
        {"type": "moo_if", "message0": "if %1", "args0": [{"type": "input_value", "name": "COND"}],
        "message1": "then %1", "args1": [{"type": "input_statement", "name": "THEN"}],
        "message2": "else %1", "args2": [{"type": "input_statement", "name": "ELSE"}],
        "extensions": ["moo_boolean_context_warning"],
        "previousStatement": null, "nextStatement": null, "style": "logic_blocks"},
        {"type": "moo_while", "message0": "while [%1] %2", "args0": [
            {"type": "field_input", "name": "LABEL", "text": ""}, {"type": "input_value", "name": "COND"}
        ], "message1": "do %1", "args1": [{"type": "input_statement", "name": "BODY"}],
        "tooltip": "The bracketed name is just a label for break/continue to target by name - plain text, not a real MOO variable (unlike fork's task name, below).",
        "extensions": ["moo_boolean_context_warning"],
        "previousStatement": null, "nextStatement": null, "style": "loop_blocks"},
        {"type": "moo_return", "message0": "return %1", "args0": [{"type": "input_value", "name": "VALUE"}],
        "previousStatement": null, "nextStatement": null, "style": "loop_blocks"},
        {"type": "moo_break", "message0": "break [%1]", "args0": [{"type": "field_input", "name": "LABEL", "text": ""}],
        "tooltip": "Targets a while-loop's bracketed label by name - leave blank to break the innermost loop.",
        "previousStatement": null, "nextStatement": null, "style": "loop_blocks"},
        {"type": "moo_continue", "message0": "continue [%1]", "args0": [{"type": "field_input", "name": "LABEL", "text": ""}],
        "tooltip": "Targets a while-loop's bracketed label by name - leave blank to continue the innermost loop.",
        "previousStatement": null, "nextStatement": null, "style": "loop_blocks"},
        {"type": "moo_expr", "message0": "%1 ;", "args0": [{"type": "input_value", "name": "VALUE"}],
        "previousStatement": null, "nextStatement": null, "style": "variable_blocks"},
        {"type": "moo_comment", "message0": "# %1", "args0": [{"type": "field_input", "name": "TEXT", "text": ""}],
        "previousStatement": null, "nextStatement": null, "style": "comment_blocks"},
        {"type": "moo_forlist", "message0": "for %1 [%2] in ( %3 )", "args0": [
            {"type": "field_input", "name": "VAR", "text": "x"},
            {"type": "field_input", "name": "INDEXVAR", "text": ""},
            {"type": "input_value", "name": "SOURCE"}
        ], "message1": "do %1", "args1": [{"type": "input_statement", "name": "BODY"}],
        "previousStatement": null, "nextStatement": null, "style": "loop_blocks"},
        {"type": "moo_forrange", "message0": "for %1 in [ %2 .. %3 ]", "args0": [
            {"type": "field_input", "name": "VAR", "text": "x"},
            {"type": "input_value", "name": "LO"}, {"type": "input_value", "name": "HI"}
        ], "message1": "do %1", "args1": [{"type": "input_statement", "name": "BODY"}],
        "previousStatement": null, "nextStatement": null, "style": "loop_blocks"},
        {"type": "moo_fork", "message0": "fork task %1 after %2", "args0": [
            {"type": "field_input", "name": "NAME", "text": ""}, {"type": "input_value", "name": "DELAY"}
        ], "message1": "do %1", "args1": [{"type": "input_statement", "name": "BODY"}],
        "tooltip": "Unlike while's bracketed label, this name (if given) is a real MOO variable bound to the forked task's id - readable elsewhere in this verb, e.g. kill_task(name). Leave blank for an anonymous fork.",
        "previousStatement": null, "nextStatement": null, "style": "loop_blocks"},
        {"type": "moo_try_finally", "message0": "try %1", "args0": [{"type": "input_statement", "name": "BODY"}],
        "message1": "finally %1", "args1": [{"type": "input_statement", "name": "HANDLER"}],
        "previousStatement": null, "nextStatement": null, "style": "logic_blocks"},
        {"type": "moo_range", "message0": "%1 .. %2", "args0": [
            {"type": "input_value", "name": "LO"}, {"type": "input_value", "name": "HI"}
        ], "inputsInline": true, "output": null, "style": "list_blocks"},
        {"type": "moo_firstindex", "message0": "$ (first)", "args0": [], "output": null, "style": "list_blocks"},
        {"type": "moo_lastindex", "message0": "^ (last)", "args0": [], "output": null, "style": "list_blocks"},
        {"type": "moo_map", "message0": "[ %1 -> %2 , %3 -> %4 , %5 -> %6 , %7 -> %8 ]", "args0": [
            {"type": "input_value", "name": "KEY0"}, {"type": "input_value", "name": "VAL0"},
            {"type": "input_value", "name": "KEY1"}, {"type": "input_value", "name": "VAL1"},
            {"type": "input_value", "name": "KEY2"}, {"type": "input_value", "name": "VAL2"},
            {"type": "input_value", "name": "KEY3"}, {"type": "input_value", "name": "VAL3"}
        ], "inputsInline": true, "output": null, "style": "list_blocks"},
        {"type": "moo_except_arm", "message0": "except %1 (%2) %3 %4 %5 %6", "args0": [
            {"type": "field_input", "name": "NAME", "text": ""},
            {"type": "field_dropdown", "name": "KIND", "options": [["any", "ANY"], ["codes", "CODES"]]},
            {"type": "input_value", "name": "CODE0"}, {"type": "input_value", "name": "CODE1"},
            {"type": "input_value", "name": "CODE2"}, {"type": "input_value", "name": "CODE3"}
        ], "message1": "do %1", "args1": [{"type": "input_statement", "name": "BODY"}],
        "previousStatement": "moo_except_arm", "nextStatement": "moo_except_arm",
        "mutator": "moo_call_extra_state", "style": "logic_blocks"},
        {"type": "moo_try_except", "message0": "try %1", "args0": [{"type": "input_statement", "name": "BODY"}],
        "message1": "except %1", "args1": [{"type": "input_statement", "name": "ARMS", "check": "moo_except_arm"}],
        "previousStatement": null, "nextStatement": null, "style": "logic_blocks"},
        {"type": "moo_catch", "message0": "catch %1", "args0": [{"type": "input_value", "name": "TRY"}],
        "message1": "codes %1 %2 %3 %4 %5", "args1": [
            {"type": "field_dropdown", "name": "KIND", "options": [["any", "ANY"], ["codes", "CODES"]]},
            {"type": "input_value", "name": "CODE0"}, {"type": "input_value", "name": "CODE1"},
            {"type": "input_value", "name": "CODE2"}, {"type": "input_value", "name": "CODE3"}
        ], "message2": "fallback %1", "args2": [{"type": "input_value", "name": "FALLBACK"}],
        "mutator": "moo_call_extra_state", "output": null, "style": "logic_blocks"},
        {"type": "moo_scatter_item", "message0": "%1 %2 %3", "args0": [
            {"type": "field_dropdown", "name": "KIND", "options": [["required", "REQUIRED"], ["optional", "OPTIONAL"], ["rest", "REST"]]},
            {"type": "field_input", "name": "NAME", "text": "x"},
            {"type": "input_value", "name": "DEFAULT"}
        ], "inputsInline": true, "output": "moo_scatter_item", "style": "variable_blocks"},
        {"type": "moo_scatter", "message0": "{ %1 %2 %3 %4 } = %5", "args0": [
            {"type": "input_value", "name": "ITEM0", "check": "moo_scatter_item"},
            {"type": "input_value", "name": "ITEM1", "check": "moo_scatter_item"},
            {"type": "input_value", "name": "ITEM2", "check": "moo_scatter_item"},
            {"type": "input_value", "name": "ITEM3", "check": "moo_scatter_item"},
            {"type": "input_value", "name": "VALUE"}
        ], "inputsInline": true, "output": null, "style": "variable_blocks"}
    ]"""

/// Registers this slice's block set and extension with Blockly - call once,
/// before mounting any workspace, mirroring `Monaco.registerMoocodeLanguage`'s
/// own "register once, create/inject as many times as needed afterward"
/// shape.
let register () : unit =
    blockly?Extensions?register ("moo_call_extra_state", extraStateExtensionFn)
    blockly?Extensions?register ("moo_boolean_context_warning", booleanContextWarningExtensionFn)
    blockly?Extensions?register ("moo_call_name_validator", callNameValidatorExtensionFn)
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
                {"kind": "block", "type": "moo_try_finally"},
                {"kind": "block", "type": "moo_try_except"},
                {"kind": "block", "type": "moo_except_arm"},
                {"kind": "block", "type": "moo_catch"},
                {"kind": "block", "type": "moo_binary", "fields": {"OP": "EQ"}},
                {"kind": "block", "type": "moo_unary", "fields": {"OP": "NOT"}}
            ]},
            {"kind": "category", "name": "Loops", "colour": "120", "contents": [
                {"kind": "block", "type": "moo_while"},
                {"kind": "block", "type": "moo_forlist"},
                {"kind": "block", "type": "moo_forrange"},
                {"kind": "block", "type": "moo_fork"},
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
                {"kind": "block", "type": "moo_index"},
                {"kind": "block", "type": "moo_range"},
                {"kind": "block", "type": "moo_firstindex"},
                {"kind": "block", "type": "moo_lastindex"},
                {"kind": "block", "type": "moo_map"}
            ]},
            {"kind": "category", "name": "Objects", "colour": "20", "contents": [
                {"kind": "block", "type": "moo_obj"},
                {"kind": "block", "type": "moo_err"},
                {"kind": "block", "type": "moo_prop"},
                {"kind": "block", "type": "moo_computed_prop"},
                {"kind": "block", "type": "moo_verbcall"},
                {"kind": "block", "type": "moo_computed_verbcall"}
            ]},
            {"kind": "category", "name": "Functions", "colour": "290", "contents": [
                {"kind": "block", "type": "moo_call"}
            ]},
            {"kind": "category", "name": "Variables", "colour": "330", "contents": [
                {"kind": "block", "type": "moo_ident"},
                {"kind": "block", "type": "moo_assign"},
                {"kind": "block", "type": "moo_expr"},
                {"kind": "block", "type": "moo_scatter"},
                {"kind": "block", "type": "moo_scatter_item"}
            ]}
        ]
    })"""

/// Classic (Blockly's built-in default theme, confirmed the only style set
/// this workspace would otherwise get - `inject` used to pass no `theme`
/// option at all) ships `colour_blocks`/`list_blocks`/`logic_blocks`/
/// `loop_blocks`/`math_blocks`/`procedure_blocks`/`text_blocks`/
/// `variable_blocks`/`variable_dynamic_blocks`/`hat_blocks` - no comment
/// style. `base: "classic"` inherits everything else from it; only
/// `comment_blocks` (referenced by `moo_comment`'s own `"style"` above) is
/// new - a muted grey, visually distinct from every real-code block, so a
/// MOO `"note";` comment idiom reads as commentary at a glance instead of
/// looking like just another statement block.
let private theme: obj =
    emitJsExpr
        ()
        """({
        "base": "classic",
        "blockStyles": {
            "comment_blocks": {"colourPrimary": "#7f7f7f", "colourSecondary": "#666666", "colourTertiary": "#4d4d4d"}
        }
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
              "theme" ==> theme
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
