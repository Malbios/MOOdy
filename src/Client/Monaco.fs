/// Thin interop over the monaco-editor npm package - "Monaco's API is a
/// direct call" per the project plan's settled decisions, so this is
/// intentionally a light wrapper (just enough typed surface for what App.fs
/// needs) rather than a full binding library.
module Client.Monaco

open Fable.Core
open Fable.Core.JsInterop
open Client.LspClient
open Language

// Vite's `?worker` import suffix bundles the target module as a Web Worker
// and gives back its constructor - the standard dependency-free way to wire
// Monaco under Vite (see Monaco's own Vite integration docs). We only ever
// use our own "moocode" Monarch language, never Monaco's built-in
// JSON/CSS/HTML/TS modes, so the plain editor worker is the only one needed.
let private editorWorkerCtor: obj = importDefault "monaco-editor/esm/vs/editor/editor.worker?worker"

emitJsStatement
    editorWorkerCtor
    "self.MonacoEnvironment = { getWorker: function (_moduleId, _label) { return new $0() } }"

let private monaco: obj = importAll "monaco-editor"

/// The Monarch tokenizer for MOOcode. Written as a raw JS object literal
/// (regex literals and all) rather than built up through generic F#
/// interop helpers - Monarch's rule shape (arrays of regex/action pairs,
/// nested tokenizer states) doesn't map cleanly onto typed bindings, and
/// this is static data with no F# values spliced in.
///
/// Deliberately not exhaustive - covers control keywords, built-in verb-call
/// variables, error constants, strings, block comments, object/property
/// references, and numbers - enough for the "stops hurting" bar M3 sets, not
/// a complete language spec.
///
/// Comments are `/* ... */` only (non-nesting - the first `*/` closes),
/// confirmed against `ToastStunt/src/parser.y:889-908` and documented in
/// `moocode-reference.md` (repo root). An earlier version of this comment
/// claimed MOOcode has no comment syntax at all - that was wrong, corrected
/// this session after reading the parser source directly. There is no `//`
/// line-comment form; a bare `/` not followed by `*` is just division.
///
/// `keywords`/`errorLiterals` are the exact two closed sets
/// `Language.Lexer.fs` uses (its own doc comments cite `keywords.gperf`/
/// `parser.y` directly) - kept as separate hand-copied JS arrays rather than
/// shared with that file, same "client keeps its own copy, hardcoded to
/// match" tradeoff `LspClient.fs`'s `semanticTokenTypes` already makes for
/// the same reason (no F#-to-JS-data-literal sharing path exists here).
/// Two corrections against the previous version of this list, both found by
/// re-reading `Lexer.fs`'s own doc comment rather than assumed: `endfinally`
/// isn't real syntax (a `try`/`finally` still closes with plain `endtry`),
/// and `pass` is a registered builtin, not a keyword - `execute.cc:3817`,
/// same source `Lexer.fs`'s own comment cites. `ANY` (the `except e (ANY)`
/// catch-all) was missing entirely; it's a real, case-sensitive keyword.
/// `errorLiterals` replaces the previous open `/\bE_[A-Z]+\b/` regex, which
/// colored *any* `E_something` spelling as if it were a real error code -
/// the actual set is closed at 19 names (`keywords.gperf`); anything else is
/// just an identifier and raises `E_VARNF` at runtime, so highlighting it as
/// a valid constant was actively misleading.
///
/// `builtinVariables` now also covers `true`/`false` and the 11 type-tag
/// constants (`NUM`/`OBJ`/etc.) - confirmed in `sym_table.cc:118-122` (bools)
/// and `sym_table.cc:88-121` (type tags) to be pre-bound entries in the
/// *local variable table*, the exact same mechanism as `this`/`player`/
/// `caller`, not real constants - they're just as shadowable/reassignable.
/// Previously `true`/`false` had their own `constants` category, which
/// rendered identically to `keyword` in vs-dark (`#569CD6` for both) - so
/// `true` already looked exactly like `if`/`while`, backwards from what a
/// pre-bound *variable* should look like. Folding them into
/// `builtinVariables` fixes that collision for free (vs-dark already gives
/// that token its own indigo, `#4864AA`) without adding a new theme rule,
/// and correctly groups OBJ/STR/etc. with `this`/`player` instead of
/// implying an immutability none of them have.
///
/// The old identifier regex's character class included `$`, which meant
/// `$foo` (corponym references, extremely common) always matched *that*
/// rule first (it comes before the dedicated `\$[\w]+` "annotation" rule
/// below) and rendered as a plain identifier - the annotation rule was
/// unreachable dead code. `Language.Lexer.fs`'s own `isIdentStart` confirms
/// `$` was never a real identifier character to begin with
/// (letters/underscore only), so excluding it here is a straight bug fix,
/// not a behavior change for any real MOOcode.
///
/// The operator regex previously had no entry at all for bare `$`/`^` (the
/// parser-context-sensitive index-position tokens, `list[$]`/`list[^]`),
/// `@` (splice, `{@a, @b}`/`f(@args)`), `~` (bitwise complement), or `^` as
/// the exponent operator / first half of `^.` (bitwise xor) - all four fell
/// through as unstyled plain text. Added to the same character class as the
/// other single-char operators.
///
/// Backtick/single-quote (the catch-expression's own asymmetric open/close
/// pair, `` `expr ! codes => fallback' ``) got no styling at all before -
/// confirmed as real, dedicated tokens (`TBacktick`/`TSingleQuote`) in
/// `Language.Lexer.fs`, not string-literal punctuation, and used nowhere
/// else in MOOcode's grammar (no character literals, no other single-quote
/// syntax), so unconditionally coloring a bare `` ` `` or `'` is safe. Given
/// `keyword.flow`'s purple already means "error-handling machinery" here
/// (it's what `constant.error` below reuses for `E_*` codes), reusing it for
/// the inline catch-expression's own delimiters keeps that association
/// consistent rather than introducing a third, unrelated color.
let private moocodeLanguage: obj =
    emitJsExpr
        ()
        """({
        defaultToken: '',
        keywords: [
            'if', 'elseif', 'else', 'endif',
            'for', 'in', 'endfor',
            'while', 'endwhile',
            'fork', 'endfork',
            'try', 'except', 'endtry', 'finally',
            'return', 'break', 'continue', 'ANY'
        ],
        errorLiterals: [
            'E_NONE', 'E_TYPE', 'E_DIV', 'E_PERM', 'E_PROPNF', 'E_VERBNF',
            'E_VARNF', 'E_INVIND', 'E_RECMOVE', 'E_MAXREC', 'E_RANGE',
            'E_ARGS', 'E_NACC', 'E_INVARG', 'E_QUOTA', 'E_FLOAT', 'E_FILE',
            'E_EXEC', 'E_INTRPT'
        ],
        builtinVariables: [
            'this', 'caller', 'player', 'verb', 'args', 'argstr',
            'dobj', 'dobjstr', 'prep', 'prepstr', 'iobj', 'iobjstr',
            'true', 'false',
            'NUM', 'OBJ', 'STR', 'LIST', 'ERR', 'INT', 'FLOAT', 'MAP',
            'ANON', 'WAIF', 'BOOL'
        ],
        tokenizer: {
            root: [
                [/[a-zA-Z_][\w]*/, {
                    cases: {
                        '@keywords': 'keyword',
                        '@errorLiterals': 'constant.error',
                        '@builtinVariables': 'variable.predefined',
                        '@default': 'variable'
                    }
                }],
                [/#-?\d+/, 'annotation'],
                [/\$[\w]+/, 'annotation'],
                [/\d+\.\d+([eE][-+]?\d+)?/, 'number.float'],
                [/\d+/, 'number'],
                [/"([^"\\]|\\.)*"/, 'string'],
                [/[`']/, 'keyword.flow'],
                [/\/\*/, 'comment', '@comment'],
                [/[{}()\[\]]/, '@brackets'],
                [/[<>=!]=|[-+*\/%<>!&|@~^$]=?|=>|\?|:|;|,|\.\.|\./, 'operator'],
                [/\s+/, 'white']
            ],
            comment: [
                [/[^\/*]+/, 'comment'],
                [/\*\//, 'comment', '@pop'],
                [/[\/*]/, 'comment']
            ]
        }
    })"""

/// Auto-indent rules for MOOcode's keyword-delimited blocks (`if/endif`,
/// `for/endfor`, `while/endwhile`, `fork/endfork`, `try/except/finally/
/// endtry`) - there are no braces to hang indentation off of, so this uses
/// Monaco's regex-based `indentationRules` instead of the bracket-based
/// auto-indent most languages get for free. Purely cosmetic: MOOcode's
/// grammar is not whitespace-sensitive (`parser.y` has no significant-
/// indentation rule anywhere), so this can never change what code actually
/// does - it only affects how newly-typed lines get indented.
///
/// `increaseIndentPattern` matches a block-opening line, indenting the
/// *next* line one level deeper - guarded by a negative lookahead so a
/// same-line-closed block (`if (x) return; endif;`, extremely common in
/// this corpus) does not also indent the line after it. `else`/`elseif`/
/// `except`/`finally` deliberately appear in *both* patterns: matching
/// `decreaseIndentPattern` dedents the keyword line itself back to its
/// opening statement's level, and matching `increaseIndentPattern` still
/// indents whatever follows it one level deeper again - the same "outdent-
/// then-indent" shape most editors give `} else {`.
let private moocodeLanguageConfiguration: obj =
    emitJsExpr
        ()
        """({
        indentationRules: {
            increaseIndentPattern: /^\s*(if|elseif|else|for|while|fork|try|except|finally)\b(?!.*\bend(if|for|while|fork|try)\b).*$/,
            decreaseIndentPattern: /^\s*(endif|endfor|endwhile|endfork|endtry|elseif|else|except|finally)\b.*$/
        }
    })"""

/// A "vs-dark" variant adding the rules Monaco's own live semantic-token
/// pass needs that vs-dark doesn't define - `inherit: true` only pulls in
/// vs-dark's *literal* token-name rules, and this project's semantic-token
/// modifiers (`defaultLibrary`/`unresolved`, `Handlers.classifySemanticToken`)
/// aren't real VS Code scope names, so vs-dark has nothing for them; without
/// an explicit rule here they silently fall back to their base type's color
/// with zero visible effect from the modifier.
///
/// - `constant.error` (the Monarch grammar's closed-set error-literal
///   token) gets its own bold purple instead of inheriting `constant`'s
///   blue (`#569CD6`) - which vs-dark's own `keyword` rule already uses
///   too, so plain `E_INVARG`/`E_PERM`/etc. were indistinguishable from
///   control keywords like `if`/`while`. `C586C0` isn't a new color - it's
///   vs-dark's own existing purple (`keyword.flow`, confirmed directly in
///   the installed package's own theme table,
///   `node_modules/monaco-editor/esm/vs/editor/standalone/common/themes.js`),
///   just unused by this grammar otherwise.
/// - `variable` (both the Monarch grammar's own `@default` token *and* the
///   live semantic pass's un-modified token for an ordinary local variable)
///   gets vs-dark's `variable.parameter`/`attribute.name` cyan (`9CDCFE`)
///   instead of inheriting vs-dark's own `variable` rule (`74B0DF`, a
///   *lighter* blue) - confirmed live that `74B0DF` sits close enough to
///   `keyword`'s `569CD6` to read as "basically the same blue" at a glance,
///   which was the actual live complaint this fixes. `9CDCFE` reuses an
///   existing vs-dark hue this grammar doesn't otherwise touch, and is the
///   same shade VS Code's own built-in TypeScript semantic highlighting
///   uses for a local/parameter binding, so it should already read as
///   familiar.
/// - `variable.defaultLibrary` gives the *live* pass's version of "this/
///   player/OBJ/true/..." (`Handlers.classifySemanticToken`'s
///   `defaultLibrary` modifier) the same indigo (`4864AA`) the *static*
///   Monarch grammar's `variable.predefined` token already gets for free
///   from vs-dark - without this rule, the live pass (which runs after the
///   static pass and wins when both apply) was silently repainting those
///   same names back to plain `variable`, undoing the distinction the
///   Monarch grammar alone can't make stick once semantic highlighting
///   takes over.
/// - `method` and `function` (`classifySemanticToken`'s tokens for
///   `obj:verb(...)` calls and bare `name(...)` calls respectively) had the
///   exact same problem, total invisibility rather than a deliberate
///   "leave calls uncolored" choice - vs-dark defines no rule for either
///   scope at all, so both silently fell back to plain body-text color no
///   matter what modifier they carried. `DCDCAA` is VS Code's own
///   long-standing default color for a function/method call (`dark_vs.json`
///   in any VS Code install, `entity.name.function`), immediately familiar
///   and shared between the two so both read as "this calls something" -
///   deliberately not splitting them into two different hues, since the
///   real distinction that matters is resolved-vs-not, handled below.
/// - `method.unresolved` (a verb call whose dispatch target couldn't be
///   statically confirmed - a computed receiver, a name not found by
///   walking the graph, ...) reuses that same yellow but italic, the same
///   "less certain" visual idiom `style.css`'s `.inspector-row-inherited`
///   already uses elsewhere in this app, rather than a different color -
///   it's still definitely a verb call, just one this tooling can't fully
///   vouch for.
/// - `method.broken` (a verb call whose starting object *is* known, but
///   live dispatch from it confirmed no callable verb - this exact call
///   raises E_VERBNF at runtime) gets `F14C4C`, this installed package's
///   own `editorError.foreground` dark-theme color
///   (`platform/theme/common/colors/editorColors.js`) - the real "this is
///   wrong" red, distinct from `unresolved`'s softer "can't tell" italic
///   yellow. Confirmed live (user report) that a confirmed-failed dispatch
///   rendered identically to a merely-unverifiable one; the two are not
///   the same confidence level and shouldn't look the same.
/// - `function.defaultLibrary` (a call the live builtins cache actually
///   recognizes, e.g. `has_property()`) gets its own dusty pink/mauve
///   (`B46695`) instead of inheriting plain `function`'s yellow - confirmed
///   live (user report) that a real builtin call was indistinguishable from
///   an ordinary verb call (`method`), even though the two are completely
///   different kinds of dispatch. `B46695` is vs-dark's own `regexp` color
///   (same "confirmed directly in the installed package's own theme table"
///   sourcing every other color here uses) - picked over the more obvious
///   "just reuse `constant.error`'s purple" because that purple (`C586C0`)
///   is already claimed for error-literal styling, and putting a genuine
///   builtin call in the same hue as `E_INVARG`/`E_PERM` would read as a
///   warning where none exists. Deliberately not doing the equivalent for
///   plain `function` without the modifier (an unrecognized bare-name call)
///   - unlike an unresolved verb dispatch, that can just as easily mean the
///   live builtins cache is stale (see [[Combined refresh for LSP builtins
///   and static graph]]) as it can mean a real compile error, and the real
///   compile-time check (`set_verb_code`'s own error list) is already
///   authoritative for that - a heuristic color here would just be a
///   second, less trustworthy signal for the same thing.
/// - `property` (`classifySemanticToken`'s token for a plain `obj.foo`
///   access) gets vs-dark's own teal `type` color (`3DC9B0`), picked
///   specifically because it's unclaimed by anything else here and reads as
///   clearly its own category rather than a shade of an existing one.
///   `property` itself had the exact same invisibility gap `method`/
///   `function` did before their own fixes - vs-dark defines no rule for
///   it at all, so it silently fell back to plain body-text color.
/// - `property.corponym` (`RefProp` whose receiver is `#0` - i.e. `$foo`
///   property-sugar, not a real dot-access) keeps the Monarch grammar's own
///   `annotation` red (`CC6666`) instead of `property`'s teal. Without this
///   split, giving plain `property` *any* single color (an earlier version
///   of this fix did exactly that, reusing `annotation`'s red for both)
///   made `$string_utils` and an ordinary `this.article` read as the exact
///   same color - confirmed live (user report) as a real regression: two
///   semantically unrelated things (a system-object reference vs. a normal
///   property read) had become visually identical. The underlying reason
///   `$foo` needs its own live-pass rule at all (not just the static
///   grammar's reddish `annotation` token) is the same as before: the live
///   pass's `RefProp` walk covers a `VerbCall`'s receiver sub-expression too
///   (`AstQuery.collectExpr`'s `VerbCall` case recurses into `recvE`), so
///   `$string_utils:capitalize()`'s `$string_utils` half gets repainted by
///   whichever `property`/`property.corponym` rule applies once semantic
///   tokens load - it needs to keep matching the `$` sigil's own static
///   color, not fall into the new plain-property teal.
///   `Handlers.classifySemanticToken`'s `RefVerbCall(ObjLit 0L, ...)` case
///   (call-sugar, `$foo(args)`/an explicit `#0:foo(args)`) reuses this exact
///   `property`/`corponym` classification too, by explicit request - not
///   `method`. `$foo(args)` desugars to a real verb dispatch (`#0:foo(...)`),
///   so `method`'s yellow would be the more "technically precise" choice,
///   and an earlier version of this fix did exactly that (a dedicated
///   Monarch rule coloring `$name(` as `method` to stop it splitting into
///   two colors against the `#0` receiver, which - unlike a `Prop`
///   receiver - generates no live reference at all:
///   `AstQuery.collectExpr`'s `ObjLit` case is a no-op). Overridden to
///   match `$string_utils`'s red instead: the user wants every `$name`
///   reference to read as one consistent "this is $-prefixed" family
///   regardless of whether it's a property read or a verb dispatch, rather
///   than splitting by call-vs-property semantics. Since the receiver `#0`
///   is already static-annotation-red and unaffected by any live token, the
///   live pass just needs to paint the verb name the same red for the two
///   passes to agree - no dedicated Monarch rule needed the way `method`
///   would have required.
let private moocodeTheme: obj =
    createObj
        [ "base" ==> "vs-dark"
          "inherit" ==> true
          "rules" ==>
            [| createObj [ "token" ==> "constant.error"; "foreground" ==> "C586C0"; "fontStyle" ==> "bold" ]
               createObj [ "token" ==> "variable"; "foreground" ==> "9CDCFE" ]
               createObj [ "token" ==> "variable.defaultLibrary"; "foreground" ==> "4864AA" ]
               createObj [ "token" ==> "function"; "foreground" ==> "DCDCAA" ]
               createObj [ "token" ==> "function.defaultLibrary"; "foreground" ==> "B46695" ]
               createObj [ "token" ==> "method"; "foreground" ==> "DCDCAA" ]
               createObj [ "token" ==> "method.unresolved"; "foreground" ==> "DCDCAA"; "fontStyle" ==> "italic" ]
               createObj [ "token" ==> "method.broken"; "foreground" ==> "F14C4C" ]
               createObj [ "token" ==> "property"; "foreground" ==> "3DC9B0" ]
               createObj [ "token" ==> "property.corponym"; "foreground" ==> "CC6666" ] |]
          "colors" ==> createObj [] ]

/// Registers the "moocode" language and its theme with Monaco. Call once,
/// before creating any editor that might use it.
let registerMoocodeLanguage () : unit =
    monaco?languages?register (createObj [ "id" ==> "moocode" ])
    monaco?languages?setMonarchTokensProvider ("moocode", moocodeLanguage)
    monaco?languages?setLanguageConfiguration ("moocode", moocodeLanguageConfiguration)
    monaco?editor?defineTheme ("moocode-dark", moocodeTheme)

/// Three fixed local boilerplate snippets - a verb's arg-spec scatter
/// skeleton, a `try`/`except (ANY)` guard, and this project's own leading
/// block-comment doc-comment convention (confirmed the real one via
/// `Handlers.fs`'s own doc comment on `inferredVerbSummary`, not guessed -
/// MOOcode has exactly one comment form, non-nesting). `$1`/`${1:text}` tab
/// stops need `insertTextRules = 4` (`InsertAsSnippet` - confirmed directly
/// in the installed package's own `editor.api.d.ts:6980-6991`, same
/// "read the real enum value" discipline `KeyCode.F2 = 60`/
/// `MarkerSeverity.Error = 8` already establish in this file) or Monaco
/// inserts the tab-stop syntax as literal text instead of parsing it.
///
/// Registered as a **second, purely client-side** completion provider for
/// "moocode", alongside (not replacing) `LspClient.fs`'s own LSP-backed
/// `registerCompletionItemProvider` call - these three are static local
/// text, nothing here needs a server round trip, so keeping this separate
/// avoids touching that provider's LSP-only contract at all.
let registerSnippetProvider () : unit =
    let snippets =
        [ "argspec", "Verb arg-spec scatter skeleton", "{${1:who}, ?${2:what} = ${3:0}, @${4:rest}} = args;"
          "try/except (ANY)", "try/except (ANY) guard", "try\n\t$1\nexcept ${2:e} (ANY)\n\t$3\nendtry"
          "/* doc comment */", "Leading doc-comment block", "/* ${1:Describe this verb.} */\n$0" ]

    let provideCompletionItems (model: obj) (position: obj) : obj =
        let wordInfo = model?getWordUntilPosition (position)

        let range =
            createObj
                [ "startLineNumber" ==> position?lineNumber
                  "startColumn" ==> wordInfo?startColumn
                  "endLineNumber" ==> position?lineNumber
                  "endColumn" ==> wordInfo?endColumn ]

        let suggestions =
            snippets
            |> List.map (fun (label, detail, insertText) ->
                createObj
                    [ "label" ==> label
                      "kind" ==> 27 // CompletionItemKind.Snippet
                      "detail" ==> detail
                      "insertText" ==> insertText
                      "insertTextRules" ==> 4 // CompletionItemInsertTextRule.InsertAsSnippet
                      "range" ==> range ])
            |> Array.ofList

        createObj [ "suggestions" ==> suggestions ]

    monaco?languages?registerCompletionItemProvider (
        "moocode",
        createObj [ "provideCompletionItems" ==> System.Func<obj, obj, obj>(fun m p -> provideCompletionItems m p) ]
    )
    |> ignore

type ITextModel =
    /// Confirmed in the installed package's own `editor.api.d.ts:2093`.
    abstract getLineMaxColumn: lineNumber: int -> int
    /// Confirmed in the installed package's own `editor.api.d.ts:2064`.
    abstract getLineCount: unit -> int
    /// Confirmed in the installed package's own `editor.api.d.ts:2068`. Used
    /// to read back each line's post-reindent leading whitespace - see
    /// `App.fs`'s `tabIndentDeltas` for why.
    abstract getLineContent: lineNumber: int -> string

/// Just enough of Monaco's `IEditorAction` to invoke a built-in action by
/// id - see `reindentLinesActionId` below for the one this app actually
/// uses.
type IEditorAction =
    abstract run: unit -> obj
    /// Same as `run` but passes `args` through - Monaco's public
    /// `IEditorAction.run(args?: unknown): Promise<void>` signature
    /// (`editor.api.d.ts`), confirmed to accept an argument even though
    /// every other caller in this file only ever needs the zero-arg form
    /// (kept as its own member rather than changing `run` itself, so those
    /// simpler call sites don't need to start passing anything). See
    /// `registerShowHoverKeybinding`'s own comment for why this specific
    /// action needs it.
    abstract runWithArgs: args: obj -> obj

/// The id of Monaco's built-in "Reindent Lines" action (confirmed directly
/// in the installed package's own source,
/// `esm/vs/editor/contrib/indentation/browser/indentation.js`, not assumed)
/// - reindents the whole document using the same `indentationRules`
/// `moocodeLanguageConfiguration` already registers. `indentationRules`
/// alone only affects *newly typed* lines (pressing Enter, pasting) - it
/// has no effect on content that arrives via `setValue`, which is how every
/// verb actually loads here. Running this action once right after
/// `setValue` is what makes already-written, flatly-indented corpus code
/// (the overwhelming majority of it) actually show up indented.
let reindentLinesActionId = "editor.action.reindentlines"

type IStandaloneCodeEditor =
    abstract getValue: unit -> string
    abstract setValue: value: string -> unit
    abstract updateOptions: options: obj -> unit
    /// Fires on every content change, whether from typing or a programmatic
    /// `setValue` call - callers that only care about *user* edits (e.g. a
    /// "dirty" flag for autosave) must account for the latter themselves.
    abstract onDidChangeModelContent: listener: (obj -> unit) -> obj
    /// Fires when focus leaves the editor *and* all of its own widgets
    /// (find/replace, suggestions, hover) - unlike `onDidBlurEditorText`,
    /// which also fires when focus merely moves to one of those widgets
    /// while conceptually still "in" the editor.
    abstract onDidBlurEditorWidget: listener: (unit -> unit) -> obj
    abstract focus: unit -> unit
    /// Called with no arguments - Monaco measures its own container. Needed
    /// because a Monaco instance living in a container that was
    /// `display:none` doesn't always pick up its new size via
    /// ResizeObserver alone once shown again; an explicit `layout()` call
    /// right after activating the Editor tab avoids a stale/blank render.
    abstract layout: unit -> unit
    /// Fires whenever the primary cursor moves - the listener's event
    /// object carries a `position: { lineNumber, column }` (both 1-based),
    /// same shape hover/definition already read via `LspClient.fs`'s
    /// `position?lineNumber`/`position?column` convention.
    abstract onDidChangeCursorPosition: listener: (obj -> unit) -> obj
    abstract getAction: id: string -> IEditorAction
    /// Binds `handler` to `keybinding` - unlike `addAction` (which merely
    /// *contributes* an action that only gets a live keybinding if nothing
    /// else already claims it), this *overrides* whatever was bound to that
    /// exact key combo before, editor-instance-scoped (confirmed directly
    /// against the installed package's own `editor.api.d.ts`, same as
    /// `MarkerSeverity.Error = 8` above). Needed for F2 specifically: Monaco
    /// ships a built-in "Rename Symbol" action already bound to F2 that
    /// silently no-ops for a language with no registered rename provider
    /// (`moocode` has none - this project's rename is a custom action, not
    /// `textDocument/rename`) - `addAction` alone left that built-in
    /// shadowing every keypress with no visible effect and no error,
    /// confirmed live before switching to `addCommand` here.
    abstract addCommand: keybinding: int * handler: System.Action -> unit
    /// The cursor's current position (`{lineNumber, column}`, both 1-based)
    /// - `null` if the editor has no model yet. Same shape
    /// `onDidChangeCursorPosition`'s event object already carries.
    abstract getPosition: unit -> obj
    /// Moves the cursor (no scrolling) - `lineNumber`/`column` both 1-based,
    /// same convention as everywhere else in this file. Used by go-to-
    /// definition's same-document case (a local variable's definition,
    /// where the target verb is already open, so nothing needs re-fetching
    /// - just the cursor moving).
    abstract setPosition: position: obj -> unit
    /// Scrolls so `position` ends up vertically centered, without stealing
    /// focus or changing the selection - pairs with `setPosition` so a
    /// same-document go-to-definition jump is actually visible, not just
    /// technically-moved off-screen.
    abstract revealPositionInCenter: position: obj -> unit
    abstract getModel: unit -> ITextModel
    /// Captures scroll position, cursor/selection, and folded regions as one
    /// opaque snapshot - meaningless outside a `restoreViewState` call on
    /// this same editor. Since this app reuses one editor instance (and
    /// model) across every verb tab via `setValue` rather than a model per
    /// tab, a caller must save/restore per tab itself (keyed by which tab
    /// the state came from) - Monaco has no notion of "tab" to do that for.
    abstract saveViewState: unit -> obj
    /// Reapplies a snapshot from `saveViewState` - only ever called with a
    /// state actually produced for *this* tab (see `tabViewStates`'s own
    /// comment), never with a `None`/missing lookup passed through.
    abstract restoreViewState: state: obj -> unit

/// Creates a standalone editor in the given DOM element, defaulting to the
/// "moocode" language and a dark theme matching the terminal's own styling.
/// Minimap enabled (not just for the code thumbnail itself) so the built-in
/// "cursor is here" mark in the overview ruler has visual separation from
/// the scrollbar - with the minimap off, that mark renders flush against
/// the scrollbar with no gap, and Monaco's public API has no option to
/// reposition it directly (it's drawn on an internal canvas, not a
/// positionable element - `hideCursorInOverviewRuler` only offers show/hide,
/// confirmed by reading the installed package's own type definitions).
let create (container: Browser.Types.HTMLElement) : IStandaloneCodeEditor =
    let options =
        createObj
            [ "value" ==> ""
              "language" ==> "moocode"
              "theme" ==> "moocode-dark"
              "automaticLayout" ==> true
              "minimap" ==> createObj [ "enabled" ==> true ]
              // Confirmed live: without this, Monaco never calls a
              // registered `DocumentSemanticTokensProvider` at all, even
              // though registration itself succeeds silently - this option
              // defaults to `'configuredByTheme'` (`editor.api.d.ts`'s own
              // doc comment), and `moocode-dark` (a `vs-dark` variant, below)
              // doesn't opt in on its own either.
              "semanticHighlighting.enabled" ==> true ]

    monaco?editor?create (container, options)

/// F2 - the universal "rename symbol" convention, not a context-menu item
/// (`KeyCode.F2 = 60`, confirmed directly in the installed package's own
/// `editor.api.d.ts`, same as `MarkerSeverity.Error = 8` above). `onRename`
/// gets no arguments - callers already have `editor` itself in scope
/// (this is called right after `create`) and read the cursor position via
/// `editor.getPosition()` themselves, rather than this function threading
/// it through.
let registerRenameAction (editor: IStandaloneCodeEditor) (onRename: unit -> unit) : unit =
    editor.addCommand (60, System.Action(onRename))

/// The id of Monaco's built-in "Show or Focus Hover" action - the same
/// documentation/type-info tooltip that already appears on mouse hover,
/// triggerable from the keyboard instead (confirmed directly in the
/// installed package's own source,
/// `esm/vs/editor/contrib/hover/browser/hoverActionIds.js`).
let showHoverActionId = "editor.action.showHover"

/// Ctrl+Shift+Space - runs `showHoverActionId` for whatever's at the current
/// cursor position. `KeyMod.CtrlCmd = 2048`, `KeyMod.Shift = 1024`,
/// `KeyCode.Space = 10` (all confirmed directly in the installed package's
/// own source, `esm/vs/editor/common/services/editorBaseApi.js` and
/// `esm/vs/editor/editor.api.d.ts` respectively - same "read the real enum
/// value" discipline `KeyCode.F2 = 60` above already establishes). Doesn't
/// override anything: Monaco's own default keybinding for this action is
/// the chord Ctrl+K Ctrl+I (confirmed in
/// `esm/vs/editor/contrib/hover/browser/hoverActions.js`), not a single
/// combo, so Ctrl+Shift+Space was unclaimed.
///
/// Passes `{ focus: "noAutoFocus" }` rather than calling `run()` bare -
/// confirmed directly in the installed package's own `hoverActions.js` that
/// this action is genuinely "show OR focus": `run()`'s own implementation
/// branches on `controller.isHoverVisible` and, when true, just calls
/// `controller.focus()` on whatever's already showing rather than
/// re-querying for the current cursor position at all. Not confirmed to be
/// the specific cause of any one report (a separate, precisely-diagnosed bug
/// in the reindent-vs-AST-position mismatch - see the card this shipped
/// under - fully explains what was actually seen), but it's a real risk on
/// its own terms: pressing this shortcut while a hover is already lingering
/// from the mouse would silently show stale, unrelated content instead of
/// the current position's. `HoverFocusBehavior.NoAutoFocus` (the string
/// `"noAutoFocus"`, confirmed in the same source) is the one `focus` value
/// whose branch always re-queries via `editor.getPosition()` regardless of
/// `isHoverVisible`, closing that gap outright.
let registerShowHoverKeybinding (editor: IStandaloneCodeEditor) : unit =
    editor.addCommand (
        2048 ||| 1024 ||| 10,
        System.Action(fun () ->
            editor.getAction(showHoverActionId).runWithArgs (createObj [ "focus" ==> "noAutoFocus" ]) |> ignore)
    )

/// Replaces every compile-diagnostic marker on `editor`'s current model
/// with `lineErrors` (line number, message, isWarning) - an empty list
/// clears them. Owner string is just a namespace so this diagnostic source
/// can't collide with another one setting markers on the same model;
/// there's only one source today. `isWarning` maps to
/// `MarkerSeverity.Warning = 4` vs `MarkerSeverity.Error = 8` (both
/// confirmed in editor.api.d.ts:90-95) - a non-blocking compiler warning
/// (e.g. `if (x = 1)`, an intentional-sometimes assignment-as-condition)
/// must not render the same as a real compile error. Clamps the reported
/// line to the model's actual last line - confirmed live (via the
/// debounced live-diagnostics check, which reports on mid-edit,
/// not-yet-saved text far more often than the old save-time-only check
/// ever did) that MOO's own compile diagnostics can name a line number
/// past the end of a short/incomplete buffer (e.g. an unterminated
/// statement on the buffer's only line reported as "Line 2"), and
/// `getLineMaxColumn` throws outright for an out-of-range line - this
/// would otherwise silently abort the whole marker update, not just that
/// one marker.
let setErrorMarkers (editor: IStandaloneCodeEditor) (lineErrors: (int * string * bool) list) : unit =
    let model = editor.getModel ()
    let lineCount = model.getLineCount ()

    let markers =
        lineErrors
        |> List.map (fun (line, message, isWarning) ->
            let clampedLine = max 1 (min line lineCount)

            createObj
                [ "severity" ==> (if isWarning then 4 else 8)
                  "message" ==> message
                  "startLineNumber" ==> clampedLine
                  "startColumn" ==> 1
                  "endLineNumber" ==> clampedLine
                  "endColumn" ==> model.getLineMaxColumn clampedLine ])
        |> List.toArray

    monaco?editor?setModelMarkers (model, "moocode-compile", markers)

/// Just enough of Monaco's diff editor for Phase 5's per-verb history view:
/// a historical version (read-only "original" side) against the live editor's
/// current content ("modified" side). One instance is created lazily and
/// reused across every commit the user picks (`setDiffModel` swaps the
/// models in place) rather than recreated per selection - a diff editor owns
/// real DOM/canvas state, so churning instances would leak them.
type IDiffEditor =
    abstract setModel: models: obj -> unit
    abstract layout: unit -> unit
    abstract dispose: unit -> unit

/// Creates a read-only, side-by-side diff editor in `container` - "read-only"
/// because this view is for comparison, not editing; restoring an old version
/// is a separate explicit action (`App.fs` calls the *regular* editor's
/// `setValue` for that, not anything here).
let createDiffEditor (container: Browser.Types.HTMLElement) : IDiffEditor =
    let options =
        createObj
            [ "theme" ==> "moocode-dark"
              "automaticLayout" ==> true
              "readOnly" ==> true
              "renderSideBySide" ==> true ]

    monaco?editor?createDiffEditor (container, options)

let private createModel (content: string) : ITextModel =
    monaco?editor?createModel (content, "moocode")

/// Swaps a diff editor's two sides to `originalText` (the historical
/// version) vs `modifiedText` (today's, usually the live editor's current
/// content) - each call creates fresh models, matching Monaco's own
/// "a model is disposable, an editor's models can be replaced wholesale"
/// convention rather than trying to mutate an existing model's text.
let setDiffModel (diffEditor: IDiffEditor) (originalText: string) (modifiedText: string) : unit =
    let original = createModel originalText
    let modified = createModel modifiedText
    diffEditor.setModel (createObj [ "original" ==> original; "modified" ==> modified ])

/// Wires the Phase 4.4 LSP server's hover/definition/completion/signature
/// help/find-references into this Monaco instance for "moocode" - see
/// `LspClient.wire` for what the callbacks are for. Call once, after
/// `create`. Returns `wire`'s own `refreshSemanticTokens` trigger unchanged
/// - see its doc comment.
let wireLsp
    (getCurrentDocument: unit -> (int64 * string) option)
    (jumpTo: int64 -> string -> int -> int -> unit)
    (showCaveat: string -> unit)
    (getIndentDelta: int64 -> string -> int[] option)
    (getLineMap: int64 -> string -> Sugar.LineMap option)
    (getFetchedLineCount: int64 -> string -> int option)
    (setHighlightingStale: bool -> unit)
    : (unit -> unit) =
    wire monaco getCurrentDocument jumpTo showCaveat getIndentDelta getLineMap getFetchedLineCount setHighlightingStale
