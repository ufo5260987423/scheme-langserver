# Fluent Scheme in VS Code with Magic Scheme + scheme-langserver

> [中文版](./fluent-scheme.md)

This guide shows how to bring Ansys Fluent's embedded Scheme development
into VS Code: syntax highlighting, auto-completion, goto definition,
hover documentation, and (early-stage) type inference. All you need is
VS Code + the Magic Scheme extension + scheme-langserver — **neither
Fluent nor a Chez Scheme installation is required**, because the
language service performs pure static analysis.

> scheme-langserver **≥ 2.1.10** is required: the `fluent` top
> environment is built in since that version.

---

## 1. Background: what is Fluent Scheme?

Ansys Fluent embeds a Scheme interpreter (based on Petite Chez Scheme),
widely used for:

- **Solver parameter read/write**: `rp-var-define`, `%rpgetvar`,
  `rpsetvar`, `RP_Get_Real`, etc.
- **Automation scripts / journals**: batch setup, parametric runs.
- **Cortex UI development**: `cx-create-panel`, `cx-create-button`,
  `cx-create-real-entry`, and other panel APIs.

Traditionally none of these APIs have any IDE support — a misspelled
function name only surfaces at runtime. The scheme-langserver `fluent`
top environment models the common APIs as built-in identifiers, together
with the Petite Chez standard library, providing completion and goto
definition.

## 2. Prerequisites

| Component | Notes |
|-----------|-------|
| VS Code | any recent version |
| Magic Scheme extension | search "Magic Scheme" in the marketplace (publisher ufo5260987423) |
| scheme-langserver ≥ 2.1.10 | **Linux x64**: downloaded automatically by Magic Scheme on first activation, no manual install; other platforms: see the [README](../README.md) for manual installation |

## 3. Configuration

### 3.1 Open the workspace

Open the folder containing your Fluent Scheme scripts (`.scm`) in VS
Code. The language service treats the opened folder as the workspace
root; **cross-file navigation only covers files inside the root**.

### 3.2 Set the top environment to Fluent

Pick either method:

**Method 1: configuration wizard (recommended)**

1. `Ctrl+Shift+P` (macOS `Cmd+Shift+P`) → run **`Configure Magic Scheme Project`**;
2. Click the `topEnvironment` row → **Custom value...** → enter `Fluent`;
3. Click **✓ Done**. The wizard writes `.vscode/magic-scheme.json` and restarts the language service automatically.

**Method 2: write `.vscode/magic-scheme.json` by hand**

```json
{
  "topEnvironment": "Fluent",
  "multiThread": "enable",
  "typeInference": "enable",
  "logPath": ".vscode/scheme-langserver.log",
  "cachePath": ".vscode/scheme-langserver-cache"
}
```

Magic Scheme restarts the language service automatically on save. The
`topEnvironment` value is passed to the server's `--top-environment`
flag (case-insensitive; `Fluent`/`fluent` both work).

### 3.3 File associations

Make sure VS Code recognizes `.scm` as Scheme. In the workspace
`settings.json`:

```json
{
  "files.associations": {
    "*.scm": "scheme",
    "*.ss": "scheme"
  }
}
```

No extra file-filter configuration is needed: plain folders (without an
`.akku` project structure) accept all standard Scheme extensions
(`.scm` `.ss` `.sls` `.sps` `.sld`) by default.

> **Do not associate `.jou` journal files with Scheme**. A journal is
> Fluent's command-record format, not Scheme source; feeding it to the
> language service only produces noise diagnostics.

## 4. What the Fluent environment provides

The `fluent` top environment = **38 Fluent-specific APIs** + the **full
Chez Scheme identifier set**.

The proprietary APIs (selection; the complete list is `fluent-raw` in
`analysis/identifier/meta.sls`):

| Category | Identifiers |
|----------|-------------|
| Solver variables | `rp-var-define`, `%rpgetvar`, `rpsetvar`, `make-new-rpvar`, `RP_Get_Real`, `RP_Get_String`, `RP_Set_Real`, `RP_Variable_Exists_P` |
| Cortex panels | `cx-create-panel`, `cx-create-button`, `cx-create-button-box`, `cx-create-drop-down-list`, `cx-create-integer-entry`, `cx-create-real-entry`, `cx-create-text-entry`, `cx-create-toggle-button`, `cx-create-table`, `cx-create-list`, `cx-create-taskpage`, `cx-show-panel`, `cx-show-taskpage`, etc. |
| Panel widget read/write | `cx-set-integer-entry`, `cx-set-real-entry`, `cx-set-text-entry`, `cx-set-toggle-button`, `cx-set-list-items`, `cx-set-list-selections`, `cx-show-integer-entry`, `cx-show-list-selections`, etc. |
| Menus | `cx-add-hitem`, etc. |

Effect: these built-in functions get completion, hover, and parameter
awareness; functions you `define` yourself keep full goto-definition
and find-references support.

## 5. Minimal working example

```scheme
; define a solver parameter
(rp-var-define 'my-swirl 0.5 'real #f)

; build a simple panel
(define (make-panel)
  (let ([panel (cx-create-panel "My Panel" "demo" #f #f)])
    (cx-create-real-entry panel "Swirl" 'my-swirl)
    (cx-create-button panel "Apply" 'apply-settings)
    (cx-show-panel panel)))

(define (apply-settings . args)
  (rpsetvar 'my-swirl (RP_Get_Real 'my-swirl)))
```

Open the folder containing this file and apply section 3:
`rp-var-define`, `cx-create-panel`, etc. complete out of the box; goto
definition on `apply-settings` works; local bindings like `my-swirl`
get local completion.

## 6. Known limitations & advice

- **API coverage is a common subset (38 identifiers)**. For example the
  TUI macro `ti-menu-load-string` is not included — it won't error,
  it just gets no completion/hover. Two workarounds:
  1. Add stub declarations at the top of your script so the analyzer
     knows them, e.g.
     `(define ti-menu-load-string (lambda (cmd) #f))` (Fluent provides
     the real implementation at runtime);
  2. Send a PR to scheme-langserver extending the `fluent-raw` table
     in `analysis/identifier/meta.sls` (format: `(identifier procedure)`).
- **Script files vs. library files**: Fluent scripts are not
  `(library ...)` forms, so they attach directly under the workspace
  root as script files — this is expected and does not affect analysis.
- **Cold start on large workspaces**: Fluent script projects are
  usually small (second-level initialization), so the default
  `cachePath` is fine. If you point the language service at a large
  mixed workspace, cold start may exceed the client timeout; consider
  narrowing the opened folder.
- **`typeInference`**: for Fluent scripts you may set it to `disable`
  (saves initialization time, at the cost of hover type information).
- **Multi-root workspaces**: each root folder uses its own
  `.vscode/magic-scheme.json`.

## 7. Troubleshooting

| Symptom | Check |
|---------|-------|
| Status bar stuck on initializing | First cold start on a large workspace can take minutes; watch the `logPath` log to confirm progress |
| No Fluent API completion | Check that `topEnvironment` in `.vscode/magic-scheme.json` is `Fluent`; the status bar should show `scheme-langserver 2.1.10+` |
| Some functions not completed | See "Known limitations" in section 6 — the API may not be among the 38 built-ins |
| Server does not start | Check the log file at `logPath` (default `.vscode/scheme-langserver.log`); confirm scheme-langserver ≥ 2.1.10 (`scheme-langserver --version`) |
| Many diagnostics | Fluent scripts rely heavily on runtime-injected global bindings, so noise is normal; focus on genuine bracket/syntax error diagnostics |
