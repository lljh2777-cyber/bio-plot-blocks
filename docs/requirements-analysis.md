# BioPlotBlocks v0.2 implementation analysis

## Product conclusion

The specification describes neither a general-purpose R IDE nor a chart wizard. Its differentiator is a faithful, inspectable representation of R/ggplot2 semantics. The MVP therefore has to prove a vertical chain rather than maximize the number of charts:

```text
ModuleSpec -> module state -> R Semantic IR -> R code -> real ggplot2 execution
                                      ^             |
                                      |-- parsing --|
```

The implementation uses a single R package plus a Shiny application because the locked local environment already provides R 4.5.1, ggplot2 4.0.1, and Shiny 1.13.0, and because this matches the recommended stack in section 13.4 of the specification.

## Frozen decisions

| Decision | MVP implementation |
|---|---|
| User-code runtime | R only |
| First-class package scope | ggplot2 only |
| App architecture | Monorepo, one R package, one Shiny app |
| Composition model | Optional assignment plus one linear ggplot2 `+` chain |
| Semantic source of truth | Typed project/module state converted to R language objects |
| Unsupported inner values | Preserved as Raw R Expression |
| Execution | Local R process used by Shiny; no public-server isolation claim |
| Data | Deterministic, redistributable example data; no hidden analysis |
| Project format | Versioned JSON |
| Visual structure | Module library / layer stack / inspector over preview / generated code |

## Requirement coverage

| Requirement family | Implemented deliverable |
|---|---|
| Workspace | Module library, layer stack, parameter inspector, preview, code view, diagnostics |
| Module library | Search, category filter, package/status labels, click-to-add, template distinction |
| Composition | Add, delete, duplicate, reorder, collapse, select, undo/redo, code selection sync |
| Parameter model | Native argument names, provenance, Common/Advanced/All, state selector, Raw Expression mode |
| Code | Live deterministic R generation, syntax highlighting, copy, `.R` download, supported-subset import |
| Preview | Actual ggplot2 execution, warnings/errors/messages, manual refresh, version display |
| Persistence | Versioned JSON save and restore, typed values, module order, versions, template provenance |
| Templates | Visible ggplot2-only volcano composition using prepared columns |
| Registry | Declarative JSON ModuleSpecs loaded and validated at runtime |
| Testing | Schema, codegen, parser, round-trip, execution, persistence, and package-scope checks |

## MVP boundaries

- Free-form R editing is import-and-reparse, not a complete IDE-like live editor.
- Parsing covers the generated subset and common hand-written variants; unsafe or unknown structures become Raw R modules or values.
- Source comments and exact whitespace are not guaranteed to round-trip.
- The local Shiny process executes with the current user's permissions. This is not a safe public-hosting configuration.
- Module metadata is locked to R 4.5.1 and ggplot2 4.0.1 for this repository; wider compatibility requires matrix testing.
- The UI exposes a meaningful subset of native arguments for each beta module. Unmodeled named arguments are preserved by the parser as Raw values and remain auditable.

## Phase alignment

This repository covers the M1-M3 vertical slice and a practical subset of M5: schemas, semantic values, declarative modules, generation, parsing, round-trip behavior, a real editor, persistence, and execution. It also includes the initial ggplot2-only volcano template and a module-authoring Skill prototype. Full human review evidence for every ggplot2 argument and multi-version CI remain release-gate work, not claims of this MVP.
