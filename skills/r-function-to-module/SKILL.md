---
name: r-function-to-module
description: Draft and validate auditable BioPlotBlocks ModuleSpecs for exported ggplot2 functions in the locked R environment. Use when adding, reviewing, or updating a ggplot2 visual module; capturing same-version signature and documentation evidence; proposing General/Advanced controls; generating mapping records and test drafts; or checking a module against the R-only, ggplot2-only scope.
---

# Map an R function to a BioPlotBlocks module

Produce an evidence bundle, not a production-ready claim. Keep R semantics, formal defaults, `...` uncertainty, and Raw Expression fallbacks visible. Never mark a generated draft stable without human review and real execution/round-trip evidence.

## Workflow

1. Read `references/mapping-contract.md` completely.
2. Confirm the repository lock in `config/compatibility-matrix.json`. Stop if the active R or ggplot2 version does not match it.
3. Confirm the target package is `ggplot2`, the function is exported, and the function is suitable as one real R-call module. Reject other add-on packages during the initial release.
4. Capture runtime evidence:

   ```powershell
   & "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" `
     skills/r-function-to-module/scripts/inspect_function.R `
     --function geom_area `
     --out-dir <review-directory>
   ```

5. Inspect the generated evidence JSON, ModuleSpec draft, mapping record, and test draft. Treat all `...`-derived controls as uncertain until same-version help/source/runtime evidence confirms their origin.
6. Refine General/Advanced grouping. Keep roughly 4-10 high-frequency native arguments in General. Do not change function semantics, invent pseudo-arguments, or flatten complex defaults.
7. Ensure every complex or uncertain value has Raw Expression fallback. Preserve `NULL`, typed `NA`, missing arguments, symbols, strings, formulas, calls, unnamed arguments, and argument order distinctly.
8. Add or update the ModuleSpec catalog, mapping record, and tests. Run repository validation:

   ```powershell
   & "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" scripts/validate-modules.R
   & "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" scripts/run-roundtrip-suite.R
   ```

9. Report uncertainties and confidence per argument. Require a human reviewer to confirm documentation provenance, `...` forwarding, grouping, control choices, version range, and execution evidence.

## Required output

- environment and export evidence;
- complete observed signature and formal-default expressions;
- argument-origin table, including explicit `...` unknowns;
- ModuleSpec draft with confidence and Raw Expression policy;
- General/Advanced and control recommendations with rationale;
- mapping record;
- schema, codegen, parser, round-trip, and execution test drafts;
- uncertainties list and human-review checklist.

## Hard boundaries

- Do not adapt Python, JavaScript, or another R package.
- Do not infer undocumented `...` arguments as verified.
- Do not replace legal R expressions with strings or simplified values.
- Do not auto-correct, optimize, refactor, or migrate code.
- Do not hide extra calls inside a control or template.
- Do not mark drafts beta/stable; generated status remains `draft` until review evidence is complete.
