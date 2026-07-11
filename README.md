# BioPlotBlocks

BioPlotBlocks is an R-only visual code-composition editor focused on ggplot2. The current repository implements a runnable Shiny MVP that keeps visual modules, typed R semantics, generated code, project persistence, and real ggplot2 execution connected through a shared semantic model.

## Run locally

From the repository root:

```powershell
& "C:\Program Files\R\R-4.5.1\bin\Rscript.exe" -e "shiny::runApp('app', launch.browser = TRUE)"
```

The application starts with a ggplot2-only volcano-plot template and a deterministic example data frame named `df`.

## What the MVP includes

- declarative ModuleSpecs for more than ten ggplot2 functions;
- a linear ggplot2 `+` composition editor with add, duplicate, delete, reorder, collapse, undo, and redo;
- typed argument states, including unset, explicit default, `NULL`, `NA`, and Raw R Expression;
- deterministic R code generation from R language objects;
- parsing of the supported generated-code subset with lossless Raw R fallback;
- real local ggplot2 execution and plot preview;
- JSON project save/restore and `.R` export;
- a ggplot2-only bioinformatics template;
- schema, unit, round-trip, scope, and execution tests.

## Scope

The MVP adapts R and ggplot2 only. It does not execute bioinformatics analyses, silently transform data, optimize plotting code, or introduce calls to other add-on R packages.

See [the implementation analysis](docs/requirements-analysis.md) for the requirements-to-deliverables mapping and known boundaries.
