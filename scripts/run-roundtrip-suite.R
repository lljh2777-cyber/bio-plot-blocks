#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1]]), winslash = "/")
root <- dirname(dirname(script_path))
options(BioPlotBlocks.root = root)

source_order <- c(
  "ir-nodes.R", "module-registry.R", "module-instance.R", "data-sources.R", "pca.R", "visual-config.R", "codegen.R",
  "project-store.R", "parser.R", "diagnostics.R", "runtime.R", "templates.R"
)
for (file in source_order) source(file.path(root, "R", file))

registry <- bp_load_registry()
project <- bp_project_from_template("bio.volcano.basic", registry)
code <- bp_generate_code(project, registry)
restored <- bp_parse_code(code, registry)
regenerated <- bp_generate_code(restored, registry)

stopifnot(identical(code, regenerated))
stopifnot(length(project$modules) == length(restored$modules))
stopifnot(identical(vapply(project$modules, `[[`, character(1), "module_id"), vapply(restored$modules, `[[`, character(1), "module_id")))

runtime <- bp_execute_project(project, registry)
stopifnot(isTRUE(runtime$ok), inherits(runtime$plot, "ggplot"))

scope <- bp_scope_scan(bp_generate_code(project, registry, include_setup = TRUE))
stopifnot(isTRUE(scope$ok), setequal(scope$packages, "ggplot2"))

cat("Round-trip, execution, and package-scope suites passed.\n")
