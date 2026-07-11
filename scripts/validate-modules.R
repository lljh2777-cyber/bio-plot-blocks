#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1]]), winslash = "/")
root <- dirname(dirname(script_path))
options(BioPlotBlocks.root = root)

source_order <- c("ir-nodes.R", "module-registry.R", "module-instance.R")
for (file in source_order) source(file.path(root, "R", file))

registry <- bp_load_registry()
stopifnot(length(registry) >= 10L)

for (spec in registry) {
  bp_validate_module_spec(spec)
  stopifnot(identical(spec$runtime, "R"))
  stopifnot(spec$package %in% c("ggplot2", "BioPlotBlocks.core"))
  if (identical(spec$package, "ggplot2")) {
    stopifnot(spec$symbol %in% getNamespaceExports("ggplot2") ||
      any(unlist(spec$code_parsing$accepted_symbols) %in% getNamespaceExports("ggplot2")))
  }
  if (length(spec$parameters)) {
    for (parameter in spec$parameters) {
      stopifnot(isTRUE(parameter$raw_expression_allowed))
      stopifnot(parameter$ui_group %in% c("common", "advanced"))
    }
  }
  required_tests <- c("schema", "codegen", "parser", "roundtrip", "execution")
  stopifnot(all(required_tests %in% unlist(spec$tests$required)))
}

cat("Validated", length(registry), "R-only core/ggplot2 ModuleSpecs.\n")
