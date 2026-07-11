#!/usr/bin/env Rscript

parse_cli <- function(args) {
  result <- list()
  index <- 1L
  while (index <= length(args)) {
    key <- args[[index]]
    if (!startsWith(key, "--") || index == length(args)) stop("Expected --key value arguments.")
    result[[sub("^--", "", key)]] <- args[[index + 1L]]
    index <- index + 2L
  }
  result
}

deparse_one <- function(value) paste(deparse(value, width.cutoff = 500L), collapse = " ")

typed_default <- function(value, source) {
  if (!nzchar(source)) return(list(type = "RMissingArgument"))
  if (is.null(value)) return(list(type = "RNull"))
  if (is.character(value) && length(value) == 1L) return(list(type = "RCharacter", value = value))
  if (is.logical(value) && length(value) == 1L && !is.na(value)) return(list(type = "RLogical", value = value))
  if (is.integer(value) && length(value) == 1L && !is.na(value)) return(list(type = "RInteger", value = value))
  if (is.double(value) && length(value) == 1L && !is.na(value)) return(list(type = "RDouble", value = value))
  if (length(value) == 1L && is.atomic(value) && is.na(value)) return(list(type = "RNA", na_type = typeof(value)))
  list(type = "RRawExpression", source = source)
}

control_for <- function(name, default) {
  if (name == "mapping") return("aes_editor")
  if (name == "data") return("symbol_or_expression")
  if (grepl("formula|facets", name)) return("formula_editor")
  if (identical(default$type, "RLogical") || identical(default$type, "RNA")) return("logical_state")
  if (default$type %in% c("RDouble", "RInteger")) return("numeric_or_expression")
  if (identical(default$type, "RCharacter")) return("text")
  "expression"
}

types_for <- function(control) {
  switch(
    control,
    aes_editor = c("aes_mapping", "r_null", "r_expression"),
    symbol_or_expression = c("data_reference", "r_null", "r_expression"),
    formula_editor = c("r_formula", "r_expression"),
    logical_state = c("r_logical", "r_na", "r_expression"),
    numeric_or_expression = c("r_number", "r_expression"),
    text = c("r_character", "r_null", "r_expression"),
    c("r_expression")
  )
}

args <- parse_cli(commandArgs(trailingOnly = TRUE))
function_name <- args[["function"]]
out_dir <- args$`out-dir`
if (is.null(function_name) || is.null(out_dir)) stop("Usage: inspect_function.R --function <name> --out-dir <path>")
if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 is not installed.")
if (!function_name %in% getNamespaceExports("ggplot2")) stop("Target must be an exported ggplot2 function: ", function_name)

fun <- getExportedValue("ggplot2", function_name)
if (!is.function(fun)) stop("Export is not an R function: ", function_name)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

formal_list <- as.list(formals(fun))
formal_names <- names(formal_list)
default_sources <- vapply(formal_list, deparse_one, character(1))
signature <- paste0(
  function_name,
  "(",
  paste(ifelse(nzchar(default_sources), paste0(formal_names, " = ", default_sources), formal_names), collapse = ", "),
  ")"
)

common_priority <- c("mapping", "data", "x", "y", "colour", "color", "fill", "size", "alpha", "shape", "linewidth", "linetype", "position", "stat", "width", "height", "bins", "binwidth")
parameters <- list()
argument_rows <- list()
for (index in seq_along(formal_list)) {
  name <- formal_names[[index]]
  if (identical(name, "...")) {
    argument_rows[[length(argument_rows) + 1L]] <- list(name = "...", origin = "dots_unknown", default = "", group = "advanced", control = "named_raw_editor", confidence = "unknown")
    next
  }
  source <- default_sources[[index]]
  default <- typed_default(formal_list[[index]], source)
  control <- control_for(name, default)
  group <- if (name %in% head(common_priority[common_priority %in% formal_names], 8L)) "common" else "advanced"
  parameter <- list(
    name = name,
    source = "formal",
    formal_default = default,
    value_types = as.list(types_for(control)),
    ui_group = group,
    ui_control = control,
    raw_expression_allowed = TRUE,
    omit_when_unset = TRUE,
    help_source = paste0("ggplot2::", function_name, " installed help"),
    version_notes = "Generated from the locked runtime signature; requires human review."
  )
  parameters[[length(parameters) + 1L]] <- parameter
  argument_rows[[length(argument_rows) + 1L]] <- list(name = name, origin = "formal", default = source, group = group, control = control, confidence = "verified_runtime")
}

spec <- list(
  schema_version = "0.2.0",
  module_version = "0.1.0",
  id = paste0("r.ggplot2.", function_name),
  runtime = "R",
  r_version = paste0(">=", R.version$major, ".", strsplit(R.version$minor, "\\.")[[1]][1], ".0 <4.6.0"),
  package = "ggplot2",
  package_version = as.character(utils::packageVersion("ggplot2")),
  symbol = function_name,
  exported = TRUE,
  module_type = "r_function_call",
  status = "draft",
  presentation = list(title = gsub("_", " ", function_name), category = if (startsWith(function_name, "geom_")) "geom" else "structure", icon = "code", summary = paste("Draft module for", function_name)),
  composition = list(output_type = "ggplot_component", accepted_contexts = list("ggplot_plus_chain"), operator = "+", multiplicity = "many"),
  parameters = parameters,
  code_generation = list(call_style = "preserve_or_named", namespace_policy = "project_setting", preserve_argument_order = TRUE, omit_unset_parameters = TRUE, preserve_explicit_defaults = TRUE, support_raw_expressions = TRUE),
  code_parsing = list(accepted_symbols = list(function_name, paste0("ggplot2::", function_name)), unknown_arguments = "preserve_as_named_raw", unknown_values = "preserve_as_raw_expression"),
  documentation = list(reference_topic = function_name, source_type = "installed_package_help"),
  compatibility = list(runtime = "R", required_context = "ggplot_plus_chain", output_type = "ggplot_component"),
  provenance = list(generated_by = "r-function-to-module", source_environment_lock = "config/compatibility-matrix.json", mapping_rule_version = "0.2.0", reviewed = FALSE, confidence = "inferred"),
  tests = list(required = list("schema", "codegen", "parser", "roundtrip", "execution"))
)

evidence <- list(
  captured_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  r_version = R.version.string,
  package = "ggplot2",
  package_version = as.character(utils::packageVersion("ggplot2")),
  "function" = function_name,
  exported = TRUE,
  signature = signature,
  formal_arguments = argument_rows,
  uncertainties = list(
    "Arguments accepted, documented, or forwarded through ... require same-version help/source/runtime review.",
    "Control recommendations are heuristic until human review.",
    "Composition semantics and returned object require an execution probe."
  )
)

jsonlite::write_json(evidence, file.path(out_dir, paste0(function_name, ".evidence.json")), pretty = TRUE, auto_unbox = TRUE, null = "null")
jsonlite::write_json(spec, file.path(out_dir, paste0(function_name, ".module.json")), pretty = TRUE, auto_unbox = TRUE, null = "null")

table_lines <- vapply(argument_rows, function(row) paste0("| `", row$name, "` | ", row$origin, " | `", row$default, "` | ", row$group, " | ", row$control, " | ", row$confidence, " |"), character(1))
record <- c(
  paste0("# Mapping Record: ggplot2::", function_name),
  "",
  "## Environment",
  paste0("- R: ", R.version.string),
  paste0("- ggplot2: ", utils::packageVersion("ggplot2")),
  "- Status: draft; human review required",
  "",
  "## Observed signature",
  "```r", signature, "```",
  "",
  "## Argument analysis",
  "| Argument | Origin | Default expression | UI group | Control draft | Confidence |",
  "|---|---|---|---|---|---|",
  table_lines,
  "",
  "## `...` analysis",
  "- Inspect installed help, forwarding targets, aesthetics, and runtime behavior. Keep unverified names `dots_unknown`.",
  "",
  "## Uncertainties",
  paste0("- ", unlist(evidence$uncertainties)),
  "",
  "## Human-review checklist",
  "- [ ] Same-version help and source reviewed",
  "- [ ] `...` destination and aesthetics recorded",
  "- [ ] General/Advanced grouping justified",
  "- [ ] Every legal complex value retains Raw Expression fallback",
  "- [ ] Code generation, parsing, and semantic round trip tested",
  "- [ ] Real ggplot2 execution and returned object verified",
  "- [ ] Version range and lifecycle confirmed"
)
writeLines(record, file.path(out_dir, paste0(function_name, ".mapping.md")), useBytes = TRUE)

test_lines <- c(
  paste0("test_that(\"", function_name, " draft generates and round-trips\", {"),
  "  skip(\"Replace with representative arguments after human mapping review\")",
  "  # Required: schema, codegen, parser, roundtrip, and real execution assertions.",
  "})"
)
writeLines(test_lines, file.path(out_dir, paste0("test-", function_name, ".R")), useBytes = TRUE)

cat("Created evidence bundle for ggplot2::", function_name, " in ", normalizePath(out_dir, winslash = "/"), "\n", sep = "")
