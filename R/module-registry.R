bp_repository_root <- function() {
  configured <- getOption("BioPlotBlocks.root")
  if (!is.null(configured) && dir.exists(configured)) {
    return(normalizePath(configured, winslash = "/", mustWork = TRUE))
  }

  installed <- system.file(package = "BioPlotBlocks")
  if (nzchar(installed)) return(installed)

  candidates <- c(getwd(), file.path(getwd(), ".."))
  hit <- candidates[file.exists(file.path(candidates, "DESCRIPTION"))][1]
  if (!is.na(hit)) return(normalizePath(hit, winslash = "/", mustWork = TRUE))

  stop("Unable to locate the BioPlotBlocks repository root.", call. = FALSE)
}

bp_inst_path <- function(...) {
  installed <- system.file(..., package = "BioPlotBlocks")
  if (nzchar(installed)) return(installed)
  file.path(bp_repository_root(), "inst", ...)
}

#' Validate a declarative ModuleSpec
#'
#' @param spec Parsed ModuleSpec list.
#' @return `TRUE`, invisibly, or an error.
#' @export
bp_validate_module_spec <- function(spec) {
  required <- c(
    "schema_version", "module_version", "id", "runtime", "package",
    "package_version", "symbol", "module_type", "status", "presentation",
    "composition", "parameters", "code_generation", "code_parsing",
    "documentation", "compatibility", "provenance", "tests"
  )
  missing <- setdiff(required, names(spec))
  if (length(missing)) {
    stop("ModuleSpec is missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!identical(spec$runtime, "R")) {
    stop("ModuleSpec runtime must equal R.", call. = FALSE)
  }
  allowed_packages <- c("ggplot2", "BioPlotBlocks.core")
  if (!spec$package %in% allowed_packages) {
    stop("Initial ModuleSpec package is outside the ggplot2/core scope: ", spec$package, call. = FALSE)
  }
  if (!is.list(spec$parameters)) {
    stop("ModuleSpec parameters must be a list.", call. = FALSE)
  }
  if (length(spec$parameters)) {
    param_names <- vapply(spec$parameters, function(x) x$name %||% "", character(1))
    if (any(!nzchar(param_names)) || anyDuplicated(param_names)) {
      stop("ModuleSpec parameter names must be non-empty and unique.", call. = FALSE)
    }
    required_parameter_fields <- c(
      "name", "source", "formal_default", "value_types", "ui_group",
      "ui_control", "raw_expression_allowed", "omit_when_unset", "help_source",
      "version_notes"
    )
    invalid <- vapply(
      spec$parameters,
      function(x) length(setdiff(required_parameter_fields, names(x))) > 0L,
      logical(1)
    )
    if (any(invalid)) {
      stop("One or more parameters do not satisfy the minimum ModuleSpec contract.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Load the declarative core and ggplot2 module registry
#'
#' @param path Optional module directory.
#' @return Named list of validated ModuleSpecs.
#' @export
bp_load_registry <- function(path = NULL) {
  path <- path %||% bp_inst_path("modules")
  files <- list.files(path, pattern = "module\\.json$", recursive = TRUE, full.names = TRUE)
  if (!length(files)) stop("No ModuleSpecs found at ", path, call. = FALSE)

  specs <- unlist(lapply(files, function(file) {
    parsed <- jsonlite::fromJSON(file, simplifyVector = FALSE)
    entries <- if (!is.null(parsed$modules) && is.null(parsed$id)) parsed$modules else list(parsed)
    lapply(entries, function(spec) {
      bp_validate_module_spec(spec)
      spec$source_file <- normalizePath(file, winslash = "/", mustWork = TRUE)
      spec
    })
  }), recursive = FALSE)
  ids <- vapply(specs, `[[`, character(1), "id")
  if (anyDuplicated(ids)) stop("Duplicate ModuleSpec IDs detected.", call. = FALSE)
  stats::setNames(specs, ids)
}

#' Retrieve one ModuleSpec
#'
#' @param registry Registry returned by [bp_load_registry()].
#' @param module_id ModuleSpec identifier.
#' @return A ModuleSpec.
#' @export
bp_get_spec <- function(registry, module_id) {
  spec <- registry[[module_id]]
  if (is.null(spec)) stop("Unknown module: ", module_id, call. = FALSE)
  spec
}

bp_find_spec_by_symbol <- function(registry, symbol, package = NULL) {
  hits <- Filter(function(spec) {
    symbol_match <- identical(spec$symbol, symbol) || symbol %in% unlist(spec$code_parsing$accepted_symbols %||% list())
    package_match <- is.null(package) || identical(spec$package, package)
    symbol_match && package_match
  }, registry)
  if (!length(hits)) return(NULL)
  hits[[1]]
}

bp_parameter_spec <- function(spec, name) {
  if (!length(spec$parameters)) return(NULL)
  hit <- Filter(function(x) identical(x$name, name) || name %in% unlist(x$aliases %||% list()), spec$parameters)
  if (!length(hit)) NULL else hit[[1]]
}
