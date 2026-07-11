#' Create an empty BioPlotBlocks project
#'
#' @param name Project name.
#' @return Versioned project list.
#' @export
bp_create_project <- function(name = "Untitled plot") {
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  list(
    schema_version = "0.2.0",
    application_version = "0.2.0",
    name = name,
    runtime = "R",
    package_scope = "ggplot2",
    assignment = list(enabled = TRUE, target = "p", operator = "<-"),
    modules = list(),
    settings = list(
      namespace_policy = "bare",
      code_format = "multiline",
      preview_width = 920,
      preview_height = 540,
      preview_dpi = 120,
      auto_preview = FALSE
    ),
    versions = list(
      r = "4.5.1",
      ggplot2 = "4.0.1",
      shiny = "1.13.0",
      module_spec_schema = "0.2.0",
      project_schema = "0.2.0",
      ir_schema = "0.2.0"
    ),
    data_sources = list(bp_example_data_source()),
    active_data_source_id = "dataset_example",
    mapping_config = list(dataset_id = "dataset_example", plot_id = NULL, mapping = list(), confirmed_by_user = TRUE),
    data_reference = list(strategy = "local_environment", source_id = "dataset_example", symbol = "df", embedded = FALSE),
    template_provenance = NULL,
    original_source = NULL,
    parse_support = "A",
    diagnostics = list(),
    created_at = now,
    updated_at = now
  )
}

#' Validate a BioPlotBlocks project
#'
#' @param project Project list.
#' @param registry Optional module registry.
#' @return `TRUE`, invisibly, or an error.
#' @export
bp_validate_project <- function(project, registry = NULL) {
  required <- c(
    "schema_version", "application_version", "name", "runtime",
    "package_scope", "assignment", "modules", "settings", "versions"
  )
  missing <- setdiff(required, names(project))
  if (length(missing)) stop("Project is missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  if (!identical(project$runtime, "R")) stop("Project runtime must be R.", call. = FALSE)
  if (!identical(project$package_scope, "ggplot2")) stop("Project package scope must be ggplot2.", call. = FALSE)
  if (!is.list(project$modules)) stop("Project modules must be a list.", call. = FALSE)

  registry <- registry %||% bp_load_registry()
  ids <- vapply(project$modules, function(x) x$instance_id %||% "", character(1))
  if (any(!nzchar(ids)) || anyDuplicated(ids)) stop("Module instance IDs must be non-empty and unique.", call. = FALSE)
  unknown <- setdiff(vapply(project$modules, `[[`, character(1), "module_id"), names(registry))
  if (length(unknown)) stop("Project references unknown modules: ", paste(unknown, collapse = ", "), call. = FALSE)
  sources <- project$data_sources %||% list()
  source_ids <- vapply(sources, function(source) source$id %||% "", character(1))
  if (length(source_ids) && (any(!nzchar(source_ids)) || anyDuplicated(source_ids))) stop("Project data source IDs must be non-empty and unique.", call. = FALSE)
  invisible(TRUE)
}

bp_migrate_project <- function(project) {
  version <- project$schema_version %||% "0.1.0"
  if (identical(version, "0.2.0")) {
    project$data_sources <- project$data_sources %||% list(bp_example_data_source())
    project$active_data_source_id <- project$active_data_source_id %||% "dataset_example"
    project$mapping_config <- project$mapping_config %||% list(dataset_id = project$active_data_source_id, plot_id = NULL, mapping = list(), confirmed_by_user = TRUE)
    project$data_reference$source_id <- project$data_reference$source_id %||% project$active_data_source_id
    return(project)
  }
  if (identical(version, "0.1.0")) {
    project$schema_version <- "0.2.0"
    project$application_version <- project$application_version %||% "0.2.0"
    project$runtime <- "R"
    project$package_scope <- "ggplot2"
    project$diagnostics <- project$diagnostics %||% list()
    return(project)
  }
  stop("Unsupported project schema version: ", version, call. = FALSE)
}

#' Save a project using an atomic JSON replacement
#'
#' @param project Project list.
#' @param path Destination path.
#' @return Normalized destination path.
#' @export
bp_save_project <- function(project, path) {
  bp_validate_project(project)
  project$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  destination <- normalizePath(dirname(path), winslash = "/", mustWork = TRUE)
  final_path <- file.path(destination, basename(path))
  temporary <- tempfile(pattern = paste0(basename(path), "."), tmpdir = destination)
  jsonlite::write_json(project, temporary, pretty = TRUE, auto_unbox = TRUE, null = "null", digits = NA)
  if (!file.rename(temporary, final_path)) {
    stop("Unable to atomically save the project.", call. = FALSE)
  }
  normalizePath(final_path, winslash = "/", mustWork = TRUE)
}

#' Load and migrate a project JSON file
#'
#' @param path Project file.
#' @return Validated project list.
#' @export
bp_load_project <- function(path) {
  project <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  project <- bp_migrate_project(project)
  bp_validate_project(project)
  project
}
