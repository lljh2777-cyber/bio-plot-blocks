bp_template_directory <- function() {
  bp_inst_path("templates")
}

bp_basic_scatter_project <- function(registry = NULL) {
  registry <- registry %||% bp_load_registry()
  project <- bp_create_project("Untitled scatter plot")
  base <- bp_instantiate_module("r.ggplot2.ggplot", registry)
  base$arguments$data <- bp_argument("explicit", bp_symbol("df"), "formal")
  base$arguments$mapping <- bp_argument(
    "explicit",
    bp_aes_mapping(list(x = bp_symbol("PC1"), y = bp_symbol("PC2"))),
    "formal"
  )
  points <- bp_instantiate_module("r.ggplot2.geom_point", registry)
  points$arguments$size <- bp_argument("explicit", bp_double(2), "dots_aesthetic")
  points$arguments$alpha <- bp_argument("explicit", bp_double(0.72), "dots_aesthetic")
  project$modules <- list(base, points)
  project
}

bp_ggplot_only_project <- function(registry = NULL) {
  registry <- registry %||% bp_load_registry()
  project <- bp_create_project("Untitled plot")
  base <- bp_instantiate_module("r.ggplot2.ggplot", registry)
  base$arguments$data <- bp_argument("explicit", bp_symbol("df"), "formal")
  base$arguments$mapping <- bp_argument(
    "explicit",
    bp_aes_mapping(list(x = bp_symbol("PC1"), y = bp_symbol("PC2"))),
    "formal"
  )
  project$modules <- list(base)
  project$mapping_config <- list(
    dataset_id = "dataset_example",
    plot_id = base$instance_id,
    mapping = list(x = "PC1", y = "PC2"),
    confirmed_by_user = TRUE
  )
  project
}

#' Load a BioPlotBlocks template definition
#'
#' @param template_id Template identifier or path to a template JSON file.
#' @param path Optional template root.
#' @return Parsed template definition.
#' @export
bp_load_template <- function(template_id = NULL, path = NULL) {
  path <- path %||% bp_template_directory()
  files <- list.files(path, pattern = "\\.template\\.json$", recursive = TRUE, full.names = TRUE)
  templates <- lapply(files, jsonlite::fromJSON, simplifyVector = FALSE)
  if (is.null(template_id)) return(templates)
  if (file.exists(template_id)) return(jsonlite::fromJSON(template_id, simplifyVector = FALSE))
  matches <- Filter(function(x) identical(x$id, template_id), templates)
  if (!length(matches)) stop("Unknown template: ", template_id, call. = FALSE)
  matches[[1]]
}

#' Expand a template into ordinary module instances
#'
#' @param template Template definition or template ID.
#' @param registry Optional module registry.
#' @return BioPlotBlocks project.
#' @export
bp_project_from_template <- function(template, registry = NULL) {
  registry <- registry %||% bp_load_registry()
  if (is.character(template)) template <- bp_load_template(template)
  if (!identical(template$runtime, "R") || !identical(template$package_scope, "ggplot2")) {
    stop("Initial templates must be R/ggplot2 only.", call. = FALSE)
  }

  project <- bp_create_project(template$title %||% "Template project")
  project$template_provenance <- list(
    id = template$id,
    version = template$version,
    required_columns = template$required_columns,
    expanded_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  project$modules <- lapply(template$modules, function(seed) {
    instance <- bp_instantiate_module(seed$module_id, registry)
    if (length(seed$arguments %||% list())) {
      for (name in names(seed$arguments)) {
        supplied <- seed$arguments[[name]]
        supplied$position <- supplied$position %||% NULL
        supplied$dynamic <- isTRUE(supplied$dynamic)
        instance$arguments[[name]] <- supplied
      }
    }
    instance
  })
  bp_validate_project(project, registry)
  project
}
