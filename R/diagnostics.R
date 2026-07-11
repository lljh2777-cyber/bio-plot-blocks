bp_walk_values <- function(value, callback) {
  if (is.null(value)) return(invisible(NULL))
  callback(value)
  type <- bp_value_type(value)
  if (type == "RAesMapping") lapply(value$mappings %||% list(), bp_walk_values, callback = callback)
  if (type == "RVector") lapply(value$items %||% list(), bp_walk_values, callback = callback)
  if (type == "RCall") lapply(value$arguments %||% list(), bp_walk_values, callback = callback)
  invisible(NULL)
}

bp_has_raw_expression <- function(project) {
  found <- FALSE
  for (module in project$modules %||% list()) {
    for (argument in module$arguments %||% list()) {
      bp_walk_values(argument$value, function(value) {
        if (identical(bp_value_type(value), "RRawExpression")) found <<- TRUE
      })
    }
  }
  found
}

bp_scope_scan <- function(code) {
  parsed <- tryCatch(parse(text = code), error = identity)
  if (inherits(parsed, "error")) {
    return(list(ok = FALSE, packages = character(), message = conditionMessage(parsed)))
  }
  packages <- character()
  walk <- function(expression) {
    if (!is.call(expression)) return()
    if (is.call(expression[[1]]) && identical(as.character(expression[[1]][[1]]), "::")) {
      packages <<- c(packages, as.character(expression[[1]][[2]]))
    }
    identity <- bp_call_identity(expression)
    if (!is.null(identity) && identity$symbol %in% c("library", "require") && length(expression) >= 2L) {
      packages <<- c(packages, as.character(expression[[2]]))
    }
    lapply(as.list(expression)[-1], walk)
  }
  lapply(as.list(parsed), walk)
  packages <- unique(packages)
  prohibited <- setdiff(packages, c("ggplot2", "base", "stats", "graphics", "grDevices", "utils"))
  list(
    ok = !length(prohibited),
    packages = packages,
    prohibited = prohibited,
    message = if (length(prohibited)) {
      paste("Code references packages outside the initial scope:", paste(prohibited, collapse = ", "))
    } else {
      "Generated code contains no undisclosed add-on package calls."
    }
  )
}

bp_project_diagnostics <- function(project, registry = NULL) {
  registry <- registry %||% bp_load_registry()
  diagnostics <- project$diagnostics %||% list()
  code <- tryCatch(bp_generate_code(project, registry), error = identity)
  if (inherits(code, "error")) {
    diagnostics[[length(diagnostics) + 1L]] <- list(level = "error", message = conditionMessage(code))
    return(diagnostics)
  }
  scope <- bp_scope_scan(code)
  if (!scope$ok) diagnostics[[length(diagnostics) + 1L]] <- list(level = "error", message = scope$message)
  if (bp_has_raw_expression(project)) {
    diagnostics[[length(diagnostics) + 1L]] <- list(
      level = "warning",
      message = "Raw R Expressions are preserved verbatim and execute with local user permissions."
    )
  }
  diagnostics
}
