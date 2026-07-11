bp_parse_single_expression <- function(source) {
  parsed <- parse(text = source, keep.source = FALSE)
  if (length(parsed) != 1L) {
    stop("A Raw R Expression must contain exactly one expression.", call. = FALSE)
  }
  parsed[[1]]
}

bp_function_head <- function(symbol, package = NULL, namespace_policy = "bare") {
  use_namespace <- identical(namespace_policy, "always") && !is.null(package) && identical(package, "ggplot2")
  if (!use_namespace) return(as.name(symbol))
  as.call(list(as.name("::"), as.name(package), as.name(symbol)))
}

bp_value_to_language <- function(value, namespace_policy = "bare") {
  if (is.null(value)) return(NULL)
  type <- bp_value_type(value)

  switch(
    type,
    RSymbol = as.name(value$name),
    RCharacter = as.character(value$value),
    RDouble = as.numeric(value$value),
    RInteger = as.integer(value$value),
    RLogical = as.logical(value$value),
    RNull = NULL,
    RNA = switch(
      value$na_type %||% "logical",
      integer = NA_integer_,
      real = NA_real_,
      character = NA_character_,
      complex = NA_complex_,
      NA
    ),
    RInf = if ((value$sign %||% 1L) < 0) -Inf else Inf,
    RNaN = NaN,
    RRawExpression = bp_parse_single_expression(value$source),
    RFormula = bp_parse_single_expression(value$source),
    RVector = {
      values <- lapply(value$items %||% list(), bp_value_to_language, namespace_policy = namespace_policy)
      as.call(c(list(as.name("c")), values))
    },
    RAesMapping = {
      mappings <- value$mappings %||% list()
      values <- lapply(mappings, bp_value_to_language, namespace_policy = namespace_policy)
      head <- bp_function_head("aes", "ggplot2", namespace_policy)
      as.call(c(list(head), values))
    },
    RCall = {
      args <- lapply(value$arguments %||% list(), bp_value_to_language, namespace_policy = namespace_policy)
      head <- bp_function_head(value$function_name, value$namespace, namespace_policy)
      as.call(c(list(head), args))
    },
    stop("Unsupported R semantic value type: ", type, call. = FALSE)
  )
}

bp_argument_to_language <- function(argument, parameter = NULL, namespace_policy = "bare") {
  if (bp_is_unset(argument)) return(structure(list(), class = "bp_omitted_argument"))
  state <- argument$state

  if (state == "explicit_null") return(NULL)
  if (state == "explicit_na" && (is.null(argument$value) || bp_value_type(argument$value) != "RNA")) {
    return(NA)
  }
  if (state == "missing") {
    return(bp_parse_single_expression("NULL"))
  }

  value <- argument$value
  if (state == "explicit_default" && is.null(value) && !is.null(parameter)) {
    value <- parameter$formal_default
  }
  bp_value_to_language(value, namespace_policy = namespace_policy)
}

bp_module_to_language <- function(instance, registry, namespace_policy = "bare") {
  if (identical(instance$module_id, "core.raw_r")) {
    raw <- instance$arguments$expression$value$source %||% instance$source_text %||% ""
    return(bp_parse_single_expression(raw))
  }

  spec <- bp_get_spec(registry, instance$module_id)
  call_args <- list()
  arguments <- instance$arguments %||% list()

  for (name in names(arguments)) {
    argument <- arguments[[name]]
    if (bp_is_unset(argument)) next
    parameter <- bp_parameter_spec(spec, name)
    value <- bp_argument_to_language(argument, parameter, namespace_policy)
    call_args[length(call_args) + 1L] <- list(value)
    if (nzchar(name)) names(call_args)[length(call_args)] <- name
  }

  head <- bp_function_head(spec$symbol, spec$package, namespace_policy)
  as.call(c(list(head), call_args))
}

bp_plot_language <- function(project, registry = NULL) {
  registry <- registry %||% bp_load_registry()
  modules <- project$modules %||% list()
  if (!length(modules)) stop("The project has no modules to generate.", call. = FALSE)
  policy <- project$settings$namespace_policy %||% "bare"
  calls <- lapply(modules, bp_module_to_language, registry = registry, namespace_policy = policy)

  if (length(calls) == 1L) return(calls[[1]])
  Reduce(function(left, right) as.call(list(as.name("+"), left, right)), calls)
}

bp_project_language <- function(project, registry = NULL) {
  expression <- bp_plot_language(project, registry)
  assignment <- project$assignment %||% list(enabled = FALSE)
  if (!isTRUE(assignment$enabled)) return(expression)
  target <- assignment$target %||% "p"
  operator <- assignment$operator %||% "<-"
  as.call(list(as.name(operator), as.name(target), expression))
}

bp_deparse_one <- function(expression) {
  paste(deparse(expression, width.cutoff = 500L, control = c("keepNA", "niceNames")), collapse = " ")
}

bp_module_source <- function(instance, registry, namespace_policy = "bare") {
  if (identical(instance$module_id, "core.raw_r")) {
    return(instance$arguments$expression$value$source %||% instance$source_text %||% "")
  }
  bp_deparse_one(bp_module_to_language(instance, registry, namespace_policy))
}

#' Generate line-oriented R code with module source mapping
#'
#' @param project BioPlotBlocks project.
#' @param registry Optional module registry.
#' @return List of line records.
#' @export
bp_generate_lines <- function(project, registry = NULL) {
  registry <- registry %||% bp_load_registry()
  modules <- project$modules %||% list()
  if (!length(modules)) return(list())
  policy <- project$settings$namespace_policy %||% "bare"

  sources <- vapply(modules, bp_module_source, character(1), registry = registry, namespace_policy = policy)
  assignment <- project$assignment %||% list(enabled = FALSE)
  prefix <- if (isTRUE(assignment$enabled)) {
    paste0(assignment$target %||% "p", " ", assignment$operator %||% "<-", " ")
  } else {
    ""
  }

  lapply(seq_along(modules), function(index) {
    is_first <- index == 1L
    is_last <- index == length(modules)
    text <- paste0(
      if (is_first) prefix else "  ",
      sources[[index]],
      if (!is_last) " +" else ""
    )
    list(
      line_number = index,
      instance_id = modules[[index]]$instance_id,
      module_id = modules[[index]]$module_id,
      text = text
    )
  })
}

#' Generate deterministic R source from a project
#'
#' @param project BioPlotBlocks project.
#' @param registry Optional module registry.
#' @param include_setup Include `library(ggplot2)` for bare namespace output.
#' @return R source string.
#' @export
bp_generate_code <- function(project, registry = NULL, include_setup = FALSE) {
  registry <- registry %||% bp_load_registry()
  lines <- bp_generate_lines(project, registry)
  code <- paste(vapply(lines, `[[`, character(1), "text"), collapse = "\n")
  if (isTRUE(include_setup) && identical(project$settings$namespace_policy %||% "bare", "bare")) {
    setup <- "library(ggplot2)"
    active_id <- project$active_data_source_id %||% "dataset_example"
    sources <- project$data_sources %||% list()
    active <- Filter(function(source) identical(source$id, active_id), sources)
    if (length(active) && !isTRUE(active[[1]]$example)) {
      source <- active[[1]]
      setup <- paste(setup, bp_data_source_setup_line(source), sep = "\n")
    }
    code <- paste(setup, code, sep = "\n\n")
  }
  code
}
