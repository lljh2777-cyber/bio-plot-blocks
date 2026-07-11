bp_call_identity <- function(expression) {
  if (!is.call(expression)) return(NULL)
  head <- expression[[1]]
  if (is.symbol(head)) {
    return(list(symbol = as.character(head), package = NULL))
  }
  if (is.call(head) && length(head) == 3L && identical(as.character(head[[1]]), "::")) {
    return(list(symbol = as.character(head[[3]]), package = as.character(head[[2]])))
  }
  NULL
}

bp_is_call_to <- function(expression, symbol) {
  identity <- bp_call_identity(expression)
  !is.null(identity) && identical(identity$symbol, symbol)
}

bp_language_to_value <- function(expression) {
  if (is.null(expression)) return(bp_null())
  if (is.symbol(expression)) return(bp_symbol(as.character(expression)))

  if (length(expression) == 1L && is.atomic(expression)) {
    if (is.nan(expression)) return(bp_nan())
    if (is.infinite(expression)) return(bp_inf(sign(expression)))
    if (is.na(expression)) {
      na_type <- switch(
        typeof(expression),
        integer = "integer",
        double = "real",
        character = "character",
        complex = "complex",
        "logical"
      )
      return(bp_na(na_type))
    }
    if (is.character(expression)) return(bp_character(expression))
    if (is.integer(expression)) return(bp_integer(expression))
    if (is.double(expression)) return(bp_double(expression))
    if (is.logical(expression)) return(bp_logical(expression))
  }

  if (is.call(expression)) {
    identity <- bp_call_identity(expression)
    head_name <- if (is.symbol(expression[[1]])) as.character(expression[[1]]) else NULL

    if (!is.null(identity) && identical(identity$symbol, "aes")) {
      args <- as.list(expression)[-1]
      arg_names <- names(args) %||% rep("", length(args))
      positional <- c("x", "y")
      for (index in seq_along(args)) {
        if (!nzchar(arg_names[[index]])) {
          arg_names[[index]] <- if (index <= length(positional)) positional[[index]] else paste0("..", index)
        }
      }
      mappings <- lapply(args, bp_language_to_value)
      names(mappings) <- arg_names
      return(bp_aes_mapping(mappings))
    }

    if (identical(head_name, "~")) {
      return(bp_formula(bp_deparse_one(expression)))
    }

    if (!is.null(identity) && identical(identity$symbol, "c")) {
      items <- lapply(as.list(expression)[-1], bp_language_to_value)
      names(items) <- names(as.list(expression)[-1])
      return(bp_vector(items))
    }

    if (identical(head_name, "-") && length(expression) == 2L && identical(expression[[2]], Inf)) {
      return(bp_inf(-1L))
    }

    return(bp_raw_expression(bp_deparse_one(expression)))
  }

  bp_raw_expression(bp_deparse_one(expression))
}

bp_argument_state_from_value <- function(value, parameter = NULL) {
  type <- bp_value_type(value)
  if (identical(type, "RNull")) return("explicit_null")
  if (identical(type, "RNA")) return("explicit_na")
  if (identical(type, "RRawExpression")) return("raw_expression")
  if (!is.null(parameter$formal_default) &&
      identical(bp_value_to_source(value), bp_value_to_source(parameter$formal_default))) {
    return("explicit_default")
  }
  "explicit"
}

bp_flatten_plus <- function(expression) {
  if (is.call(expression) && length(expression) == 3L && identical(as.character(expression[[1]]), "+")) {
    return(c(bp_flatten_plus(expression[[2]]), bp_flatten_plus(expression[[3]])))
  }
  list(expression)
}

bp_parse_module_call <- function(expression, registry) {
  identity <- bp_call_identity(expression)
  source <- bp_deparse_one(expression)
  if (is.null(identity)) {
    raw <- bp_instantiate_module("core.raw_r", registry)
    raw$arguments$expression <- bp_argument("raw_expression", bp_raw_expression(source), "nested_expression", source)
    raw$source_text <- source
    raw$parse_support <- "D"
    return(raw)
  }

  if (!is.null(identity$package) && !identity$package %in% c("ggplot2", "base", "stats")) {
    raw <- bp_instantiate_module("core.raw_r", registry)
    raw$arguments$expression <- bp_argument("raw_expression", bp_raw_expression(source), "nested_expression", source)
    raw$source_text <- source
    raw$parse_support <- "D"
    return(raw)
  }

  spec <- bp_find_spec_by_symbol(registry, identity$symbol, identity$package)
  if (is.null(spec)) {
    raw <- bp_instantiate_module("core.raw_r", registry)
    raw$arguments$expression <- bp_argument("raw_expression", bp_raw_expression(source), "nested_expression", source)
    raw$source_text <- source
    raw$parse_support <- "D"
    return(raw)
  }

  instance <- bp_instantiate_module(spec$id, registry)
  args <- as.list(expression)[-1]
  if (!length(args)) {
    instance$source_text <- source
    return(instance)
  }

  arg_names <- names(args) %||% rep("", length(args))
  formal_order <- vapply(spec$parameters %||% list(), `[[`, character(1), "name")
  parse_support <- "A"

  for (position in seq_along(args)) {
    input_name <- arg_names[[position]]
    if (!nzchar(input_name)) {
      input_name <- if (position <= length(formal_order)) formal_order[[position]] else ""
    }
    if (!nzchar(input_name)) input_name <- paste0("..", position)

    parameter <- bp_parameter_spec(spec, input_name)
    value <- bp_language_to_value(args[[position]])
    state <- bp_argument_state_from_value(value, parameter)
    if (state == "raw_expression") parse_support <- "C"
    origin <- parameter$source %||% "dots_unknown"
    canonical_name <- input_name

    instance$arguments[[canonical_name]] <- bp_argument(
      state = state,
      value = value,
      origin = origin,
      source_text = bp_deparse_one(args[[position]]),
      position = position,
      dynamic = is.null(parameter)
    )
  }

  instance$source_text <- source
  instance$parse_support <- parse_support
  instance
}

bp_raw_project <- function(code, registry, diagnostic) {
  project <- bp_create_project("Imported R code")
  project$assignment$enabled <- FALSE
  module <- bp_instantiate_module("core.raw_r", registry)
  module$arguments$expression <- bp_argument(
    "raw_expression", bp_raw_expression(code), "nested_expression", code
  )
  module$source_text <- code
  module$parse_support <- "D"
  project$modules <- list(module)
  project$original_source <- code
  project$parse_support <- "D"
  project$diagnostics <- list(list(level = "error", message = diagnostic))
  project
}

#' Parse supported R/ggplot2 code into a BioPlotBlocks project
#'
#' Unknown inner expressions are retained as Raw R values; unsupported outer
#' structures are retained in a Raw R module.
#'
#' @param code R source.
#' @param registry Optional module registry.
#' @return BioPlotBlocks project.
#' @export
bp_parse_code <- function(code, registry = NULL) {
  registry <- registry %||% bp_load_registry()
  code <- paste(code, collapse = "\n")
  parsed <- tryCatch(parse(text = code, keep.source = TRUE), error = identity)
  if (inherits(parsed, "error")) {
    return(bp_raw_project(code, registry, conditionMessage(parsed)))
  }
  if (!length(parsed)) return(bp_create_project("Imported R code"))

  expressions <- as.list(parsed)
  is_library <- vapply(expressions, function(x) bp_is_call_to(x, "library") || bp_is_call_to(x, "require"), logical(1))
  candidates <- expressions[!is_library]
  if (!length(candidates)) {
    return(bp_raw_project(code, registry, "No ggplot2 expression was found."))
  }

  is_assignment <- vapply(candidates, function(x) {
    is.call(x) && length(x) == 3L && as.character(x[[1]]) %in% c("<-", "=")
  }, logical(1))
  expression <- if (any(is_assignment)) candidates[[which(is_assignment)[1]]] else candidates[[1]]

  project <- bp_create_project("Imported R code")
  project$original_source <- code

  if (is.call(expression) && length(expression) == 3L && as.character(expression[[1]]) %in% c("<-", "=")) {
    if (!is.symbol(expression[[2]])) {
      return(bp_raw_project(code, registry, "The assignment target is not a simple R symbol."))
    }
    project$assignment <- list(
      enabled = TRUE,
      target = as.character(expression[[2]]),
      operator = as.character(expression[[1]])
    )
    expression <- expression[[3]]
  } else {
    project$assignment$enabled <- FALSE
  }

  components <- bp_flatten_plus(expression)
  modules <- lapply(components, bp_parse_module_call, registry = registry)
  project$modules <- modules
  levels <- vapply(modules, `[[`, character(1), "parse_support")
  project$parse_support <- if ("D" %in% levels) "D" else if ("C" %in% levels) "C" else "A"
  project$diagnostics <- if (project$parse_support == "A") list() else list(list(
    level = "warning",
    message = "One or more source fragments were preserved as Raw R Expressions."
  ))
  bp_validate_project(project, registry)
  project
}
