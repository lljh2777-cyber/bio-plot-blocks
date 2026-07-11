# Typed R semantic values ---------------------------------------------------

bp_ir_value <- function(type, ...) {
  structure(c(list(type = type), list(...)), class = c("bp_ir_value", "list"))
}

bp_symbol <- function(name) {
  stopifnot(is.character(name), length(name) == 1L, nzchar(name))
  bp_ir_value("RSymbol", name = name)
}

bp_character <- function(value) {
  bp_ir_value("RCharacter", value = as.character(value))
}

bp_double <- function(value) {
  bp_ir_value("RDouble", value = as.numeric(value))
}

bp_integer <- function(value) {
  bp_ir_value("RInteger", value = as.integer(value))
}

bp_logical <- function(value) {
  bp_ir_value("RLogical", value = as.logical(value))
}

bp_null <- function() {
  bp_ir_value("RNull")
}

bp_na <- function(na_type = "logical") {
  bp_ir_value("RNA", na_type = na_type)
}

bp_inf <- function(sign = 1L) {
  bp_ir_value("RInf", sign = if (sign < 0) -1L else 1L)
}

bp_nan <- function() {
  bp_ir_value("RNaN")
}

bp_raw_expression <- function(source) {
  bp_ir_value("RRawExpression", source = as.character(source))
}

bp_formula <- function(source) {
  bp_ir_value("RFormula", source = as.character(source))
}

bp_vector <- function(items = list()) {
  bp_ir_value("RVector", items = items)
}

bp_aes_mapping <- function(mappings = list()) {
  bp_ir_value("RAesMapping", mappings = mappings)
}

#' Construct a structured R function-call value
#'
#' @param function_name Function symbol without a package prefix.
#' @param arguments Named list of typed values.
#' @param namespace Optional R package namespace.
#' @return A typed R-call value.
#' @export
bp_call_value <- function(function_name, arguments = list(), namespace = NULL) {
  bp_ir_value(
    "RCall",
    function_name = as.character(function_name),
    namespace = namespace,
    arguments = arguments
  )
}

#' Construct a module argument with an explicit semantic state
#'
#' @param state One of the BioPlotBlocks argument states.
#' @param value A typed R semantic value.
#' @param origin Argument provenance.
#' @param source_text Optional source fragment.
#' @return A serializable argument record.
#' @export
bp_argument <- function(
    state = "unset",
    value = NULL,
    origin = "formal",
    source_text = NULL,
    position = NULL,
    dynamic = FALSE) {
  allowed_states <- c(
    "unset", "explicit", "explicit_default", "explicit_null",
    "explicit_na", "raw_expression", "missing", "inherited"
  )
  allowed_origins <- c(
    "formal", "dots_documented", "dots_aesthetic", "dots_forwarded",
    "dots_unknown", "nested_expression"
  )
  if (!state %in% allowed_states) {
    stop("Unknown argument state: ", state, call. = FALSE)
  }
  if (!origin %in% allowed_origins) {
    stop("Unknown argument origin: ", origin, call. = FALSE)
  }
  list(
    state = state,
    value = value,
    origin = origin,
    source_text = source_text,
    position = position,
    dynamic = isTRUE(dynamic)
  )
}

bp_value_type <- function(value) {
  if (is.null(value$type)) "RRawExpression" else value$type
}

bp_is_unset <- function(argument) {
  is.null(argument) || argument$state %in% c("unset", "inherited")
}

bp_value_from_text <- function(text, control = "expression", state = "explicit") {
  text <- trimws(as.character(text %||% ""))
  if (state == "explicit_null") return(bp_null())
  if (state == "explicit_na") return(bp_na())
  if (state == "raw_expression") return(bp_raw_expression(text))

  switch(
    control,
    numeric =,
    number =,
    numeric_or_expression = {
      value <- suppressWarnings(as.numeric(text))
      if (!is.na(value)) bp_double(value) else bp_raw_expression(text)
    },
    integer = {
      value <- suppressWarnings(as.integer(text))
      if (!is.na(value)) bp_integer(value) else bp_raw_expression(text)
    },
    logical_state = {
      if (identical(toupper(text), "TRUE")) bp_logical(TRUE)
      else if (identical(toupper(text), "FALSE")) bp_logical(FALSE)
      else if (identical(toupper(text), "NA")) bp_na()
      else bp_raw_expression(text)
    },
    enum = bp_character(text),
    string =,
    text =,
    color =,
    color_or_expression = bp_character(text),
    symbol =,
    data_reference =,
    symbol_or_expression = {
      if (grepl("^[.A-Za-z][.A-Za-z0-9_]*$", text)) bp_symbol(text) else bp_raw_expression(text)
    },
    formula =,
    formula_editor = bp_formula(text),
    vector =,
    vector_editor = bp_raw_expression(text),
    expression =,
    code = bp_raw_expression(text),
    bp_raw_expression(text)
  )
}

bp_value_to_source <- function(value) {
  if (is.null(value)) return("")
  type <- bp_value_type(value)
  switch(
    type,
    RSymbol = value$name,
    RCharacter = encodeString(value$value, quote = '"'),
    RDouble = format(value$value, scientific = FALSE, trim = TRUE),
    RInteger = paste0(value$value, "L"),
    RLogical = if (isTRUE(value$value)) "TRUE" else "FALSE",
    RNull = "NULL",
    RNA = switch(
      value$na_type %||% "logical",
      integer = "NA_integer_",
      real = "NA_real_",
      character = "NA_character_",
      complex = "NA_complex_",
      "NA"
    ),
    RInf = if ((value$sign %||% 1L) < 0) "-Inf" else "Inf",
    RNaN = "NaN",
    RRawExpression = value$source,
    RFormula = value$source,
    RVector = {
      items <- value$items %||% list()
      item_text <- vapply(items, bp_value_to_source, character(1))
      if (!is.null(names(items))) {
        named <- nzchar(names(items))
        item_text[named] <- paste0(names(items)[named], " = ", item_text[named])
      }
      paste0("c(", paste(item_text, collapse = ", "), ")")
    },
    RAesMapping = {
      mappings <- value$mappings %||% list()
      pieces <- vapply(mappings, bp_value_to_source, character(1))
      if (length(pieces)) pieces <- paste0(names(mappings), " = ", pieces)
      paste0("aes(", paste(pieces, collapse = ", "), ")")
    },
    RCall = {
      args <- value$arguments %||% list()
      pieces <- vapply(args, bp_value_to_source, character(1))
      if (!is.null(names(args))) {
        named <- nzchar(names(args))
        pieces[named] <- paste0(names(args)[named], " = ", pieces[named])
      }
      fn <- if (is.null(value$namespace) || !nzchar(value$namespace)) {
        value$function_name
      } else {
        paste0(value$namespace, "::", value$function_name)
      }
      paste0(fn, "(", paste(pieces, collapse = ", "), ")")
    },
    value$source %||% ""
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}
