bp_new_instance_id <- local({
  counter <- 0L
  function(prefix = "m") {
    counter <<- counter + 1L
    paste0(prefix, "-", format(Sys.time(), "%H%M%S"), "-", sprintf("%04d", counter))
  }
})

bp_default_from_spec <- function(parameter) {
  default <- parameter$formal_default
  if (is.null(default) || is.null(default$type)) return(NULL)
  default
}

#' Create a module instance from a ModuleSpec
#'
#' @param module_id ModuleSpec identifier.
#' @param registry Module registry.
#' @param instance_id Optional stable instance ID.
#' @return Serializable module instance.
#' @export
bp_instantiate_module <- function(module_id, registry, instance_id = NULL) {
  spec <- bp_get_spec(registry, module_id)
  parameters <- spec$parameters %||% list()
  arguments <- lapply(parameters, function(parameter) {
    bp_argument(
      state = "unset",
      value = bp_default_from_spec(parameter),
      origin = parameter$source
    )
  })
  names(arguments) <- vapply(parameters, `[[`, character(1), "name")

  list(
    instance_id = instance_id %||% bp_new_instance_id(gsub("[^A-Za-z0-9]+", "-", spec$symbol)),
    module_id = module_id,
    collapsed = FALSE,
    arguments = arguments,
    source_range = NULL,
    source_text = NULL,
    parse_support = "A"
  )
}

bp_clone_instance <- function(instance) {
  clone <- unserialize(serialize(instance, NULL))
  clone$instance_id <- bp_new_instance_id("copy")
  clone$source_range <- NULL
  clone
}

bp_set_argument <- function(instance, name, argument) {
  instance$arguments[[name]] <- argument
  instance
}

bp_get_argument <- function(instance, name) {
  instance$arguments[[name]] %||% bp_argument(origin = "dots_unknown", dynamic = TRUE)
}

bp_module_summary <- function(instance, registry, max_length = 90L) {
  spec <- bp_get_spec(registry, instance$module_id)
  args <- instance$arguments %||% list()
  set_args <- Filter(function(x) !bp_is_unset(x), args)
  if (!length(set_args)) return(paste0(spec$symbol, "()"))

  pieces <- vapply(names(set_args), function(name) {
    argument <- set_args[[name]]
    paste0(name, " = ", bp_value_to_source(argument$value))
  }, character(1))
  result <- paste0(spec$symbol, "(", paste(pieces, collapse = ", "), ")")
  if (nchar(result) > max_length) paste0(substr(result, 1L, max_length - 1L), "…") else result
}
