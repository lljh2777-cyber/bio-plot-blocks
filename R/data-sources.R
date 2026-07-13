bp_example_data_source <- function() {
  list(
    id = "dataset_example",
    name = "df",
    source_type = "example",
    original_file_name = NULL,
    object_type = "data.frame",
    rows = 420L,
    columns = 11L,
    status = "ready",
    example = TRUE,
    relink_required = FALSE,
    column_metadata = list(),
    quality = list(warnings = list()),
    parse_options = list()
  )
}

bp_data_source_id <- function(project) {
  existing <- vapply(project$data_sources %||% list(), function(source) source$id %||% "", character(1))
  index <- 1L
  repeat {
    candidate <- sprintf("dataset_%03d", index)
    if (!candidate %in% existing) return(candidate)
    index <- index + 1L
  }
}

bp_data_source_name <- function(file_name, existing = character()) {
  base <- tools::file_path_sans_ext(basename(file_name %||% "dataset"))
  base <- gsub("[^A-Za-z0-9_.]+", "_", base)
  base <- make.names(base, unique = FALSE)
  base <- gsub("\\.", "_", base)
  if (!nzchar(base)) base <- "dataset"
  candidate <- base
  index <- 2L
  while (candidate %in% existing) {
    candidate <- paste0(base, "_", index)
    index <- index + 1L
  }
  candidate
}

bp_delimiter_value <- function(value, extension = "csv") {
  if (is.null(value) || identical(value, "auto")) {
    return(if (tolower(extension) == "csv") "," else "\t")
  }
  switch(value, comma = ",", tab = "\t", semicolon = ";", pipe = "|", value)
}

bp_read_delimited_data <- function(path, original_name, options = list(), max_bytes = 25 * 1024^2) {
  info <- file.info(path)
  if (!is.finite(info$size) || info$size <= 0) stop("The selected data file is empty.", call. = FALSE)
  if (info$size > max_bytes) {
    stop(sprintf("The selected file is %.1f MB; the first-stage importer limit is %.0f MB.", info$size / 1024^2, max_bytes / 1024^2), call. = FALSE)
  }
  extension <- tolower(tools::file_ext(original_name))
  if (!extension %in% c("csv", "tsv", "txt")) {
    stop("First-stage import supports CSV, TSV, and TXT files.", call. = FALSE)
  }
  separator <- bp_delimiter_value(options$delimiter %||% "auto", extension)
  encoding <- options$encoding %||% "UTF-8"
  header <- !identical(options$header, FALSE)
  quote <- options$quote %||% '"'
  decimal <- options$decimal %||% "."
  skip <- suppressWarnings(as.integer(options$skip %||% 0L))
  if (is.na(skip) || skip < 0L) skip <- 0L
  na_values <- options$na_values %||% c("", "NA", "N/A", "null", "NULL")
  if (length(na_values) == 1L) {
    na_values <- trimws(strsplit(as.character(na_values), ",", fixed = TRUE)[[1]])
  }

  data <- tryCatch(
    utils::read.table(
      file = path,
      header = header,
      sep = separator,
      quote = quote,
      dec = decimal,
      na.strings = na_values,
      skip = skip,
      fileEncoding = encoding,
      check.names = FALSE,
      stringsAsFactors = FALSE,
      comment.char = "",
      fill = TRUE,
      blank.lines.skip = TRUE
    ),
    error = function(error) {
      stop("Unable to parse the delimited file: ", conditionMessage(error), call. = FALSE)
    }
  )
  if (!is.data.frame(data) || !ncol(data)) stop("No data columns were found. Check the delimiter, header, encoding, and skipped rows.", call. = FALSE)
  list(
    data = data,
    options = list(
      delimiter = separator,
      encoding = encoding,
      header = header,
      na_values = na_values,
      quote = quote,
      decimal = decimal,
      skip = skip
    )
  )
}

bp_plain_list_table <- function(object) {
  if (!is.list(object) || is.object(object) || !length(object)) return(FALSE)
  valid_column <- vapply(object, function(column) is.atomic(column) || is.factor(column), logical(1))
  all(valid_column) && length(unique(lengths(object))) == 1L
}

bp_r_object_class <- function(object) {
  classes <- attr(object, "class", exact = TRUE)
  if (is.null(classes) || !length(classes)) typeof(object) else paste(as.character(classes), collapse = ", ")
}

bp_inspect_r_object <- function(name, object) {
  object_type <- typeof(object)
  classes <- attr(object, "class", exact = TRUE) %||% character()
  forbidden <- object_type %in% c("closure", "environment", "externalptr", "weakref") || any(classes %in% c("connection", "formula"))
  is_data_frame <- object_type == "list" && any(classes == "data.frame") && all(classes %in% c("data.frame", "tbl_df", "tbl"))
  is_tibble <- is_data_frame && any(classes %in% c("tbl_df", "tbl"))
  is_matrix_object <- object_type %in% c("logical", "integer", "double", "complex", "character", "raw") &&
    length(attr(object, "dim", exact = TRUE) %||% integer()) == 2L
  is_list_table <- bp_plain_list_table(object)
  kind <- if (forbidden) "forbidden" else if (is_tibble) "tibble" else if (is_data_frame) "data.frame" else if (is_matrix_object) "matrix" else if (is_list_table) "list_table" else "unsupported"
  supported <- kind %in% c("data.frame", "tibble", "matrix", "list_table")
  requires_conversion <- kind %in% c("matrix", "list_table")
  dimensions <- if (kind %in% c("data.frame", "tibble", "list_table")) {
    c(if (length(object)) length(object[[1]]) else 0L, length(object))
  } else if (identical(kind, "matrix")) {
    as.integer(attr(object, "dim", exact = TRUE))
  } else c(NA_integer_, NA_integer_)
  object_row_names <- if (kind %in% c("data.frame", "tibble")) attr(object, "row.names", exact = TRUE) else if (identical(kind, "matrix")) dimnames(object)[[1]] %||% NULL else NULL
  has_row_names <- !is.null(object_row_names) && length(object_row_names) == dimensions[[1]] && !identical(as.character(object_row_names), as.character(seq_len(dimensions[[1]])))
  list(
    name = name,
    kind = kind,
    r_class = bp_r_object_class(object),
    rows = dimensions[[1]],
    columns = dimensions[[2]],
    supported = supported,
    requires_conversion = requires_conversion,
    has_row_names = has_row_names,
    status = if (forbidden) "forbidden" else if (!supported) "unsupported" else if (requires_conversion) "conversion_required" else "ready",
    message = if (forbidden) "Functions, environments, connections, formulas, and external pointers cannot be imported." else if (!supported) paste0("R class '", bp_r_object_class(object), "' is not supported in this stage.") else if (requires_conversion) paste0(kind, " will be converted to a data.frame after confirmation.") else "Ready for plotting."
  )
}

bp_read_r_data_objects <- function(path, original_name, max_bytes = 25 * 1024^2) {
  info <- file.info(path)
  if (!is.finite(info$size) || info$size <= 0) stop("The selected R data file is empty.", call. = FALSE)
  if (info$size > max_bytes) stop(sprintf("The selected R data file is %.1f MB; the current safety limit is %.0f MB.", info$size / 1024^2, max_bytes / 1024^2), call. = FALSE)
  extension <- tolower(tools::file_ext(original_name))
  if (!extension %in% c("rds", "rdata", "rda")) stop("R object import supports .rds, .RData, and .rda files.", call. = FALSE)
  if (identical(extension, "rds")) {
    object <- tryCatch(readRDS(path), error = function(error) stop("Unable to read RDS: ", conditionMessage(error), call. = FALSE))
    object_name <- bp_data_source_name(original_name)
    metadata <- list(bp_inspect_r_object(object_name, object))
    objects <- if (isTRUE(metadata[[1]]$supported)) stats::setNames(list(object), object_name) else list()
    return(list(format = "rds", objects = objects, metadata = metadata, file_size = unname(info$size)))
  }
  isolated <- new.env(parent = emptyenv())
  object_names <- tryCatch(load(path, envir = isolated), error = function(error) stop("Unable to load RData/rda in an isolated environment: ", conditionMessage(error), call. = FALSE))
  metadata <- lapply(object_names, function(name) tryCatch(
    bp_inspect_r_object(name, get(name, envir = isolated, inherits = FALSE)),
    error = function(error) list(name = name, kind = "unsupported", r_class = "unknown", rows = NA_integer_, columns = NA_integer_, supported = FALSE, requires_conversion = FALSE, has_row_names = FALSE, status = "unsupported", message = paste0("Inspection failed: ", conditionMessage(error)))
  ))
  supported_names <- vapply(Filter(function(item) isTRUE(item$supported), metadata), `[[`, character(1), "name")
  objects <- stats::setNames(lapply(supported_names, function(name) get(name, envir = isolated, inherits = FALSE)), supported_names)
  rm(isolated)
  list(format = extension, objects = objects, metadata = metadata, file_size = unname(info$size))
}

bp_convert_r_object <- function(object, row_names = "preserve", row_name_column = "RowName") {
  inspection <- bp_inspect_r_object("object", object)
  if (!isTRUE(inspection$supported)) stop(inspection$message, call. = FALSE)
  data <- switch(
    inspection$kind,
    data.frame = object,
    tibble = as.data.frame(object, optional = TRUE),
    matrix = as.data.frame(object, stringsAsFactors = FALSE, optional = TRUE),
    list_table = data.frame(object, check.names = FALSE, stringsAsFactors = FALSE),
    stop("Unsupported R object conversion.", call. = FALSE)
  )
  if (is.null(names(data))) names(data) <- paste0("V", seq_len(ncol(data)))
  original_row_names <- if (identical(inspection$kind, "matrix")) dimnames(object)[[1]] %||% row.names(data) else row.names(data)
  row_names <- match.arg(row_names, c("preserve", "column", "ignore"))
  if (identical(row_names, "column")) {
    row_name_column <- trimws(row_name_column %||% "")
    if (!nzchar(row_name_column)) stop("Row-name column must have a non-empty name.", call. = FALSE)
    if (row_name_column %in% names(data)) stop("Row-name column conflicts with an existing column: ", row_name_column, call. = FALSE)
    row_column <- data.frame(as.character(original_row_names), check.names = FALSE, stringsAsFactors = FALSE)
    names(row_column) <- row_name_column
    data <- cbind(row_column, data)
    row.names(data) <- NULL
  } else if (identical(row_names, "ignore")) row.names(data) <- NULL
  attr(data, "bp_conversion") <- list(from = inspection$kind, to = "data.frame", row_names = row_names, row_name_column = if (identical(row_names, "column")) row_name_column else NULL)
  data
}

bp_register_data_source <- function(project, source) {
  project <- unserialize(serialize(project, NULL))
  project$data_sources <- project$data_sources %||% list()
  existing <- which(vapply(project$data_sources, function(item) identical(item$id, source$id), logical(1)))
  if (length(existing)) project$data_sources[[existing[[1]]]] <- source else project$data_sources[[length(project$data_sources) + 1L]] <- source
  project
}

bp_rename_data_source <- function(project, source_id, new_name) {
  project <- unserialize(serialize(project, NULL))
  index <- which(vapply(project$data_sources %||% list(), function(source) identical(source$id, source_id), logical(1)))
  if (!length(index)) stop("Unknown data source: ", source_id, call. = FALSE)
  source <- project$data_sources[[index[[1]]]]
  if (isTRUE(source$example)) stop("The built-in example data source cannot be renamed.", call. = FALSE)
  old_name <- source$name
  source$name <- new_name
  project$data_sources[[index[[1]]]] <- source
  if (identical(project$active_data_source_id, source_id)) {
    root_indices <- which(vapply(project$modules %||% list(), function(module) identical(module$module_id, "r.ggplot2.ggplot"), logical(1)))
    if (length(root_indices)) project$modules[[root_indices[[1]]]]$arguments$data <- bp_argument("explicit", bp_symbol(new_name), "formal")
    project$data_reference$symbol <- new_name
  }
  project
}

bp_remove_data_source <- function(project, source_id) {
  if (identical(source_id, "dataset_example")) stop("The built-in example data source cannot be removed.", call. = FALSE)
  if (identical(project$active_data_source_id, source_id)) stop("Map another data source to the plot before removing the active source.", call. = FALSE)
  project <- unserialize(serialize(project, NULL))
  keep <- !vapply(project$data_sources %||% list(), function(source) identical(source$id, source_id), logical(1))
  if (all(keep)) stop("Unknown data source: ", source_id, call. = FALSE)
  project$data_sources <- project$data_sources[keep]
  project
}

bp_column_type <- function(column) {
  if (inherits(column, "POSIXt")) return("datetime")
  if (inherits(column, "Date")) return("date")
  if (is.factor(column)) return("factor")
  if (is.logical(column)) return("logical")
  if (is.integer(column)) return("integer")
  if (is.numeric(column)) return("numeric")
  if (is.character(column)) return("character")
  class(column)[[1]] %||% typeof(column)
}

bp_column_valid_for <- function(type, unique_count, row_count) {
  if (type %in% c("numeric", "integer")) return(c("x", "y", "size", "alpha", "continuousColor"))
  if (type %in% c("factor", "logical") || (type == "character" && unique_count <= min(50L, max(12L, ceiling(sqrt(max(row_count, 1L))))))) {
    return(c("x", "color", "fill", "shape", "group", "facet", "label"))
  }
  c("x", "label")
}

bp_profile_column <- function(column, name, row_count) {
  type <- bp_column_type(column)
  missing <- sum(is.na(column))
  finite_values <- if (is.numeric(column)) column[is.finite(column)] else NULL
  unique_count <- length(unique(column[!is.na(column)]))
  numeric_candidate <- FALSE
  if (identical(type, "character")) {
    present <- trimws(column[!is.na(column)])
    numeric_candidate <- length(present) > 0L && all(!is.na(suppressWarnings(as.numeric(present))))
  }
  flags <- character()
  if (missing == row_count) flags <- c(flags, "empty")
  if (row_count > 0L && missing / row_count >= 0.5) flags <- c(flags, "high_missing")
  if (unique_count <= 1L && missing < row_count) flags <- c(flags, "constant")
  if (identical(type, "character") && row_count > 20L && unique_count / max(1L, row_count - missing) >= 0.8) flags <- c(flags, "high_unique_text")
  if (numeric_candidate) flags <- c(flags, "numeric_text_candidate")
  normalized <- tolower(gsub("[^a-z0-9]", "", name))
  p_value_candidate <- normalized %in% c("p", "pvalue", "pval", "padj", "fdr", "qvalue", "adjpval", "adjustedpvalue")
  out_of_range <- if (p_value_candidate && is.numeric(column)) sum(column < 0 | column > 1, na.rm = TRUE) else 0L
  list(
    name = name,
    storage_type = typeof(column),
    detected_type = type,
    recommended_type = if (numeric_candidate) "numeric" else type,
    missing_count = missing,
    unique_count = unique_count,
    infinite_count = if (is.numeric(column)) sum(is.infinite(column), na.rm = TRUE) else 0L,
    nan_count = if (is.numeric(column)) sum(is.nan(column), na.rm = TRUE) else 0L,
    minimum = if (length(finite_values)) min(finite_values) else NULL,
    maximum = if (length(finite_values)) max(finite_values) else NULL,
    valid_for = bp_column_valid_for(type, unique_count, row_count),
    flags = flags,
    p_value_out_of_range = out_of_range,
    inference = paste("R class", paste(class(column), collapse = "/"), "and storage type", typeof(column))
  )
}

bp_profile_dataset <- function(data) {
  stopifnot(is.data.frame(data))
  rows <- nrow(data)
  names_original <- names(data)
  columns <- lapply(seq_along(data), function(index) bp_profile_column(data[[index]], names_original[[index]], rows))
  types <- vapply(columns, `[[`, character(1), "detected_type")
  duplicate_rows <- tryCatch(sum(duplicated(data)), error = function(error) NA_integer_)
  duplicate_names <- unique(names_original[duplicated(names_original)])
  warnings <- list()
  add_warning <- function(code, message, level = "warning") {
    warnings[[length(warnings) + 1L]] <<- list(code = code, level = level, message = message)
  }
  missing_total <- sum(vapply(columns, `[[`, numeric(1), "missing_count"))
  if (missing_total) add_warning("missing_values", paste(format(missing_total, big.mark = ","), "missing values were preserved."), "info")
  if (length(duplicate_names)) add_warning("duplicate_column_names", paste("Duplicate column names:", paste(duplicate_names, collapse = ", ")))
  if (is.finite(duplicate_rows) && duplicate_rows > 0L) add_warning("duplicate_rows", paste(format(duplicate_rows, big.mark = ","), "duplicate rows were detected and preserved."), "info")
  if (any(!nzchar(trimws(names_original)))) add_warning("empty_column_names", "One or more column names are empty.")
  for (column in columns) {
    if (column$infinite_count > 0L) add_warning("infinite_values", paste(column$name, "contains", column$infinite_count, "infinite values."))
    if (column$nan_count > 0L) add_warning("nan_values", paste(column$name, "contains", column$nan_count, "NaN values."))
    if (column$p_value_out_of_range > 0L) add_warning("p_value_range", paste(column$name, "contains", column$p_value_out_of_range, "values outside [0, 1]."))
    if ("constant" %in% column$flags) add_warning("constant_column", paste(column$name, "is constant."), "info")
    if ("high_missing" %in% column$flags) add_warning("high_missing_column", paste(column$name, "has at least 50% missing values."))
  }
  list(
    object_type = "data.frame",
    rows = rows,
    columns = ncol(data),
    numeric_columns = sum(types %in% c("numeric", "integer")),
    categorical_columns = sum(types %in% c("factor", "logical") | vapply(columns, function(column) "color" %in% column$valid_for, logical(1))),
    missing_values = missing_total,
    duplicate_rows = duplicate_rows,
    duplicate_column_names = duplicate_names,
    column_metadata = columns,
    warnings = warnings
  )
}

bp_convert_column <- function(column, target) {
  current <- bp_column_type(column)
  if (identical(current, target)) return(column)
  switch(
    target,
    character = as.character(column),
    factor = factor(column),
    logical = {
      normalized <- toupper(trimws(as.character(column)))
      if (any(!is.na(column) & !normalized %in% c("TRUE", "FALSE", "T", "F", "1", "0"))) stop("Values cannot all be converted to logical.", call. = FALSE)
      value <- normalized %in% c("TRUE", "T", "1")
      value[is.na(column)] <- NA
      value
    },
    integer = {
      value <- suppressWarnings(as.integer(as.character(column)))
      if (any(!is.na(column) & is.na(value))) stop("Values cannot all be converted to integer.", call. = FALSE)
      value
    },
    numeric = {
      value <- suppressWarnings(as.numeric(as.character(column)))
      if (any(!is.na(column) & is.na(value))) stop("Values cannot all be converted to numeric.", call. = FALSE)
      value
    },
    date = {
      value <- as.Date(as.character(column))
      if (any(!is.na(column) & is.na(value))) stop("Values cannot all be converted to Date.", call. = FALSE)
      value
    },
    datetime = {
      value <- as.POSIXct(as.character(column), tz = "UTC")
      if (any(!is.na(column) & is.na(value))) stop("Values cannot all be converted to date-time.", call. = FALSE)
      value
    },
    stop("Unsupported target column type: ", target, call. = FALSE)
  )
}

bp_symbol_source_name <- function(name) {
  if (grepl("^[.A-Za-z][.A-Za-z0-9_]*$", name) && !name %in% c("if", "else", "repeat", "while", "function", "for", "in", "next", "break", "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA")) return(name)
  paste(deparse(as.name(name), width.cutoff = 500L, backtick = TRUE), collapse = "")
}

bp_data_source_columns <- function(source, data = NULL) {
  if (is.data.frame(data)) return(names(data))
  if (isTRUE(source$example)) return(names(bp_default_environment()$df))
  metadata <- source$column_metadata %||% list()
  columns <- vapply(metadata, function(column) column$name %||% "", character(1))
  columns[nzchar(columns)]
}

bp_mapping_column_reference <- function(value) {
  if (is.null(value)) return(list(direct = FALSE, column = NULL))
  type <- bp_value_type(value)
  if (identical(type, "RSymbol")) return(list(direct = TRUE, column = value$name))
  if (!identical(type, "RRawExpression")) return(list(direct = FALSE, column = NULL))
  parsed <- tryCatch(parse(text = value$source %||% "", keep.source = FALSE), error = function(error) expression())
  if (length(parsed) == 1L && is.symbol(parsed[[1]])) {
    return(list(direct = TRUE, column = as.character(parsed[[1]])))
  }
  list(direct = FALSE, column = NULL)
}

bp_mapping_argument_sources <- function(argument) {
  value <- argument$value
  if (is.null(value) || !identical(bp_value_type(value), "RAesMapping")) return(list())
  lapply(value$mappings %||% list(), function(mapping_value) {
    reference <- bp_mapping_column_reference(mapping_value)
    if (isTRUE(reference$direct)) reference$column else bp_value_to_source(mapping_value)
  })
}

bp_sanitize_mapping_argument <- function(argument, columns, instance) {
  value <- argument$value
  if (is.null(value) || !identical(bp_value_type(value), "RAesMapping")) {
    return(list(argument = argument, preserved = list(), cleared = list()))
  }
  mappings <- value$mappings %||% list()
  kept <- list()
  preserved <- list()
  cleared <- list()
  for (key in names(mappings)) {
    mapping_value <- mappings[[key]]
    reference <- bp_mapping_column_reference(mapping_value)
    detail <- list(
      instance_id = instance$instance_id,
      module_id = instance$module_id,
      key = key,
      column = reference$column,
      source = bp_value_to_source(mapping_value),
      direct = isTRUE(reference$direct)
    )
    if (isTRUE(reference$direct) && !reference$column %in% columns) {
      cleared[[length(cleared) + 1L]] <- detail
    } else {
      kept[[key]] <- mapping_value
      preserved[[length(preserved) + 1L]] <- detail
    }
  }
  argument$value <- bp_aes_mapping(kept)
  if (length(mappings) && !length(kept) && !argument$state %in% c("unset", "inherited")) {
    argument$state <- "explicit"
  }
  list(argument = argument, preserved = preserved, cleared = cleared)
}

bp_switch_project_data_source <- function(project, source, data = NULL, mapping_override = NULL) {
  if (identical(source$status, "relink_required") || isTRUE(source$relink_required)) {
    stop("This data source must be relinked before it can be used in the plot.", call. = FALSE)
  }
  project <- unserialize(serialize(project, NULL))
  root_indices <- which(vapply(project$modules %||% list(), function(module) identical(module$module_id, "r.ggplot2.ggplot"), logical(1)))
  if (!length(root_indices)) stop("The project needs a ggplot() root module before data can be mapped.", call. = FALSE)
  root_index <- root_indices[[1]]
  root <- project$modules[[root_index]]
  root$arguments$data <- bp_argument("explicit", bp_symbol(source$name), "formal")
  project$modules[[root_index]] <- root

  if (!is.null(mapping_override)) {
    selected <- mapping_override[nzchar(unlist(mapping_override, use.names = FALSE))]
    aes_values <- lapply(selected, function(column) bp_symbol(as.character(column)))
    project$modules[[root_index]]$arguments$mapping <- bp_argument("explicit", bp_aes_mapping(aes_values), "formal")
  }

  columns <- bp_data_source_columns(source, data)
  preserved <- list()
  cleared <- list()
  for (index in seq_along(project$modules)) {
    instance <- project$modules[[index]]
    argument <- instance$arguments$mapping
    if (is.null(argument)) next
    sanitized <- bp_sanitize_mapping_argument(argument, columns, instance)
    instance$arguments$mapping <- sanitized$argument
    project$modules[[index]] <- instance
    preserved <- c(preserved, sanitized$preserved)
    cleared <- c(cleared, sanitized$cleared)
  }

  project$data_sources <- project$data_sources %||% list()
  existing <- which(vapply(project$data_sources, function(item) identical(item$id, source$id), logical(1)))
  if (length(existing)) project$data_sources[[existing[[1]]]] <- source else project$data_sources[[length(project$data_sources) + 1L]] <- source
  project$active_data_source_id <- source$id
  project$data_reference <- list(
    strategy = if (isTRUE(source$example)) "local_environment" else "registered_data_source",
    source_id = source$id, symbol = source$name, embedded = FALSE
  )
  root <- project$modules[[root_index]]
  project$mapping_config <- list(
    dataset_id = source$id,
    plot_id = root$instance_id,
    mapping = bp_mapping_argument_sources(root$arguments$mapping),
    confirmed_by_user = TRUE
  )
  list(
    project = project,
    source = source,
    columns = columns,
    preserved = preserved,
    cleared = cleared,
    preserved_count = length(preserved),
    cleared_count = length(cleared),
    root_instance_id = root$instance_id
  )
}

bp_apply_dataset_mapping <- function(project, source, mapping) {
  bp_switch_project_data_source(project, source, mapping_override = mapping)$project
}

bp_runtime_dataset_values <- function(project, data_objects) {
  sources <- project$data_sources %||% list()
  values <- list()
  for (source in sources) {
    object <- data_objects[[source$id]]
    if (!is.null(object) && nzchar(source$name %||% "")) values[[source$name]] <- object
  }
  values
}

bp_active_data_column_suggestions <- function(project, data_objects = list()) {
  active_id <- project$active_data_source_id %||% "dataset_example"
  sources <- Filter(function(source) identical(source$id, active_id), project$data_sources %||% list())
  source <- if (length(sources)) sources[[1]] else if (identical(active_id, "dataset_example")) bp_example_data_source() else NULL
  data <- data_objects[[active_id]]
  if (is.null(data) && isTRUE(source$example)) data <- bp_default_environment()$df

  if (is.data.frame(data)) {
    column_names <- names(data)
    column_types <- vapply(data, bp_column_type, character(1))
  } else {
    metadata <- source$column_metadata %||% list()
    column_names <- vapply(metadata, function(column) column$name %||% "", character(1))
    column_types <- vapply(metadata, function(column) column$recommended_type %||% column$detected_type %||% "column", character(1))
  }
  keep <- nzchar(column_names)
  column_names <- column_names[keep]
  column_types <- column_types[keep]
  if (!length(column_names)) return(character())
  values <- vapply(column_names, bp_symbol_source_name, character(1))
  stats::setNames(values, paste0(column_names, " · ", column_types))
}

bp_data_source_reference_suggestions <- function(project) {
  sources <- project$data_sources %||% list()
  if (!length(sources)) return(character())
  values <- vapply(sources, function(source) bp_symbol_source_name(source$name %||% ""), character(1))
  labels <- vapply(sources, function(source) paste0(
    source$name %||% "data", " · ", toupper(source$source_type %||% "data"),
    if (identical(source$status, "relink_required")) " · relink required" else " · ready"
  ), character(1))
  keep <- nzchar(values)
  stats::setNames(values[keep], labels[keep])
}

bp_mark_data_sources_for_relink <- function(project) {
  project$data_sources <- lapply(project$data_sources %||% list(), function(source) {
    if (!isTRUE(source$example) && !isTRUE(source$derived)) {
      source$status <- "relink_required"
      source$relink_required <- TRUE
    } else if (isTRUE(source$derived)) {
      source$status <- "derived_stale"
      source$relink_required <- FALSE
    }
    source
  })
  project
}

bp_data_source_setup_line <- function(source) {
  source_type <- tolower(source$source_type %||% "")
  symbol <- bp_symbol_source_name(source$name)
  file_name <- encodeString(source$original_file_name, quote = '"')
  conversion <- source$conversion %||% list()
  conversion_lines <- function() {
    lines <- character()
    if ((conversion$from %||% source$original_object_type %||% "data.frame") %in% c("matrix", "list_table", "tibble")) {
      lines <- c(lines, paste0(symbol, " <- as.data.frame(", symbol, ", stringsAsFactors = FALSE, optional = TRUE)"))
    }
    if (identical(conversion$row_names, "column")) {
      row_name <- encodeString(conversion$row_name_column %||% "RowName", quote = '"')
      lines <- c(lines, paste0(symbol, " <- cbind(setNames(data.frame(row.names(", symbol, "), check.names = FALSE), ", row_name, "), ", symbol, ")"), paste0("row.names(", symbol, ") <- NULL"))
    } else if (identical(conversion$row_names, "ignore")) {
      lines <- c(lines, paste0("row.names(", symbol, ") <- NULL"))
    }
    lines
  }
  if (identical(source_type, "rds")) {
    return(c(paste0(symbol, " <- readRDS(", file_name, ")"), conversion_lines()))
  }
  if (source_type %in% c("rdata", "rda")) {
    environment_name <- paste0(".bioplotblocks_data_", gsub("[^A-Za-z0-9_]", "_", source$id %||% "source"))
    return(c(
      paste0(environment_name, " <- new.env(parent = emptyenv())"),
      paste0("load(", file_name, ", envir = ", environment_name, ")"),
      paste0(symbol, " <- get(", encodeString(source$object_name %||% source$name, quote = '"'), ", envir = ", environment_name, ", inherits = FALSE)"),
      paste0("rm(", environment_name, ")"),
      conversion_lines()
    ))
  }
  options <- source$parse_options %||% list()
  na_values <- options$na_values %||% c("", "NA", "N/A", "null", "NULL")
  na_source <- paste0("c(", paste(vapply(na_values, encodeString, character(1), quote = '"'), collapse = ", "), ")")
  paste0(
    symbol, " <- read.table(",
    file_name,
    ", header = ", if (isTRUE(options$header)) "TRUE" else "FALSE",
    ", sep = ", encodeString(options$delimiter %||% if (identical(source$source_type, "csv")) "," else "\t", quote = '"'),
    ", quote = ", encodeString(options$quote %||% '"', quote = '"'),
    ", dec = ", encodeString(options$decimal %||% ".", quote = '"'),
    ", na.strings = ", na_source,
    ", skip = ", as.integer(options$skip %||% 0L),
    ", fileEncoding = ", encodeString(options$encoding %||% "UTF-8", quote = '"'),
    ", check.names = FALSE, stringsAsFactors = FALSE, comment.char = \"\")"
  )
}
