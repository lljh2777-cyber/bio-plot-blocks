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

bp_apply_dataset_mapping <- function(project, source, mapping) {
  project <- unserialize(serialize(project, NULL))
  root_index <- which(vapply(project$modules %||% list(), function(module) identical(module$module_id, "r.ggplot2.ggplot"), logical(1)))[1]
  if (is.na(root_index)) stop("The project needs a ggplot() root module before data can be mapped.", call. = FALSE)
  root <- project$modules[[root_index]]
  root$arguments$data <- bp_argument("explicit", bp_symbol(source$name), "formal")
  selected <- mapping[nzchar(unlist(mapping, use.names = FALSE))]
  aes_values <- lapply(selected, function(column) bp_symbol(column))
  root$arguments$mapping <- bp_argument("explicit", bp_aes_mapping(aes_values), "formal")
  project$modules[[root_index]] <- root
  for (index in seq_along(project$modules)) {
    if (index == root_index) next
    if (!is.null(project$modules[[index]]$arguments$mapping)) {
      project$modules[[index]]$arguments$mapping <- bp_argument("unset", origin = "formal")
    }
  }
  project$data_sources <- project$data_sources %||% list()
  existing <- which(vapply(project$data_sources, function(item) identical(item$id, source$id), logical(1)))
  if (length(existing)) project$data_sources[[existing[[1]]]] <- source else project$data_sources[[length(project$data_sources) + 1L]] <- source
  project$active_data_source_id <- source$id
  project$data_reference <- list(strategy = "registered_data_source", source_id = source$id, symbol = source$name, embedded = FALSE)
  project$mapping_config <- list(dataset_id = source$id, plot_id = root$instance_id, mapping = mapping, confirmed_by_user = TRUE)
  project
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

bp_mark_data_sources_for_relink <- function(project) {
  project$data_sources <- lapply(project$data_sources %||% list(), function(source) {
    if (!isTRUE(source$example)) {
      source$status <- "relink_required"
      source$relink_required <- TRUE
    }
    source
  })
  project
}

bp_data_source_setup_line <- function(source) {
  options <- source$parse_options %||% list()
  na_values <- options$na_values %||% c("", "NA", "N/A", "null", "NULL")
  na_source <- paste0("c(", paste(vapply(na_values, encodeString, character(1), quote = '"'), collapse = ", "), ")")
  paste0(
    bp_symbol_source_name(source$name), " <- read.table(",
    encodeString(source$original_file_name, quote = '"'),
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
