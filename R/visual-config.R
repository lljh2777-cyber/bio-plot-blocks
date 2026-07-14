# Visual scatter configuration ------------------------------------------------

bp_visual_scatter_defaults <- function(project = NULL) {
  list(
    chart_type = "scatter",
    data_source_id = project$active_data_source_id %||% "dataset_example",
    x_field = "",
    y_field = "",
    color_field = "",
    size_field = "",
    label_field = "",
    point_color = "#2C7FB8",
    point_size = 2,
    alpha = 0.72,
    shape = "16",
    palette = "default",
    trend_line = "none",
    title = "",
    x_label = "",
    y_label = "",
    legend_title = "",
    x_scale = "linear",
    y_scale = "linear",
    theme = "classic",
    base_size = 12,
    vertical_reference_lines = "",
    horizontal_reference_lines = "",
    reference_line_color = "#6B7280",
    reference_line_width = 0.6,
    advanced_preserved = FALSE
  )
}

bp_visual_volcano_defaults <- function(project = NULL) {
  defaults <- bp_visual_scatter_defaults(project)
  defaults$chart_type <- "volcano"
  defaults$point_color <- "#AEB7C4"
  defaults$point_size <- 1.8
  defaults$alpha <- 0.75
  defaults$palette <- "blue_red"
  defaults$trend_line <- "none"
  defaults$title <- "Volcano plot"
  defaults$x_label <- "log2 Fold Change"
  defaults$y_label <- "-log10 adjusted p-value"
  defaults$legend_title <- "Regulation"
  defaults$fold_change_cutoff <- 1
  defaults$significance_cutoff <- 0.05
  defaults$auto_status <- TRUE
  defaults
}

bp_visual_boxplot_defaults <- function(project = NULL) {
  defaults <- bp_visual_scatter_defaults(project)
  defaults$chart_type <- "boxplot"
  defaults$point_color <- "#7DB3D7"
  defaults$point_size <- 0.65
  defaults$alpha <- 0.85
  defaults$palette <- "default"
  defaults$trend_line <- "none"
  defaults$title <- "Boxplot"
  defaults$x_label <- "Group"
  defaults$y_label <- "Value"
  defaults$legend_title <- "Group"
  defaults$box_border_color <- "#334155"
  defaults$box_show_outliers <- TRUE
  defaults$box_outlier_restore <- TRUE
  defaults$box_outlier_size <- 1.5
  defaults$box_jitter <- FALSE
  defaults$box_jitter_color <- "#334155"
  defaults$box_jitter_size <- 1.4
  defaults$box_jitter_alpha <- 0.55
  defaults$box_jitter_width <- 0.16
  defaults
}

bp_visual_chart_defaults <- function(chart_type, project = NULL) {
  switch(
    chart_type,
    volcano = bp_visual_volcano_defaults(project),
    boxplot = bp_visual_boxplot_defaults(project),
    pca = bp_pca_defaults(project),
    bp_visual_scatter_defaults(project)
  )
}

bp_visual_repair_cross_chart_labels <- function(config, chart_type, project = NULL) {
  chart_type <- if (chart_type %in% c("scatter", "volcano", "boxplot", "pca")) chart_type else "scatter"
  config <- config %||% bp_visual_chart_defaults(chart_type, project)
  fields <- c("title", "x_label", "y_label", "legend_title")
  signature <- function(value) {
    vapply(fields, function(field) bp_visual_scalar_character(value[[field]], ""), character(1))
  }
  current <- signature(config)
  other_types <- setdiff(c("scatter", "volcano", "boxplot", "pca"), chart_type)
  inherited <- any(vapply(other_types, function(other_type) {
    other <- signature(bp_visual_chart_defaults(other_type, project))
    any(nzchar(other)) && identical(current, other)
  }, logical(1)))
  if (!inherited) return(config)
  defaults <- bp_visual_chart_defaults(chart_type, project)
  for (field in fields) config[[field]] <- defaults[[field]]
  config
}

bp_visual_scalar_character <- function(value, default = "") {
  if (is.null(value) || !length(value) || is.na(value[[1]])) return(default)
  as.character(value[[1]])
}

bp_visual_scalar_number <- function(value, default, minimum = -Inf, maximum = Inf) {
  value <- suppressWarnings(as.numeric(value %||% default))
  if (!length(value) || is.na(value[[1]]) || !is.finite(value[[1]])) value <- default
  min(max(value[[1]], minimum), maximum)
}

bp_visual_parse_reference_values <- function(value) {
  source <- trimws(bp_visual_scalar_character(value, ""))
  if (!nzchar(source)) return(list(valid = TRUE, values = numeric(), invalid = character()))
  source <- gsub("，", ",", source, fixed = TRUE)
  tokens <- unlist(strsplit(source, "[,;[:space:]]+", perl = TRUE), use.names = FALSE)
  tokens <- tokens[nzchar(tokens)]
  values <- suppressWarnings(as.numeric(tokens))
  invalid <- tokens[is.na(values) | !is.finite(values)]
  list(valid = !length(invalid), values = values[!is.na(values) & is.finite(values)], invalid = invalid)
}

bp_visual_reference_values_source <- function(values) {
  sources <- vapply(values, bp_visual_number_source, character(1))
  if (length(sources) == 1L) sources[[1]] else paste0("c(", paste(sources, collapse = ", "), ")")
}

bp_normalize_visual_scatter_config <- function(config, project = NULL) {
  defaults <- bp_visual_scatter_defaults(project)
  config <- utils::modifyList(defaults, config %||% list(), keep.null = TRUE)
  character_fields <- c(
    "chart_type", "data_source_id", "x_field", "y_field", "color_field",
    "size_field", "label_field", "point_color", "shape", "palette",
    "trend_line", "title", "x_label", "y_label", "legend_title",
    "x_scale", "y_scale", "theme", "vertical_reference_lines",
    "horizontal_reference_lines", "reference_line_color"
  )
  for (name in character_fields) config[[name]] <- bp_visual_scalar_character(config[[name]], defaults[[name]])
  if (!grepl("^#[0-9A-Fa-f]{6}$", config$point_color)) config$point_color <- defaults$point_color
  config$point_size <- bp_visual_scalar_number(config$point_size, defaults$point_size, 0.1, 20)
  config$alpha <- bp_visual_scalar_number(config$alpha, defaults$alpha, 0, 1)
  config$base_size <- bp_visual_scalar_number(config$base_size, defaults$base_size, 6, 40)
  config$reference_line_width <- bp_visual_scalar_number(config$reference_line_width, defaults$reference_line_width, 0.1, 10)
  if (!grepl("^#[0-9A-Fa-f]{6}$", config$reference_line_color)) config$reference_line_color <- defaults$reference_line_color
  if (!config$shape %in% as.character(c(0:25))) config$shape <- defaults$shape
  if (!config$palette %in% c("default", "blue_red", "viridis_like")) config$palette <- defaults$palette
  if (!config$trend_line %in% c("none", "linear", "smooth")) config$trend_line <- defaults$trend_line
  if (!config$theme %in% c("classic", "minimal", "bw")) config$theme <- defaults$theme
  if (!config$x_scale %in% c("linear", "log10", "neg_log10")) config$x_scale <- defaults$x_scale
  if (!config$y_scale %in% c("linear", "log10", "neg_log10")) config$y_scale <- defaults$y_scale
  config$chart_type <- "scatter"
  config$advanced_preserved <- isTRUE(config$advanced_preserved)
  config
}

bp_normalize_visual_volcano_config <- function(config, project = NULL) {
  defaults <- bp_visual_volcano_defaults(project)
  config <- bp_normalize_visual_scatter_config(utils::modifyList(defaults, config %||% list(), keep.null = TRUE), project)
  config$chart_type <- "volcano"
  config$trend_line <- "none"
  config$fold_change_cutoff <- bp_visual_scalar_number(config$fold_change_cutoff, defaults$fold_change_cutoff, 0, 1000)
  config$significance_cutoff <- bp_visual_scalar_number(config$significance_cutoff, defaults$significance_cutoff, .Machine$double.eps, 1)
  config$auto_status <- if (is.null(config$auto_status)) TRUE else isTRUE(config$auto_status)
  config
}

bp_normalize_visual_boxplot_config <- function(config, project = NULL) {
  defaults <- bp_visual_boxplot_defaults(project)
  config <- bp_normalize_visual_scatter_config(utils::modifyList(defaults, config %||% list(), keep.null = TRUE), project)
  config$chart_type <- "boxplot"
  config$x_scale <- "linear"
  config$size_field <- ""
  config$label_field <- ""
  config$trend_line <- "none"
  config$point_size <- bp_visual_scalar_number(config$point_size, defaults$point_size, 0.1, 2)
  config$box_border_color <- bp_visual_scalar_character(config$box_border_color, defaults$box_border_color)
  if (!grepl("^#[0-9A-Fa-f]{6}$", config$box_border_color)) config$box_border_color <- defaults$box_border_color
  config$box_show_outliers <- if (is.null(config$box_show_outliers)) TRUE else isTRUE(config$box_show_outliers)
  config$box_outlier_restore <- if (is.null(config$box_outlier_restore)) {
    config$box_show_outliers
  } else {
    isTRUE(config$box_outlier_restore)
  }
  config$box_outlier_size <- bp_visual_scalar_number(config$box_outlier_size, defaults$box_outlier_size, 0.1, 10)
  config$box_jitter <- isTRUE(config$box_jitter)
  config$box_jitter_color <- bp_visual_scalar_character(config$box_jitter_color, defaults$box_jitter_color)
  if (!grepl("^#[0-9A-Fa-f]{6}$", config$box_jitter_color)) config$box_jitter_color <- defaults$box_jitter_color
  config$box_jitter_size <- bp_visual_scalar_number(config$box_jitter_size, defaults$box_jitter_size, 0.1, 10)
  config$box_jitter_alpha <- bp_visual_scalar_number(config$box_jitter_alpha, defaults$box_jitter_alpha, 0, 1)
  config$box_jitter_width <- bp_visual_scalar_number(config$box_jitter_width, defaults$box_jitter_width, 0, 1)
  config
}

bp_visual_active_source <- function(project) {
  active_id <- project$active_data_source_id %||% "dataset_example"
  sources <- Filter(function(source) identical(source$id, active_id), project$data_sources %||% list())
  if (length(sources)) sources[[1]] else if (identical(active_id, "dataset_example")) bp_example_data_source() else NULL
}

bp_visual_column_profile <- function(source, data = NULL) {
  columns <- bp_data_source_columns(source, data)
  if (!length(columns)) return(data.frame(name = character(), type = character(), unique = integer(), stringsAsFactors = FALSE))
  if (is.data.frame(data)) {
    type <- vapply(data, bp_column_type, character(1))
    unique <- vapply(data, function(column) length(unique(column[!is.na(column)])), integer(1))
  } else {
    metadata <- source$column_metadata %||% list()
    metadata_names <- vapply(metadata, function(column) column$name %||% "", character(1))
    type_lookup <- stats::setNames(vapply(metadata, function(column) {
      column$recommended_type %||% column$detected_type %||% "column"
    }, character(1)), metadata_names)
    unique_lookup <- stats::setNames(vapply(metadata, function(column) {
      as.integer(column$unique_values %||% column$unique_count %||% NA_integer_)
    }, integer(1)), metadata_names)
    type <- unname(type_lookup[columns])
    unique <- unname(unique_lookup[columns])
    type[is.na(type)] <- "column"
  }
  data.frame(name = columns, type = type, unique = unique, stringsAsFactors = FALSE)
}

bp_visual_recommend_scatter_fields <- function(source, data = NULL) {
  profile <- bp_visual_column_profile(source, data)
  columns <- profile$name
  if (!length(columns)) return(list(x_field = "", y_field = "", color_field = "", size_field = "", label_field = ""))
  lower <- tolower(columns)
  match_name <- function(candidates) {
    index <- match(tolower(candidates), lower, nomatch = 0L)
    index <- index[index > 0L]
    if (length(index)) columns[[index[[1]]]] else ""
  }
  numeric_columns <- profile$name[tolower(profile$type) %in% c("numeric", "double", "integer", "number")]
  pairs <- list(c("PC1", "PC2"), c("log2FC", "neg_log10_padj"), c("x", "y"))
  chosen_pair <- NULL
  for (pair in pairs) {
    candidate <- vapply(pair, function(name) match_name(name), character(1))
    if (all(nzchar(candidate))) {
      chosen_pair <- candidate
      break
    }
  }
  if (is.null(chosen_pair)) chosen_pair <- c(numeric_columns, "", "")[1:2]
  color <- match_name(c("status", "regulated", "group", "condition", "treatment", "cluster"))
  if (!nzchar(color)) {
    categorical <- profile$name[tolower(profile$type) %in% c("character", "factor", "logical", "ordered")]
    if (length(categorical)) {
      counts <- profile$unique[match(categorical, profile$name)]
      suitable <- categorical[is.na(counts) | (counts >= 2L & counts <= 20L)]
      color <- if (length(suitable)) suitable[[1]] else ""
    }
  }
  label <- match_name(c("SYMBOL", "gene", "gene_name", "ENSEMBL", "feature", "id"))
  list(
    x_field = chosen_pair[[1]] %||% "",
    y_field = chosen_pair[[2]] %||% "",
    color_field = color,
    size_field = "",
    label_field = label
  )
}

bp_visual_recommend_volcano_fields <- function(source, data = NULL) {
  profile <- bp_visual_column_profile(source, data)
  columns <- profile$name
  empty <- list(
    x_field = "", y_field = "", color_field = "", size_field = "",
    label_field = "", status_field = "", x_scale = "linear", y_scale = "neg_log10", available = FALSE
  )
  if (!length(columns)) return(empty)
  lower <- tolower(columns)
  numeric <- tolower(profile$type) %in% c("numeric", "double", "integer", "number")
  match_numeric <- function(candidates, pattern = NULL) {
    indices <- match(tolower(candidates), lower, nomatch = 0L)
    indices <- indices[indices > 0L]
    indices <- indices[numeric[indices]]
    if (!length(indices) && !is.null(pattern)) indices <- which(numeric & grepl(pattern, lower, perl = TRUE))
    if (length(indices)) columns[[indices[[1]]]] else ""
  }
  match_any <- function(candidates) {
    indices <- match(tolower(candidates), lower, nomatch = 0L)
    indices <- indices[indices > 0L]
    if (length(indices)) columns[[indices[[1]]]] else ""
  }

  x <- match_numeric(
    c("log2FC", "logFC", "avg_log2FC", "log_fold_change", "fold_change", "effect", "estimate"),
    "(^|[._])log2?fc$|fold[._]?change|effect[._]?size"
  )
  y <- match_numeric(
    c("neg_log10_padj", "minus_log10_padj", "padj", "FDR", "qvalue", "adj.P.Val", "PValue", "pvalue", "p_val"),
    "neg.*log.*p|minus.*log.*p|padj|fdr|q[._]?value|adj.*p|(^|[._])p[._]?val(ue)?$"
  )
  status <- match_any(c("status", "regulated", "regulation", "direction", "significance"))
  label <- match_any(c("SYMBOL", "gene", "gene_name", "ENSEMBL", "feature", "id"))
  y_lower <- tolower(y)
  transformed <- nzchar(y) && grepl("neg.*log|minus.*log|-log|log10", y_lower, perl = TRUE)
  list(
    x_field = x,
    y_field = y,
    color_field = "",
    size_field = "",
    label_field = label,
    status_field = status,
    x_scale = "linear",
    y_scale = if (transformed) "linear" else "neg_log10",
    available = nzchar(x) && nzchar(y)
  )
}

bp_visual_recommend_boxplot_fields <- function(source, data = NULL) {
  profile <- bp_visual_column_profile(source, data)
  columns <- profile$name
  empty <- list(
    x_field = "", y_field = "", color_field = "", size_field = "", label_field = "",
    x_scale = "linear", y_scale = "linear", available = FALSE
  )
  if (!length(columns)) return(empty)
  lower <- tolower(columns)
  types <- tolower(profile$type)
  numeric <- types %in% c("numeric", "double", "integer", "number")
  categorical <- types %in% c("character", "factor", "logical", "ordered")
  match_column <- function(candidates, eligible = rep(TRUE, length(columns))) {
    indices <- match(tolower(candidates), lower, nomatch = 0L)
    indices <- indices[indices > 0L]
    indices <- indices[eligible[indices]]
    if (length(indices)) columns[[indices[[1]]]] else ""
  }
  group <- match_column(c("group", "condition", "treatment", "status", "class", "category", "cluster"), categorical)
  if (!nzchar(group)) {
    suitable <- which(categorical & (is.na(profile$unique) | (profile$unique >= 2L & profile$unique <= 30L)))
    if (length(suitable)) group <- columns[[suitable[[1]]]]
  }
  value <- match_column(c("value", "expression", "abundance", "intensity", "score", "measurement"), numeric)
  if (!nzchar(value)) {
    candidates <- which(numeric & columns != group)
    if (length(candidates)) value <- columns[[candidates[[1]]]]
  }
  list(
    x_field = group,
    y_field = value,
    color_field = group,
    size_field = "",
    label_field = "",
    x_scale = "linear",
    y_scale = "linear",
    available = nzchar(group) && nzchar(value)
  )
}

bp_visual_argument_value <- function(instance, name) {
  argument <- instance$arguments[[name]]
  if (bp_is_unset(argument)) NULL else argument$value
}

bp_visual_argument_character <- function(instance, name, default = "") {
  value <- bp_visual_argument_value(instance, name)
  if (is.null(value) || !identical(bp_value_type(value), "RCharacter")) return(default)
  bp_visual_scalar_character(value$value, default)
}

bp_visual_argument_number <- function(instance, name, default) {
  value <- bp_visual_argument_value(instance, name)
  if (is.null(value) || !bp_value_type(value) %in% c("RDouble", "RInteger")) return(default)
  bp_visual_scalar_number(value$value, default)
}

bp_visual_mapping_field <- function(value) {
  reference <- bp_mapping_column_reference(value)
  if (isTRUE(reference$direct)) return(list(field = reference$column, scale = "linear", supported = TRUE))
  if (is.null(value) || !identical(bp_value_type(value), "RRawExpression")) {
    return(list(field = "", scale = "linear", supported = FALSE))
  }
  parsed <- tryCatch(parse(text = value$source %||% "", keep.source = FALSE), error = function(error) expression())
  if (length(parsed) != 1L) return(list(field = "", scale = "linear", supported = FALSE))
  expression <- parsed[[1]]
  if (is.call(expression) && identical(as.character(expression[[1]]), "log10") && length(expression) == 2L && is.symbol(expression[[2]])) {
    return(list(field = as.character(expression[[2]]), scale = "log10", supported = TRUE))
  }
  if (is.call(expression) && identical(as.character(expression[[1]]), "-") && length(expression) == 2L) {
    inner <- expression[[2]]
    if (is.call(inner) && identical(as.character(inner[[1]]), "log10") && length(inner) == 2L && is.symbol(inner[[2]])) {
      return(list(field = as.character(inner[[2]]), scale = "neg_log10", supported = TRUE))
    }
  }
  list(field = "", scale = "linear", supported = FALSE)
}

bp_visual_set_text_argument <- function(instance, name, value, origin = NULL) {
  current <- instance$arguments[[name]] %||% bp_argument(origin = origin %||% "formal")
  if (!nzchar(trimws(value %||% ""))) {
    current$state <- "unset"
  } else {
    current$state <- "explicit"
    current$value <- bp_character(value)
  }
  instance$arguments[[name]] <- current
  instance
}

bp_visual_set_numeric_argument <- function(instance, name, value, origin = NULL) {
  current <- instance$arguments[[name]] %||% bp_argument(origin = origin %||% "formal")
  current$state <- "explicit"
  current$value <- bp_double(value)
  instance$arguments[[name]] <- current
  instance
}

bp_visual_module_set_arguments <- function(instance) {
  names(Filter(function(argument) !bp_is_unset(argument), instance$arguments %||% list()))
}

bp_visual_first_instance <- function(project, module_ids, managed_first = TRUE) {
  indices <- which(vapply(project$modules %||% list(), function(instance) instance$module_id %in% module_ids, logical(1)))
  if (!length(indices)) return(NA_integer_)
  if (isTRUE(managed_first)) {
    managed <- indices[vapply(project$modules[indices], function(instance) isTRUE(instance$visual_managed), logical(1))]
    if (length(managed)) return(managed[[length(managed)]])
  }
  indices[[1]]
}

bp_visual_scatter_config_from_project <- function(project) {
  stored <- project$visual_config$scatter %||% list()
  config <- bp_normalize_visual_scatter_config(stored, project)
  config$data_source_id <- project$active_data_source_id %||% config$data_source_id
  advanced <- FALSE
  volcano_managed <- identical(project$visual_config$active_chart_type %||% "scatter", "volcano")

  root_index <- bp_visual_first_instance(project, "r.ggplot2.ggplot")
  if (!is.na(root_index)) {
    root <- project$modules[[root_index]]
    mapping <- bp_visual_argument_value(root, "mapping")
    if (!is.null(mapping) && identical(bp_value_type(mapping), "RAesMapping")) {
      mappings <- mapping$mappings %||% list()
      x <- bp_visual_mapping_field(mappings$x)
      y <- bp_visual_mapping_field(mappings$y)
      if (is.null(mappings$x)) config$x_field <- ""
      if (is.null(mappings$y)) config$y_field <- ""
      if (isTRUE(x$supported)) {
        config$x_field <- x$field
        config$x_scale <- x$scale
      } else if (!is.null(mappings$x)) advanced <- TRUE
      if (isTRUE(y$supported)) {
        config$y_field <- y$field
        config$y_scale <- y$scale
      } else if (!is.null(mappings$y)) advanced <- TRUE
      for (name in c("color", "size", "label")) {
        value <- mappings[[name]]
        if (is.null(value) && identical(name, "color")) value <- mappings$colour
        detail <- bp_visual_mapping_field(value)
        if (is.null(value)) config[[paste0(name, "_field")]] <- ""
        else if (isTRUE(detail$supported) && identical(detail$scale, "linear")) config[[paste0(name, "_field")]] <- detail$field
        else if (!(isTRUE(volcano_managed) && identical(name, "color"))) advanced <- TRUE
      }
      if (length(setdiff(names(mappings), c("x", "y", "color", "colour", "size", "label")))) advanced <- TRUE
    } else if (!is.null(mapping)) advanced <- TRUE
    if (length(setdiff(bp_visual_module_set_arguments(root), c("data", "mapping")))) advanced <- TRUE
  }

  point_indices <- which(vapply(project$modules %||% list(), function(instance) identical(instance$module_id, "r.ggplot2.geom_point"), logical(1)))
  if (length(point_indices)) {
    point <- project$modules[[point_indices[[1]]]]
    local_mapping <- bp_visual_argument_value(point, "mapping")
    if (!is.null(local_mapping) && identical(bp_value_type(local_mapping), "RAesMapping")) {
      local_mappings <- local_mapping$mappings %||% list()
      for (name in c("color", "size", "label")) {
        target <- paste0(name, "_field")
        value <- local_mappings[[name]]
        if (is.null(value) && identical(name, "color")) value <- local_mappings$colour
        detail <- bp_visual_mapping_field(value)
        if (!nzchar(config[[target]]) && isTRUE(detail$supported) && identical(detail$scale, "linear")) {
          config[[target]] <- detail$field
        } else if (!is.null(value) && !isTRUE(detail$supported)) advanced <- TRUE
      }
      if (length(setdiff(names(local_mappings), c("color", "colour", "size", "label")))) advanced <- TRUE
    } else if (!is.null(local_mapping)) advanced <- TRUE
    config$point_color <- bp_visual_argument_character(point, "color", config$point_color)
    config$point_size <- bp_visual_argument_number(point, "size", config$point_size)
    config$alpha <- bp_visual_argument_number(point, "alpha", config$alpha)
    shape <- bp_visual_argument_value(point, "shape")
    if (!is.null(shape) && bp_value_type(shape) %in% c("RDouble", "RInteger", "RCharacter")) {
      config$shape <- as.character(shape$value)
    }
    if (length(setdiff(bp_visual_module_set_arguments(point), c("mapping", "color", "size", "alpha", "shape")))) advanced <- TRUE
    if (length(point_indices) > 1L) advanced <- TRUE
  }

  config$title <- ""
  config$x_label <- ""
  config$y_label <- ""
  config$legend_title <- ""
  labs_index <- bp_visual_first_instance(project, "r.ggplot2.labs")
  if (!is.na(labs_index)) {
    labels <- project$modules[[labs_index]]
    config$title <- bp_visual_argument_character(labels, "title", "")
    config$x_label <- bp_visual_argument_character(labels, "x", "")
    config$y_label <- bp_visual_argument_character(labels, "y", "")
    config$legend_title <- bp_visual_argument_character(labels, "color", "")
    if (length(setdiff(bp_visual_module_set_arguments(labels), c("title", "x", "y", "color")))) advanced <- TRUE
  }

  theme_ids <- c(classic = "r.ggplot2.theme_classic", minimal = "r.ggplot2.theme_minimal", bw = "r.ggplot2.theme_bw")
  theme_index <- bp_visual_first_instance(project, unname(theme_ids))
  if (!is.na(theme_index)) {
    theme <- project$modules[[theme_index]]
    config$theme <- names(theme_ids)[match(theme$module_id, theme_ids)]
    config$base_size <- bp_visual_argument_number(theme, "base_size", config$base_size)
    if (length(setdiff(bp_visual_module_set_arguments(theme), "base_size"))) advanced <- TRUE
  }

  config$trend_line <- "none"
  smooth <- Filter(function(instance) identical(instance$module_id, "r.ggplot2.geom_smooth") && isTRUE(instance$visual_managed), project$modules %||% list())
  if (length(smooth)) {
    method <- bp_visual_argument_character(smooth[[length(smooth)]], "method", "loess")
    config$trend_line <- if (identical(method, "lm")) "linear" else "smooth"
  }
  config$palette <- "default"
  palette <- Filter(function(instance) identical(instance$module_id, "r.ggplot2.scale_color_manual") && isTRUE(instance$visual_managed), project$modules %||% list())
  if (length(palette)) config$palette <- palette[[length(palette)]]$visual_palette %||% config$palette

  controlled_ids <- c("r.ggplot2.ggplot", "r.ggplot2.geom_point", "r.ggplot2.labs", unname(theme_ids))
  for (instance in project$modules %||% list()) {
    if (!instance$module_id %in% controlled_ids && !isTRUE(instance$visual_managed)) advanced <- TRUE
  }
  config$advanced_preserved <- isTRUE(advanced)
  bp_normalize_visual_scatter_config(config, project)
}

bp_visual_mapping_value <- function(field, scale = "linear") {
  if (!nzchar(field %||% "")) return(NULL)
  symbol <- bp_symbol_source_name(field)
  switch(
    scale,
    log10 = bp_raw_expression(paste0("log10(", symbol, ")")),
    neg_log10 = bp_raw_expression(paste0("-log10(", symbol, ")")),
    bp_symbol(field)
  )
}

bp_visual_palette_source <- function(palette) {
  switch(
    palette,
    blue_red = 'c(Down = "#2C7FB8", NS = "grey70", Up = "#D73027")',
    viridis_like = 'c("#440154", "#3B528B", "#21918C", "#5EC962", "#FDE725")',
    NULL
  )
}

bp_visual_apply_reference_layer <- function(project, config, registry, module_id, role, intercept_name, values_source) {
  parsed <- bp_visual_parse_reference_values(values_source)
  if (!isTRUE(parsed$valid)) return(project)
  indices <- which(vapply(project$modules %||% list(), function(instance) {
    identical(instance$module_id, module_id) && identical(instance$visual_role %||% "", role)
  }, logical(1)))
  layer <- if (length(indices)) project$modules[[indices[[1]]]] else bp_instantiate_module(module_id, registry)
  if (length(indices)) project$modules <- project$modules[-indices]
  if (!length(parsed$values)) return(project)
  layer$visual_managed <- TRUE
  layer$visual_role <- role
  layer$arguments[[intercept_name]] <- bp_argument(
    "raw_expression", bp_raw_expression(bp_visual_reference_values_source(parsed$values)), "formal"
  )
  layer$arguments$color <- bp_argument("explicit", bp_character(config$reference_line_color), "dots_aesthetic")
  layer$arguments$linetype <- bp_argument("explicit", bp_character("dashed"), "dots_aesthetic")
  layer$arguments$linewidth <- bp_argument("explicit", bp_double(config$reference_line_width), "dots_aesthetic")
  project$modules[[length(project$modules) + 1L]] <- layer
  project
}

bp_visual_apply_reference_lines <- function(project, config, registry) {
  project <- bp_visual_apply_reference_layer(
    project, config, registry, "r.ggplot2.geom_vline", "visual_vertical_reference_lines",
    "xintercept", config$vertical_reference_lines
  )
  bp_visual_apply_reference_layer(
    project, config, registry, "r.ggplot2.geom_hline", "visual_horizontal_reference_lines",
    "yintercept", config$horizontal_reference_lines
  )
}

bp_apply_visual_scatter_config <- function(project, config, registry = NULL) {
  registry <- registry %||% bp_load_registry()
  project <- unserialize(serialize(project, NULL))
  config <- bp_normalize_visual_scatter_config(config, project)
  project$modules <- Filter(function(instance) {
    !isTRUE(instance$visual_managed) || !instance$visual_role %in% c(
      "volcano_fc_threshold", "volcano_significance_threshold",
      "visual_boxplot_layer", "visual_boxplot_jitter", "visual_boxplot_fill_scale",
      "visual_pca_ellipse"
    )
  }, project$modules %||% list())
  modules <- project$modules %||% list()

  root_index <- bp_visual_first_instance(project, "r.ggplot2.ggplot")
  if (is.na(root_index)) {
    root <- bp_instantiate_module("r.ggplot2.ggplot", registry)
    modules <- c(list(root), modules)
    project$modules <- modules
    root_index <- 1L
  }
  root <- project$modules[[root_index]]
  source <- Filter(function(item) identical(item$id, config$data_source_id), project$data_sources %||% list())
  if (length(source)) {
    root$arguments$data <- bp_argument("explicit", bp_symbol(source[[1]]$name), "formal")
    project$active_data_source_id <- source[[1]]$id
    project$data_reference <- list(
      strategy = if (isTRUE(source[[1]]$example)) "local_environment" else "registered_data_source",
      source_id = source[[1]]$id, symbol = source[[1]]$name, embedded = FALSE
    )
  }
  mapping_argument <- root$arguments$mapping %||% bp_argument(origin = "formal")
  mapping <- mapping_argument$value
  if (is.null(mapping) || !identical(bp_value_type(mapping), "RAesMapping")) mapping <- bp_aes_mapping()
  mappings <- mapping$mappings %||% list()
  mappings$colour <- NULL
  if (identical(bp_visual_chart_type(project), "boxplot")) mappings$fill <- NULL
  mappings$x <- bp_visual_mapping_value(config$x_field, config$x_scale)
  mappings$y <- bp_visual_mapping_value(config$y_field, config$y_scale)
  mappings$color <- bp_visual_mapping_value(config$color_field)
  mappings$size <- bp_visual_mapping_value(config$size_field)
  mappings$label <- bp_visual_mapping_value(config$label_field)
  mapping_argument$state <- "explicit"
  mapping_argument$value <- bp_aes_mapping(mappings)
  root$arguments$mapping <- mapping_argument
  root$visual_managed <- TRUE
  project$modules[[root_index]] <- root

  point_index <- bp_visual_first_instance(project, "r.ggplot2.geom_point", managed_first = FALSE)
  if (is.na(point_index)) {
    point <- bp_instantiate_module("r.ggplot2.geom_point", registry)
    project$modules <- append(project$modules, list(point), after = root_index)
    point_index <- root_index + 1L
  }
  point <- project$modules[[point_index]]
  point$visual_managed <- TRUE
  local_mapping_argument <- point$arguments$mapping
  if (!is.null(local_mapping_argument) && !bp_is_unset(local_mapping_argument) &&
      identical(bp_value_type(local_mapping_argument$value), "RAesMapping")) {
    local_mappings <- local_mapping_argument$value$mappings %||% list()
    for (name in c("color", "colour", "size", "label")) local_mappings[[name]] <- NULL
    local_mapping_argument$value <- bp_aes_mapping(local_mappings)
    if (!length(local_mappings)) local_mapping_argument$state <- "unset"
    point$arguments$mapping <- local_mapping_argument
  }
  if (nzchar(config$color_field)) {
    point$arguments$color$state <- "unset"
  } else {
    point$arguments$color <- bp_argument("explicit", bp_character(config$point_color), "dots_aesthetic")
  }
  if (nzchar(config$size_field)) {
    point$arguments$size$state <- "unset"
  } else {
    point$arguments$size <- bp_argument("explicit", bp_double(config$point_size), "dots_aesthetic")
  }
  point$arguments$alpha <- bp_argument("explicit", bp_double(config$alpha), "dots_aesthetic")
  point$arguments$shape <- bp_argument("explicit", bp_double(as.numeric(config$shape)), "dots_aesthetic")
  project$modules[[point_index]] <- point
  project <- bp_visual_apply_reference_lines(project, config, registry)

  label_indices <- which(vapply(project$modules, function(instance) {
    identical(instance$module_id, "r.ggplot2.geom_text") && isTRUE(instance$visual_managed)
  }, logical(1)))
  if (nzchar(config$label_field)) {
    if (length(label_indices)) {
      label_index <- label_indices[[length(label_indices)]]
      label_layer <- project$modules[[label_index]]
    } else {
      label_layer <- bp_instantiate_module("r.ggplot2.geom_text", registry)
      label_layer$visual_managed <- TRUE
      project$modules <- append(project$modules, list(label_layer), after = point_index)
      label_index <- point_index + 1L
    }
    label_layer$arguments$vjust <- bp_argument("explicit", bp_double(-0.6), "dots_aesthetic")
    label_layer$arguments$size <- bp_argument("explicit", bp_double(3), "dots_aesthetic")
    label_layer$arguments$check_overlap <- bp_argument("explicit", bp_logical(TRUE), "formal")
    project$modules[[label_index]] <- label_layer
  } else if (length(label_indices)) {
    project$modules <- project$modules[-label_indices]
  }

  smooth_indices <- which(vapply(project$modules, function(instance) {
    identical(instance$module_id, "r.ggplot2.geom_smooth") && isTRUE(instance$visual_managed)
  }, logical(1)))
  if (identical(config$trend_line, "none")) {
    if (length(smooth_indices)) project$modules <- project$modules[-smooth_indices]
  } else {
    if (length(smooth_indices)) {
      smooth_index <- smooth_indices[[length(smooth_indices)]]
      smooth <- project$modules[[smooth_index]]
    } else {
      smooth <- bp_instantiate_module("r.ggplot2.geom_smooth", registry)
      smooth$visual_managed <- TRUE
      project$modules <- append(project$modules, list(smooth), after = bp_visual_first_instance(project, "r.ggplot2.geom_point", managed_first = FALSE))
      smooth_index <- bp_visual_first_instance(project, "r.ggplot2.geom_smooth")
    }
    smooth$arguments$method <- bp_argument("explicit", bp_character(if (identical(config$trend_line, "linear")) "lm" else "loess"), "formal")
    smooth$arguments$se <- bp_argument("explicit", bp_logical(FALSE), "formal")
    smooth$arguments$color <- bp_argument("explicit", bp_character("#35445D"), "dots_aesthetic")
    smooth$arguments$linewidth <- bp_argument("explicit", bp_double(0.8), "dots_aesthetic")
    project$modules[[smooth_index]] <- smooth
  }

  labs_index <- bp_visual_first_instance(project, "r.ggplot2.labs")
  if (is.na(labs_index)) {
    labels <- bp_instantiate_module("r.ggplot2.labs", registry)
    labels$visual_managed <- TRUE
    project$modules[[length(project$modules) + 1L]] <- labels
    labs_index <- length(project$modules)
  }
  labels <- project$modules[[labs_index]]
  labels$visual_managed <- TRUE
  labels <- bp_visual_set_text_argument(labels, "title", config$title, "formal")
  labels <- bp_visual_set_text_argument(labels, "x", config$x_label, "dots_documented")
  labels <- bp_visual_set_text_argument(labels, "y", config$y_label, "dots_documented")
  labels <- bp_visual_set_text_argument(labels, "color", if (nzchar(config$color_field)) config$legend_title else "", "dots_documented")
  project$modules[[labs_index]] <- labels

  palette_indices <- which(vapply(project$modules, function(instance) {
    identical(instance$module_id, "r.ggplot2.scale_color_manual") && isTRUE(instance$visual_managed)
  }, logical(1)))
  palette_source <- if (nzchar(config$color_field)) bp_visual_palette_source(config$palette) else NULL
  if (is.null(palette_source)) {
    if (length(palette_indices)) project$modules <- project$modules[-palette_indices]
  } else {
    if (length(palette_indices)) {
      palette_index <- palette_indices[[length(palette_indices)]]
      scale <- project$modules[[palette_index]]
    } else {
      scale <- bp_instantiate_module("r.ggplot2.scale_color_manual", registry)
      scale$visual_managed <- TRUE
      project$modules[[length(project$modules) + 1L]] <- scale
      palette_index <- length(project$modules)
    }
    scale$arguments$values <- bp_argument("raw_expression", bp_raw_expression(palette_source), "formal")
    scale$visual_palette <- config$palette
    project$modules[[palette_index]] <- scale
  }

  theme_ids <- c(classic = "r.ggplot2.theme_classic", minimal = "r.ggplot2.theme_minimal", bw = "r.ggplot2.theme_bw")
  theme_indices <- which(vapply(project$modules, function(instance) {
    instance$module_id %in% theme_ids && isTRUE(instance$visual_managed)
  }, logical(1)))
  if (!length(theme_indices)) {
    reusable <- which(vapply(project$modules, function(instance) {
      instance$module_id %in% theme_ids && !length(setdiff(bp_visual_module_set_arguments(instance), "base_size"))
    }, logical(1)))
    theme_index <- if (length(reusable)) reusable[[length(reusable)]] else NA_integer_
  } else theme_index <- theme_indices[[length(theme_indices)]]
  target_theme_id <- unname(theme_ids[[config$theme]])
  if (is.na(theme_index)) {
    theme <- bp_instantiate_module(target_theme_id, registry)
    theme$visual_managed <- TRUE
    project$modules[[length(project$modules) + 1L]] <- theme
    theme_index <- length(project$modules)
  } else if (!identical(project$modules[[theme_index]]$module_id, target_theme_id)) {
    instance_id <- project$modules[[theme_index]]$instance_id
    theme <- bp_instantiate_module(target_theme_id, registry, instance_id = instance_id)
    theme$visual_managed <- TRUE
    project$modules[[theme_index]] <- theme
  }
  theme <- project$modules[[theme_index]]
  theme$visual_managed <- TRUE
  theme$arguments$base_size <- bp_argument("explicit", bp_double(config$base_size), "formal")
  project$modules[[theme_index]] <- theme
  if (theme_index < length(project$modules)) {
    theme <- project$modules[[theme_index]]
    project$modules <- append(project$modules[-theme_index], list(theme), after = length(project$modules) - 1L)
  }

  root_index <- bp_visual_first_instance(project, "r.ggplot2.ggplot")
  root <- project$modules[[root_index]]
  project$mapping_config <- list(
    dataset_id = project$active_data_source_id %||% config$data_source_id,
    plot_id = root$instance_id,
    mapping = bp_mapping_argument_sources(root$arguments$mapping),
    confirmed_by_user = TRUE
  )
  config$advanced_preserved <- bp_visual_scatter_config_from_project(project)$advanced_preserved
  project$visual_config <- project$visual_config %||% list()
  project$visual_config$scatter <- bp_normalize_visual_scatter_config(config, project)
  project$visual_config$active_chart_type <- "scatter"
  list(project = project, root_instance_id = root$instance_id, config = project$visual_config$scatter)
}

bp_validate_visual_scatter_config <- function(config, columns = character()) {
  raw_config <- config %||% list()
  vertical <- bp_visual_parse_reference_values(raw_config$vertical_reference_lines %||% "")
  horizontal <- bp_visual_parse_reference_values(raw_config$horizontal_reference_lines %||% "")
  reference_color <- trimws(bp_visual_scalar_character(raw_config$reference_line_color, "#6B7280"))
  config <- bp_normalize_visual_scatter_config(config)
  errors <- character()
  if (!nzchar(config$x_field)) errors <- c(errors, "请选择 X 轴字段。")
  if (!nzchar(config$y_field)) errors <- c(errors, "请选择 Y 轴字段。")
  if (length(columns)) {
    for (name in c("x_field", "y_field", "color_field", "size_field", "label_field")) {
      value <- config[[name]]
      if (nzchar(value) && !value %in% columns) errors <- c(errors, paste0("字段 ‘", value, "’ 不在当前数据源中。"))
    }
  }
  if (!isTRUE(vertical$valid)) errors <- c(errors, paste0("纵向虚线位置包含无效数值：", paste(vertical$invalid, collapse = "、"), "。"))
  if (!isTRUE(horizontal$valid)) errors <- c(errors, paste0("横向虚线位置包含无效数值：", paste(horizontal$invalid, collapse = "、"), "。"))
  if (!grepl("^#[0-9A-Fa-f]{6}$", reference_color)) errors <- c(errors, "虚线颜色需使用 6 位十六进制颜色，例如 #6B7280。")
  list(valid = !length(errors), errors = unique(errors))
}

bp_visual_chart_type <- function(project) {
  chart_type <- project$visual_config$active_chart_type %||% "scatter"
  if (chart_type %in% c("scatter", "volcano", "boxplot", "pca")) chart_type else "scatter"
}

bp_visual_volcano_field_is_transformed <- function(field) {
  nzchar(field %||% "") && grepl("neg.*log|minus.*log|-log|log10", tolower(field), perl = TRUE)
}

bp_visual_number_source <- function(value) {
  format(as.numeric(value), digits = 15L, scientific = FALSE, trim = TRUE)
}

bp_visual_volcano_significance_source <- function(config) {
  field <- bp_symbol_source_name(config$y_field)
  cutoff <- bp_visual_number_source(config$significance_cutoff)
  if (identical(config$y_scale, "linear") && bp_visual_volcano_field_is_transformed(config$y_field)) {
    paste0(field, " >= -log10(", cutoff, ")")
  } else {
    paste0(field, " <= ", cutoff)
  }
}

bp_visual_volcano_status_source <- function(config) {
  x <- bp_symbol_source_name(config$x_field)
  cutoff <- bp_visual_number_source(config$fold_change_cutoff)
  significant <- bp_visual_volcano_significance_source(config)
  paste0(
    "ifelse(", x, " >= ", cutoff, " & ", significant, ", \"Up\", ",
    "ifelse(", x, " <= -", cutoff, " & ", significant, ", \"Down\", \"NS\"))"
  )
}

bp_visual_volcano_y_threshold_source <- function(config) {
  cutoff <- bp_visual_number_source(config$significance_cutoff)
  if (identical(config$y_scale, "neg_log10") ||
      (identical(config$y_scale, "linear") && bp_visual_volcano_field_is_transformed(config$y_field))) {
    return(paste0("-log10(", cutoff, ")"))
  }
  if (identical(config$y_scale, "log10")) return(paste0("log10(", cutoff, ")"))
  cutoff
}

bp_visual_volcano_config_from_project <- function(project) {
  stored <- bp_normalize_visual_volcano_config(project$visual_config$volcano %||% list(), project)
  probe <- unserialize(serialize(project, NULL))
  probe$visual_config <- probe$visual_config %||% list()
  probe$visual_config$scatter <- stored
  probe$visual_config$active_chart_type <- "volcano"
  shared <- bp_visual_scatter_config_from_project(probe)
  shared$chart_type <- "volcano"
  shared$fold_change_cutoff <- stored$fold_change_cutoff
  shared$significance_cutoff <- stored$significance_cutoff
  shared$auto_status <- stored$auto_status
  bp_normalize_visual_volcano_config(shared, project)
}

bp_visual_boxplot_config_from_project <- function(project) {
  config <- bp_normalize_visual_boxplot_config(project$visual_config$boxplot %||% list(), project)
  config$data_source_id <- project$active_data_source_id %||% config$data_source_id
  advanced <- FALSE

  root_index <- bp_visual_first_instance(project, "r.ggplot2.ggplot")
  if (!is.na(root_index)) {
    root <- project$modules[[root_index]]
    mapping <- bp_visual_argument_value(root, "mapping")
    if (!is.null(mapping) && identical(bp_value_type(mapping), "RAesMapping")) {
      mappings <- mapping$mappings %||% list()
      x <- bp_visual_mapping_field(mappings$x)
      y <- bp_visual_mapping_field(mappings$y)
      fill <- bp_visual_mapping_field(mappings$fill)
      config$x_field <- if (isTRUE(x$supported)) x$field else ""
      config$y_field <- if (isTRUE(y$supported)) y$field else ""
      config$y_scale <- if (isTRUE(y$supported)) y$scale else config$y_scale
      config$color_field <- if (isTRUE(fill$supported) && identical(fill$scale, "linear")) fill$field else ""
      if (length(setdiff(names(mappings), c("x", "y", "fill")))) advanced <- TRUE
    } else if (!is.null(mapping)) advanced <- TRUE
    if (length(setdiff(bp_visual_module_set_arguments(root), c("data", "mapping")))) advanced <- TRUE
  }

  box_indices <- which(vapply(project$modules %||% list(), function(instance) {
    identical(instance$module_id, "r.ggplot2.geom_boxplot") && identical(instance$visual_role %||% "", "visual_boxplot_layer")
  }, logical(1)))
  if (length(box_indices)) {
    box <- project$modules[[box_indices[[length(box_indices)]]]]
    config$point_color <- bp_visual_argument_character(box, "fill", config$point_color)
    config$box_border_color <- bp_visual_argument_character(box, "color", config$box_border_color)
    config$point_size <- bp_visual_argument_number(box, "width", config$point_size)
    config$alpha <- bp_visual_argument_number(box, "alpha", config$alpha)
    config$box_outlier_size <- bp_visual_argument_number(box, "outlier.size", config$box_outlier_size)
    outlier_shape <- bp_visual_argument_value(box, "outlier.shape")
    config$box_show_outliers <- is.null(outlier_shape) || !identical(bp_value_type(outlier_shape), "RNA")
    allowed <- c("fill", "color", "width", "alpha", "outlier.shape", "outlier.size")
    if (length(setdiff(bp_visual_module_set_arguments(box), allowed))) advanced <- TRUE
  }

  jitter_indices <- which(vapply(project$modules %||% list(), function(instance) {
    identical(instance$module_id, "r.ggplot2.geom_jitter") && identical(instance$visual_role %||% "", "visual_boxplot_jitter")
  }, logical(1)))
  config$box_jitter <- length(jitter_indices) > 0L
  if (length(jitter_indices)) {
    jitter <- project$modules[[jitter_indices[[length(jitter_indices)]]]]
    config$box_jitter_color <- bp_visual_argument_character(jitter, "color", config$box_jitter_color)
    config$box_jitter_size <- bp_visual_argument_number(jitter, "size", config$box_jitter_size)
    config$box_jitter_alpha <- bp_visual_argument_number(jitter, "alpha", config$box_jitter_alpha)
    config$box_jitter_width <- bp_visual_argument_number(jitter, "width", config$box_jitter_width)
    allowed <- c("width", "height", "color", "size", "alpha", "shape")
    if (length(setdiff(bp_visual_module_set_arguments(jitter), allowed))) advanced <- TRUE
  }

  labs_index <- bp_visual_first_instance(project, "r.ggplot2.labs")
  if (!is.na(labs_index)) {
    labels <- project$modules[[labs_index]]
    config$title <- bp_visual_argument_character(labels, "title", config$title)
    config$x_label <- bp_visual_argument_character(labels, "x", config$x_label)
    config$y_label <- bp_visual_argument_character(labels, "y", config$y_label)
    config$legend_title <- bp_visual_argument_character(labels, "fill", config$legend_title)
  }

  theme_ids <- c(classic = "r.ggplot2.theme_classic", minimal = "r.ggplot2.theme_minimal", bw = "r.ggplot2.theme_bw")
  theme_index <- bp_visual_first_instance(project, unname(theme_ids))
  if (!is.na(theme_index)) {
    theme <- project$modules[[theme_index]]
    config$theme <- names(theme_ids)[match(theme$module_id, theme_ids)]
    config$base_size <- bp_visual_argument_number(theme, "base_size", config$base_size)
  }

  fill_scale <- Filter(function(instance) {
    identical(instance$module_id, "r.ggplot2.scale_fill_manual") && identical(instance$visual_role %||% "", "visual_boxplot_fill_scale")
  }, project$modules %||% list())
  config$palette <- if (length(fill_scale)) fill_scale[[length(fill_scale)]]$visual_palette %||% "default" else "default"
  controlled_ids <- c(
    "r.ggplot2.ggplot", "r.ggplot2.geom_boxplot", "r.ggplot2.geom_jitter",
    "r.ggplot2.labs", unname(theme_ids), "r.ggplot2.scale_fill_manual"
  )
  for (instance in project$modules %||% list()) {
    if (!instance$module_id %in% controlled_ids && !isTRUE(instance$visual_managed)) advanced <- TRUE
  }
  config$advanced_preserved <- isTRUE(advanced)
  bp_normalize_visual_boxplot_config(config, project)
}

bp_visual_config_from_project <- function(project) {
  if (identical(bp_visual_chart_type(project), "volcano")) {
    bp_visual_volcano_config_from_project(project)
  } else if (identical(bp_visual_chart_type(project), "boxplot")) {
    bp_visual_boxplot_config_from_project(project)
  } else if (identical(bp_visual_chart_type(project), "pca")) {
    bp_pca_config_from_project(project)
  } else {
    bp_visual_scatter_config_from_project(project)
  }
}

bp_visual_remove_boxplot_group_mappings <- function(project) {
  project <- unserialize(serialize(project, NULL))
  changed <- FALSE
  target_indices <- which(vapply(project$modules %||% list(), function(instance) {
    identical(instance$module_id, "r.ggplot2.ggplot") ||
      (identical(instance$visual_role %||% "", "visual_boxplot_layer") &&
        identical(instance$module_id, "r.ggplot2.geom_boxplot")) ||
      (identical(instance$visual_role %||% "", "visual_boxplot_jitter") &&
        identical(instance$module_id, "r.ggplot2.geom_jitter"))
  }, logical(1)))

  for (index in target_indices) {
    instance <- project$modules[[index]]
    argument <- instance$arguments$mapping %||% NULL
    mapping <- argument$value %||% NULL
    if (is.null(argument) || is.null(mapping) || !identical(bp_value_type(mapping), "RAesMapping")) next
    mappings <- mapping$mappings %||% list()
    if (is.null(mappings$group)) next
    mappings$group <- NULL
    argument$value <- bp_aes_mapping(mappings)
    instance$arguments$mapping <- argument
    project$modules[[index]] <- instance
    changed <- TRUE
  }

  if (isTRUE(changed)) {
    root_index <- bp_visual_first_instance(project, "r.ggplot2.ggplot")
    if (!is.na(root_index)) {
      root <- project$modules[[root_index]]
      project$mapping_config <- project$mapping_config %||% list()
      project$mapping_config$plot_id <- root$instance_id
      project$mapping_config$mapping <- bp_mapping_argument_sources(root$arguments$mapping)
    }
  }
  list(project = project, changed = changed)
}

bp_visual_remove_automatic_volcano_lines <- function(project) {
  legacy_template <- identical(project$template_provenance$id %||% "", "bio.volcano.basic")
  project$modules <- Filter(function(instance) {
    role <- instance$visual_role %||% ""
    if (role %in% c("volcano_fc_threshold", "volcano_significance_threshold")) return(FALSE)
    if (!legacy_template || isTRUE(instance$visual_managed)) return(TRUE)
    if (identical(instance$module_id, "r.ggplot2.geom_vline")) {
      return(!identical(instance$arguments$xintercept$value$source %||% "", "c(-1, 1)"))
    }
    if (identical(instance$module_id, "r.ggplot2.geom_hline")) {
      return(!identical(instance$arguments$yintercept$value$source %||% "", "-log10(0.05)"))
    }
    TRUE
  }, project$modules %||% list())
  project
}

bp_visual_prepare_volcano_template <- function(project) {
  project <- bp_visual_remove_automatic_volcano_lines(project)
  if (!identical(project$template_provenance$id %||% "", "bio.volcano.basic")) return(project)
  project$modules <- lapply(project$modules %||% list(), function(instance) {
    if (identical(instance$module_id, "r.ggplot2.scale_color_manual")) {
      instance$visual_managed <- TRUE
      instance$visual_palette <- "blue_red"
    }
    instance
  })
  project
}

bp_apply_visual_volcano_config <- function(project, config, registry = NULL) {
  registry <- registry %||% bp_load_registry()
  project <- unserialize(serialize(project, NULL))
  config <- bp_normalize_visual_volcano_config(config, project)
  stored_scatter <- project$visual_config$scatter %||% bp_visual_scatter_defaults(project)
  project <- bp_visual_prepare_volcano_template(project)

  scatter_config <- config
  scatter_config$chart_type <- "scatter"
  scatter_config$trend_line <- "none"
  scatter_result <- bp_apply_visual_scatter_config(project, scatter_config, registry)
  project <- scatter_result$project
  project$visual_config$scatter <- stored_scatter

  root_index <- bp_visual_first_instance(project, "r.ggplot2.ggplot")
  root <- project$modules[[root_index]]
  mapping_argument <- root$arguments$mapping %||% bp_argument(origin = "formal")
  mapping <- mapping_argument$value
  if (is.null(mapping) || !identical(bp_value_type(mapping), "RAesMapping")) mapping <- bp_aes_mapping()
  mappings <- mapping$mappings %||% list()
  computed_status <- !nzchar(config$color_field) && isTRUE(config$auto_status) && nzchar(config$x_field) && nzchar(config$y_field)
  if (computed_status) {
    mappings$color <- bp_raw_expression(bp_visual_volcano_status_source(config))
    point_index <- bp_visual_first_instance(project, "r.ggplot2.geom_point", managed_first = FALSE)
    if (!is.na(point_index)) project$modules[[point_index]]$arguments$color$state <- "unset"
  }
  mapping_argument$state <- "explicit"
  mapping_argument$value <- bp_aes_mapping(mappings)
  root$arguments$mapping <- mapping_argument
  project$modules[[root_index]] <- root

  has_color_mapping <- nzchar(config$color_field) || computed_status
  if (has_color_mapping && !nzchar(trimws(config$legend_title %||% ""))) {
    config$legend_title <- if (computed_status) "Regulation" else config$color_field
  }
  labs_index <- bp_visual_first_instance(project, "r.ggplot2.labs")
  if (!is.na(labs_index)) {
    labels <- project$modules[[labs_index]]
    labels <- bp_visual_set_text_argument(labels, "color", if (has_color_mapping) config$legend_title else "", "dots_documented")
    project$modules[[labs_index]] <- labels
  }

  if (has_color_mapping) {
    scale_indices <- which(vapply(project$modules, function(instance) {
      identical(instance$module_id, "r.ggplot2.scale_color_manual") && isTRUE(instance$visual_managed)
    }, logical(1)))
    if (length(scale_indices)) {
      scale_index <- scale_indices[[length(scale_indices)]]
      scale <- project$modules[[scale_index]]
    } else {
      scale <- bp_instantiate_module("r.ggplot2.scale_color_manual", registry)
      scale_index <- length(project$modules) + 1L
    }
    palette <- if (identical(config$palette, "default")) "blue_red" else config$palette
    scale$arguments$values <- bp_argument("raw_expression", bp_raw_expression(bp_visual_palette_source(palette)), "formal")
    scale$visual_managed <- TRUE
    scale$visual_palette <- palette
    project$modules[[scale_index]] <- scale
  }

  root_index <- bp_visual_first_instance(project, "r.ggplot2.ggplot")
  root <- project$modules[[root_index]]
  project$mapping_config <- list(
    dataset_id = project$active_data_source_id %||% config$data_source_id,
    plot_id = root$instance_id,
    mapping = bp_mapping_argument_sources(root$arguments$mapping),
    confirmed_by_user = TRUE
  )
  project$visual_config <- project$visual_config %||% list()
  project$visual_config$active_chart_type <- "volcano"
  project$visual_config$volcano <- config
  config <- bp_visual_volcano_config_from_project(project)
  project$visual_config$volcano <- config
  list(project = project, root_instance_id = root$instance_id, config = config)
}

bp_validate_visual_volcano_config <- function(config, columns = character()) {
  config <- bp_normalize_visual_volcano_config(config)
  validation <- bp_validate_visual_scatter_config(config, columns)
  errors <- validation$errors
  if (config$fold_change_cutoff < 0) errors <- c(errors, "倍数变化阈值不能小于 0。")
  if (config$significance_cutoff <= 0 || config$significance_cutoff > 1) {
    errors <- c(errors, "显著性阈值必须大于 0 且不超过 1。")
  }
  list(valid = !length(errors), errors = unique(errors))
}

bp_visual_box_palette_source <- function(palette) {
  switch(
    palette,
    blue_red = 'c("#2C7FB8", "grey70", "#D73027")',
    viridis_like = 'c("#440154", "#3B528B", "#21918C", "#5EC962", "#FDE725")',
    NULL
  )
}

bp_apply_visual_boxplot_config <- function(project, config, registry = NULL) {
  registry <- registry %||% bp_load_registry()
  project <- unserialize(serialize(project, NULL))
  config <- bp_normalize_visual_boxplot_config(config, project)
  stored_scatter <- project$visual_config$scatter %||% bp_visual_scatter_defaults(project)

  shared <- config
  shared$chart_type <- "scatter"
  shared$color_field <- ""
  shared$size_field <- ""
  shared$label_field <- ""
  shared$trend_line <- "none"
  shared$x_scale <- "linear"
  scatter_result <- bp_apply_visual_scatter_config(project, shared, registry)
  project <- scatter_result$project
  project$visual_config$scatter <- stored_scatter

  project$modules <- Filter(function(instance) {
    !(isTRUE(instance$visual_managed) && instance$module_id %in% c(
      "r.ggplot2.geom_point", "r.ggplot2.geom_text", "r.ggplot2.geom_smooth", "r.ggplot2.scale_color_manual"
    ))
  }, project$modules %||% list())

  root_index <- bp_visual_first_instance(project, "r.ggplot2.ggplot")
  root <- project$modules[[root_index]]
  mapping_argument <- root$arguments$mapping %||% bp_argument(origin = "formal")
  mapping <- mapping_argument$value
  if (is.null(mapping) || !identical(bp_value_type(mapping), "RAesMapping")) mapping <- bp_aes_mapping()
  mappings <- mapping$mappings %||% list()
  # A group mapping left behind by an imported/advanced plot makes
  # geom_boxplot() dodge boxes within each x value, while geom_jitter()
  # remains centred on x. Visual mode owns the grouping through x/fill, so
  # remove the stale explicit group aesthetic when rebuilding a boxplot.
  for (name in c("color", "colour", "size", "label", "group")) mappings[[name]] <- NULL
  mappings$x <- bp_visual_mapping_value(config$x_field, "linear")
  mappings$y <- bp_visual_mapping_value(config$y_field, config$y_scale)
  mappings$fill <- bp_visual_mapping_value(config$color_field, "linear")
  mapping_argument$state <- "explicit"
  mapping_argument$value <- bp_aes_mapping(mappings)
  root$arguments$mapping <- mapping_argument
  root$visual_managed <- TRUE
  project$modules[[root_index]] <- root

  box <- bp_instantiate_module("r.ggplot2.geom_boxplot", registry)
  box$visual_managed <- TRUE
  box$visual_role <- "visual_boxplot_layer"
  if (nzchar(config$color_field)) {
    box$arguments$fill$state <- "unset"
  } else {
    box$arguments$fill <- bp_argument("explicit", bp_character(config$point_color), "dots_aesthetic")
  }
  box$arguments$color <- bp_argument("explicit", bp_character(config$box_border_color), "dots_aesthetic")
  box$arguments$width <- bp_argument("explicit", bp_double(config$point_size), "formal")
  box$arguments$alpha <- bp_argument("explicit", bp_double(config$alpha), "dots_aesthetic")
  box$arguments$outlier.shape <- if (isTRUE(config$box_show_outliers)) {
    bp_argument("explicit", bp_double(16), "dots_documented")
  } else {
    bp_argument("explicit_na", bp_na("integer"), "dots_documented")
  }
  box$arguments$outlier.size <- bp_argument("explicit", bp_double(config$box_outlier_size), "dots_documented")
  project$modules <- append(project$modules, list(box), after = root_index)

  if (isTRUE(config$box_jitter)) {
    jitter <- bp_instantiate_module("r.ggplot2.geom_jitter", registry)
    jitter$visual_managed <- TRUE
    jitter$visual_role <- "visual_boxplot_jitter"
    jitter$arguments$width <- bp_argument("explicit", bp_double(config$box_jitter_width), "formal")
    jitter$arguments$height <- bp_argument("explicit", bp_double(0), "formal")
    jitter$arguments$color <- bp_argument("explicit", bp_character(config$box_jitter_color), "dots_aesthetic")
    jitter$arguments$size <- bp_argument("explicit", bp_double(config$box_jitter_size), "dots_aesthetic")
    jitter$arguments$alpha <- bp_argument("explicit", bp_double(config$box_jitter_alpha), "dots_aesthetic")
    jitter$arguments$shape <- bp_argument("explicit", bp_double(16), "dots_aesthetic")
    project$modules <- append(project$modules, list(jitter), after = root_index + 1L)
  }

  labs_index <- bp_visual_first_instance(project, "r.ggplot2.labs")
  if (!is.na(labs_index)) {
    labels <- project$modules[[labs_index]]
    labels <- bp_visual_set_text_argument(labels, "color", "", "dots_documented")
    labels <- bp_visual_set_text_argument(labels, "fill", if (nzchar(config$color_field)) config$legend_title else "", "dots_documented")
    project$modules[[labs_index]] <- labels
  }

  palette_source <- if (nzchar(config$color_field)) bp_visual_box_palette_source(config$palette) else NULL
  if (!is.null(palette_source)) {
    scale <- bp_instantiate_module("r.ggplot2.scale_fill_manual", registry)
    scale$visual_managed <- TRUE
    scale$visual_role <- "visual_boxplot_fill_scale"
    scale$visual_palette <- config$palette
    scale$arguments$values <- bp_argument("raw_expression", bp_raw_expression(palette_source), "formal")
    project$modules[[length(project$modules) + 1L]] <- scale
  }

  root_index <- bp_visual_first_instance(project, "r.ggplot2.ggplot")
  root <- project$modules[[root_index]]
  project$mapping_config <- list(
    dataset_id = project$active_data_source_id %||% config$data_source_id,
    plot_id = root$instance_id,
    mapping = bp_mapping_argument_sources(root$arguments$mapping),
    confirmed_by_user = TRUE
  )
  project$visual_config <- project$visual_config %||% list()
  project$visual_config$active_chart_type <- "boxplot"
  project$visual_config$boxplot <- config
  config <- bp_visual_boxplot_config_from_project(project)
  project$visual_config$boxplot <- config
  list(project = project, root_instance_id = root$instance_id, config = config)
}

bp_validate_visual_boxplot_config <- function(config, columns = character()) {
  raw_config <- config %||% list()
  config <- bp_normalize_visual_boxplot_config(config)
  validation <- bp_validate_visual_scatter_config(config, columns)
  errors <- validation$errors
  if (!nzchar(config$x_field)) errors <- c(errors, "请选择箱线图分组字段。")
  if (!nzchar(config$y_field)) errors <- c(errors, "请选择箱线图数值字段。")
  jitter_color <- trimws(bp_visual_scalar_character(raw_config$box_jitter_color, config$box_jitter_color))
  if (isTRUE(config$box_jitter) && !grepl("^#[0-9A-Fa-f]{6}$", jitter_color)) {
    errors <- c(errors, "Boxplot jitter color must use a 6-digit hexadecimal value such as #334155.")
  }
  border <- trimws(bp_visual_scalar_character(raw_config$box_border_color, config$box_border_color))
  if (!grepl("^#[0-9A-Fa-f]{6}$", border)) errors <- c(errors, "箱线图边框颜色需使用 6 位十六进制颜色。")
  list(valid = !length(errors), errors = unique(errors))
}

bp_apply_visual_pca_config <- function(project, config, registry = NULL, analysis_result = NULL) {
  registry <- registry %||% bp_load_registry()
  project <- unserialize(serialize(project, NULL))
  config <- bp_normalize_pca_config(config, project)
  stored_scatter <- project$visual_config$scatter %||% bp_visual_scatter_defaults(project)
  project <- bp_pca_upsert_derived_sources(project, config, analysis_result)

  scatter <- bp_visual_scatter_defaults(project)
  scatter$data_source_id <- "dataset_pca_scores"
  scatter$x_field <- config$x_component
  scatter$y_field <- config$y_component
  scatter$color_field <- config$color_field
  scatter$label_field <- config$label_field
  scatter$point_color <- config$point_color
  scatter$point_size <- config$point_size
  scatter$alpha <- config$alpha
  scatter$shape <- config$shape
  scatter$palette <- config$palette
  scatter$trend_line <- "none"
  scatter$title <- config$title
  scatter$x_label <- ""
  scatter$y_label <- ""
  scatter$legend_title <- config$legend_title
  scatter$theme <- config$theme
  scatter$base_size <- config$base_size
  result <- bp_apply_visual_scatter_config(project, scatter, registry)
  project <- result$project
  project$visual_config$scatter <- stored_scatter

  root_index <- bp_visual_first_instance(project, "r.ggplot2.ggplot")
  root <- project$modules[[root_index]]
  mapping_argument <- root$arguments$mapping %||% bp_argument(origin = "formal")
  mapping <- mapping_argument$value
  if (is.null(mapping) || !identical(bp_value_type(mapping), "RAesMapping")) mapping <- bp_aes_mapping()
  mappings <- mapping$mappings %||% list()
  mappings$shape <- bp_visual_mapping_value(config$shape_field)
  mapping_argument$state <- "explicit"
  mapping_argument$value <- bp_aes_mapping(mappings)
  root$arguments$mapping <- mapping_argument
  root$visual_managed <- TRUE
  project$modules[[root_index]] <- root

  point_index <- bp_visual_first_instance(project, "r.ggplot2.geom_point", managed_first = FALSE)
  if (!is.na(point_index) && nzchar(config$shape_field)) project$modules[[point_index]]$arguments$shape$state <- "unset"

  project$modules <- Filter(function(instance) {
    !identical(instance$visual_role %||% "", "visual_pca_ellipse")
  }, project$modules %||% list())
  if (isTRUE(config$show_ellipse) && nzchar(config$color_field)) {
    ellipse <- bp_instantiate_module("r.ggplot2.stat_ellipse", registry)
    ellipse$visual_managed <- TRUE
    ellipse$visual_role <- "visual_pca_ellipse"
    ellipse$arguments$mapping <- bp_argument(
      "explicit",
      bp_aes_mapping(list(group = bp_symbol(config$color_field))),
      "formal"
    )
    ellipse$arguments$level <- bp_argument("explicit", bp_double(config$ellipse_level), "formal")
    ellipse$arguments$linewidth <- bp_argument("explicit", bp_double(0.7), "dots_aesthetic")
    ellipse$arguments$na.rm <- bp_argument("explicit", bp_logical(TRUE), "formal")
    point_index <- bp_visual_first_instance(project, "r.ggplot2.geom_point", managed_first = FALSE)
    project$modules <- append(project$modules, list(ellipse), after = point_index)
  }

  labs_index <- bp_visual_first_instance(project, "r.ggplot2.labs")
  if (!is.na(labs_index)) {
    labels <- project$modules[[labs_index]]
    x_component <- encodeString(config$x_component, quote = '"')
    y_component <- encodeString(config$y_component, quote = '"')
    labels$arguments$x <- bp_argument(
      "raw_expression",
      bp_raw_expression(paste0("sprintf(\"", config$x_component, " (%.1f%%)\", explained_variance[[", x_component, "]])")),
      "dots_documented"
    )
    labels$arguments$y <- bp_argument(
      "raw_expression",
      bp_raw_expression(paste0("sprintf(\"", config$y_component, " (%.1f%%)\", explained_variance[[", y_component, "]])")),
      "dots_documented"
    )
    labels <- bp_visual_set_text_argument(labels, "shape", if (nzchar(config$shape_field)) config$legend_title else "", "dots_documented")
    project$modules[[labs_index]] <- labels
  }

  root_index <- bp_visual_first_instance(project, "r.ggplot2.ggplot")
  root <- project$modules[[root_index]]
  project$mapping_config <- list(
    dataset_id = "dataset_pca_scores",
    plot_id = root$instance_id,
    mapping = bp_mapping_argument_sources(root$arguments$mapping),
    confirmed_by_user = TRUE
  )
  project$active_data_source_id <- "dataset_pca_scores"
  project$data_reference <- list(
    strategy = "derived_analysis",
    source_id = "dataset_pca_scores",
    symbol = "pca_scores",
    embedded = FALSE
  )
  project$visual_config <- project$visual_config %||% list()
  project$visual_config$active_chart_type <- "pca"
  project$visual_config$pca <- config
  project$analysis_recipes <- project$analysis_recipes %||% list()
  project$analysis_recipes$pca <- list(
    contract_version = "0.1.0",
    type = if (identical(config$input_semantic_type, "raw_counts")) "raw_count_pca" else "matrix_pca",
    input_source_ids = Filter(nzchar, c(config$expression_source_id, config$metadata_source_id)),
    confirmed_signature = config$raw_count_recipe_confirmed_signature %||% "",
    config = config,
    package_versions = {
      versions <- analysis_result$preparation$package_versions %||% list(R = as.character(getRversion()))
      versions$stats <- as.character(utils::packageVersion("stats"))
      if (requireNamespace("ggplot2", quietly = TRUE)) versions$ggplot2 <- as.character(utils::packageVersion("ggplot2"))
      versions
    },
    updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
  list(project = project, root_instance_id = root$instance_id, config = config)
}

bp_validate_visual_pca_config <- function(config, columns = character()) {
  config <- bp_normalize_pca_config(config)
  errors <- character()
  if (!nzchar(config$expression_source_id)) errors <- c(errors, "请选择 PCA 表达数据源。")
  if (!grepl("^PC[0-9]+$", config$x_component)) errors <- c(errors, "请选择有效的横轴主成分。")
  if (!grepl("^PC[0-9]+$", config$y_component)) errors <- c(errors, "请选择有效的纵轴主成分。")
  if (identical(config$x_component, config$y_component)) errors <- c(errors, "横轴和纵轴不能使用同一个主成分。")
  if (length(columns)) {
    for (name in c("x_component", "y_component", "color_field", "shape_field", "label_field")) {
      field <- config[[name]]
      if (nzchar(field) && !field %in% columns) errors <- c(errors, paste0("PCA 得分表中不存在字段 ‘", field, "’。"))
    }
  }
  if (isTRUE(config$show_ellipse) && !nzchar(config$color_field)) errors <- c(errors, "启用分组椭圆前请先选择颜色分组字段。")
  if (!grepl("^#[0-9A-Fa-f]{6}$", config$point_color)) errors <- c(errors, "PCA 点颜色需使用 6 位十六进制颜色。")
  list(valid = !length(errors), errors = unique(errors))
}

bp_apply_visual_config <- function(project, config, registry = NULL) {
  if (identical(config$chart_type %||% "scatter", "volcano")) {
    bp_apply_visual_volcano_config(project, config, registry)
  } else if (identical(config$chart_type %||% "scatter", "boxplot")) {
    bp_apply_visual_boxplot_config(project, config, registry)
  } else if (identical(config$chart_type %||% "scatter", "pca")) {
    bp_apply_visual_pca_config(project, config, registry)
  } else {
    bp_apply_visual_scatter_config(project, config, registry)
  }
}

bp_validate_visual_config <- function(config, columns = character()) {
  if (identical(config$chart_type %||% "scatter", "volcano")) {
    bp_validate_visual_volcano_config(config, columns)
  } else if (identical(config$chart_type %||% "scatter", "boxplot")) {
    bp_validate_visual_boxplot_config(config, columns)
  } else if (identical(config$chart_type %||% "scatter", "pca")) {
    bp_validate_visual_pca_config(config, columns)
  } else {
    bp_validate_visual_scatter_config(config, columns)
  }
}
