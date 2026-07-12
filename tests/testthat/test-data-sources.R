test_that("CSV and TSV files are parsed without changing original column names", {
  csv <- tempfile(fileext = ".csv")
  tsv <- tempfile(fileext = ".tsv")
  on.exit(if (file.exists(csv)) unlink(csv), add = TRUE)
  on.exit(if (file.exists(tsv)) unlink(tsv), add = TRUE)
  writeLines(c("log2 fold change,padj,group", "1.2,0.01,A", "-0.8,NA,B"), csv)
  writeLines(c("x\ty\tlabel", "1\t2\tone", "3\t4\ttwo"), tsv)

  csv_result <- bp_read_delimited_data(csv, "results.csv")
  tsv_result <- bp_read_delimited_data(tsv, "results.tsv")
  expect_identical(names(csv_result$data), c("log2 fold change", "padj", "group"))
  expect_identical(dim(csv_result$data), c(2L, 3L))
  expect_identical(dim(tsv_result$data), c(2L, 3L))
  expect_true(is.numeric(csv_result$data$padj))
})

test_that("dataset profiling reports types and quality without cleaning", {
  data <- data.frame(
    value = c(1, 1, NA, Inf),
    padj = c(0.1, 0.1, 1.2, NA),
    group = c("A", "A", "B", "B"),
    constant = "same",
    check.names = FALSE
  )
  data <- rbind(data, data[1, , drop = FALSE])
  profile <- bp_profile_dataset(data)
  expect_identical(profile$rows, 5L)
  expect_gt(profile$missing_values, 0)
  expect_gt(profile$duplicate_rows, 0)
  expect_true(any(vapply(profile$warnings, function(warning) identical(warning$code, "p_value_range"), logical(1))))
  expect_true(any(vapply(profile$column_metadata, function(column) "constant" %in% column$flags, logical(1))))
  expect_identical(nrow(data), 5L)
})

test_that("registered data mapping drives code generation and real execution", {
  project <- bp_basic_scatter_project(registry)
  data <- data.frame("log2 fold change" = c(-1, 0, 1), padj = c(0.1, 0.5, 0.01), group = c("A", "B", "A"), check.names = FALSE)
  profile <- bp_profile_dataset(data)
  source <- list(
    id = "dataset_001", name = "real_results", source_type = "csv",
    original_file_name = "real-results.csv", object_type = "data.frame",
    rows = nrow(data), columns = ncol(data), status = "ready", example = FALSE,
    relink_required = FALSE, column_metadata = profile$column_metadata,
    quality = list(warnings = profile$warnings),
    parse_options = list(delimiter = ",", encoding = "UTF-8", header = TRUE, na_values = c("", "NA"), quote = '"', decimal = ".", skip = 0L)
  )
  mapping <- list(x = "log2 fold change", y = "padj", color = "group", fill = "", shape = "", size = "", alpha = "", label = "", group = "")
  project <- bp_apply_dataset_mapping(project, source, mapping)
  code <- bp_generate_code(project, registry)
  exported <- bp_generate_code(project, registry, include_setup = TRUE)
  expect_match(code, "data = real_results", fixed = TRUE)
  expect_match(code, "`log2 fold change`", fixed = TRUE)
  expect_match(exported, "real_results <- read.table", fixed = TRUE)
  result <- bp_execute_project(project, registry, datasets = list(real_results = data))
  expect_true(result$ok)
  expect_s3_class(result$plot, "ggplot")
  expect_identical(project$mapping_config$dataset_id, "dataset_001")
  path <- tempfile(fileext = ".json")
  on.exit(if (file.exists(path)) unlink(path), add = TRUE)
  bp_save_project(project, path)
  restored <- bp_load_project(path)
  expect_identical(restored$active_data_source_id, "dataset_001")
  expect_identical(restored$mapping_config$mapping$x, "log2 fold change")
  expect_false("data" %in% names(restored$data_sources[[2]]))
})

test_that("restored imported sources require explicit relinking", {
  project <- bp_create_project()
  source <- bp_example_data_source()
  source$id <- "dataset_001"
  source$name <- "results"
  source$example <- FALSE
  source$status <- "ready"
  source$original_file_name <- "results.csv"
  project$data_sources <- list(source)
  project$active_data_source_id <- source$id
  restored <- bp_mark_data_sources_for_relink(project)
  expect_identical(restored$data_sources[[1]]$status, "relink_required")
  expect_true(restored$data_sources[[1]]$relink_required)
})

test_that("active data columns become code-safe mapping suggestions", {
  project <- bp_create_project()
  suggestions <- bp_active_data_column_suggestions(project)
  expect_true(all(c("log2FC · numeric", "status · factor") %in% names(suggestions)))
  expect_identical(unname(suggestions[["log2FC · numeric"]]), "log2FC")

  project$data_sources <- list(list(
    id = "dataset_custom", name = "custom", example = FALSE,
    column_metadata = list(list(name = "Gene ID", recommended_type = "character"))
  ))
  project$active_data_source_id <- "dataset_custom"
  suggestions <- bp_active_data_column_suggestions(project)
  expect_identical(unname(suggestions[["Gene ID · character"]]), "`Gene ID`")
})

test_that("registered data sources become code-safe data argument suggestions", {
  project <- bp_create_project()
  suggestions <- bp_data_source_reference_suggestions(project)
  expect_identical(unname(suggestions[["df · EXAMPLE · ready"]]), "df")

  imported <- bp_example_data_source()
  imported$id <- "dataset_custom"
  imported$name <- "results_table"
  imported$source_type <- "rds"
  imported$example <- FALSE
  imported$status <- "ready"
  project$data_sources <- c(project$data_sources, list(imported))
  suggestions <- bp_data_source_reference_suggestions(project)
  expect_identical(unname(suggestions[["results_table · RDS · ready"]]), "results_table")

  project$data_sources[[2]]$status <- "relink_required"
  suggestions <- bp_data_source_reference_suggestions(project)
  expect_identical(unname(suggestions[["results_table · RDS · relink required"]]), "results_table")
})

test_that("switching registered data preserves compatible mappings and clears missing columns", {
  project <- bp_project_from_template("bio.volcano.basic", registry)
  root_index <- which(vapply(project$modules, function(module) identical(module$module_id, "r.ggplot2.ggplot"), logical(1)))[[1]]
  point_index <- which(vapply(project$modules, function(module) identical(module$module_id, "r.ggplot2.geom_point"), logical(1)))[[1]]
  project$modules[[root_index]]$arguments$mapping <- bp_argument("explicit", bp_aes_mapping(list(
    x = bp_symbol("log2FC"), y = bp_symbol("neg_log10_padj")
  )), "formal")
  project$modules[[point_index]]$arguments$mapping <- bp_argument("explicit", bp_aes_mapping(list(
    color = bp_symbol("status"), label = bp_symbol("missing_label"), group = bp_raw_expression("interaction(status, batch)")
  )), "formal")
  original_size <- project$modules[[point_index]]$arguments$size
  data <- data.frame(log2FC = c(-1, 1), status = c("Down", "Up"))
  profile <- bp_profile_dataset(data)
  source <- list(
    id = "dataset_switch", name = "switched_data", source_type = "rds",
    original_file_name = "switched.rds", object_type = "data.frame",
    rows = nrow(data), columns = ncol(data), status = "ready", example = FALSE,
    relink_required = FALSE, column_metadata = profile$column_metadata,
    quality = list(warnings = list()), parse_options = list()
  )

  result <- bp_switch_project_data_source(project, source, data)
  switched <- result$project
  root_mapping <- switched$modules[[root_index]]$arguments$mapping
  point_mapping <- switched$modules[[point_index]]$arguments$mapping
  expect_setequal(names(root_mapping$value$mappings), "x")
  expect_setequal(names(point_mapping$value$mappings), c("color", "group"))
  expect_identical(bp_value_to_source(point_mapping$value$mappings$group), "interaction(status, batch)")
  expect_identical(switched$modules[[point_index]]$arguments$size, original_size)
  expect_identical(result$preserved_count, 3L)
  expect_identical(result$cleared_count, 2L)
  expect_identical(switched$active_data_source_id, source$id)
  expect_identical(switched$data_reference$symbol, source$name)
  expect_identical(switched$mapping_config$mapping$x, "log2FC")
  expect_false("y" %in% names(switched$mapping_config$mapping))
})

test_that("an explicitly mapped aes stays explicit when every direct column is cleared", {
  project <- bp_basic_scatter_project(registry)
  data <- data.frame(other = 1:3)
  profile <- bp_profile_dataset(data)
  source <- list(
    id = "dataset_other", name = "other_data", source_type = "csv",
    original_file_name = "other.csv", object_type = "data.frame",
    rows = nrow(data), columns = ncol(data), status = "ready", example = FALSE,
    relink_required = FALSE, column_metadata = profile$column_metadata,
    quality = list(warnings = list()), parse_options = list()
  )
  result <- bp_switch_project_data_source(project, source, data)
  mapping <- result$project$modules[[1]]$arguments$mapping
  expect_identical(mapping$state, "explicit")
  expect_identical(bp_value_to_source(mapping$value), "aes()")
  expect_identical(result$cleared_count, 2L)

  source$status <- "relink_required"
  source$relink_required <- TRUE
  expect_error(bp_switch_project_data_source(project, source, data), "must be relinked")
})
