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
