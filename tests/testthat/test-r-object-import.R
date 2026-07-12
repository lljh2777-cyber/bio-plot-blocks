test_that("RDS data frames and matrices are inspected without global injection", {
  frame_path <- tempfile(fileext = ".rds")
  matrix_path <- tempfile(fileext = ".rds")
  on.exit(if (file.exists(frame_path)) unlink(frame_path), add = TRUE)
  on.exit(if (file.exists(matrix_path)) unlink(matrix_path), add = TRUE)
  frame <- data.frame(gene = c("A", "B"), score = c(1.2, 2.4), row.names = c("g1", "g2"))
  matrix <- matrix(1:6, nrow = 2, dimnames = list(c("gene_a", "gene_b"), c("S1", "S2", "S3")))
  saveRDS(frame, frame_path)
  saveRDS(matrix, matrix_path)

  frame_result <- bp_read_r_data_objects(frame_path, "results.rds")
  matrix_result <- bp_read_r_data_objects(matrix_path, "counts.rds")
  expect_identical(frame_result$metadata[[1]]$kind, "data.frame")
  expect_true(frame_result$metadata[[1]]$supported)
  expect_identical(matrix_result$metadata[[1]]$kind, "matrix")
  expect_true(matrix_result$metadata[[1]]$requires_conversion)

  converted <- bp_convert_r_object(matrix_result$objects[[1]], row_names = "column", row_name_column = "Feature")
  expect_s3_class(converted, "data.frame")
  expect_identical(names(converted), c("Feature", "S1", "S2", "S3"))
  expect_identical(converted$Feature, c("gene_a", "gene_b"))
  expect_identical(attr(converted, "bp_conversion")$row_names, "column")
})

test_that("RData objects are browsed in isolation with safe statuses", {
  path <- tempfile(fileext = ".RData")
  on.exit(if (file.exists(path)) unlink(path), add = TRUE)
  deg <- data.frame(log2FC = c(-1, 1), padj = c(0.01, 0.02))
  counts <- matrix(1:4, nrow = 2)
  fit <- stats::lm(mpg ~ wt, data = mtcars)
  custom_function <- function(x) x
  save(deg, counts, fit, custom_function, file = path)

  result <- bp_read_r_data_objects(path, "analysis.RData")
  statuses <- stats::setNames(vapply(result$metadata, `[[`, character(1), "status"), vapply(result$metadata, `[[`, character(1), "name"))
  expect_identical(statuses[["deg"]], "ready")
  expect_identical(statuses[["counts"]], "conversion_required")
  expect_identical(statuses[["fit"]], "unsupported")
  expect_identical(statuses[["custom_function"]], "forbidden")
  expect_setequal(names(result$objects), c("deg", "counts"))
  expect_false(exists("deg", envir = .GlobalEnv, inherits = FALSE))
})

test_that("R-object data sources generate reproducible setup code", {
  rds_source <- list(
    id = "dataset_001", name = "counts", source_type = "rds", original_file_name = "counts.rds",
    original_object_type = "matrix", conversion = list(from = "matrix", row_names = "column", row_name_column = "Gene")
  )
  rdata_source <- list(
    id = "dataset_002", name = "deg", source_type = "rdata", original_file_name = "analysis.RData",
    object_name = "deg", original_object_type = "data.frame", conversion = list(from = "data.frame", row_names = "preserve")
  )
  expect_match(paste(bp_data_source_setup_line(rds_source), collapse = "\n"), "readRDS", fixed = TRUE)
  expect_match(paste(bp_data_source_setup_line(rds_source), collapse = "\n"), "Gene", fixed = TRUE)
  rdata_code <- paste(bp_data_source_setup_line(rdata_source), collapse = "\n")
  expect_match(rdata_code, "new.env(parent = emptyenv())", fixed = TRUE)
  expect_match(rdata_code, 'get("deg"', fixed = TRUE)
  expect_silent(parse(text = rdata_code))
})
