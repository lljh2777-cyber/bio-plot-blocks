test_that("mapping editor combines column suggestions with manual input", {
  source(file.path(root, "app", "modules", "workspace-server.R"), local = environment())
  argument <- bp_argument("explicit", bp_aes_mapping(list(x = bp_symbol("log2FC"))), "formal")
  editor <- bp_aes_editor(
    "ggplot-test", "mapping", argument,
    c("log2FC · numeric" = "log2FC", "Gene ID · character" = "`Gene ID`")
  )
  html <- htmltools::renderTags(editor)$html
  expect_match(html, 'list="bp-aes-columns-ggplot-test-mapping-x"', fixed = TRUE)
  expect_match(html, 'class="bp-aes-suggestion-button"', fixed = TRUE)
  expect_match(html, 'data-aes-input-id="bp-aes-columns-ggplot-test-mapping-x-input"', fixed = TRUE)
  expect_match(html, 'value="log2FC" label="log2FC · numeric"', fixed = TRUE)
  expect_match(html, 'value="`Gene ID`" label="Gene ID · character"', fixed = TRUE)
  expect_match(html, 'data-aes-key="size"', fixed = TRUE)
  expect_match(html, 'data-aes-key="alpha"', fixed = TRUE)
  expect_match(html, "choose a suggestion or type an R expression", fixed = TRUE)
})

test_that("data argument combines registered source suggestions with manual input", {
  source(file.path(root, "app", "modules", "workspace-server.R"), local = environment())
  parameter <- list(name = "data", ui_control = "data_reference", formal_default = bp_null())
  control <- bp_value_control(
    "ggplot-test", "data", bp_argument("unset", origin = "formal"), parameter,
    c("df · EXAMPLE · ready" = "df", "results · RDS · ready" = "results")
  )
  html <- htmltools::renderTags(control)$html
  expect_match(html, 'class="bp-param-hybrid-control"', fixed = TRUE)
  expect_match(html, 'list="bp-data-sources-ggplot-test-data"', fixed = TRUE)
  expect_match(html, 'data-aes-input-id="bp-data-sources-ggplot-test-data-input"', fixed = TRUE)
  expect_match(html, 'aria-label="Choose data from registered data sources"', fixed = TRUE)
  expect_match(html, 'value="results" label="results · RDS · ready"', fixed = TRUE)
  expect_match(html, 'placeholder="data object or R expression"', fixed = TRUE)
})

test_that("mapping dropdowns render outside the modal clipping boundary", {
  source(file.path(root, "app", "modules", "workspace-server.R"), local = environment())
  data <- data.frame(
    ENSEMBL = c("ENSG1", "ENSG2"), SYMBOL = c("A", "B"),
    logFC = c(-1, 1), FDR = c(0.01, 0.02), regulated = c("Down", "Up")
  )
  controls <- bp_data_mapping_controls(
    list(data = data, profile = bp_profile_dataset(data)),
    bp_create_project()
  )
  html <- htmltools::renderTags(controls)$html
  expect_match(html, '"dropdownParent":"body"', fixed = TRUE)
  expect_match(html, '"dropdownClass":"selectize-dropdown bp-mapping-dropdown"', fixed = TRUE)
  expect_match(html, '"plugins":["auto_position"', fixed = TRUE)
})

test_that("Shiny import flow registers mapped CSV data", {
  skip_if_not_installed("shiny")
  source(file.path(root, "app", "modules", "workspace-server.R"), local = environment())
  csv <- tempfile(fileext = ".csv")
  on.exit(if (file.exists(csv)) unlink(csv), add = TRUE)
  writeLines(c(
    "feature,x_value,y_value,group",
    "g1,-1.2,2.4,Down",
    "g2,0.1,0.4,NS",
    "g3,1.5,3.1,Up"
  ), csv)

  server <- function(input, output, session) {
    session$userData$bp_state <- bp_workspace_server(input, output, session, registry, bp_load_template(), root)
  }
  shiny::testServer(
    server,
    {
      state <- session$userData$bp_state
      example_preview <- htmltools::renderTags(output$active_data_preview)$html
      expect_match(example_preview, "GENE1", fixed = TRUE)
      expect_match(example_preview, "Showing 30 of 420 rows", fixed = TRUE)
      visual_example_preview <- htmltools::renderTags(output$visual_active_data_preview)$html
      expect_match(visual_example_preview, "显示前 30 行，共 420 行 · 11 列", fixed = TRUE)
      expect_match(visual_example_preview, "GENE30", fixed = TRUE)
      expect_true(all(vapply(names(bp_default_environment()$df), grepl, logical(1), x = visual_example_preview, fixed = TRUE)))
      session$setInputs(import_data = 1)
      session$setInputs(
        data_delimiter = "auto", data_encoding = "UTF-8", data_header = TRUE,
        data_na_values = ",NA,N/A,null,NULL", data_quote = '"', data_decimal = ".", data_skip = 0,
        data_file = list(name = "uploaded-results.csv", size = file.info(csv)$size, type = "text/csv", datapath = csv)
      )
      session$flushReact()
      expect_null(state$data_import$error)
      expect_identical(state$data_import$profile$rows, 3L)
      expect_identical(state$data_import$profile$columns, 4L)

      session$setInputs(
        data_source_name = "uploaded_results",
        data_type_1 = "character", data_type_2 = "numeric", data_type_3 = "numeric", data_type_4 = "character",
        data_map_x = "x_value", data_map_y = "y_value", data_map_color = "group",
        data_map_fill = "", data_map_shape = "", data_map_size = "", data_map_alpha = "",
        data_map_label = "feature", data_map_group = "",
        register_data_source = 1
      )
      session$flushReact()
      expect_identical(state$project$data_reference$symbol, "uploaded_results")
      expect_identical(state$project$mapping_config$mapping$x, "x_value")
      expect_identical(state$project$mapping_config$mapping$color, "group")
      expect_true(is.data.frame(state$data_objects[[state$project$active_data_source_id]]))
      expect_match(bp_generate_code(state$project, registry), "data = uploaded_results", fixed = TRUE)
      imported_preview <- htmltools::renderTags(output$active_data_preview)$html
      expect_match(imported_preview, "uploaded_results", fixed = TRUE)
      expect_match(imported_preview, "g1", fixed = TRUE)
      expect_match(imported_preview, "Showing 3 of 3 rows", fixed = TRUE)
      visual_imported_preview <- htmltools::renderTags(output$visual_active_data_preview)$html
      expect_match(visual_imported_preview, "显示前 3 行，共 3 行 · 4 列", fixed = TRUE)
      expect_match(visual_imported_preview, "feature", fixed = TRUE)
      expect_match(visual_imported_preview, "group", fixed = TRUE)
      session$setInputs(data_preview_source_id = "dataset_example")
      session$flushReact()
      example_after_import <- htmltools::renderTags(output$active_data_preview)$html
      expect_match(example_after_import, "df — Example data", fixed = TRUE)
      expect_match(example_after_import, "GENE1", fixed = TRUE)
      expect_identical(state$project$data_reference$symbol, "uploaded_results")
      if (!is.null(state$preview_process) && state$preview_process$is_alive()) state$preview_process$kill()
    }
  )
})

test_that("Shiny import flow can register confirmed raw counts without plot mapping", {
  skip_if_not_installed("shiny")
  source(file.path(root, "app", "modules", "workspace-server.R"), local = environment())
  csv <- tempfile(fileext = ".csv")
  on.exit(if (file.exists(csv)) unlink(csv), add = TRUE)
  writeLines(c(
    "Gene,S1,S2,S3,S4",
    "g1,10,12,18,20",
    "g2,0,0,1,0",
    "g3,80,95,120,130",
    "g4,22,25,19,31"
  ), csv)

  server <- function(input, output, session) {
    session$userData$bp_state <- bp_workspace_server(input, output, session, registry, bp_load_template(), root)
  }
  shiny::testServer(
    server,
    {
      state <- session$userData$bp_state
      session$setInputs(import_data = 1)
      session$setInputs(
        data_delimiter = "auto", data_encoding = "UTF-8", data_header = TRUE,
        data_na_values = ",NA,N/A,null,NULL", data_quote = '"', data_decimal = ".", data_skip = 0,
        data_file = list(name = "counts.csv", size = file.info(csv)$size, type = "text/csv", datapath = csv)
      )
      session$flushReact()
      review <- htmltools::renderTags(output$data_import_results)$html
      expect_match(review, "可能是 RNA-seq Raw count", fixed = TRUE)
      expect_match(review, "只注册数据源", fixed = TRUE)

      session$setInputs(
        data_source_name = "counts_raw",
        data_type_1 = "character", data_type_2 = "integer", data_type_3 = "integer",
        data_type_4 = "integer", data_type_5 = "integer",
        data_semantic_type = "raw_counts", data_register_only = TRUE,
        register_data_source = 1
      )
      session$flushReact()
      source <- Filter(function(item) identical(item$name, "counts_raw"), state$project$data_sources)[[1]]
      expect_true(source$semantic_confirmed)
      expect_identical(source$semantic_type, "raw_counts")
      expect_identical(state$project$analysis_workflow_mode, "rna_seq")
      expect_identical(state$project$active_data_source_id, "dataset_example")
      expect_true(is.data.frame(state$data_objects[[source$id]]))
    }
  )
})
