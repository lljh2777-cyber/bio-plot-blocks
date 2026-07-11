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
      if (!is.null(state$preview_process) && state$preview_process$is_alive()) state$preview_process$kill()
    }
  )
})
