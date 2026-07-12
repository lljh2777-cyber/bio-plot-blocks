test_that("Shiny registers multiple safe RData objects without changing the active plot", {
  skip_if_not_installed("shiny")
  source(file.path(root, "app", "modules", "workspace-server.R"), local = environment())
  path <- tempfile(fileext = ".RData")
  on.exit(if (file.exists(path)) unlink(path), add = TRUE)
  deg <- data.frame(gene = c("A", "B"), log2FC = c(-1, 1), padj = c(0.01, 0.02))
  counts <- matrix(1:6, nrow = 2, dimnames = list(c("g1", "g2"), c("S1", "S2", "S3")))
  blocked <- function(x) x
  save(deg, counts, blocked, file = path)

  server <- function(input, output, session) {
    session$userData$bp_state <- bp_workspace_server(input, output, session, registry, bp_load_template(), root)
  }
  shiny::testServer(server, {
    state <- session$userData$bp_state
    session$setInputs(import_data = 1)
    session$setInputs(data_file = list(name = "analysis.RData", size = file.info(path)$size, type = "application/octet-stream", datapath = path))
    session$flushReact()
    expect_identical(state$data_import$format, "rdata")
    expect_setequal(names(state$data_import$objects), c("deg", "counts"))
    expect_identical(Filter(function(item) identical(item$name, "blocked"), state$data_import$metadata)[[1]]$status, "forbidden")

    session$setInputs(
      r_object_selection = c("deg", "counts"),
      r_row_names = "column", r_row_name_column = "Feature",
      register_data_source = 1
    )
    session$flushReact()
    imported <- Filter(function(source) !isTRUE(source$example), state$project$data_sources)
    expect_length(imported, 2L)
    manager_html <- htmltools::renderTags(output$data_source_manager_list)$html
    expect_match(manager_html, "analysis.RData", fixed = TRUE)
    expect_match(manager_html, "Use in plot", fixed = TRUE)
    expect_identical(state$project$active_data_source_id, "dataset_example")
    expect_true(all(vapply(imported, function(source) identical(source$status, "ready"), logical(1))))
    expect_true(all(vapply(imported, function(source) is.data.frame(state$data_objects[[source$id]]), logical(1))))
    counts_source <- Filter(function(source) identical(source$object_name, "counts"), imported)[[1]]
    expect_identical(names(state$data_objects[[counts_source$id]])[[1]], "Feature")
    expect_identical(state$data_objects[[counts_source$id]]$Feature, c("g1", "g2"))

    deg_source <- Filter(function(source) identical(source$object_name, "deg"), imported)[[1]]
    ggplot_instance <- Filter(function(instance) identical(instance$module_id, "r.ggplot2.ggplot"), state$project$modules)[[1]]
    session$setInputs(param_change = list(
      kind = "value", instance_id = ggplot_instance$instance_id, param = "data",
      control = "data_reference", value = deg_source$name, nonce = 0.5
    ))
    session$flushReact()
    expect_identical(state$project$active_data_source_id, deg_source$id)
    expect_identical(state$project$data_reference$source_id, deg_source$id)
    expect_identical(state$project$data_reference$symbol, deg_source$name)
    expect_identical(state$project$mapping_config$dataset_id, deg_source$id)
    expect_identical(state$data_preview_source_id, deg_source$id)
    expect_identical(state$last_data_switch$preserved_count, 1L)
    expect_identical(state$last_data_switch$cleared_count, 2L)
    root_after_switch <- Filter(function(instance) identical(instance$module_id, "r.ggplot2.ggplot"), state$project$modules)[[1]]
    expect_setequal(names(root_after_switch$arguments$mapping$value$mappings), "x")
    inspector_after_switch <- htmltools::renderTags(output$parameter_inspector)$html
    expect_match(inspector_after_switch, 'value="gene" label="gene · character"', fixed = TRUE)
    expect_match(inspector_after_switch, 'value="padj" label="padj · numeric"', fixed = TRUE)

    session$setInputs(undo = 1)
    session$flushReact()
    expect_identical(state$project$active_data_source_id, "dataset_example")
    expect_identical(state$data_preview_source_id, "dataset_example")
    session$setInputs(redo = 1)
    session$flushReact()
    expect_identical(state$project$active_data_source_id, deg_source$id)
    expect_identical(state$data_preview_source_id, deg_source$id)

    mappings_before_expression <- lapply(state$project$modules, function(instance) instance$arguments$mapping)
    session$setInputs(param_change = list(
      kind = "value", instance_id = ggplot_instance$instance_id, param = "data",
      control = "data_reference", value = paste0("subset(", deg_source$name, ", log2FC > 0)"), nonce = 0.75
    ))
    session$flushReact()
    expect_identical(state$project$active_data_source_id, deg_source$id)
    expect_identical(lapply(state$project$modules, function(instance) instance$arguments$mapping), mappings_before_expression)
    expect_true(isTRUE(state$last_data_switch$custom_expression))

    relink_project <- bp_clone_project(state$project)
    counts_index <- which(vapply(relink_project$data_sources, function(source) identical(source$id, counts_source$id), logical(1)))[[1]]
    relink_project$data_sources[[counts_index]]$status <- "relink_required"
    relink_project$data_sources[[counts_index]]$relink_required <- TRUE
    state$project <- relink_project
    data_before_rejected_switch <- bp_value_to_source(Filter(function(instance) identical(instance$module_id, "r.ggplot2.ggplot"), state$project$modules)[[1]]$arguments$data$value)
    session$setInputs(param_change = list(
      kind = "value", instance_id = ggplot_instance$instance_id, param = "data",
      control = "data_reference", value = counts_source$name, nonce = 0.9
    ))
    session$flushReact()
    expect_identical(state$project$active_data_source_id, deg_source$id)
    expect_identical(
      bp_value_to_source(Filter(function(instance) identical(instance$module_id, "r.ggplot2.ggplot"), state$project$modules)[[1]]$arguments$data$value),
      data_before_rejected_switch
    )

    session$setInputs(data_source_action = list(source_id = deg_source$id, action = "map", nonce = 1))
    session$setInputs(
      data_map_x = "log2FC", data_map_y = "padj", data_map_color = "",
      data_map_fill = "", data_map_shape = "", data_map_size = "", data_map_alpha = "",
      data_map_label = "gene", data_map_group = "", apply_data_source_mapping = 1
    )
    session$flushReact()
    expect_identical(state$project$active_data_source_id, deg_source$id)
    expect_match(bp_generate_code(state$project, registry), paste0("data = ", deg_source$name), fixed = TRUE)
    expect_identical(state$last_data_switch$source$id, deg_source$id)
    expect_identical(state$last_data_switch$cleared_count, 0L)
    expect_setequal(names(Filter(function(instance) identical(instance$module_id, "r.ggplot2.ggplot"), state$project$modules)[[1]]$arguments$mapping$value$mappings), c("x", "y", "label"))

    session$setInputs(data_source_action = list(source_id = counts_source$id, action = "rename", nonce = 2))
    session$setInputs(renamed_data_source = "renamed_counts", apply_data_source_rename = 1)
    session$flushReact()
    renamed <- Filter(function(source) identical(source$id, counts_source$id), state$project$data_sources)[[1]]
    expect_identical(renamed$name, "renamed_counts")
    session$setInputs(data_source_action = list(source_id = counts_source$id, action = "remove", nonce = 3))
    session$flushReact()
    expect_false(counts_source$id %in% vapply(state$project$data_sources, `[[`, character(1), "id"))
    expect_null(state$data_objects[[counts_source$id]])
    if (!is.null(state$preview_process) && state$preview_process$is_alive()) state$preview_process$kill()
  })
})

test_that("stage-two import and manager modals expose R workflow controls", {
  skip_if_not_installed("shiny")
  source(file.path(root, "app", "modules", "workspace-server.R"), local = environment())
  import_html <- htmltools::renderTags(bp_data_import_modal())$html
  manager_html <- htmltools::renderTags(bp_data_source_manager_modal())$html
  expect_match(import_html, ".rds", fixed = TRUE)
  expect_match(import_html, ".RData", fixed = TRUE)
  expect_match(import_html, "Register / use data", fixed = TRUE)
  expect_match(manager_html, "Data Sources / 数据源", fixed = TRUE)
  expect_match(manager_html, "Import another data source", fixed = TRUE)
})
