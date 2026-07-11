bp_icon <- function(name, size = 18, label = NULL) {
  path <- function(d, ...) htmltools::tags$path(d = d, ...)
  line <- function(x1, y1, x2, y2, ...) htmltools::tags$line(x1 = x1, y1 = y1, x2 = x2, y2 = y2, ...)
  circle <- function(cx, cy, r, ...) htmltools::tags$circle(cx = cx, cy = cy, r = r, ...)
  rect <- function(x, y, width, height, ...) htmltools::tags$rect(x = x, y = y, width = width, height = height, ...)
  polyline <- function(points, ...) htmltools::tags$polyline(points = points, ...)

  children <- switch(
    name,
    search = list(circle(11, 11, 7), line(16.5, 16.5, 21, 21)),
    plus = list(line(12, 5, 12, 19), line(5, 12, 19, 12)),
    play = list(htmltools::tags$polygon(points = "8,5 19,12 8,19", fill = "none")),
    save = list(path("M5 3h12l3 3v15H4V3h1Z"), rect(7, 3, 9, 6), rect(7, 14, 10, 7)),
    export = list(path("M12 3v12"), polyline("7,8 12,3 17,8"), path("M5 13v7h14v-7")),
    import = list(path("M12 3v12"), polyline("7,10 12,15 17,10"), path("M5 4v4M19 4v4M5 19h14")),
    undo = list(path("M9 7 4 12l5 5"), path("M5 12h9a5 5 0 0 1 5 5")),
    redo = list(path("m15 7 5 5-5 5"), path("M19 12h-9a5 5 0 0 0-5 5")),
    duplicate = list(rect(8, 8, 11, 12, rx = 2), path("M16 8V5a2 2 0 0 0-2-2H5a2 2 0 0 0-2 2v11a2 2 0 0 0 2 2h3")),
    trash = list(path("M4 7h16"), path("M9 7V4h6v3"), path("m7 7 1 13h8l1-13"), line(10, 10, 10, 17), line(14, 10, 14, 17)),
    chevron_down = list(polyline("6,9 12,15 18,9")),
    chevron_up = list(polyline("6,15 12,9 18,15")),
    chevron_right = list(polyline("9,6 15,12 9,18")),
    grip = list(circle(8, 6, 1, fill = "currentColor", stroke = "none"), circle(16, 6, 1, fill = "currentColor", stroke = "none"), circle(8, 12, 1, fill = "currentColor", stroke = "none"), circle(16, 12, 1, fill = "currentColor", stroke = "none"), circle(8, 18, 1, fill = "currentColor", stroke = "none"), circle(16, 18, 1, fill = "currentColor", stroke = "none")),
    check = list(circle(12, 12, 9), polyline("8,12 11,15 17,9")),
    warning = list(path("M12 3 2.8 20h18.4L12 3Z"), line(12, 9, 12, 14), circle(12, 17, 0.6, fill = "currentColor", stroke = "none")),
    info = list(circle(12, 12, 9), line(12, 11, 12, 17), circle(12, 7.5, 0.7, fill = "currentColor", stroke = "none")),
    code = list(polyline("8,7 3,12 8,17"), polyline("16,7 21,12 16,17"), line(14, 4, 10, 20)),
    copy = list(rect(9, 9, 11, 11, rx = 2), path("M15 9V5a2 2 0 0 0-2-2H5a2 2 0 0 0-2 2v8a2 2 0 0 0 2 2h4")),
    download = list(path("M12 3v12"), polyline("7,10 12,15 17,10"), path("M5 20h14")),
    close = list(line(5, 5, 19, 19), line(19, 5, 5, 19)),
    open = list(path("M3 6h7l2 2h9l-2 11H5L3 6Z"), path("M3 6V4h7l2 2")),
    folder = list(path("M3 6h7l2 2h9v11H3V6Z")),
    mapping = list(circle(5, 6, 2), circle(19, 6, 2), circle(12, 18, 2), line(7, 6, 17, 6), line(6.5, 7.5, 10.5, 16.5), line(17.5, 7.5, 13.5, 16.5)),
    point = list(circle(6, 15, 2, fill = "currentColor", stroke = "none"), circle(11, 8, 2, fill = "currentColor", stroke = "none"), circle(18, 13, 2, fill = "currentColor", stroke = "none")),
    boxplot = list(line(4, 12, 8, 12), rect(8, 6, 8, 12), line(12, 3, 12, 6), line(12, 18, 12, 21), line(16, 12, 20, 12), line(8, 12, 16, 12)),
    jitter = list(circle(6, 7, 1.5, fill = "currentColor", stroke = "none"), circle(11, 15, 1.5, fill = "currentColor", stroke = "none"), circle(16, 5, 1.5, fill = "currentColor", stroke = "none"), circle(19, 16, 1.5, fill = "currentColor", stroke = "none"), circle(7, 19, 1.5, fill = "currentColor", stroke = "none")),
    hline = list(line(3, 12, 21, 12), line(6, 7, 6, 17)),
    vline = list(line(12, 3, 12, 21), line(7, 18, 17, 18)),
    line = list(polyline("3,17 8,10 13,14 21,5")),
    label = list(rect(3, 5, 18, 14, rx = 2), line(7, 10, 17, 10), line(7, 14, 14, 14)),
    facet = list(rect(3, 4, 18, 16, rx = 1), line(12, 4, 12, 20), line(3, 12, 21, 12)),
    coordinates = list(line(4, 20, 4, 4), line(4, 20, 20, 20), polyline("4,8 9,8 9,13 15,13 15,6 20,6")),
    theme = list(circle(12, 12, 9), path("M12 3a9 9 0 0 0 0 18Z", fill = "currentColor", stroke = "none", opacity = 0.18), line(12, 3, 12, 21)),
    palette = list(path("M12 3a9 9 0 0 0 0 18h1.5a2 2 0 0 0 0-4H14a2 2 0 0 1 0-4h2a5 5 0 0 0 5-5 9 9 0 0 0-9-8Z"), circle(7.5, 10, 1, fill = "currentColor", stroke = "none"), circle(10, 6.5, 1, fill = "currentColor", stroke = "none"), circle(15, 7, 1, fill = "currentColor", stroke = "none")),
    plot = list(line(4, 20, 4, 4), line(4, 20, 21, 20), circle(8, 15, 1.4, fill = "currentColor", stroke = "none"), circle(12, 11, 1.4, fill = "currentColor", stroke = "none"), circle(17, 7, 1.4, fill = "currentColor", stroke = "none")),
    template = list(line(4, 20, 4, 4), line(4, 20, 21, 20), rect(7, 12, 2.5, 6, fill = "currentColor", stroke = "none"), rect(11, 8, 2.5, 10, fill = "currentColor", stroke = "none"), rect(15, 5, 2.5, 13, fill = "currentColor", stroke = "none")),
    move_up = list(polyline("6,14 12,8 18,14")),
    move_down = list(polyline("6,10 12,16 18,10")),
    list(rect(4, 4, 16, 16, rx = 3))
  )

  htmltools::tags$svg(
    class = "bp-icon",
    viewBox = "0 0 24 24",
    width = size,
    height = size,
    fill = "none",
    stroke = "currentColor",
    `stroke-width` = "1.75",
    `stroke-linecap` = "round",
    `stroke-linejoin` = "round",
    role = if (is.null(label)) NULL else "img",
    `aria-label` = label,
    `aria-hidden` = if (is.null(label)) "true" else NULL,
    children
  )
}

bp_brand_mark <- function(size = 34) {
  htmltools::tags$svg(
    class = "bp-brand-mark",
    viewBox = "0 0 40 40",
    width = size,
    height = size,
    fill = "none",
    stroke = "currentColor",
    `stroke-width` = "2.6",
    `stroke-linejoin` = "round",
    `aria-hidden` = "true",
    htmltools::tags$path(d = "M20 3 35 11.5v17L20 37 5 28.5v-17L20 3Z"),
    htmltools::tags$path(d = "M20 3v34M5 11.5l15 8.5 15-8.5M5 28.5 20 20l15 8.5")
  )
}

bp_state_label <- function(state) {
  switch(
    state,
    unset = "Unset",
    explicit = "Explicit",
    explicit_default = "Explicit default",
    explicit_null = "NULL",
    explicit_na = "NA",
    raw_expression = "Raw expression",
    missing = "Missing",
    inherited = "Inherited",
    state
  )
}

bp_state_options <- function() {
  c(
    "Unset" = "unset",
    "Explicit" = "explicit",
    "Explicit default" = "explicit_default",
    "NULL" = "explicit_null",
    "NA" = "explicit_na",
    "Raw expression" = "raw_expression"
  )
}

bp_highlight_r_line <- function(text) {
  pattern <- paste0(
    '"(?:\\\\.|[^"\\\\])*"',
    "|'(?:\\\\.|[^'\\\\])*'",
    "|#[^\\n]*",
    "|\\b(?:TRUE|FALSE|NULL|NA(?:_(?:integer|real|character|complex)_)?|NaN|Inf)\\b",
    "|(?<![A-Za-z0-9_.])(?:[0-9]+(?:\\.[0-9]+)?|\\.[0-9]+)L?",
    "|[A-Za-z.][A-Za-z0-9._]*(?=\\s*\\()",
    "|<-|::|\\+"
  )
  matches <- gregexpr(pattern, text, perl = TRUE)[[1]]
  if (identical(matches[[1]], -1L)) return(as.character(htmltools::htmlEscape(text)))
  lengths <- attr(matches, "match.length")
  cursor <- 1L
  result <- character()

  for (index in seq_along(matches)) {
    start <- matches[[index]]
    length <- lengths[[index]]
    if (start > cursor) {
      result <- c(result, as.character(htmltools::htmlEscape(substr(text, cursor, start - 1L))))
    }
    token <- substr(text, start, start + length - 1L)
    class <- if (grepl('^["\']', token)) {
      "string"
    } else if (startsWith(token, "#")) {
      "comment"
    } else if (token %in% c("TRUE", "FALSE", "NULL", "NA", "NaN", "Inf") || startsWith(token, "NA_")) {
      "constant"
    } else if (grepl("^[0-9.]", token)) {
      "number"
    } else if (token %in% c("<-", "::", "+")) {
      "operator"
    } else {
      "function"
    }
    result <- c(
      result,
      paste0('<span class="bp-syntax-', class, '">', htmltools::htmlEscape(token), "</span>")
    )
    cursor <- start + length
  }
  if (cursor <= nchar(text)) {
    result <- c(result, as.character(htmltools::htmlEscape(substr(text, cursor, nchar(text)))))
  }
  paste0(result, collapse = "")
}

bp_effective_mapping <- function(project, selected_index) {
  mapping <- list()
  if (length(project$modules)) {
    first <- project$modules[[1]]
    global <- first$arguments$mapping$value
    if (!is.null(global) && identical(bp_value_type(global), "RAesMapping")) {
      mapping <- global$mappings %||% list()
    }
  }
  selected <- project$modules[[selected_index]]
  local <- selected$arguments$mapping$value
  inherit <- selected$arguments$inherit.aes
  if (!is.null(inherit) && identical(inherit$state, "explicit") &&
      identical(bp_value_type(inherit$value), "RLogical") && !isTRUE(inherit$value$value)) {
    mapping <- list()
  }
  if (!is.null(local) && identical(bp_value_type(local), "RAesMapping")) {
    mapping[names(local$mappings)] <- local$mappings
  }
  mapping
}
