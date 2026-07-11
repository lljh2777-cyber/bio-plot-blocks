#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_path <- normalizePath(sub("^--file=", "", script_arg[[1]]), winslash = "/")
root <- dirname(dirname(script_path))
port <- as.integer(Sys.getenv("BIOPLOTBLOCKS_PORT", "3838"))
host <- Sys.getenv("BIOPLOTBLOCKS_HOST", "127.0.0.1")

shiny::runApp(file.path(root, "app"), host = host, port = port, launch.browser = FALSE)
