#!/usr/bin/env Rscript

get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) return(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE))
  for (i in rev(seq_along(sys.frames()))) {
    ofile <- sys.frame(i)$ofile
    if (!is.null(ofile)) return(normalizePath(ofile, winslash = "/", mustWork = FALSE))
  }
  NULL
}

script_path <- get_script_path()
project_root <- if (!is.null(script_path)) normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE) else getwd()
config_path <- file.path(project_root, "config", "boston_mts2011.yml")
pipeline <- file.path(project_root, "scripts", "08_run_pipeline.R")
source_id <- "boston_mts2011"

required <- c(
  config_path,
  pipeline,
  file.path(project_root, "data", "raw", "surveys", "boston", "HH.zip")
)
missing <- required[!file.exists(required)]
if (length(missing) > 0) {
  stop(paste("Missing required file(s):", paste(missing, collapse = "\n")), call. = FALSE)
}

setwd(project_root)
message("Boston pipeline wrapper starting...")
message("Project dir: ", project_root)
status <- system2(file.path(R.home("bin"), "Rscript"), c(pipeline, config_path, source_id), stdout = "", stderr = "")
if (!identical(status, 0L)) stop(paste("Boston pipeline failed with exit status", status), call. = FALSE)
message("Boston pipeline wrapper finished.")
