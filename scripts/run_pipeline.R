get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE))
  }
  for (i in rev(seq_along(sys.frames()))) {
    ofile <- sys.frame(i)$ofile
    if (!is.null(ofile)) {
      return(normalizePath(ofile, winslash = "/", mustWork = FALSE))
    }
  }
  NULL
}

script_path <- get_script_path()
scripts_dir <- if (!is.null(script_path)) dirname(script_path) else file.path(getwd(), "scripts")
source(file.path(scripts_dir, "08_run_pipeline.R"))
