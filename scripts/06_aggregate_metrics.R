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

bootstrap_config_args <- function(default_config, project_root) {
  args <- commandArgs(trailingOnly = TRUE)
  config_path <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else default_config
  if (!grepl("^(~|/|[A-Za-z]:)", config_path)) {
    config_path <- file.path(project_root, config_path)
  }
  source_id <- if (length(args) >= 2 && nzchar(args[[2]])) args[[2]] else NULL
  list(config_path = normalizePath(config_path, winslash = "/", mustWork = FALSE), source_id = source_id)
}

script_path <- get_script_path()
project_root <- if (!is.null(script_path)) {
  normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
} else {
  getwd()
}
source(file.path(project_root, "R", "packages.R"))
boot_args <- bootstrap_config_args(default_config = file.path("config", "chicago.yml"), project_root = project_root)
java_boot <- peek_java_config(boot_args$config_path)
configure_java_for_r5r(java_memory = java_boot$java_memory, java_active_processors = java_boot$java_active_processors)
source(file.path(project_root, "R", "load_project.R"))
load_project(project_root)

args <- parse_args_config(default_config = file.path("config", "chicago.yml"), project_root = project_root)
cfg <- load_project_config(args$config_path, args$source_id)
run_aggregate_metrics(cfg)
message("Accessibility aggregation complete for ", cfg$project$city_id)
