get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = FALSE))
  }
  NULL
}

script_path <- get_script_path()
project_root <- if (!is.null(script_path)) normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE) else getwd()

# Main, user-friendly knobs:
pipeline_main <- list(
  config_path = file.path("config", "chicago.yml"),
  source_id = NULL,
  analysis_unit = NULL, # "tract", "zip", or "taz"
  run_label = NULL,
  force_all = FALSE
)

source(file.path(project_root, "R", "packages.R"))
java_boot <- peek_java_config(pipeline_main$config_path)
configure_java_for_r5r(java_memory = java_boot$java_memory, java_active_processors = java_boot$java_active_processors)
source(file.path(project_root, "R", "load_project.R"))
load_project(project_root)

cfg <- load_project_config(pipeline_main$config_path, pipeline_main$source_id)
cfg <- apply_runtime_overrides(cfg, pipeline_main[c("analysis_unit", "run_label", "force_all")])

on.exit({
  try(cleanup_empty_run_dirs(cfg), silent = TRUE)
}, add = TRUE)

ensure_project_dirs(cfg)
write_run_metadata(cfg)

run_download_geography(cfg)
run_download_optional_covariates(cfg)
run_standardize_surveys(cfg, source_ids = pipeline_main$source_id)
run_build_od_weights(cfg, source_id = pipeline_main$source_id)
run_gtfs_supply_compare(cfg)
run_compute_od_travel_times(cfg)
run_aggregate_metrics(cfg)
run_make_maps(cfg)

message("Pipeline complete. Run ID: ", cfg$run$run_id)
