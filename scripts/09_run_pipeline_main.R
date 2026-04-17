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
  config_path = file.path("config", "boston.yml"),
  source_id = NULL,
  analysis_unit = "zip", # "tract", "zip", or "taz"
  run_label = NULL,
  force_all = FALSE,
  synthetic_survey_enabled = NULL,
  synthetic_survey_max_travel_minutes = NULL
)

# Optional CLI override:
# Rscript scripts/09_run_pipeline_main.R <config_path> [source_id] [analysis_unit] [run_label] [force_all] [synthetic_survey_enabled] [synthetic_survey_max_travel_minutes]
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1 && nzchar(args[[1]])) pipeline_main$config_path <- args[[1]]
if (length(args) >= 2 && nzchar(args[[2]])) pipeline_main$source_id <- args[[2]]
if (length(args) >= 3 && nzchar(args[[3]])) pipeline_main$analysis_unit <- tolower(args[[3]])
if (length(args) >= 4 && nzchar(args[[4]])) pipeline_main$run_label <- args[[4]]
if (length(args) >= 5 && nzchar(args[[5]])) pipeline_main$force_all <- tolower(args[[5]]) %in% c("1", "true", "t", "yes", "y")
if (length(args) >= 6 && nzchar(args[[6]])) pipeline_main$synthetic_survey_enabled <- tolower(args[[6]]) %in% c("1", "true", "t", "yes", "y")
if (length(args) >= 7 && nzchar(args[[7]])) pipeline_main$synthetic_survey_max_travel_minutes <- suppressWarnings(as.numeric(args[[7]]))

if (!fs::is_absolute_path(pipeline_main$config_path)) {
  pipeline_main$config_path <- normalizePath(file.path(project_root, pipeline_main$config_path), winslash = "/", mustWork = FALSE)
}

source(file.path(project_root, "R", "packages.R"))
java_boot <- peek_java_config(pipeline_main$config_path)
configure_java_for_r5r(java_memory = java_boot$java_memory, java_active_processors = java_boot$java_active_processors)
source(file.path(project_root, "R", "load_project.R"))
load_project(project_root)

cfg <- load_project_config(pipeline_main$config_path, pipeline_main$source_id)
cfg <- apply_runtime_overrides(cfg, pipeline_main[c("analysis_unit", "run_label", "force_all", "synthetic_survey_enabled", "synthetic_survey_max_travel_minutes")])

on.exit({
  try(cleanup_empty_run_dirs(cfg), silent = TRUE)
}, add = TRUE)

run_download_geography(cfg)
run_download_optional_covariates(cfg)
run_standardize_surveys(cfg, source_ids = pipeline_main$source_id)
run_build_od_weights(cfg, source_id = pipeline_main$source_id)
run_gtfs_supply_compare(cfg)
run_compute_od_travel_times(cfg)
run_aggregate_metrics(cfg)
run_make_maps(cfg)

message("Pipeline complete. Run ID: ", cfg$run$run_id)
