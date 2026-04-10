#!/usr/bin/env Rscript
project_root <- "/Users/kwk1001/Desktop/transit_access_project"
config_path <- file.path(project_root, "config", "philadelphia.yml")
pipeline <- file.path(project_root, "scripts", "08_run_pipeline.R")
source_id <- "philadelphia_public_release"
required <- c(
  config_path,
  pipeline,
  file.path(project_root, "data", "raw", "gtfs", "philadelphia", "SEPTA-08-06.zip"),
  file.path(project_root, "data", "raw", "gtfs", "philadelphia", "SEPTA-08-25.zip"),
  file.path(project_root, "data", "raw", "gtfs", "philadelphia", "SEPTA-09-12.zip")
)
missing <- required[!file.exists(required)]
if (length(missing) > 0) stop(paste("Missing required file(s):", paste(missing, collapse = "
")), call. = FALSE)
legacy <- file.path(project_root, "data", "raw", "surveys", "philadelphia", "publicdb_release (1).zip")
preferred <- file.path(project_root, "data", "raw", "surveys", "philadelphia", "publicdb_release.zip")
if (!file.exists(preferred) && file.exists(legacy)) file.copy(legacy, preferred, overwrite = TRUE)
if (!file.exists(preferred)) stop(paste("Missing required survey zip:", preferred), call. = FALSE)
setwd(project_root)
message("Philadelphia pipeline wrapper starting...")
message("Project dir: ", project_root)
status <- system2(file.path(R.home("bin"), "Rscript"), c(pipeline, config_path, source_id), stdout = "", stderr = "")
if (!identical(status, 0L)) stop(paste("Philadelphia pipeline failed with exit status", status), call. = FALSE)
message("Philadelphia pipeline wrapper finished.")
