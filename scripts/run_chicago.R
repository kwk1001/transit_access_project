#!/usr/bin/env Rscript
project_root <- "/Users/kwk1001/Desktop/transit_access_project"
config_path <- file.path(project_root, "config", "chicago.yml")
pipeline <- file.path(project_root, "scripts", "08_run_pipeline.R")
source_id <- "cmap_2024_2025_phase1"

required <- c(
  config_path,
  pipeline,
  file.path(project_root, "data", "raw", "surveys", "chicago", "MyDailyTravel_2024_2025Phase1.zip")
)
missing <- required[!file.exists(required)]
if (length(missing) > 0) stop(paste("Missing required file(s):", paste(missing, collapse = "
")), call. = FALSE)
setwd(project_root)
message("Chicago pipeline wrapper starting...")
message("Project dir: ", project_root)
status <- system2(file.path(R.home("bin"), "Rscript"), c(pipeline, config_path, source_id), stdout = "", stderr = "")
if (!identical(status, 0L)) stop(paste("Chicago pipeline failed with exit status", status), call. = FALSE)
message("Chicago pipeline wrapper finished.")
