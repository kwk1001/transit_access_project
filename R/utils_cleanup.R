prune_empty_dirs <- function(root_dir) {
  if (is.null(root_dir) || !dir.exists(root_dir)) return(invisible(character()))

  removed <- character()

  repeat {
    all_dirs <- list.dirs(root_dir, recursive = TRUE, full.names = TRUE)
    if (length(all_dirs) == 0) break

    all_dirs <- sort(all_dirs, decreasing = TRUE)
    removed_this_round <- character()

    for (d in all_dirs) {
      if (!dir.exists(d)) next
      contents <- list.files(d, all.files = TRUE, no.. = TRUE, full.names = TRUE)
      if (length(contents) == 0) {
        unlink(d, recursive = FALSE, force = TRUE)
        if (!dir.exists(d)) {
          removed_this_round <- c(removed_this_round, d)
        }
      }
    }

    if (length(removed_this_round) == 0) break
    removed <- c(removed, removed_this_round)
  }

  invisible(unique(removed))
}

cleanup_empty_run_dirs <- function(cfg) {
  project_root <- cfg$paths$project_root %||% "."
  city_id <- cfg$project$city_id
  source_id <- cfg$active_survey_source$source_id
  unit_id <- normalize_analysis_unit(cfg$geography$analysis_unit %||% "tract")

  processed_root <- file.path(project_root, "data", "processed", city_id, "runs", source_id, unit_id)
  maps_root <- file.path(project_root, "outputs", city_id, "maps", source_id, unit_id)
  logs_root <- file.path(project_root, "logs", city_id, source_id, unit_id)

  removed_processed <- prune_empty_dirs(processed_root)
  removed_maps <- prune_empty_dirs(maps_root)
  removed_logs <- prune_empty_dirs(logs_root)

  removed <- unique(c(removed_processed, removed_maps, removed_logs))
  if (length(removed) > 0) {
    message("Removed empty run directories:")
    message(paste0("  - ", removed), sep = "\n")
  } else {
    message("No empty run directories found.")
  }

  invisible(removed)
}
