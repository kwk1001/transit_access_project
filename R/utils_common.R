zero_pad <- function(x, width) {
  x_chr <- as.character(x)
  x_chr[is.na(x)] <- NA_character_
  stringr::str_pad(x_chr, width = width, side = "left", pad = "0")
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

safe_integer <- function(x) {
  suppressWarnings(as.integer(x))
}

coalesce_na_character <- function(x, y) {
  dplyr::coalesce(as.character(x), as.character(y))
}

standardize_geoid11 <- function(x) {
  if (is.null(x)) {
    return(character())
  }

  x_chr <- as.character(x)
  x_chr <- stringr::str_replace_all(x_chr, "[^0-9]", "")
  x_chr[x_chr == ""] <- NA_character_
  ifelse(is.na(x_chr), NA_character_, stringr::str_pad(x_chr, width = 11, side = "left", pad = "0"))
}

standardize_zone_id <- function(x, unit = "tract") {
  unit_use <- normalize_analysis_unit(unit)
  x_chr <- as.character(x)
  x_chr <- trimws(x_chr)
  x_chr[x_chr == ""] <- NA_character_

  if (unit_use %in% c("tract", "census_tract")) {
    return(standardize_geoid11(x_chr))
  }

  if (unit_use %in% c("zip", "zipcode", "zcta")) {
    digits <- stringr::str_replace_all(x_chr, "[^0-9]", "")
    digits[digits == ""] <- NA_character_
    return(ifelse(is.na(digits), NA_character_, stringr::str_pad(digits, width = 5, side = "left", pad = "0")))
  }

  x_chr
}

normalize_analysis_unit <- function(unit) {
  unit_chr <- tolower(trimws(as.character(unit %||% "tract")))
  unit_chr <- stringr::str_replace_all(unit_chr, "[-\\s]+", "_")
  dplyr::case_when(
    unit_chr %in% c("tract", "census_tract") ~ "tract",
    unit_chr %in% c("zip", "zipcode", "zip_code", "zcta") ~ "zip",
    unit_chr == "taz" ~ "taz",
    TRUE ~ unit_chr
  )
}

standardize_tract_id_cols <- function(df, cols = c("GEOID", "tract_id", "origin_id", "destination_id", "from_id", "to_id", "origin_tract", "destination_tract", "home_tract", "work_tract", "school_tract")) {
  cols_use <- intersect(cols, names(df))
  for (nm in cols_use) {
    df[[nm]] <- standardize_geoid11(df[[nm]])
  }
  df
}

coerce_flag <- function(x) {
  if (is.logical(x)) {
    return(dplyr::coalesce(x, FALSE))
  }

  x_chr <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    is.na(x_chr) ~ FALSE,
    x_chr %in% c("true", "t", "1", "yes", "y") ~ TRUE,
    x_chr %in% c("false", "f", "0", "no", "n") ~ FALSE,
    TRUE ~ FALSE
  )
}

build_geoid11_from_components <- function(state_fips, county_fips, tract_fips) {
  state_part <- zero_pad(state_fips, 2)
  county_part <- zero_pad(county_fips, 3)
  tract_part <- zero_pad(tract_fips, 6)
  out <- paste0(state_part, county_part, tract_part)
  out[is.na(state_part) | is.na(county_part) | is.na(tract_part)] <- NA_character_
  out
}

hms_to_seconds <- function(x) {
  if (inherits(x, "hms")) {
    return(as.numeric(x))
  }

  x_chr <- as.character(x)
  x_chr[is.na(x_chr) | x_chr == ""] <- NA_character_
  parts <- stringr::str_split_fixed(x_chr, ":", 3)
  ok <- !is.na(x_chr)

  out <- rep(NA_real_, length(x_chr))
  out[ok] <- safe_numeric(parts[ok, 1]) * 3600 + safe_numeric(parts[ok, 2]) * 60 + safe_numeric(parts[ok, 3])
  out
}

hhmm_to_seconds <- function(x) {
  x_chr <- as.character(x)
  x_chr[is.na(x_chr) | x_chr == ""] <- NA_character_
  parts <- stringr::str_split_fixed(x_chr, ":", 2)
  ok <- !is.na(x_chr)

  out <- rep(NA_real_, length(x_chr))
  out[ok] <- safe_numeric(parts[ok, 1]) * 3600 + safe_numeric(parts[ok, 2]) * 60
  out
}

time_to_minutes_of_day <- function(seconds_vec) {
  ifelse(is.na(seconds_vec), NA_real_, seconds_vec / 60)
}

window_minutes <- function(start_time, end_time) {
  (hms_to_seconds(end_time) - hms_to_seconds(start_time)) / 60
}

combine_date_time <- function(date_vec, time_chr, tz = "UTC") {
  time_chr <- as.character(time_chr)
  lubridate::ymd_hms(paste(as.Date(date_vec), time_chr), tz = tz, quiet = TRUE)
}

weighted_mean_safe <- function(x, weights) {
  x_num <- safe_numeric(x)
  w_num <- safe_numeric(weights)
  ok <- !(is.na(x_num) | is.na(w_num))

  if (!any(ok)) {
    return(NA_real_)
  }

  x_num <- x_num[ok]
  w_num <- w_num[ok]
  w_sum <- sum(w_num, na.rm = TRUE)

  if (!is.finite(w_sum) || is.na(w_sum) || w_sum <= 0) {
    return(NA_real_)
  }

  stats::weighted.mean(x_num, w_num, na.rm = TRUE)
}

mean_or_na <- function(x) {
  x_num <- safe_numeric(x)
  if (all(is.na(x_num))) {
    return(NA_real_)
  }
  mean(x_num, na.rm = TRUE)
}

mean_penalized <- function(x, penalty) {
  x_num <- safe_numeric(x)
  mean(dplyr::coalesce(x_num, penalty), na.rm = TRUE)
}

validate_gtfs_zip_quick <- function(gtfs_path) {
  if (!file_is_nonempty(gtfs_path, min_bytes = 1024)) {
    stop(paste0("GTFS file is missing or empty: ", gtfs_path), call. = FALSE)
  }

  listed <- tryCatch(utils::unzip(gtfs_path, list = TRUE), error = function(e) NULL)
  if (is.null(listed) || nrow(listed) == 0) {
    stop(paste0("Could not inspect GTFS zip: ", gtfs_path), call. = FALSE)
  }

  files <- tolower(basename(listed$Name))
  required <- c("stops.txt", "routes.txt", "trips.txt", "stop_times.txt")
  missing <- setdiff(required, files)
  if (length(missing) > 0) {
    stop(paste0("GTFS zip is missing required files: ", paste(missing, collapse = ", "), " in ", gtfs_path), call. = FALSE)
  }

  invisible(TRUE)
}

read_csv_guess <- function(path, ...) {
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE, guess_max = 100000, ...)
}

read_csv_raw_character <- function(path, ...) {
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA", "N/A", "NULL"),
    show_col_types = FALSE,
    progress = FALSE,
    ...
  )
}

write_csv_gz <- function(df, path) {
  fs::dir_create(dirname(path))
  readr::write_csv(df, path)
}

first_non_missing <- function(x) {
  y <- x[!is.na(x)]
  if (length(y) == 0) return(NA_character_)
  as.character(y[[1]])
}

write_json_pretty <- function(x, path) {
  fs::dir_create(dirname(path))
  jsonlite::write_json(x, path, pretty = TRUE, auto_unbox = TRUE, na = "null")
}

add_period_comparison_ids <- function(period_ids) {
  period_ids_use <- period_ids %>%
    as.character() %>%
    trimws()
  period_ids_use <- period_ids_use[!is.na(period_ids_use) & nzchar(period_ids_use) & tolower(period_ids_use) != "na"] %>% unique()

  if (length(period_ids_use) < 2) {
    return(tibble::tibble(base_period_id = character(), compare_period_id = character(), comparison_id = character()))
  }

  utils::combn(period_ids_use, 2, simplify = FALSE) %>%
    purrr::map_dfr(~ tibble(
      base_period_id = .x[[1]],
      compare_period_id = .x[[2]],
      comparison_id = paste0(.x[[2]], "_vs_", .x[[1]])
    ))
}

normalize_periods_tbl <- function(periods_tbl) {
  out <- tibble::as_tibble(periods_tbl)

  if ("start_date" %in% names(out)) {
    out <- out %>% rename(period_start = start_date)
  }

  if ("end_date_exclusive" %in% names(out)) {
    out <- out %>% rename(period_end_exclusive = end_date_exclusive)
  }

  if ("period_end" %in% names(out) && !"period_end_exclusive" %in% names(out)) {
    out <- out %>% mutate(period_end_exclusive = as.Date(period_end) + 1)
  }

  out %>%
    mutate(
      period_start = as.Date(period_start),
      period_end_exclusive = as.Date(period_end_exclusive)
    ) %>%
    select(period_id, period_label, period_start, period_end_exclusive)
}

choose_feed_name <- function(target_date, registry_tbl) {
  candidates <- registry_tbl %>%
    dplyr::filter(valid_start <= as.Date(target_date), valid_end_exclusive > as.Date(target_date)) %>%
    dplyr::arrange(dplyr::desc(valid_start))

  if (nrow(candidates) == 0) {
    return(NA_character_)
  }

  candidates$feed_name[[1]]
}

build_analysis_dates <- function(periods, weekday_only, excluded_dates, feed_registry) {
  out <- periods %>%
    rowwise() %>%
    mutate(analysis_date = list(seq(as.Date(period_start), as.Date(period_end_exclusive) - 1, by = "day"))) %>%
    tidyr::unnest(analysis_date) %>%
    ungroup()

  if (isTRUE(weekday_only)) {
    out <- out %>%
      filter(lubridate::wday(analysis_date, week_start = 1) <= 5)
  }

  if (!is.null(excluded_dates) && length(excluded_dates) > 0) {
    ex_dates <- as.Date(unlist(excluded_dates))
    out <- out %>% filter(!analysis_date %in% ex_dates)
  }

  out %>%
    mutate(feed_name = purrr::map_chr(analysis_date, choose_feed_name, registry_tbl = feed_registry)) %>%
    filter(!is.na(feed_name))
}

sample_dates_evenly <- function(date_tbl, n_per_period = 3) {
  date_tbl %>%
    group_by(period_id, period_label) %>%
    group_modify(~ {
      if (nrow(.x) <= n_per_period) {
        return(.x)
      }
      idx <- unique(round(seq(1, nrow(.x), length.out = n_per_period)))
      .x[idx, , drop = FALSE]
    }) %>%
    ungroup()
}

file_is_nonempty <- function(path, min_bytes = 1) {
  file.exists(path) && !dir.exists(path) && is.finite(file.info(path)$size[[1]]) && file.info(path)$size[[1]] >= min_bytes
}

all_files_nonempty <- function(paths, min_bytes = 1) {
  if (length(paths) == 0) {
    return(FALSE)
  }
  all(vapply(paths, file_is_nonempty, logical(1), min_bytes = min_bytes))
}

output_is_fresh <- function(output_path, input_paths = character(), min_bytes = 1) {
  if (!file_is_nonempty(output_path, min_bytes = min_bytes)) {
    return(FALSE)
  }

  input_paths <- unique(input_paths[file.exists(input_paths)])
  if (length(input_paths) == 0) {
    return(TRUE)
  }

  out_mtime <- file.info(output_path)$mtime[[1]]
  in_mtime <- max(file.info(input_paths)$mtime, na.rm = TRUE)
  is.finite(as.numeric(out_mtime)) && is.finite(as.numeric(in_mtime)) && out_mtime >= in_mtime
}

text_md5 <- function(text) {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(enc2utf8(text), tmp, useBytes = TRUE)
  unname(tools::md5sum(tmp))
}

sanitize_path_component <- function(x) {
  x <- as.character(x %||% "")
  x <- trimws(x)
  x <- stringr::str_replace_all(x, "[^A-Za-z0-9_]+", "_")
  x <- stringr::str_replace_all(x, "_+", "_")
  x <- stringr::str_replace_all(x, "^_|_$", "")
  ifelse(!nzchar(x), "item", x)
}

flatten_list_for_table <- function(x, prefix = NULL) {
  key <- prefix %||% "value"

  if (is.null(x)) {
    return(tibble(parameter = key, value = NA_character_))
  }

  if (is.data.frame(x)) {
    return(flatten_list_for_table(as.list(x), prefix = key))
  }

  if (is.atomic(x) && length(x) <= 1 && !is.list(x)) {
    return(tibble(parameter = key, value = as.character(x)))
  }

  if (is.atomic(x) && !is.list(x)) {
    return(tibble(parameter = key, value = paste(as.character(x), collapse = "; ")))
  }

  if (is.list(x) && length(x) == 0) {
    return(tibble(parameter = key, value = ""))
  }

  out <- purrr::imap_dfr(x, function(val, nm) {
    nm_use <- if (is.null(nm) || !nzchar(nm)) "item" else nm
    prefix_use <- if (is.null(prefix) || !nzchar(prefix)) nm_use else paste(prefix, nm_use, sep = ".")
    flatten_list_for_table(val, prefix_use)
  })

  if (!all(c("parameter", "value") %in% names(out))) {
    out <- tibble(parameter = key, value = paste(capture.output(str(x, max.level = 1)), collapse = " | "))
  }

  out %>% dplyr::distinct(parameter, .keep_all = TRUE)
}

write_run_metadata <- function(cfg) {
  fs::dir_create(cfg$paths$metadata_dir)
  fs::dir_create(cfg$paths$logs_dir)

  manifest <- list(
    city_id = cfg$project$city_id,
    city_label = cfg$project$city_label,
    survey_source_id = cfg$active_survey_source_id,
    run_id = cfg$run$run_id,
    run_label = cfg$run$run_label,
    analysis_signature = cfg$run$analysis_signature,
    analysis_signature_short = cfg$run$analysis_signature_short,
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    config_path = cfg$project$config_path,
    analysis_config = cfg$run$analysis_config,
    runtime_config = cfg$run$runtime_config,
    paths = cfg$paths
  )

  write_json_pretty(manifest, file.path(cfg$paths$metadata_dir, "run_manifest.json"))

  param_table <- dplyr::bind_rows(
    flatten_list_for_table(cfg$run$analysis_config, prefix = "analysis"),
    flatten_list_for_table(cfg$run$runtime_config, prefix = "runtime")
  )
  readr::write_csv(param_table, file.path(cfg$paths$metadata_dir, "parameters_flat.csv"))

  execution_stamp <- tibble(
    run_id = cfg$run$run_id,
    analysis_signature = cfg$run$analysis_signature,
    executed_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    config_path = cfg$project$config_path
  )
  stamp_path <- file.path(cfg$paths$logs_dir, "execution_latest.csv")
  readr::write_csv(execution_stamp, stamp_path)

  invisible(manifest)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}
