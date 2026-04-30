get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep('^--file=', cmd_args, value = TRUE)
  if (length(file_arg) > 0) return(normalizePath(sub('^--file=', '', file_arg[[1]]), winslash = '/', mustWork = FALSE))
  NULL
}

script_path <- get_script_path()
project_root <- if (!is.null(script_path)) normalizePath(file.path(dirname(script_path), '..'), winslash = '/', mustWork = FALSE) else getwd()

source(file.path(project_root, 'R', 'packages.R'))
source(file.path(project_root, 'R', 'load_project.R'))
load_project(project_root)

cfg_path <- file.path(project_root, 'config', 'boston_mts2011.yml')
cfg <- load_project_config(cfg_path)
cat('=== Boston pipeline diagnostics ===\n')
cat('Config:', cfg_path, '\n')

# 1) OSM vs county coverage
states <- unique(toupper(vapply(cfg$analysis_area$counties, function(x) as.character(x$state), character(1))))
cat('\n[1] County states:', paste(states, collapse = ', '), '\n')
cat('OSM URL:', cfg$osm$download_url, '\n')
if ('NH' %in% states && grepl('massachusetts-latest\\.osm\\.pbf', tolower(cfg$osm$download_url))) {
  cat('WARNING: NH counties present but OSM appears Massachusetts-only.\n')
}

# 2) GTFS files existence and service date coverage check
parse_gtfs_service_dates <- function(zip_path) {
  td <- tempfile('gtfs_diag_')
  dir.create(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(zip_path, exdir = td)

  cal_path <- file.path(td, 'calendar.txt')
  cal_dates_path <- file.path(td, 'calendar_dates.txt')

  dates <- as.Date(character())
  if (file.exists(cal_path)) {
    cal <- readr::read_csv(cal_path, show_col_types = FALSE)
    if (nrow(cal) > 0) {
      all_days <- seq(min(as.Date(as.character(cal$start_date), '%Y%m%d')), max(as.Date(as.character(cal$end_date), '%Y%m%d')), by = 'day')
      w <- as.integer(lubridate::wday(all_days, week_start = 1))
      for (i in seq_len(nrow(cal))) {
        sd <- as.Date(as.character(cal$start_date[[i]]), '%Y%m%d')
        ed <- as.Date(as.character(cal$end_date[[i]]), '%Y%m%d')
        day_seq <- all_days[all_days >= sd & all_days <= ed]
        w2 <- w[all_days >= sd & all_days <= ed]
        keep <- (w2 == 1 & cal$monday[[i]] == 1) | (w2 == 2 & cal$tuesday[[i]] == 1) | (w2 == 3 & cal$wednesday[[i]] == 1) |
          (w2 == 4 & cal$thursday[[i]] == 1) | (w2 == 5 & cal$friday[[i]] == 1) | (w2 == 6 & cal$saturday[[i]] == 1) | (w2 == 7 & cal$sunday[[i]] == 1)
        dates <- c(dates, day_seq[keep])
      }
    }
  }

  if (file.exists(cal_dates_path)) {
    cd <- readr::read_csv(cal_dates_path, show_col_types = FALSE)
    if (nrow(cd) > 0) {
      d <- as.Date(as.character(cd$date), '%Y%m%d')
      add <- d[cd$exception_type == 1]
      rem <- d[cd$exception_type == 2]
      dates <- union(dates, add)
      dates <- setdiff(dates, rem)
    }
  }
  sort(unique(dates))
}

cat('\n[2] GTFS feed checks\n')
for (f in cfg$gtfs_feeds) {
  fpath <- file.path(project_root, f$gtfs_files[[1]])
  cat('- feed:', f$feed_name, '| file:', fpath, '\n')
  cat('  exists:', file.exists(fpath), '\n')
  if (file.exists(fpath)) {
    svc <- tryCatch(parse_gtfs_service_dates(fpath), error = function(e) as.Date(character()))
    cat('  service_dates_count:', length(svc), '\n')
    if (length(svc) > 0) {
      cat('  first_service:', as.character(min(svc)), '| last_service:', as.character(max(svc)), '\n')
    }
  }
}

# 3) Analysis dates selected by current strategy
cat('\n[3] Routing date selection check\n')
routing_dates <- choose_routing_dates(cfg)
cat('selected_dates:', nrow(routing_dates), '\n')
print(routing_dates %>% dplyr::arrange(analysis_date) %>% dplyr::select(period_id, analysis_date, feed_name))

cat('\n[3b] Service intensity for selected routing dates\n')
feed_lookup <- cfg$gtfs_feeds %>% purrr::map_dfr(~ tibble(feed_name = as.character(.x$feed_name), gtfs_file = as.character(.x$gtfs_files[[1]])))
svc_tbl <- purrr::map_dfr(seq_len(nrow(feed_lookup)), function(i) {
  p <- file.path(project_root, feed_lookup$gtfs_file[[i]])
  if (!file.exists(p)) return(tibble(feed_name = feed_lookup$feed_name[[i]], analysis_date = as.Date(character()), n_services = integer(), service_ratio = numeric()))
  svc <- parse_gtfs_service_dates(p)
  if (length(svc) == 0) return(tibble(feed_name = feed_lookup$feed_name[[i]], analysis_date = as.Date(character()), n_services = integer(), service_ratio = numeric()))
  tibble(feed_name = feed_lookup$feed_name[[i]], analysis_date = svc, n_services = 1L) %>% count(feed_name, analysis_date, wt = n_services, name = 'n_services')
})
if (nrow(svc_tbl) > 0) {
  svc_tbl <- svc_tbl %>% group_by(feed_name) %>% mutate(service_ratio = n_services / max(n_services, na.rm = TRUE)) %>% ungroup()
  sel <- routing_dates %>% left_join(svc_tbl, by = c('feed_name', 'analysis_date')) %>% arrange(analysis_date)
  print(sel %>% select(period_id, analysis_date, feed_name, n_services, service_ratio))
}

# 4) Routing point validity
cat('\n[4] Routing points validity\n')
geo <- get_active_geography_for_routing(cfg)
od_path <- file.path(cfg$paths$od_dir, 'od_weights_all.csv.gz')
if (file.exists(od_path)) {
  od_tbl <- read_csv_guess(od_path)
  all_ids <- unique(c(as.character(od_tbl$origin_id), as.character(od_tbl$destination_id)))
  all_ids <- all_ids[!is.na(all_ids) & nzchar(all_ids)]
  cat('using_ids_from_od_weights:', length(all_ids), '\n')
} else {
  all_ids <- unique(c(geo$analysis_zone_centroids$zone_id))
  cat('using_ids_from_analysis_zone_centroids:', length(all_ids), '\n')
}
pts <- make_routing_points(geo$routing_zone_centroids %||% geo$analysis_zone_centroids, all_ids, cfg)
cat('routing_points:', nrow(pts), '\n')
cat('invalid_lonlat_rows:', sum(!is.finite(pts$lon) | !is.finite(pts$lat) | is.na(pts$lon) | is.na(pts$lat)), '\n')
cat('lon_range:', paste(range(pts$lon, na.rm = TRUE), collapse = ' to '), '\n')
cat('lat_range:', paste(range(pts$lat, na.rm = TRUE), collapse = ' to '), '\n')

cat('\nDone.\n')
