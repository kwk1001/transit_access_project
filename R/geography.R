download_osm_pbf_if_needed <- function(cfg) {
  fs::dir_create(dirname(cfg$osm$local_pbf_path))

  if (file_is_nonempty(cfg$osm$local_pbf_path, min_bytes = 1024 * 1024)) {
    message("Using existing OSM PBF at ", cfg$osm$local_pbf_path)
    return(invisible(cfg$osm$local_pbf_path))
  }

  message("Downloading OSM PBF from ", cfg$osm$download_url)
  utils::download.file(
    url = cfg$osm$download_url,
    destfile = cfg$osm$local_pbf_path,
    mode = "wb",
    quiet = FALSE
  )

  if (!file_is_nonempty(cfg$osm$local_pbf_path, min_bytes = 1024 * 1024)) {
    stop("Downloaded OSM PBF is missing or too small. Delete the file and try again.", call. = FALSE)
  }

  invisible(cfg$osm$local_pbf_path)
}

lookup_county_fips_exact <- function(state, county_name, year) {
  county_tbl <- tigris::counties(state = state, cb = TRUE, year = year, class = "sf", progress_bar = FALSE)
  row_i <- county_tbl %>% dplyr::filter(NAME == county_name)

  if (nrow(row_i) == 0) {
    stop(paste0("Could not find county `", county_name, "` in state `", state, "`."), call. = FALSE)
  }

  row_i %>% dplyr::slice(1)
}

read_geography_outputs <- function(cfg) {
  core_files <- c(
    file.path(cfg$paths$geography_dir, "tracts.gpkg"),
    file.path(cfg$paths$geography_dir, "tract_centroids.gpkg"),
    file.path(cfg$paths$geography_dir, "county_outlines.gpkg")
  )
  zone_files <- c(
    file.path(cfg$paths$geography_dir, "analysis_zones.gpkg"),
    file.path(cfg$paths$geography_dir, "analysis_zone_centroids.gpkg"),
    file.path(cfg$paths$geography_dir, "tract_to_analysis_zone.csv")
  )
  needs_zone_files <- tolower(cfg$geography$analysis_unit %||% "tract") != "tract"

  if (!all_files_nonempty(core_files, min_bytes = 100) || (needs_zone_files && !all_files_nonempty(zone_files, min_bytes = 50))) {
    message("Geography files are missing for unit `", cfg$geography$analysis_unit, "`. Rebuilding geography outputs automatically.")
    download_county_tract_geography(cfg)
  }

  tracts <- sf::st_read(file.path(cfg$paths$geography_dir, "tracts.gpkg"), quiet = TRUE) %>% standardize_tract_id_cols(c("GEOID"))
  tract_centroids <- sf::st_read(file.path(cfg$paths$geography_dir, "tract_centroids.gpkg"), quiet = TRUE) %>% standardize_tract_id_cols(c("tract_id"))
  county_outlines <- sf::st_read(file.path(cfg$paths$geography_dir, "county_outlines.gpkg"), quiet = TRUE)
  zones_path <- file.path(cfg$paths$geography_dir, "analysis_zones.gpkg")
  zone_centroids_path <- file.path(cfg$paths$geography_dir, "analysis_zone_centroids.gpkg")
  crosswalk_path <- file.path(cfg$paths$geography_dir, "tract_to_analysis_zone.csv")

  analysis_zones <- if (file_is_nonempty(zones_path, 100)) sf::st_read(zones_path, quiet = TRUE) else tracts %>% dplyr::transmute(zone_id = GEOID, zone_name = NAME, geometry)
  analysis_zone_centroids <- if (file_is_nonempty(zone_centroids_path, 100)) sf::st_read(zone_centroids_path, quiet = TRUE) else tract_centroids %>% dplyr::transmute(zone_id = tract_id, geometry)
  tract_to_zone <- if (file_is_nonempty(crosswalk_path, 10)) read_csv_guess(crosswalk_path) else tibble(tract_id = tracts$GEOID, zone_id = tracts$GEOID)

  analysis_zones <- analysis_zones %>% mutate(zone_id = standardize_zone_id(zone_id, cfg$geography$analysis_unit), zone_name = as.character(zone_name))
  analysis_zone_centroids <- analysis_zone_centroids %>% mutate(zone_id = standardize_zone_id(zone_id, cfg$geography$analysis_unit))
  tract_to_zone <- tract_to_zone %>% mutate(tract_id = standardize_geoid11(tract_id), zone_id = standardize_zone_id(zone_id, cfg$geography$analysis_unit))

  list(
    tracts = tracts,
    tract_centroids = tract_centroids,
    county_outlines = county_outlines,
    analysis_zones = analysis_zones,
    analysis_zone_centroids = analysis_zone_centroids,
    tract_to_zone = tract_to_zone
  )
}

build_analysis_zones <- function(cfg, tracts_sf) {
  unit <- tolower(cfg$geography$analysis_unit %||% "tract")

  if (unit %in% c("tract", "census_tract")) {
    zones <- tracts_sf %>% dplyr::transmute(zone_id = GEOID, zone_name = NAME, geometry)
    crosswalk <- tibble(tract_id = tracts_sf$GEOID, zone_id = tracts_sf$GEOID)
    return(list(zones = zones, crosswalk = crosswalk))
  }

  if (unit %in% c("zip", "zipcode", "zcta")) {
    zcta_year <- as.integer(cfg$analysis_area$zip_zcta_year %||% 2020L)
    if (is.na(zcta_year) || zcta_year <= 0) zcta_year <- 2020L
    if (zcta_year > 2020L) {
      message("Requested ZCTA year ", zcta_year, " is not available as CB release; forcing year 2020.")
      zcta_year <- 2020L
    }

    zcta_all <- tigris::zctas(year = zcta_year, class = "sf", cb = TRUE, progress_bar = FALSE) %>%
      sf::st_transform(4326) %>%
      {
        zcta_id_col <- dplyr::first(intersect(c("ZCTA5CE20", "ZCTA5CE10", "GEOID20", "GEOID10", "ZCTA5CE"), names(.)))
        if (is.null(zcta_id_col) || is.na(zcta_id_col) || !nzchar(zcta_id_col)) {
          stop("Could not identify a ZIP/ZCTA id column in tigris::zctas() output.", call. = FALSE)
        }
        dplyr::transmute(., zone_id = standardize_zone_id(.data[[zcta_id_col]], "zip"), zone_name = paste0("ZIP ", standardize_zone_id(.data[[zcta_id_col]], "zip")), geometry)
      }

    tract_pts <- tracts_sf %>% dplyr::transmute(tract_id = GEOID, geometry = sf::st_point_on_surface(geometry))
    tract_match <- sf::st_join(tract_pts, zcta_all, left = TRUE, join = sf::st_within) %>%
      sf::st_drop_geometry() %>%
      dplyr::select(tract_id, zone_id)
    if (any(is.na(tract_match$zone_id))) {
      idx_missing <- which(is.na(tract_match$zone_id))
      nearest_idx <- sf::st_nearest_feature(tract_pts[idx_missing, , drop = FALSE], zcta_all)
      tract_match$zone_id[idx_missing] <- zcta_all$zone_id[nearest_idx]
    }

    zones <- zcta_all %>% dplyr::filter(zone_id %in% unique(tract_match$zone_id))
    return(list(zones = zones, crosswalk = tract_match))
  }

  if (unit == "taz") {
    taz_file <- cfg$analysis_area$taz_file
    if (is.null(taz_file) || !file.exists(taz_file)) {
      stop("For analysis_unit = 'taz', provide analysis_area.taz_file in config.", call. = FALSE)
    }
    taz_sf <- sf::st_read(taz_file, quiet = TRUE) %>% sf::st_transform(4326)
    taz_id_col <- cfg$analysis_area$taz_id_col %||% "taz_id"
    taz_name_col <- cfg$analysis_area$taz_name_col
    if (!taz_id_col %in% names(taz_sf)) {
      stop("TAZ file is missing configured taz_id_col.", call. = FALSE)
    }
    zone_name_vec <- if (!is.null(taz_name_col) && taz_name_col %in% names(taz_sf)) as.character(taz_sf[[taz_name_col]]) else as.character(taz_sf[[taz_id_col]])
    zones <- taz_sf %>% dplyr::transmute(zone_id = as.character(.data[[taz_id_col]]), zone_name = zone_name_vec, geometry)

    tract_pts <- tracts_sf %>% dplyr::transmute(tract_id = GEOID, geometry = sf::st_point_on_surface(geometry))
    tract_match <- sf::st_join(tract_pts, zones, left = TRUE, join = sf::st_within) %>% sf::st_drop_geometry() %>% dplyr::select(tract_id, zone_id)
    tract_match$zone_id[is.na(tract_match$zone_id)] <- tract_match$tract_id[is.na(tract_match$zone_id)]
    return(list(zones = zones, crosswalk = tract_match))
  }

  stop("Unsupported geography.analysis_unit. Use tract, zip, or taz.", call. = FALSE)
}

download_county_tract_geography <- function(cfg) {
  options(tigris_use_cache = TRUE)

  tract_path <- file.path(cfg$paths$geography_dir, "tracts.gpkg")
  centroid_path <- file.path(cfg$paths$geography_dir, "tract_centroids.gpkg")
  county_path <- file.path(cfg$paths$geography_dir, "county_outlines.gpkg")
  zones_path <- file.path(cfg$paths$geography_dir, "analysis_zones.gpkg")
  zone_centroids_path <- file.path(cfg$paths$geography_dir, "analysis_zone_centroids.gpkg")
  crosswalk_path <- file.path(cfg$paths$geography_dir, "tract_to_analysis_zone.csv")

  if (!isTRUE(cfg$run_options$force_downloads) && all_files_nonempty(c(tract_path, centroid_path, county_path, zones_path, zone_centroids_path, crosswalk_path), min_bytes = 50)) {
    message("Using cached tract and county geography.")
    return(read_geography_outputs(cfg))
  }

  county_sf_list <- purrr::map(cfg$analysis_area$counties, function(spec) {
    lookup_county_fips_exact(spec$state, spec$county, cfg$analysis_area$county_outline_year) %>%
      sf::st_transform(4326) %>%
      dplyr::select(STATEFP, COUNTYFP, GEOID, NAME, geometry)
  })

  tract_sf_list <- purrr::map(cfg$analysis_area$counties, function(spec) {
    county_row <- lookup_county_fips_exact(spec$state, spec$county, cfg$analysis_area$county_outline_year)
    tigris::tracts(
      state = spec$state,
      county = county_row$COUNTYFP[[1]],
      year = cfg$analysis_area$tract_year,
      cb = TRUE,
      class = "sf",
      progress_bar = FALSE
    ) %>%
      sf::st_transform(4326)
  })

  county_outlines <- dplyr::bind_rows(county_sf_list)
  tracts_sf <- dplyr::bind_rows(tract_sf_list) %>%
    dplyr::select(GEOID, NAME, ALAND, AWATER, geometry) %>%
    mutate(GEOID = standardize_geoid11(GEOID))

  tract_points <- suppressWarnings(sf::st_point_on_surface(sf::st_geometry(tracts_sf)))
  tract_centroids <- sf::st_as_sf(tibble(tract_id = tracts_sf$GEOID, geometry = tract_points), crs = 4326)
  zone_build <- build_analysis_zones(cfg, tracts_sf)
  zone_centroids <- suppressWarnings(sf::st_point_on_surface(sf::st_geometry(zone_build$zones)))
  zone_centroids_sf <- sf::st_as_sf(tibble(zone_id = zone_build$zones$zone_id, geometry = zone_centroids), crs = 4326)

  fs::dir_create(cfg$paths$geography_dir)
  sf::st_write(tracts_sf, tract_path, delete_dsn = TRUE, quiet = TRUE)
  sf::st_write(tract_centroids, centroid_path, delete_dsn = TRUE, quiet = TRUE)
  sf::st_write(county_outlines, county_path, delete_dsn = TRUE, quiet = TRUE)
  sf::st_write(zone_build$zones, file.path(cfg$paths$geography_dir, "analysis_zones.gpkg"), delete_dsn = TRUE, quiet = TRUE)
  sf::st_write(zone_centroids_sf, file.path(cfg$paths$geography_dir, "analysis_zone_centroids.gpkg"), delete_dsn = TRUE, quiet = TRUE)
  readr::write_csv(zone_build$crosswalk, file.path(cfg$paths$geography_dir, "tract_to_analysis_zone.csv"))

  invisible(list(tracts = tracts_sf, tract_centroids = tract_centroids, county_outlines = county_outlines, analysis_zones = zone_build$zones, analysis_zone_centroids = zone_centroids_sf, tract_to_zone = zone_build$crosswalk))
}

maybe_download_optional_covariates <- function(cfg) {
  if (!isTRUE(cfg$optional_covariates$download_population)) {
    message("Optional ACS population download is disabled in config. Skipping.")
    return(invisible(NULL))
  }

  warning("Automatic ACS covariate download is not implemented in this version. Supply a local tract covariates CSV if you want population multipliers.", call. = FALSE)
  invisible(NULL)
}
