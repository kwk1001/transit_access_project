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
  tracts <- sf::st_read(file.path(cfg$paths$geography_dir, "tracts.gpkg"), quiet = TRUE) %>% standardize_tract_id_cols(c("GEOID"))
  tract_centroids <- sf::st_read(file.path(cfg$paths$geography_dir, "tract_centroids.gpkg"), quiet = TRUE) %>% standardize_tract_id_cols(c("tract_id"))
  county_outlines <- sf::st_read(file.path(cfg$paths$geography_dir, "county_outlines.gpkg"), quiet = TRUE)

  list(
    tracts = tracts,
    tract_centroids = tract_centroids,
    county_outlines = county_outlines
  )
}

download_county_tract_geography <- function(cfg) {
  options(tigris_use_cache = TRUE)

  tract_path <- file.path(cfg$paths$geography_dir, "tracts.gpkg")
  centroid_path <- file.path(cfg$paths$geography_dir, "tract_centroids.gpkg")
  county_path <- file.path(cfg$paths$geography_dir, "county_outlines.gpkg")

  if (!isTRUE(cfg$run_options$force_downloads) && all_files_nonempty(c(tract_path, centroid_path, county_path), min_bytes = 100)) {
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

  fs::dir_create(cfg$paths$geography_dir)
  sf::st_write(tracts_sf, tract_path, delete_dsn = TRUE, quiet = TRUE)
  sf::st_write(tract_centroids, centroid_path, delete_dsn = TRUE, quiet = TRUE)
  sf::st_write(county_outlines, county_path, delete_dsn = TRUE, quiet = TRUE)

  invisible(list(tracts = tracts_sf, tract_centroids = tract_centroids, county_outlines = county_outlines))
}

maybe_download_optional_covariates <- function(cfg) {
  if (!isTRUE(cfg$optional_covariates$download_population)) {
    message("Optional ACS population download is disabled in config. Skipping.")
    return(invisible(NULL))
  }

  warning("Automatic ACS covariate download is not implemented in this version. Supply a local tract covariates CSV if you want population multipliers.", call. = FALSE)
  invisible(NULL)
}
