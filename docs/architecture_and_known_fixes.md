# Project architecture and fixes

## Core flow

1. Survey adapter standardizes local survey microdata into common tables.
2. OD weights are built from standardized trips under one or more survey scenarios.
3. GTFS plus OSM are used to build R5 networks and tract to tract travel times.
4. OD travel times are aggregated back to origin tract accessibility metrics.
5. Interactive leaflet maps are written under an analysis signature specific run folder.

## Adding a new city

1. Copy `config/template_city.yml`.
2. Add one adapter in `R/adapters_<city>.R`.
3. Register the adapter name in `R/utils_survey_common.R`.
4. Use the single main entry script `scripts/09_run_pipeline_main.R` and switch city/unit via parameters.

## Errors fixed in this unified package

- weighted mean now handles missing weights safely
- county name ambiguity is avoided by exact county lookup and FIPS aware geography handling
- YAML config parsing now supports explicit `survey.source_id`
- empty list config values no longer break parameter flattening
- Philadelphia survey adapter field names were corrected after `clean_names()`
- GTFS service area filtering now repairs invalid geometry and avoids fragile spherical intersection for the service buffer
- map legends show only the current layer
- OD line slider traverses nested GeoJSON layers so slider changes actually update visible lines
- run outputs are separated by analysis signature so changed parameters do not silently reuse stale results
- Chicago and Philadelphia configs now both use explicit source ids and city specific wrappers
