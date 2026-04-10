# Transit accessibility workflow project

This project is an end to end R workflow for tract level transit accessibility analysis using:

1. local household travel survey data to build tract to tract OD weights
2. GTFS feeds to measure service changes across named study periods
3. `r5r` to compute tract to tract transit travel times
4. weighted aggregation back to tract level
5. interactive `leaflet` HTML maps

The code is organized so that city specific survey logic lives in separate adapters, while the OD builder, GTFS comparison, routing, aggregation, and mapping steps stay generic.

## What was fixed in this revision

This version resolves the main issues from the earlier draft:

- one unified config loader for both `.R` and `.yml` configs
- a robust `weighted_mean_safe()` that drops missing values in both the value and weight vectors before testing the weight sum
- exact county lookup through `tigris::counties()` followed by tract download with county FIPS, which avoids the `Will` and `Williamson` ambiguity warning
- raw survey CSV readers now ingest all columns as character and convert selected fields explicitly, which removes most `readr` parsing warnings
- period boundaries are treated as closed open intervals, so the date at a boundary is not double counted
- the step scripts and the full pipeline script now all call the same code path
- unreachable OD pairs are preserved during aggregation rather than disappearing when travel times are missing

## Included survey adapters

- Chicago 2024 to 2025 Phase 1 CMAP household travel survey
- Chicago 2018 to 2019 My Daily Travel survey
- Philadelphia DVRPC public household travel survey database

Chicago is configured as the primary default city. Philadelphia is included as a second reusable config.

## Important design choices

### Periods are treated as closed open intervals

The three study periods are stored as:

- `2025-08-04` to `2025-08-25`
- `2025-08-25` to `2025-09-08`
- `2025-09-08` to `2025-09-29`

In the code, the `period_end_exclusive` field is exclusive. This avoids double counting boundary dates such as August 25 and September 8.

### `r5r` is run using each named time window directly

For each study day and each named window, the routing step calls `travel_time_matrix()` with:

- `departure_datetime` set to the start of the window
- `time_window` set to the full window length in minutes

### Walk speed is stored in km/h

`r5r::travel_time_matrix()` expects `walk_speed` in km/h. The default config uses `4.68`, which is roughly `1.3 m/s`.

## Folder structure

```text
transit_access_project/
  R/
  config/
  scripts/
  docs/
  data/
  outputs/
```

The project writes reusable survey standardization outputs into `data/processed/<city_id>/survey/<source_id>/`. All analysis outputs that depend on parameters are written into a run specific folder keyed by a parameter signature, plus an optional run label. Maps are written into `outputs/<city_id>/maps/<source_id>/<run_id>/`.

## Before you run

### 1. Put the survey files in the expected folders

Chicago:

- `data/raw/surveys/chicago/MyDailyTravel_2024_2025Phase1.zip`
- `data/raw/surveys/chicago/MyDailyTravel_2018_2019.zip`

Philadelphia:

- `data/raw/surveys/philadelphia/publicdb_release.zip`

### 2. Put the GTFS zip files in the expected folders

Chicago defaults:

- `data/raw/gtfs/chicago/CTA-07-11.zip`
- `data/raw/gtfs/chicago/CTA-08-14.zip`

Philadelphia defaults are placeholders. Update the GTFS registry in the config to match your actual local files.

### 3. Check Java for `r5r`

`r5r` requires Java Development Kit 21.

On macOS, the most common failure is that no JDK is installed, so `/usr/libexec/java_home` cannot find a JVM and `rJava` fails before `r5r` can install.

Recommended order:

```bash
Rscript scripts/00_check_java_r5r.R
Rscript scripts/00_install_packages.R
```

If the check script says no JDK was detected, install JDK 21 first, restart R, and then rerun `00_install_packages.R`.

If you prefer to install Java from inside R, the project also installs `rJavaEnv`, which provides helper functions for Java setup in R projects.

## Run order

From the project root:

```bash
Rscript scripts/00_install_packages.R
Rscript scripts/03_download_geography.R config/chicago.yml cmap_2024_2025_phase1
Rscript scripts/01_standardize_survey.R config/chicago.yml cmap_2024_2025_phase1
Rscript scripts/02_build_od_weights.R config/chicago.yml cmap_2024_2025_phase1
Rscript scripts/04_summarize_gtfs.R config/chicago.yml cmap_2024_2025_phase1
Rscript scripts/05_compute_travel_times.R config/chicago.yml cmap_2024_2025_phase1
Rscript scripts/06_aggregate_metrics.R config/chicago.yml cmap_2024_2025_phase1
Rscript scripts/07_make_interactive_maps.R config/chicago.yml cmap_2024_2025_phase1
```

Or run the full sequence:

```bash
Rscript scripts/08_run_pipeline.R config/chicago.yml cmap_2024_2025_phase1
```

You can also use the YAML configs:

```bash
Rscript scripts/08_run_pipeline.R config/chicago.yml
```

## Core outputs

### Survey standardization

`data/processed/<city_id>/survey/<source_id>/`

- `households_std.csv.gz`
- `persons_std.csv.gz`
- `days_std.csv.gz` when available
- `trips_std.csv.gz`
- `qa_summary.csv.gz`

### OD weights

`data/processed/<city_id>/runs/<source_id>/<run_id>/od/`

- `od_weights_all.csv.gz`
- `od_marginals_origin.csv`
- `od_marginals_destination.csv`
- `top_od_pairs.csv`

### GTFS supply summaries

`data/processed/<city_id>/runs/<source_id>/<run_id>/gtfs_supply/`

- `analysis_dates_used.csv`
- `route_frequency_day_summary.csv`
- `trip_day_summary.csv`
- `period_summary.csv`
- `headway_compare.csv`
- `trip_compare.csv`

### Routing outputs

`data/processed/<city_id>/runs/<source_id>/<run_id>/travel_times/`

- daily chunk files in `daily/`
- `period_travel_times.csv.gz`

### Accessibility metrics

`data/processed/<city_id>/runs/<source_id>/<run_id>/accessibility/`

- `tract_period_metrics.csv`
- `tract_period_comparisons.csv`
- `tract_period_metrics.gpkg`
- `tract_period_comparisons.gpkg`

### Maps

`outputs/<city_id>/maps/<source_id>/<run_id>/`

- one HTML file per metric
- `index.html` linking to all generated maps

## Notes on future extension

The code already leaves hooks for:

- additional survey adapters
- optional tract covariates such as ACS population multipliers
- changing the OD selection rules by purpose, time window, transit only trips, income group, or vehicle ownership
- changing routing parameters without rewriting the routing step
- adding more output metrics in the aggregation and map steps

The cleanest way to add a new city is:

1. copy one of the configs
2. add a new survey adapter if needed
3. point the config at the survey zip, GTFS feeds, and OSM PBF URL


## Rerun behavior

The pipeline now separates reusable caches from parameter dependent outputs. Downloads, base geography, standardized survey tables, and cached R5 networks are reused across runs. OD weights, GTFS summaries, travel times, accessibility metrics, and maps are written into a run specific folder keyed by a parameter signature, so changing analytical settings creates a new output folder instead of silently reusing stale results. Each run folder also gets a `run_manifest.json` and a flattened parameter table.

## Java memory for r5r

The bootstrap script now sets `options(java.parameters=...)` before loading `r5r`, because the r5r docs require Java memory to be configured before the package starts the JVM. The default project configs set `java_memory: 12G` and `java_active_processors: 2`.


## Quick start

Chicago:

```bash
Rscript scripts/run_chicago.R
```

Philadelphia:

```bash
Rscript scripts/run_philadelphia.R
```

Generic entry point:

```bash
Rscript scripts/08_run_pipeline.R config/chicago.yml cmap_2024_2025_phase1
Rscript scripts/08_run_pipeline.R config/philadelphia.yml philadelphia_public_release
```

Results that depend on analysis parameters are written to a run specific folder under `data/processed/<city>/runs/<source_id>/<run_id>/` and matching map folders under `outputs/<city>/maps/<source_id>/<run_id>/`. Shared assets such as base geography, OSM downloads, standardized survey tables, and cached R5 networks are reused when still valid.
