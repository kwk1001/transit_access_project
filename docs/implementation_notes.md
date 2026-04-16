# Implementation notes

## Main Chicago path

The primary path is:

1. `config/chicago_cmap_phase1.yml`
2. `scripts/09_run_pipeline_main.R`

This path assumes:

- Chicago survey input is `MyDailyTravel_2024_2025Phase1.zip`
- GTFS files are local CTA ZIP files listed in the config
- Routing geography is Cook County tracts only
- OD weights come from linked trips in `trip_linked.csv`
- Peak and midday scenarios use the same study periods as the GTFS comparison workflow

## How this differs from the earlier scripts

- GTFS summary now averages over all eligible weekdays in each period instead of relying on a single representative day.
- Routing now computes tract to tract travel times instead of tract to one downtown destination.
- Accessibility metrics keep track of unreachable OD pairs instead of dropping them in the map stage.
- Maps are generated directly from tract period metrics and tract period comparisons.

## What to test first after placing GTFS files locally

1. Run `scripts/01_standardize_survey.R` and inspect `trips_std.csv.gz`
2. Run `scripts/02_build_od_weights.R` and inspect `top_od_pairs.csv`
3. Run `scripts/04_summarize_gtfs.R` and verify `period_summary.csv`
4. Run `scripts/05_compute_travel_times.R` on one reduced config if runtime is high
5. Run `scripts/06_aggregate_metrics.R` and inspect `tract_period_metrics.csv`
6. Run `scripts/07_make_interactive_maps.R`

## Best first extensions

- Add origin population multipliers
- Add destination employment or school enrollment multipliers
- Split OD scenarios by purpose group
- Add a regional Chicago config with Cook, DuPage, Lake, Kane, McHenry, and Will counties
- Add an expanded route breakdown workflow with `expanded_travel_time_matrix()` for selected hotspot OD pairs

## Philadelphia status

A first DVRPC adapter is included. It is intended as a second test configuration after the Chicago workflow is running locally.
