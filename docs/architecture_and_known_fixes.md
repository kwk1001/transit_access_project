# Architecture and consolidated fixes

This document summarizes the current project architecture and the main issues addressed in the consolidated rebuild.

## 1. Core architecture

The project is split into reusable modules.

- `R/adapters_*` standardize raw survey files to a common schema
- `R/geography.R` prepares tract base geography and analysis geography
- `R/od_weights.R` builds survey based OD weights at the selected analysis unit
- `R/routing_r5r.R` computes OD travel times
- `R/accessibility_metrics.R` aggregates OD travel times back to origin zones
- `R/maps_leaflet.R` exports interactive HTML maps
- `R/load_project.R` wires together caching and step execution

## 2. Spatial analysis unit

A new `spatial` block in YAML controls the analysis unit.

Supported units:
- `tract`
- `zcta` or `zip`
- `custom`

### ZIP/ZCTA behavior
ZIP scale analysis uses tract centroid assignment to ZCTA polygons. This is deliberate because ZIP and tract boundaries do not nest cleanly.

### Custom zone behavior
Custom polygon layers such as TAZ can be used by setting:
- `spatial.analysis_unit: custom`
- `spatial.custom.zones_file`
- `spatial.custom.zone_id_col`
- `spatial.custom.zone_label_col`

If the survey already contains raw zone ids, `assign_from: raw_zone` can be used so OD aggregation is performed directly on the raw survey zone fields.

## 3. Run structure and caching

Reusable assets are stored separately from run specific outputs.

Reusable assets:
- raw OSM download
- standardized survey tables
- base tract and county geography
- cached R5 networks

Run specific outputs live under:
- `data/processed/<city>/runs/<source>/<run_id>/`
- `outputs/<city>/maps/<source>/<run_id>/`

`run_id` now uses a readable slug plus a short signature, for example:
- `chicago__cmap_2024_2025_phase1__unit_tract__sig_ab12cd34ef56`

This keeps runs distinguishable while still avoiding silent overwrite.

## 4. Empty run folder cleanup

The previous project often left behind empty or metadata only `sig_*` folders.

Current cleanup logic removes run directories when they contain no substantive outputs outside `metadata/` or `logs/`.

## 5. Map fixes

### Metric map legends
Older versions used one legend per layer and then tried to hide legends by DOM order. This caused frequent mismatches between the visible layer and the visible legend.

Current metric maps now use one explicit dynamic legend control. The legend HTML is keyed by layer group and swapped on `baselayerchange`.

### OD line slider
Older OD line maps tried to read `feature.properties.rank` and `feature.properties.scenario_id`, but the lines were added with `leaflet::addPolylines()` and therefore did not reliably expose those properties in the browser.

Current OD line maps instead parse:
- `scenario_id`
- `rank`

from the `layerId`, which is encoded as `scenario__rank`.

A second issue caused newly revealed lines to become too thin. This happened because hidden layers overwrote their visible width, and later redraws reused the modified width. The current version caches the original line width once in `layer._baseWeight` and always restores that width when a line is shown again.

## 6. Better user entry points

Recommended entry points are:
- `scripts/run_chicago.R`
- `scripts/run_philadelphia.R`
- `scripts/main_run.R`

`main_run.R` is meant for fast experimentation without editing YAML repeatedly.

## 7. Output metadata

Each run writes:
- `metadata/run_manifest.json`
- `metadata/parameters_flat.csv`
- `run_summary.txt`

These are written to both the processed run directory and the output run directory so results are easier to interpret later.
