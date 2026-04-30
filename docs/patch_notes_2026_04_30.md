# Patch notes for routing reliability and cross-city compatibility

## Fixed

- Java bootstrapping now reads `r5r_network.java_memory` and `r5r_network.java_active_processors` from YAML configs before `r5r` loads, with backward-compatible fallback to `routing.*` when older config styles are used.
- Java bootstrapping now refreshes `options(java.parameters=...)` whenever `rJava` has not yet loaded, preventing stale settings from previous runs in the same R session.
- Daily routing chunk cache checks no longer treat header-only CSV or CSV.GZ files as successful outputs.
- Travel time matrix IDs are now standardized to the configured analysis unit before joining to OD pairs, preventing leading-zero mismatches for ZIP-based runs.
- When a routing chunk returns rows from R5 but the downstream join yields zero rows, a structured diagnostic log is written to `logs_dir`.
- Empty travel-time matrix helpers are now defined once at file scope and reused consistently.

## Cleaned up

- Removed unused `R/bootstrap.R`.
- Removed `R/.DS_Store`.

## Notes

These changes are intended to be compatible with tract, ZIP, and TAZ workflows across cities, and with both YAML-based and older list-style configuration flows.
