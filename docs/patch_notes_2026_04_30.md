# Patch notes

Updated code to address current routing failures across cities:

- Read Java settings from `r5r_network` first, with backward-compatible fallback to `routing`.
- Hardened ZIP standardization so tract GEOIDs cannot leak into ZIP workflows.
- Prevented tract fallback in non-tract OD building.
- Added data-aware checks for cached `daily/*.csv.gz` files so header-only files are not treated as complete results.
- Normalized R5 results before downstream binding and logged malformed per-origin fallback outputs.
- Standardized `from_id` and `to_id` before joining routing results back to OD pairs.
- Filtered OD rows whose origins or destinations lack valid routing points before routing begins.
- Split missing routing ids from invalid coordinate routing ids in warnings.
- Fixed fallback point QC flag naming.
- Removed obvious unused macOS metadata files and bootstrap stub.
