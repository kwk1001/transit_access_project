# Survey standardization notes

## Common standard schema

### households_std
- `city_id`
- `survey_name`
- `survey_wave`
- `household_id`
- `household_weight`
- `home_tract`
- `home_county_fips`
- `home_state_fips`
- `income_code_raw`
- `income_group_raw`
- `vehicles`
- `persons`
- `workers`
- `students`
- `survey_start_date`
- `survey_end_date`
- `complete_flag`
- `raw_source_file`

### persons_std
- `city_id`
- `survey_name`
- `survey_wave`
- `household_id`
- `person_id`
- `household_weight`
- `person_weight`
- `age`
- `gender_code_raw`
- `worker_flag`
- `student_flag`
- `work_tract`
- `school_tract`
- `person_type_raw`
- `complete_flag`
- `raw_source_file`

### trips_std
- `city_id`
- `survey_name`
- `survey_wave`
- `trip_id`
- `household_id`
- `person_id`
- `day_id`
- `travel_date`
- `travel_dow`
- `weekday_flag`
- `complete_day_flag`
- `complete_trip_flag`
- `origin_tract`
- `destination_tract`
- `origin_county_fips`
- `destination_county_fips`
- `depart_time_local`
- `arrive_time_local`
- `depart_minutes_of_day`
- `duration_minutes`
- `distance_miles`
- `main_mode_code_raw`
- `main_mode_label`
- `mode_group`
- `transit_involved_flag`
- `origin_purpose_code_raw`
- `origin_purpose_label`
- `destination_purpose_code_raw`
- `destination_purpose_label`
- `purpose_category_code_raw`
- `purpose_category_label`
- `purpose_group`
- `household_weight`
- `person_weight`
- `trip_weight`
- `analysis_weight`
- `raw_source_file`
- `raw_record_id`

## Chicago My Daily Travel 2024-2025 Phase 1

Default source table for OD weights: `trip_linked.csv`

Useful fields:
- Household weight: `hh_weight`
- Person weight: `person_weight`
- Linked trip weight: `linked_trip_weight`
- Origin tract: `o_tract_2020`
- Destination tract: `d_tract_2020`
- Linked trip mode: `linked_trip_mode`
- Destination purpose: `d_purpose`
- Destination purpose category: `d_purpose_category`
- Day linkage: `day_id`
- Day metadata for completion and weekday: `day.csv`

## Chicago My Daily Travel 2018-2019

Default source table for OD weights: reconstructed from `place.csv` plus `location.csv`

Useful fields:
- Household weight: `wthhfin`
- Person weight: `wtperfin`
- Home date and weekday: `household.csv`
- Destination place sequence: `placeno`
- Origin and destination locations: lagged and current `locno`
- Tracts: `location.csv` fields `state_fips`, `county_fips`, `tract_fips`
- Mode: `mode`
- Purpose at place: `tpurp`

## Philadelphia DVRPC public release

Default source table for OD weights: `4_Trip_Public.xlsx`

Useful fields:
- Household weight: `HH_WEIGHT`
- Person weight: `P_WEIGHT`
- GPS expansion multiplier: `GPSFactor`
- Composite trip weight: `CompositeWeight`
- Origin tract: `O_TRACT`
- Destination tract: `D_TRACT`
- Mode aggregate: `MODE_AGG`
- Survey travel time: `Survey_TravTime`
- Model travel time: `Model_TravTime`
- Activity fields at origin: `ACTIV1` through `ACTIV4`
- Destination activity for trip _t_ should be read from the next record in sequence, including record 97 for the last destination of the day.
