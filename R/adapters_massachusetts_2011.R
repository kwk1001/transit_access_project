read_mts2011_zip <- function(zip_path, tmp_dir) {
  utils::unzip(zip_path, exdir = tmp_dir, overwrite = TRUE)
  tmp_dir
}

mts2011_date_from_assn <- function(assn_code) {
  assn_chr <- as.character(safe_integer(assn_code))
  lookup <- c(
    "666" = as.Date("2010-06-14"),
    "667" = as.Date("2010-06-15"),
    "668" = as.Date("2010-06-16"),
    "669" = as.Date("2010-06-17"),
    "670" = as.Date("2010-06-18"),
    "673" = as.Date("2010-06-21"),
    "674" = as.Date("2010-06-22"),
    "675" = as.Date("2010-06-23"),
    "676" = as.Date("2010-06-24"),
    "677" = as.Date("2010-06-25"),
    "680" = as.Date("2010-06-28"),
    "681" = as.Date("2010-06-29"),
    "682" = as.Date("2010-06-30"),
    "683" = as.Date("2010-07-01"),
    "684" = as.Date("2010-07-02"),
    "687" = as.Date("2010-07-05"),
    "688" = as.Date("2010-07-06"),
    "689" = as.Date("2010-07-07"),
    "690" = as.Date("2010-07-08"),
    "691" = as.Date("2010-07-09"),
    "694" = as.Date("2010-07-12"),
    "695" = as.Date("2010-07-13"),
    "696" = as.Date("2010-07-14"),
    "697" = as.Date("2010-07-15"),
    "698" = as.Date("2010-07-16"),
    "701" = as.Date("2010-07-19"),
    "702" = as.Date("2010-07-20"),
    "703" = as.Date("2010-07-21"),
    "704" = as.Date("2010-07-22"),
    "705" = as.Date("2010-07-23"),
    "708" = as.Date("2010-07-26"),
    "709" = as.Date("2010-07-27"),
    "710" = as.Date("2010-07-28"),
    "711" = as.Date("2010-07-29"),
    "712" = as.Date("2010-07-30"),
    "715" = as.Date("2010-08-02"),
    "716" = as.Date("2010-08-03"),
    "717" = as.Date("2010-08-04"),
    "718" = as.Date("2010-08-05"),
    "719" = as.Date("2010-08-06"),
    "722" = as.Date("2010-08-09"),
    "723" = as.Date("2010-08-10"),
    "724" = as.Date("2010-08-11"),
    "725" = as.Date("2010-08-12"),
    "726" = as.Date("2010-08-13"),
    "729" = as.Date("2010-08-16"),
    "730" = as.Date("2010-08-17"),
    "731" = as.Date("2010-08-18"),
    "732" = as.Date("2010-08-19"),
    "733" = as.Date("2010-08-20"),
    "736" = as.Date("2010-08-23"),
    "737" = as.Date("2010-08-24"),
    "738" = as.Date("2010-08-25"),
    "739" = as.Date("2010-08-26"),
    "740" = as.Date("2010-08-27"),
    "743" = as.Date("2010-08-30"),
    "744" = as.Date("2010-08-31"),
    "745" = as.Date("2010-09-01"),
    "746" = as.Date("2010-09-02"),
    "747" = as.Date("2010-09-03"),
    "750" = as.Date("2010-09-06"),
    "751" = as.Date("2010-09-07"),
    "752" = as.Date("2010-09-08"),
    "753" = as.Date("2010-09-09"),
    "754" = as.Date("2010-09-10"),
    "757" = as.Date("2010-09-13"),
    "758" = as.Date("2010-09-14"),
    "759" = as.Date("2010-09-15"),
    "760" = as.Date("2010-09-16"),
    "761" = as.Date("2010-09-17"),
    "764" = as.Date("2010-09-20"),
    "765" = as.Date("2010-09-21"),
    "766" = as.Date("2010-09-22"),
    "767" = as.Date("2010-09-23"),
    "768" = as.Date("2010-09-24"),
    "771" = as.Date("2010-09-27"),
    "772" = as.Date("2010-09-28"),
    "773" = as.Date("2010-09-29"),
    "774" = as.Date("2010-09-30"),
    "775" = as.Date("2010-10-01"),
    "778" = as.Date("2010-10-04"),
    "779" = as.Date("2010-10-05"),
    "780" = as.Date("2010-10-06"),
    "781" = as.Date("2010-10-07"),
    "782" = as.Date("2010-10-08"),
    "785" = as.Date("2010-10-11"),
    "786" = as.Date("2010-10-12"),
    "787" = as.Date("2010-10-13"),
    "788" = as.Date("2010-10-14"),
    "789" = as.Date("2010-10-15"),
    "792" = as.Date("2010-10-18"),
    "793" = as.Date("2010-10-19"),
    "794" = as.Date("2010-10-20"),
    "795" = as.Date("2010-10-21"),
    "796" = as.Date("2010-10-22"),
    "799" = as.Date("2010-10-25"),
    "800" = as.Date("2010-10-26"),
    "801" = as.Date("2010-10-27"),
    "802" = as.Date("2010-10-28"),
    "803" = as.Date("2010-10-29"),
    "806" = as.Date("2010-11-01"),
    "807" = as.Date("2010-11-02"),
    "808" = as.Date("2010-11-03"),
    "809" = as.Date("2010-11-04"),
    "810" = as.Date("2010-11-05"),
    "813" = as.Date("2010-11-08"),
    "814" = as.Date("2010-11-09"),
    "815" = as.Date("2010-11-10"),
    "816" = as.Date("2010-11-11"),
    "817" = as.Date("2010-11-12"),
    "820" = as.Date("2010-11-15"),
    "821" = as.Date("2010-11-16"),
    "822" = as.Date("2010-11-17"),
    "823" = as.Date("2010-11-18"),
    "824" = as.Date("2010-11-19"),
    "827" = as.Date("2010-11-22"),
    "828" = as.Date("2010-11-23"),
    "829" = as.Date("2010-11-24"),
    "830" = as.Date("2010-11-25"),
    "831" = as.Date("2010-11-26"),
    "834" = as.Date("2010-11-29"),
    "835" = as.Date("2010-11-30"),
    "836" = as.Date("2010-12-01"),
    "837" = as.Date("2010-12-02"),
    "838" = as.Date("2010-12-03"),
    "841" = as.Date("2010-12-06"),
    "842" = as.Date("2010-12-07"),
    "843" = as.Date("2010-12-08"),
    "844" = as.Date("2010-12-09"),
    "845" = as.Date("2010-12-10"),
    "848" = as.Date("2010-12-13"),
    "849" = as.Date("2010-12-14"),
    "850" = as.Date("2010-12-15"),
    "851" = as.Date("2010-12-16"),
    "852" = as.Date("2010-12-17"),
    "855" = as.Date("2010-12-20"),
    "856" = as.Date("2010-12-21"),
    "857" = as.Date("2010-12-22"),
    "858" = as.Date("2010-12-23"),
    "859" = as.Date("2010-12-24"),
    "862" = as.Date("2010-12-27"),
    "863" = as.Date("2010-12-28"),
    "864" = as.Date("2010-12-29"),
    "865" = as.Date("2010-12-30"),
    "866" = as.Date("2010-12-31"),
    "11103" = as.Date("2011-01-03"),
    "11104" = as.Date("2011-01-04"),
    "11105" = as.Date("2011-01-05"),
    "11106" = as.Date("2011-01-06"),
    "11107" = as.Date("2011-01-07"),
    "11110" = as.Date("2011-01-10"),
    "11111" = as.Date("2011-01-11"),
    "11112" = as.Date("2011-01-12"),
    "11113" = as.Date("2011-01-13"),
    "11114" = as.Date("2011-01-14"),
    "11117" = as.Date("2011-01-17"),
    "11118" = as.Date("2011-01-18"),
    "11119" = as.Date("2011-01-19"),
    "11120" = as.Date("2011-01-20"),
    "11121" = as.Date("2011-01-21"),
    "11124" = as.Date("2011-01-24"),
    "11125" = as.Date("2011-01-25"),
    "11126" = as.Date("2011-01-26"),
    "11127" = as.Date("2011-01-27"),
    "11128" = as.Date("2011-01-28"),
    "11131" = as.Date("2011-01-31"),
    "11132" = as.Date("2011-02-01"),
    "11133" = as.Date("2011-02-02"),
    "11134" = as.Date("2011-02-03"),
    "11135" = as.Date("2011-02-04"),
    "11138" = as.Date("2011-02-07"),
    "11139" = as.Date("2011-02-08"),
    "11140" = as.Date("2011-02-09"),
    "11141" = as.Date("2011-02-10"),
    "11142" = as.Date("2011-02-11"),
    "11145" = as.Date("2011-02-14"),
    "11146" = as.Date("2011-02-15"),
    "11147" = as.Date("2011-02-16"),
    "11148" = as.Date("2011-02-17"),
    "11149" = as.Date("2011-02-18"),
    "11152" = as.Date("2011-02-21"),
    "11153" = as.Date("2011-02-22"),
    "11154" = as.Date("2011-02-23"),
    "11155" = as.Date("2011-02-24"),
    "11156" = as.Date("2011-02-25"),
    "11159" = as.Date("2011-02-28"),
    "11161" = as.Date("2011-03-01"),
    "11162" = as.Date("2011-03-02"),
    "11163" = as.Date("2011-03-03"),
    "11164" = as.Date("2011-03-04"),
    "11167" = as.Date("2011-03-07"),
    "11168" = as.Date("2011-03-08"),
    "11169" = as.Date("2011-03-09"),
    "11170" = as.Date("2011-03-10"),
    "11171" = as.Date("2011-03-11"),
    "11174" = as.Date("2011-03-14"),
    "11175" = as.Date("2011-03-15"),
    "11176" = as.Date("2011-03-16"),
    "11177" = as.Date("2011-03-17"),
    "11178" = as.Date("2011-03-18"),
    "11181" = as.Date("2011-03-21"),
    "11182" = as.Date("2011-03-22"),
    "11183" = as.Date("2011-03-23"),
    "11184" = as.Date("2011-03-24"),
    "11185" = as.Date("2011-03-25"),
    "11188" = as.Date("2011-03-28"),
    "11189" = as.Date("2011-03-29"),
    "11190" = as.Date("2011-03-30"),
    "11191" = as.Date("2011-03-31"),
    "11192" = as.Date("2011-04-01"),
    "11195" = as.Date("2011-04-04"),
    "11196" = as.Date("2011-04-05"),
    "11197" = as.Date("2011-04-06"),
    "11198" = as.Date("2011-04-07"),
    "11199" = as.Date("2011-04-08"),
    "11202" = as.Date("2011-04-11"),
    "11203" = as.Date("2011-04-12"),
    "11204" = as.Date("2011-04-13"),
    "11205" = as.Date("2011-04-14"),
    "11206" = as.Date("2011-04-15"),
    "11209" = as.Date("2011-04-18"),
    "11210" = as.Date("2011-04-19"),
    "11211" = as.Date("2011-04-20"),
    "11212" = as.Date("2011-04-21"),
    "11213" = as.Date("2011-04-22"),
    "11216" = as.Date("2011-04-25"),
    "11217" = as.Date("2011-04-26"),
    "11218" = as.Date("2011-04-27"),
    "11219" = as.Date("2011-04-28"),
    "11220" = as.Date("2011-04-29"),
    "11223" = as.Date("2011-05-02"),
    "11224" = as.Date("2011-05-03"),
    "11225" = as.Date("2011-05-04"),
    "11226" = as.Date("2011-05-05"),
    "11227" = as.Date("2011-05-06"),
    "11230" = as.Date("2011-05-09"),
    "11231" = as.Date("2011-05-10"),
    "11232" = as.Date("2011-05-11"),
    "11233" = as.Date("2011-05-12"),
    "11234" = as.Date("2011-05-13"),
    "11237" = as.Date("2011-05-16"),
    "11238" = as.Date("2011-05-17"),
    "11239" = as.Date("2011-05-18"),
    "11240" = as.Date("2011-05-19"),
    "11241" = as.Date("2011-05-20"),
    "11244" = as.Date("2011-05-23"),
    "11245" = as.Date("2011-05-24"),
    "11246" = as.Date("2011-05-25"),
    "11247" = as.Date("2011-05-26"),
    "11248" = as.Date("2011-05-27"),
    "11251" = as.Date("2011-05-30"),
    "11252" = as.Date("2011-05-31"),
    "11253" = as.Date("2011-06-01"),
    "11254" = as.Date("2011-06-02"),
    "11255" = as.Date("2011-06-03"),
    "11258" = as.Date("2011-06-06"),
    "11259" = as.Date("2011-06-07"),
    "11260" = as.Date("2011-06-08"),
    "11261" = as.Date("2011-06-09"),
    "11262" = as.Date("2011-06-10"),
    "11265" = as.Date("2011-06-13"),
    "11266" = as.Date("2011-06-14"),
    "11267" = as.Date("2011-06-15"),
    "11268" = as.Date("2011-06-16"),
    "11269" = as.Date("2011-06-17"),
    "11272" = as.Date("2011-06-20"),
    "11273" = as.Date("2011-06-21"),
    "11274" = as.Date("2011-06-22"),
    "11275" = as.Date("2011-06-23"),
    "11276" = as.Date("2011-06-24"),
    "11279" = as.Date("2011-06-27"),
    "11280" = as.Date("2011-06-28"),
    "11281" = as.Date("2011-06-29"),
    "11282" = as.Date("2011-06-30"),
    "11283" = as.Date("2011-07-01"),
    "11286" = as.Date("2011-07-04"),
    "11287" = as.Date("2011-07-05"),
    "11288" = as.Date("2011-07-06"),
    "11289" = as.Date("2011-07-07"),
    "11290" = as.Date("2011-07-08"),
    "11293" = as.Date("2011-07-11"),
    "11294" = as.Date("2011-07-12"),
    "11295" = as.Date("2011-07-13"),
    "11296" = as.Date("2011-07-14"),
    "11297" = as.Date("2011-07-15"),
    "11300" = as.Date("2011-07-18"),
    "11301" = as.Date("2011-07-19"),
    "11302" = as.Date("2011-07-20"),
    "11303" = as.Date("2011-07-21"),
    "11304" = as.Date("2011-07-22"),
    "11307" = as.Date("2011-07-25"),
    "11308" = as.Date("2011-07-26"),
    "11309" = as.Date("2011-07-27"),
    "11310" = as.Date("2011-07-28"),
    "11311" = as.Date("2011-07-29"),
    "11314" = as.Date("2011-08-01"),
    "11315" = as.Date("2011-08-02"),
    "11316" = as.Date("2011-08-03"),
    "11317" = as.Date("2011-08-04"),
    "11318" = as.Date("2011-08-05"),
    "11321" = as.Date("2011-08-08"),
    "11322" = as.Date("2011-08-09"),
    "11323" = as.Date("2011-08-10"),
    "11324" = as.Date("2011-08-11"),
    "11325" = as.Date("2011-08-12"),
    "11328" = as.Date("2011-08-15"),
    "11329" = as.Date("2011-08-16"),
    "11330" = as.Date("2011-08-17"),
    "11331" = as.Date("2011-08-18"),
    "11332" = as.Date("2011-08-19"),
    "11335" = as.Date("2011-08-22"),
    "11336" = as.Date("2011-08-23"),
    "11337" = as.Date("2011-08-24"),
    "11338" = as.Date("2011-08-25"),
    "11339" = as.Date("2011-08-26"),
    "11342" = as.Date("2011-08-29"),
    "11343" = as.Date("2011-08-30"),
    "11344" = as.Date("2011-08-31"),
    "11345" = as.Date("2011-09-01"),
    "11346" = as.Date("2011-09-02"),
    "11349" = as.Date("2011-09-05"),
    "11350" = as.Date("2011-09-06"),
    "11351" = as.Date("2011-09-07"),
    "11352" = as.Date("2011-09-08"),
    "11353" = as.Date("2011-09-09"),
    "11356" = as.Date("2011-09-12"),
    "11357" = as.Date("2011-09-13"),
    "11358" = as.Date("2011-09-14"),
    "11359" = as.Date("2011-09-15"),
    "11360" = as.Date("2011-09-16"),
    "11363" = as.Date("2011-09-19"),
    "11364" = as.Date("2011-09-20"),
    "11365" = as.Date("2011-09-21"),
    "11366" = as.Date("2011-09-22"),
    "11367" = as.Date("2011-09-23"),
    "11370" = as.Date("2011-09-26"),
    "11371" = as.Date("2011-09-27"),
    "11372" = as.Date("2011-09-28"),
    "11373" = as.Date("2011-09-29"),
    "11374" = as.Date("2011-09-30"),
    "11377" = as.Date("2011-10-03"),
    "11378" = as.Date("2011-10-04"),
    "11379" = as.Date("2011-10-05"),
    "11380" = as.Date("2011-10-06"),
    "11381" = as.Date("2011-10-07"),
    "11384" = as.Date("2011-10-10"),
    "11385" = as.Date("2011-10-11"),
    "11386" = as.Date("2011-10-12"),
    "11387" = as.Date("2011-10-13"),
    "11388" = as.Date("2011-10-14"),
    "11391" = as.Date("2011-10-17"),
    "11392" = as.Date("2011-10-18"),
    "11393" = as.Date("2011-10-19"),
    "11394" = as.Date("2011-10-20"),
    "11395" = as.Date("2011-10-21"),
    "11398" = as.Date("2011-10-24")
  )
  out <- as.Date(rep(NA_character_, length(assn_chr)))
  matched <- !is.na(assn_chr) & assn_chr %in% names(lookup)
  out[matched] <- unname(lookup[assn_chr[matched]])
  out
}

mts2011_hhmm_to_chr <- function(hr, min) {
  hr_int <- safe_integer(hr)
  min_int <- safe_integer(min)
  ok <- !(is.na(hr_int) | is.na(min_int))
  out <- rep(NA_character_, length(hr_int))
  out[ok] <- sprintf("%02d:%02d:00", hr_int[ok], min_int[ok])
  out
}

mts2011_geoid11 <- function(state10, county10, tract10) {
  state_part <- zero_pad(safe_integer(state10), 2)
  county_part <- zero_pad(safe_integer(county10), 3)
  tract_num <- safe_integer(tract10)
  tract_num[tract_num <= 0] <- NA_integer_
  tract_part <- zero_pad(tract_num, 6)
  out <- paste0(state_part, county_part, tract_part)
  out[is.na(state_part) | is.na(county_part) | is.na(tract_part)] <- NA_character_
  out
}

mts2011_mode_label <- function(mode_code, mode2_code = NA) {
  mode_int <- safe_integer(mode_code)
  mode2_int <- safe_integer(mode2_code)
  out <- dplyr::case_when(
    is.na(mode_int) ~ NA_character_,
    mode_int == 1L ~ "Walk",
    mode_int == 2L ~ "Bicycle",
    mode_int == 3L ~ "Auto driver",
    mode_int == 4L ~ "Auto passenger",
    mode_int == 5L & mode2_int == 1L ~ "Local bus",
    mode_int == 5L & mode2_int == 2L ~ "Express bus",
    mode_int == 5L & mode2_int == 3L ~ "Shuttle bus",
    mode_int == 5L ~ "Transit bus",
    mode_int == 6L & mode2_int == 4L ~ "Subway",
    mode_int == 6L & mode2_int == 5L ~ "Commuter rail",
    mode_int == 6L ~ "Transit rail",
    mode_int == 7L ~ "Ferry",
    mode_int == 8L ~ "Dial-a-ride / paratransit",
    mode_int == 9L ~ "Taxi",
    mode_int == 10L ~ "School bus",
    mode_int == 11L ~ "Motorcycle driver",
    mode_int == 12L ~ "Motorcycle passenger",
    mode_int == 97L ~ "Other",
    TRUE ~ paste0("Mode ", mode_int)
  )
  out
}

mts2011_mode_group <- function(mode_code) {
  mode_int <- safe_integer(mode_code)
  dplyr::case_when(
    is.na(mode_int) ~ NA_character_,
    mode_int == 1L ~ "walk",
    mode_int == 2L ~ "bike_micromobility",
    mode_int == 3L ~ "private_auto",
    mode_int == 4L ~ "carpool",
    mode_int %in% c(5L, 6L, 7L, 8L) ~ "public_transit",
    mode_int == 10L ~ "school_bus",
    mode_int == 9L ~ "taxi_tnc",
    TRUE ~ "other"
  )
}

mts2011_purpose_label <- function(purp_code) {
  purp_int <- safe_integer(purp_code)
  dplyr::case_when(
    is.na(purp_int) ~ NA_character_,
    purp_int == 1L ~ "Working at home",
    purp_int == 2L ~ "Home",
    purp_int == 3L ~ "Work",
    purp_int == 4L ~ "Other work activity",
    purp_int == 5L ~ "Volunteer activity",
    purp_int == 6L ~ "Attending class",
    purp_int == 7L ~ "Other school activity",
    purp_int == 8L ~ "Change mode",
    purp_int == 9L ~ "Drop off passenger",
    purp_int == 10L ~ "Pick up passenger",
    purp_int == 11L ~ "While traveling other",
    purp_int == 12L ~ "Work business",
    purp_int == 13L ~ "Vehicle service",
    purp_int == 14L ~ "Routine shopping",
    purp_int == 15L ~ "Major shopping",
    purp_int == 16L ~ "Household errand",
    purp_int == 17L ~ "Personal business",
    purp_int == 18L ~ "Meal outside home",
    purp_int == 19L ~ "Health care",
    purp_int == 20L ~ "Civic or religious activity",
    purp_int == 21L ~ "Outdoor recreation",
    purp_int == 22L ~ "Indoor recreation",
    purp_int == 23L ~ "Visit friends or relatives",
    purp_int == 96L ~ "Loop trip",
    purp_int == 97L ~ "Other",
    TRUE ~ paste0("Purpose ", purp_int)
  )
}

mts2011_purpose_group <- function(purp_code) {
  purp_int <- safe_integer(purp_code)
  dplyr::case_when(
    is.na(purp_int) ~ NA_character_,
    purp_int %in% c(1L, 2L) ~ "home",
    purp_int %in% c(3L, 4L, 12L) ~ "work",
    purp_int %in% c(6L, 7L) ~ "school",
    purp_int == 8L ~ "change_mode",
    purp_int %in% c(9L, 10L) ~ "escort",
    purp_int %in% c(14L, 15L) ~ "shopping",
    purp_int %in% c(13L, 16L, 17L, 19L) ~ "errand_personal_business",
    purp_int == 18L ~ "meal",
    purp_int %in% c(5L, 20L, 21L, 22L, 23L) ~ "social_recreation",
    purp_int == 96L ~ "loop",
    TRUE ~ "other"
  )
}

standardize_massachusetts_2011 <- function(cfg) {
  tmp_dir <- tempfile("mts2011_")
  fs::dir_create(tmp_dir)
  on.exit(fs::dir_delete(tmp_dir), add = TRUE)

  data_dir <- read_mts2011_zip(cfg$active_survey_source$file_path, tmp_dir)

  hh_path <- file.path(data_dir, "HH.xlsx")
  per_path <- file.path(data_dir, "PER.xlsx")
  place_path <- file.path(data_dir, "PLACE.xlsx")
  veh_path <- file.path(data_dir, "VEH.xlsx")

  if (!all(file.exists(c(hh_path, per_path, place_path)))) {
    stop("Massachusetts 2011 survey zip must contain HH.xlsx, PER.xlsx, and PLACE.xlsx.", call. = FALSE)
  }

  hh <- readxl::read_excel(hh_path) %>% janitor::clean_names()
  per <- readxl::read_excel(per_path) %>% janitor::clean_names()
  place <- readxl::read_excel(place_path) %>% janitor::clean_names()
  veh <- if (file.exists(veh_path)) readxl::read_excel(veh_path) %>% janitor::clean_names() else NULL

  hh <- hh %>%
    mutate(
      household_id = as.character(sampn),
      survey_day_code = safe_integer(assn),
      travel_date = mts2011_date_from_assn(survey_day_code),
      travel_dow = safe_integer(day),
      home_tract = mts2011_geoid11(hstate10, hcounty10, htract10),
      home_zip_raw = standardize_geoid5(hzip),
      household_weight = safe_numeric(hhwgt),
      household_exp_weight = safe_numeric(expwgt),
      complete_household = !is.na(household_weight)
    )

  person_day_context <- per %>%
    mutate(
      household_id = as.character(sampn),
      person_id = as.character(perno),
      person_weight = safe_numeric(pwgt),
      person_exp_weight = safe_numeric(exppwgt),
      work_tract = mts2011_geoid11(wstate10, wcounty10, wtract10),
      school_tract = mts2011_geoid11(sstate10, scounty10, stract10),
      work_zip_raw = standardize_geoid5(wzip),
      school_zip_raw = standardize_geoid5(szip)
    ) %>%
    left_join(
      hh %>%
        transmute(
          household_id,
          survey_day_code,
          travel_date,
          travel_dow,
          household_weight,
          household_exp_weight,
          income_group_simple = make_income_group_simple(income),
          vehicle_group_simple = make_vehicle_group_simple(hhveh),
          home_tract,
          home_zip_raw
        ),
      by = "household_id"
    )

  households_std <- hh %>%
    transmute(
      city_id = cfg$project$city_id,
      survey_name = cfg$project$survey_name,
      survey_wave = cfg$project$survey_wave,
      household_id = household_id,
      household_weight = household_weight,
      home_tract = home_tract,
      home_county_fips = stringr::str_sub(home_tract, 1, 5),
      home_state_fips = stringr::str_sub(home_tract, 1, 2),
      home_taz_raw = NA_character_,
      home_zip_raw = home_zip_raw,
      income_code_raw = as.character(income),
      income_group_raw = as.character(income),
      income_group_simple = make_income_group_simple(income),
      vehicles = safe_integer(hhveh),
      vehicle_group_simple = make_vehicle_group_simple(hhveh),
      persons = safe_integer(hhsiz),
      workers = safe_integer(hhwrk),
      students = safe_integer(hhstu),
      survey_start_date = travel_date,
      survey_end_date = travel_date,
      complete_flag = complete_household,
      raw_source_file = "HH.xlsx"
    )

  persons_std <- person_day_context %>%
    transmute(
      city_id = cfg$project$city_id,
      survey_name = cfg$project$survey_name,
      survey_wave = cfg$project$survey_wave,
      household_id = household_id,
      person_id = person_id,
      person_weight = person_weight,
      age = safe_integer(age),
      gender_code_raw = as.character(gend),
      worker_flag = safe_integer(works) == 1L,
      student_flag = safe_integer(enrol) == 1L,
      work_tract = work_tract,
      school_tract = school_tract,
      work_taz_raw = NA_character_,
      school_taz_raw = NA_character_,
      person_type_raw = as.character(relate),
      complete_flag = !is.na(person_weight),
      raw_source_file = "PER.xlsx"
    ) %>%
    left_join(
      households_std %>% select(household_id, household_weight, income_group_simple, vehicle_group_simple),
      by = "household_id"
    )

  days_std <- person_day_context %>%
    transmute(
      city_id = cfg$project$city_id,
      survey_name = cfg$project$survey_name,
      survey_wave = cfg$project$survey_wave,
      household_id = household_id,
      person_id = person_id,
      day_id = paste0(household_id, "_", person_id, "_", survey_day_code),
      travel_date = travel_date,
      travel_dow = travel_dow,
      weekday_flag = travel_dow %in% 1:5,
      day_weight = person_weight,
      complete_day_flag = !is.na(person_weight) & !is.na(travel_dow),
      raw_source_file = "HH.xlsx + PER.xlsx"
    )

  place_seq <- place %>%
    mutate(
      household_id = as.character(sampn),
      person_id = as.character(perno),
      place_no = safe_integer(plano),
      purpose_code = safe_integer(tpurp),
      purpose_label = mts2011_purpose_label(tpurp),
      purpose_group = mts2011_purpose_group(tpurp),
      place_mode_code = safe_integer(mode),
      place_mode2_code = safe_integer(mode2),
      place_mode_label = mts2011_mode_label(mode, mode2),
      place_mode_group = mts2011_mode_group(mode),
      place_arrive_time = mts2011_hhmm_to_chr(arr_hr, arr_min),
      place_depart_time = mts2011_hhmm_to_chr(dep_hr, dep_min),
      place_arrive_seconds = hms_to_seconds(place_arrive_time),
      place_depart_seconds = hms_to_seconds(place_depart_time),
      place_tract = mts2011_geoid11(state10, county10, tract10),
      place_zip_raw = standardize_geoid5(zip),
      duration_minutes_here = safe_numeric(actdur),
      trip_duration_to_here = safe_numeric(trpdur)
    ) %>%
    arrange(household_id, person_id, place_no) %>%
    group_by(household_id, person_id) %>%
    mutate(
      prev_place_no = lag(place_no),
      origin_tract = lag(place_tract),
      destination_tract = place_tract,
      origin_zip_raw = lag(place_zip_raw),
      destination_zip_raw = place_zip_raw,
      origin_purpose_code_raw = lag(as.character(purpose_code)),
      origin_purpose_label = lag(purpose_label),
      destination_purpose_code_raw = as.character(purpose_code),
      destination_purpose_label = purpose_label,
      purpose_group_trip = purpose_group,
      depart_time_local = lag(place_depart_time),
      arrive_time_local = place_arrive_time,
      depart_seconds = lag(place_depart_seconds),
      depart_minutes_of_day = time_to_minutes_of_day(depart_seconds),
      main_mode_code_raw = as.character(place_mode_code),
      main_mode_label = place_mode_label,
      mode_group = place_mode_group,
      transit_involved_flag = place_mode_code %in% c(5L, 6L, 7L, 8L)
    ) %>%
    ungroup() %>%
    filter(!is.na(prev_place_no), place_no > 1L)

  trips_std <- place_seq %>%
    left_join(
      person_day_context %>%
        transmute(
          household_id,
          person_id,
          survey_day_code,
          household_weight,
          person_weight,
          income_group_simple,
          vehicle_group_simple,
          travel_dow,
          travel_date
        ),
      by = c("household_id", "person_id")
    ) %>%
    mutate(
      complete_day_flag = !is.na(person_weight) & !is.na(travel_dow),
      complete_trip_flag = !is.na(origin_tract) & !is.na(destination_tract) & !is.na(depart_seconds),
      trip_weight = dplyr::coalesce(person_weight, household_weight),
      analysis_weight = trip_weight,
      travel_dow_label = dplyr::case_match(
        safe_integer(travel_dow),
        1L ~ "Monday",
        2L ~ "Tuesday",
        3L ~ "Wednesday",
        4L ~ "Thursday",
        5L ~ "Friday",
        6L ~ "Saturday",
        7L ~ "Sunday",
        .default = as.character(travel_dow)
      )
    ) %>%
    transmute(
      city_id = cfg$project$city_id,
      survey_name = cfg$project$survey_name,
      survey_wave = cfg$project$survey_wave,
      trip_id = paste0(household_id, "_", person_id, "_", place_no),
      household_id = household_id,
      person_id = person_id,
      day_id = paste0(household_id, "_", person_id, "_", survey_day_code),
      travel_date = travel_date,
      travel_dow = safe_integer(travel_dow),
      travel_dow_label = travel_dow_label,
      weekday_flag = safe_integer(travel_dow) %in% 1:5,
      complete_day_flag = complete_day_flag,
      complete_trip_flag = complete_trip_flag,
      origin_tract = origin_tract,
      destination_tract = destination_tract,
      origin_county_fips = stringr::str_sub(origin_tract, 1, 5),
      destination_county_fips = stringr::str_sub(destination_tract, 1, 5),
      origin_taz_raw = NA_character_,
      destination_taz_raw = NA_character_,
      origin_zip_raw = origin_zip_raw,
      destination_zip_raw = destination_zip_raw,
      depart_time_local = depart_time_local,
      arrive_time_local = arrive_time_local,
      depart_seconds = depart_seconds,
      depart_minutes_of_day = depart_minutes_of_day,
      duration_minutes = trip_duration_to_here,
      distance_miles = NA_real_,
      main_mode_code_raw = main_mode_code_raw,
      main_mode_label = main_mode_label,
      mode_group = mode_group,
      transit_involved_flag = transit_involved_flag,
      origin_purpose_code_raw = origin_purpose_code_raw,
      origin_purpose_label = origin_purpose_label,
      destination_purpose_code_raw = destination_purpose_code_raw,
      destination_purpose_label = destination_purpose_label,
      purpose_category_code_raw = destination_purpose_code_raw,
      purpose_category_label = destination_purpose_label,
      purpose_group = purpose_group_trip,
      household_weight = household_weight,
      person_weight = person_weight,
      day_weight = person_weight,
      trip_weight = trip_weight,
      analysis_weight = analysis_weight,
      income_group_simple = income_group_simple,
      vehicle_group_simple = vehicle_group_simple,
      raw_source_file = "PLACE.xlsx",
      raw_record_id = paste0(household_id, "_", person_id, "_", place_no)
    )

  households_std <- households_std %>% filter(complete_flag)
  persons_std <- persons_std %>% filter(complete_flag)
  days_std <- days_std %>% filter(complete_day_flag)
  trips_std <- trips_std %>%
    filter(complete_trip_flag) %>%
    semi_join(persons_std %>% select(person_id, household_id), by = c("person_id", "household_id")) %>%
    semi_join(households_std %>% select(household_id), by = "household_id")

  qa_summary <- tibble::tibble(
    metric = c(
      "n_households",
      "n_persons",
      "n_days",
      "n_places",
      "n_trips"
    ),
    value = c(
      nrow(households_std),
      nrow(persons_std),
      nrow(days_std),
      nrow(place),
      nrow(trips_std)
    )
  )

  metadata <- list(
    adapter = "massachusetts_2011",
    survey_zip = cfg$active_survey_source$file_path,
    files_used = c("HH.xlsx", "PER.xlsx", "PLACE.xlsx", if (!is.null(veh)) "VEH.xlsx"),
    trip_construction = "Trips are derived from consecutive PLACE records within person-day sequences.",
    travel_day_field = "HH.ASSN",
    notes = c(
      "ASSN is converted to actual survey dates using the public data dictionary travel-day lookup.",
      "TRACT10 fields are combined with STATE10 and COUNTY10 components to form 11-digit census tract GEOIDs.",
      "ZIP fields are standardized to 5-digit strings.",
      "Public transit includes bus, rail, ferry, and dial-a-ride/paratransit in this adapter."
    ),
    n_households = nrow(households_std),
    n_persons = nrow(persons_std),
    n_days = nrow(days_std),
    n_places = nrow(place),
    n_trips = nrow(trips_std)
  )

  list(
    households = households_std,
    persons = persons_std,
    days = days_std,
    trips = trips_std,
    stages = NULL,
    qa_summary = qa_summary,
    metadata = metadata
  )
}
