###_____________________________________________________________________________
# Load the files used to categorize mechanism and nature of injury ----
# based on the ICD-10 injury code
# NOTICE THAT IN ORDER TO GET THE SAME COUNTS AS IN POWER BI WITH REGARD TO THE
# CAUSE OF INJURY / NATURE OF INJURY / body region (lvl1 and lvl2) you must run
# RUN distinct(Unique_Incident_ID, [coi_ar, cc2, body region], .keep_all = TRUE)
# and then your count() function or else you will not get the same counts in R.
# POWER BI does a better job of automating the grouping via AI, and in R you have
# to do that manually.
###_____________________________________________________________________________

# mechanism of injury mapping ----
mechanism_injury_mapping <- readr::read_csv(
  file = mech_injury_path
)

# select variables of interest for mappings
mechanism_injury_mapping <- mechanism_injury_mapping |>
  dplyr::select(
    UPPER_CODE,
    INTENTIONALITY,
    CUSTOM_CATEGORY2,
    CAUSE_OF_INJURY_AR
  )

# nature of injury mapping ----
nature_injury_mapping <- readxl::read_excel(path = injury_matrix_path)

# select variables of interest for mappings
nature_injury_mapping <- nature_injury_mapping |>
  dplyr::select(
    ICD_10_CODE_FULL,
    ICD_10_CODE_TRIM,
    NATURE_OF_INJURY_DESCRIPTOR,
    BODY_REGION_CATEGORY_LEVEL_1,
    BODY_REGION_CATEGORY_LEVEL_2
  )

###___________________________________________________________________________
# TBI codes ----
###___________________________________________________________________________

## tbi codes from matrix
tbi_matrix_codes <- nature_injury_mapping |>
  dplyr::filter(BODY_REGION_CATEGORY_LEVEL_2 == "TBI") |>
  dplyr::pull(ICD_10_CODE_FULL) |>
  sort()

## make the unioned codes more suitable for regex ----
tbi_codes_sub <- gsub(
  pattern = "\\.",
  replacement = "\\\\.",
  x = tbi_matrix_codes
)

## transform raw codes into a regex ----
tbi_codes_pattern <- paste0(
  "(?:",
  paste0(tbi_codes_sub, collapse = "|"),
  ")"
)

###___________________________________________________________________________
# E-Scooter codes ----
###___________________________________________________________________________

## all e-scooter injuries
all_escooter_pattern <- paste0(
  "(?:v00\\.(?:03|84)|v0[1-6]\\.[019]3)"
)

## e-scooter with MVC ----
escooter_mvc_pattern <- paste0(
  "(?:v0[345]\\.[019]3)"
)

## e-scooter strike or fall ----
# strike a pedestrian, or the ground, respectively
escooter_strike_fall_pattern <- paste0(
  "(?:v00\\.03|v00\\.84)"
)

## e-scooter non-motor vehicle collision ----
# e.g. bicycle
escooter_non_mvc_pattern <- paste0(
  "(?:v0[16]\\.[019]3)"
)

###___________________________________________________________________________
# E-Bike codes ----
###___________________________________________________________________________

## all e-bike codes ----
all_ebike_pattern <- paste0(
  "(?:v2[0-8]\\.[0123459]1|v29\\.[012456][09]1|v29\\.8[18]1|v29\\.31|v29\\.91)"
)

## mvc traffic collision e-bike codes ----
ebike_mvc_pattern <- paste0(
  "(?:v2[2345]\\.[459]1|v29\\.[456][09]1|v29\\.8[18]1)"
)

###___________________________________________________________________________
# All e-scooter and e-bike ----
###___________________________________________________________________________

## all e-scooter and e-bike ----
escooter_ebike_pattern <- paste0(
  all_escooter_pattern,
  "|",
  all_ebike_pattern
)

## all e-scooter and e-bike MVC ----
escooter_ebike_mvc_pattern <- paste0(
  escooter_mvc_pattern,
  "|",
  ebike_mvc_pattern
)

# classify counties in the data ----
location_data <- readxl::read_excel(path = iowa_counties_districts_path)

# select variables of interest for Iowa county classification
location_data <- location_data |>
  dplyr::select(County, Designation, Urbanicity)
