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

# classify counties in the data ----
location_data <- readxl::read_excel(path = iowa_counties_districts_path)

# select variables of interest for Iowa county classification
location_data <- location_data |>
  dplyr::select(County, Designation, Urbanicity)
