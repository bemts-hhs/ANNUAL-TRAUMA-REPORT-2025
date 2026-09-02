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
## get TBI codes utilized by Toby Yak to classify records ----
tbi_codes <- c(
  # C70 series
  "C70.0",
  "C71.0",
  "C71.1",
  "C71.2",
  "C71.3",
  "C71.4",
  "C71.5",
  "C71.6",
  "C71.7",
  "C71.8",

  # C72 series
  "C72.50",
  "C72.59",

  # C79 series
  "C79.31",
  "C79.32",
  "C79.49",

  # D32 / D33 series
  "D32.0",
  "D32.9",
  "D33.3",
  "D33.2",

  # G-series
  "G04",
  "G05",
  "G06.0",
  "G06.1",
  "G93.1",

  # I60 block (entered as 160 in your criteria)
  "I60",
  # 160.9 excluded by the original filter logic, so not included

  # Remaining I-series
  "I61",
  "I62.1",
  "I65",
  "I66",
  "G45",
  "I67.89",
  "I67.9",

  # T-series toxic effects etc.
  "T41",
  "T42",
  "T43",
  "T44",
  "T51",
  "T58",
  "T71",
  "T75.1XXA",
  "T74.1",
  "T74.4",

  # S02 fractures
  "S02.0XXA",
  "S02.0XXB",
  "S02.1",
  "S02.91XA",

  # S06 concussion codes
  "S06.0X0A",
  "S06.0X1A",
  "S06.0X9A",

  # S06 other intracranial injuries
  "S06.31",
  "S06.32",
  "S06.33",
  "S06.4",
  "S06.5",
  "S06.6",
  "S06.36",
  "S06.8",
  "S06.9"
)

## tbi codes from matrix
tbi_matrix_codes <- nature_injury_mapping |> 
  dplyr::filter(BODY_REGION_CATEGORY_LEVEL_2 == "TBI") |> 
  dplyr::pull(ICD_10_CODE_FULL) |> 
  sort()

## union the tbi codes
tbi_codes_full <- base::union(tbi_codes, tbi_matrix_codes) |> unique() |> sort()

## transform raw codes into a regex ----
tbi_codes_pattern <- paste0(
  "(?:",
  paste0(
    gsub(pattern = "\\.", replacement = "\\\\.", x = tbi_codes_full),
    collapse = "|"
  ),
  ")"
)

# classify counties in the data ----
location_data <- readxl::read_excel(path = iowa_counties_districts_path)

# select variables of interest for Iowa county classification
location_data <- location_data |>
  dplyr::select(County, Designation, Urbanicity)
