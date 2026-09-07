###_____________________________________________________________________________
### Load data for the 2025 Annual Trauma report project ----
### For any analyses, these data must be loaded for the applicable section
### You must run setup.R first before utilizing this script
###_____________________________________________________________________________

### trauma data ----
trauma_data_2021 <- readr::read_csv(file = trauma_data_path_2021)
trauma_data_2022 <- readr::read_csv(file = trauma_data_path_2022)
trauma_data_2023 <- readr::read_csv(file = trauma_data_path_2023)
trauma_data_2024 <- readr::read_csv(file = trauma_data_path_2024)
trauma_data_2025 <- readr::read_csv(file = trauma_data_path_2025)
trauma_data_2026 <- readr::read_csv(file = trauma_data_path_2026)

## union the trauma data ----
trauma_data <- dplyr::bind_rows(
  trauma_data_2021 |>
    dplyr::mutate(dplyr::across(
      .cols = tidyselect::matches("_zip$|_fips$"),
      ~ as.character(.)
    )),
  trauma_data_2022 |>
    dplyr::mutate(dplyr::across(
      .cols = tidyselect::matches("_zip$|_fips$"),
      ~ as.character(.)
    )),
  trauma_data_2023 |>
    dplyr::mutate(dplyr::across(
      .cols = tidyselect::matches("_zip$|_fips$"),
      ~ as.character(.)
    )),
  trauma_data_2024 |>
    dplyr::mutate(dplyr::across(
      .cols = tidyselect::matches("_zip$|_fips$"),
      ~ as.character(.)
    )),
  trauma_data_2025 |>
    dplyr::mutate(dplyr::across(
      .cols = tidyselect::matches("_zip$|_fips$"),
      ~ as.character(.)
    ))
)

## deal with missing values in cause of injury categories ----
trauma_data_clean <- trauma_data |>
  dplyr::mutate(
    Age_Group = dplyr::case_when(
      Patient_Age_Years < 5 ~ "0-4",
      Patient_Age_Years >= 5 & Patient_Age_Years < 10 ~ "5-9",
      Patient_Age_Years >= 10 & Patient_Age_Years < 15 ~ "10-14",
      Patient_Age_Years >= 15 & Patient_Age_Years < 20 ~ "15-19",
      Patient_Age_Years >= 20 & Patient_Age_Years < 25 ~ "20-24",
      Patient_Age_Years >= 25 & Patient_Age_Years < 30 ~ "25-29",
      Patient_Age_Years >= 30 & Patient_Age_Years < 35 ~ "30-34",
      Patient_Age_Years >= 35 & Patient_Age_Years < 40 ~ "35-39",
      Patient_Age_Years >= 40 & Patient_Age_Years < 45 ~ "40-44",
      Patient_Age_Years >= 45 & Patient_Age_Years < 50 ~ "45-49",
      Patient_Age_Years >= 50 & Patient_Age_Years < 55 ~ "50-54",
      Patient_Age_Years >= 55 & Patient_Age_Years < 60 ~ "55-59",
      Patient_Age_Years >= 60 & Patient_Age_Years < 65 ~ "60-64",
      Patient_Age_Years >= 65 & Patient_Age_Years < 70 ~ "65-69",
      Patient_Age_Years >= 70 & Patient_Age_Years < 75 ~ "70-74",
      Patient_Age_Years >= 75 & Patient_Age_Years < 80 ~ "75-79",
      Patient_Age_Years >= 80 & Patient_Age_Years < 85 ~ "80-84",
      Patient_Age_Years >= 85 ~ "85+",
      TRUE ~ "Missing",
      .default = "Missing"
    ),
    Age_Group = factor(
      Age_Group,
      levels = c(
        "0-4",
        "5-9",
        "10-14",
        "15-19",
        "20-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50-54",
        "55-59",
        "60-64",
        "65-69",
        "70-74",
        "75-79",
        "80-84",
        "85+",
        "Missing"
      )
    ),
    .after = Age_Range
  ) |>
  dplyr::mutate(
    Injury_County = stringr::str_to_title(Injury_County),
    Injury_County = dplyr::if_else(
      grepl(pattern = "o'b", x = Injury_County, ignore.case = TRUE),
      "O'Brien",
      Injury_County
    ),
    NATURE_OF_INJURY_DESCRIPTOR_1 = dplyr::if_else(
      is.na(NATURE_OF_INJURY_DESCRIPTOR_1),
      NATURE_OF_INJURY_DESCRIPTOR_2,
      NATURE_OF_INJURY_DESCRIPTOR_1
    ),
    BODY_REGION_CATEGORY_LEVEL_1_1 = dplyr::if_else(
      is.na(BODY_REGION_CATEGORY_LEVEL_1_1),
      BODY_REGION_CATEGORY_LEVEL_1_2,
      BODY_REGION_CATEGORY_LEVEL_1_1
    ),
    BODY_REGION_CATEGORY_LEVEL_2_1 = dplyr::if_else(
      is.na(BODY_REGION_CATEGORY_LEVEL_2_1),
      BODY_REGION_CATEGORY_LEVEL_2_2,
      BODY_REGION_CATEGORY_LEVEL_2_1
    ),
    BODY_REGION_CATEGORY_LEVEL_3_1 = dplyr::if_else(
      is.na(BODY_REGION_CATEGORY_LEVEL_3_1),
      BODY_REGION_CATEGORY_LEVEL_3_2,
      BODY_REGION_CATEGORY_LEVEL_3_1
    ),
    INTENTIONALITY_1 = dplyr::if_else(
      is.na(INTENTIONALITY_1),
      INTENTIONALITY_2,
      INTENTIONALITY_1
    ),
    MECHANISM_1 = dplyr::if_else(is.na(MECHANISM_1), MECHANISM_2, MECHANISM_1),
    LEVEL_FALL1_1 = dplyr::if_else(
      is.na(LEVEL_FALL1_1),
      LEVEL_FALL1_2,
      LEVEL_FALL1_1
    ),
    CAUSE_OF_INJURY_AR_1 = dplyr::if_else(
      is.na(CAUSE_OF_INJURY_AR_1),
      CAUSE_OF_INJURY_AR_2,
      CAUSE_OF_INJURY_AR_1
    )
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(Patient_Gender, Sex_Assigned_at_Birth),
      ~ stringr::str_squish(.)
    ),
    Sex = dplyr::case_when(
      is.na(Sex_Assigned_at_Birth) & !is.na(Patient_Gender) ~ Patient_Gender,
      TRUE ~ Sex_Assigned_at_Birth
    ),
    Sex = ifelse(is.na(Sex), "Not Known/Not Recorded", Sex),
    .after = Sex_Assigned_at_Birth
  ) |>
  dplyr::mutate(
    TBI = grepl(
      pattern = tbi_codes_pattern,
      x = ICD_10_Diagnosis_Codes_List,
      ignore.case = TRUE
    ) |
      grepl(
        pattern = tbi_codes_pattern,
        x = ICD_10_Injury_Codes_List,
        ignore.case = TRUE
      ),
    .after = ICD_10_Injury_Codes_List
  ) |>
  dplyr::mutate(
    Ever_Death = any(Death, na.rm = T),
    .by = Unique_Patient_ID,
    .after = Death
  ) |>
  dplyr::left_join(location_data, by = c("Patient_County" = "County")) |>
  dplyr::rename(
    Designation_Patient = Designation,
    Urbanicity_Patient = Urbanicity
  ) |>
  dplyr::relocate(
    tidyselect::all_of(c("Designation_Patient", "Urbanicity_Patient")),
    .after = Patient_County
  ) |>
  dplyr::left_join(location_data, by = c("Injury_County" = "County")) |>
  dplyr::rename(
    Designation_Injury = Designation,
    Urbanicity_Injury = Urbanicity
  ) |>
  dplyr::relocate(
    tidyselect::all_of(c("Designation_Injury", "Urbanicity_Injury")),
    .after = Injury_County
  ) |>
  dplyr::left_join(location_data, by = "County") |>
  dplyr::relocate(
    tidyselect::all_of(c("Designation", "Urbanicity")),
    .after = County
  )

## get the trauma data for the year of interest ----
trauma_2025 <- trauma_data_clean |> dplyr::filter(Year == 2025)

## check the trauma data ----
dplyr::glimpse(trauma_2025)

## ems data ----
ems_data <- readr::read_csv(file = ems_data_path)

## deal with missing injury categories ----
ems_data_clean <- ems_data |>
  dplyr::mutate(Injury_1 = dplyr::if_else(is.na(Injury_1), Injury_2, Injury_1))

## check the ems data ----
dplyr::glimpse(ems_data_clean)
