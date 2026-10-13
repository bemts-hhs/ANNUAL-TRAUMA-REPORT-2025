###___________________________________________________________________________
# SEQIC Indicator Calculations for Q3 2026 meeting
###___________________________________________________________________________

# seqic indicator 7 ----

## seqic 7 by mechanism ----
seqic_7_mechanism <- trauma_data_clean |>
  dplyr::filter(Acute_Transfer_Out == "No") |>
  traumar::seqic_indicator_7(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    time_from_injury_to_arrival = Time_From_Injury_to_Arrival_Calc,
    transfer_out_indicator = Acute_Transfer_Out,
    groups = c("Year", "CAUSE_OF_INJURY_AR_1"),
    calculate_ci = "c"
  ) |>
  dplyr::mutate(
    dplyr::across(
      numerator_7:denominator_7,
      ~ prettyNum(x = ., big.mark = ",")
    ),
    dplyr::across(
      seqic_7:upper_ci_7,
      ~ scales::percent(., accuracy = 0.1)
    ),
    performance = glue::glue(
      "{seqic_7} [{lower_ci_7}, {upper_ci_7}], n={denominator_7}"
    )
  ) |>
  dplyr::select(Year, CAUSE_OF_INJURY_AR_1, performance) |>
  tidyr::pivot_wider(
    id_cols = CAUSE_OF_INJURY_AR_1,
    names_from = Year,
    values_from = performance
  ) |>
  dplyr::filter(!is.na(CAUSE_OF_INJURY_AR_1))

### save seqic 7 by mechanism to disk ----
seqic_7_mechanism_wb <- openxlsx2::wb_workbook() |>
  openxlsx2::wb_add_worksheet(sheet = "seqic_7_mechansim") |>
  openxlsx2::wb_add_data(x = seqic_7_mechanism)

#### save operation ----
openxlsx2::wb_save(
  wb = seqic_7_mechanism_wb,
  file = paste0(output_folder, "/seqic/seqic_7_mechanism_wb.xlsx"),
  overwrite = TRUE
)

## seqic 7 by districts ----
seqic_7_district <- trauma_data_clean |>
  dplyr::filter(Acute_Transfer_Out == "No") |>
  traumar::seqic_indicator_7(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    time_from_injury_to_arrival = Time_From_Injury_to_Arrival_Calc,
    transfer_out_indicator = Acute_Transfer_Out,
    groups = c("Year", "Service Area"),
    calculate_ci = "c"
  ) |>
  dplyr::mutate(
    dplyr::across(
      numerator_7:denominator_7,
      ~ prettyNum(x = ., big.mark = ",")
    ),
    dplyr::across(
      seqic_7:upper_ci_7,
      ~ scales::percent(., accuracy = 0.1)
    ),
    performance = glue::glue(
      "{seqic_7} [{lower_ci_7}, {upper_ci_7}], n={denominator_7}"
    )
  ) |>
  dplyr::select(Year, `Service Area`, performance) |>
  tidyr::pivot_wider(
    id_cols = `Service Area`,
    names_from = Year,
    values_from = performance
  )

### save seqic 7 by district to disk ----
seqic_7_district_wb <- openxlsx2::wb_workbook() |>
  openxlsx2::wb_add_worksheet(sheet = "seqic_7_district") |>
  openxlsx2::wb_add_data(x = seqic_7_district)

#### save operation ----
openxlsx2::wb_save(
  wb = seqic_7_district_wb,
  file = paste0(output_folder, "/seqic/seqic_7_district_wb.xlsx")
)

## seqic 7 by districts ----
seqic_7_iss <- trauma_data_clean |>
  dplyr::filter(Acute_Transfer_Out == "No") |>
  traumar::seqic_indicator_7(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    time_from_injury_to_arrival = Time_From_Injury_to_Arrival_Calc,
    transfer_out_indicator = Acute_Transfer_Out,
    groups = c("Year", "ISS_Range"),
    calculate_ci = "c"
  ) |>
  dplyr::mutate(
    dplyr::across(
      numerator_7:denominator_7,
      ~ prettyNum(x = ., big.mark = ",")
    ),
    dplyr::across(
      seqic_7:upper_ci_7,
      ~ scales::percent(., accuracy = 0.1)
    ),
    performance = glue::glue(
      "{seqic_7} [{lower_ci_7}, {upper_ci_7}], n={denominator_7}"
    )
  ) |>
  dplyr::select(Year, ISS_Range, performance) |>
  tidyr::pivot_wider(
    id_cols = ISS_Range,
    names_from = Year,
    values_from = performance
  ) |>
  dplyr::filter(!is.na(ISS_Range))

### save seqic 7 by district to disk ----
seqic_7_iss_wb <- openxlsx2::wb_workbook() |>
  openxlsx2::wb_add_worksheet(sheet = "seqic_7_iss") |>
  openxlsx2::wb_add_data(x = seqic_7_iss)

#### save operation ----
openxlsx2::wb_save(
  wb = seqic_7_iss_wb,
  file = paste0(output_folder, "/seqic/seqic_7_iss_wb.xlsx"),
  overwrite = TRUE
)

# seqic indicator 10

## seqic indicator 10a by mechanism ----
seqic_10_mechanism_under <- trauma_data_clean |>
  traumar::seqic_indicator_10(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    trauma_team_activation_level = Trauma_Team_Activation_Level,
    iss = ISS_Calculated,
    nfti = NULL,
    groups = c("Year", "CAUSE_OF_INJURY_AR_1"),
    calculate_ci = "c"
  ) |>
  purrr::pluck(1) |>
  dplyr::select(-tidyselect::matches("_.+(?:b|c)$")) |>
  dplyr::mutate(
    dplyr::across(
      numerator_10a:denominator_10a,
      ~ prettyNum(x = ., big.mark = ",")
    ),
    dplyr::across(
      seqic_10a:upper_ci_10a,
      ~ scales::percent(., accuracy = 0.1)
    ),
    performance = glue::glue(
      "{seqic_10a} [{lower_ci_10a}, {upper_ci_10a}], n={denominator_10a}"
    )
  ) |>
  dplyr::select(Year, CAUSE_OF_INJURY_AR_1, performance) |>
  tidyr::pivot_wider(
    id_cols = CAUSE_OF_INJURY_AR_1,
    names_from = Year,
    values_from = performance
  ) |>
  dplyr::filter(!is.na(CAUSE_OF_INJURY_AR_1))

### save seqic 10 by mechanism to disk ----
seqic_10_mechanism_under_wb <- openxlsx2::wb_workbook() |>
  openxlsx2::wb_add_worksheet(sheet = "seqic_10_mechanism_under") |>
  openxlsx2::wb_add_data(x = seqic_10_mechanism_under)

#### save operation ----
openxlsx2::wb_save(
  wb = seqic_10_mechanism_under_wb,
  file = paste0(output_folder, "/seqic/seqic_10_mechanism_under_wb.xlsx"),
  overwrite = TRUE
)

## seqic indicator 10b by mechanism ----
seqic_10_mechanism_over <- trauma_data_clean |>
  traumar::seqic_indicator_10(
    level = Level,
    unique_incident_id = Unique_Incident_ID,
    transfer_out_indicator = Acute_Transfer_Out,
    trauma_team_activation_level = Trauma_Team_Activation_Level,
    iss = ISS_Calculated,
    nfti = NULL,
    groups = c("Year", "CAUSE_OF_INJURY_AR_1"),
    calculate_ci = "c"
  ) |>
  purrr::pluck(1) |>
  dplyr::select(-tidyselect::matches("_.+(?:a|c)$")) |>
  dplyr::mutate(
    dplyr::across(
      numerator_10b:denominator_10b,
      ~ prettyNum(x = ., big.mark = ",")
    ),
    dplyr::across(
      seqic_10b:upper_ci_10b,
      ~ scales::percent(., accuracy = 0.1)
    ),
    performance = glue::glue(
      "{seqic_10b} [{lower_ci_10b}, {upper_ci_10b}], n={denominator_10b}"
    )
  ) |>
  dplyr::select(Year, CAUSE_OF_INJURY_AR_1, performance) |>
  tidyr::pivot_wider(
    id_cols = CAUSE_OF_INJURY_AR_1,
    names_from = Year,
    values_from = performance
  ) |>
  dplyr::filter(!is.na(CAUSE_OF_INJURY_AR_1))

### save seqic 10 by mechanism to disk ----
seqic_10_mechanism_over_wb <- openxlsx2::wb_workbook() |>
  openxlsx2::wb_add_worksheet(sheet = "seqic_10_mechanism_over") |>
  openxlsx2::wb_add_data(x = seqic_10_mechanism_over)

#### save operation ----
openxlsx2::wb_save(
  wb = seqic_10_mechanism_over_wb,
  file = paste0(output_folder, "/seqic/seqic_10_mechanism_over_wb.xlsx"),
  overwrite = TRUE
)

## seqic indicator 10 by nature of injury ----

## seqic indicator 10 by body region ----
