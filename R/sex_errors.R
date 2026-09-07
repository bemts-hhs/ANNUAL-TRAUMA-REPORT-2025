###___________________________________________________________________________
# Significant submission errors occurred in the sex assigned at birth (SAB) field ----
# This script creates files that give facilities identifiable data about their
# records missing SAB, and calculates the scale of the missingness. All these
# items are then saved to disk to help facilities report these missing records.
###___________________________________________________________________________

## get 2025 and 2026 data to estimate all errors ----
### union 2025 and 2026 ----
trauma_2025_2026 <- trauma_data_2025 |>
  dplyr::mutate(
    dplyr::across(tidyselect::matches("_zip$|fips$"), ~ as.character(.))
  ) |>
  dplyr::bind_rows(
    trauma_data_2026 |>
      dplyr::mutate(
        dplyr::across(tidyselect::matches("_zip$|fips$"), ~ as.character(.))
      )
  )

### summarize errors per year ----
total_sex_errors <- trauma_2025_2026 |>
  dplyr::distinct(Year, Unique_Incident_ID, .keep_all = TRUE) |>
  dplyr::count(Year, Patient_Gender)

#### save the state level errors to disk ----
readr::write_csv(
  x = total_sex_errors,
  file = paste0(error_path, "/total_sex_errors.csv")
)

### summarize errors per year-month  ----
total_sex_errors_months <- trauma_2025_2026 |>
  dplyr::mutate(
    Month = lubridate::floor_date(ED_Acute_Care_Admission_Date, unit = "month")
  ) |>
  dplyr::distinct(Year, Unique_Incident_ID, .keep_all = TRUE) |>
  dplyr::summarize(
    total_errors = sum(Patient_Gender == "Not Known/Not Recorded", na.rm = T),
    records = dplyr::n(),
    pct_error = total_errors / records,
    .by = Month
  ) |>
  dplyr::arrange(Month) |>
  dplyr::mutate(
    Year = lubridate::year(Month)
  ) |>
  dplyr::mutate(
    cumulative_errors_yr = cumsum(total_errors),
    cumulative_pct_yr = cumulative_errors_yr / sum(total_errors),
    .by = Year
  ) |>
  dplyr::mutate(
    cumulative_errors_total = cumsum(total_errors),
    cumulative_pct_total = cumulative_errors_total / sum(total_errors)
  ) |>
  dplyr::relocate(Year, .before = 1)

#### save the state level errors by month to disk ----
readr::write_csv(
  x = total_sex_errors_months,
  file = paste0(error_path, "/total_sex_errors_months.csv")
)

### plot errors over months and years ----
total_sex_errors_plot <- total_sex_errors_months |>
  ggplot2::ggplot() +
  ggplot2::geom_line(
    ggplot2::aes(x = Month, y = total_errors, group = 1),
    linewidth = 1.5,
    linejoin = "round",
    lineend = "round",
    color = "steelblue"
  ) +
  ggplot2::labs(
    title = "Data Entry Errors in Sex Assigned at Birth",
    subtitle = "Iowa Trauma Registry | 2025-2026",
    x = "",
    y = "Record Error Count"
  ) +
  ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  ggthemes::theme_tufte() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

#### save the total errors plot ----
ggplot2::ggsave(
  filename = "total_sex_errors.png",
  plot = total_sex_errors_plot,
  path = error_path
)

### plot cumulative pct errors over time ----
cumulative_pct_errors_plot <- total_sex_errors_months |>
  ggplot2::ggplot() +
  ggplot2::geom_line(
    ggplot2::aes(x = Month, y = cumulative_pct_total, group = 1),
    linewidth = 1.5,
    linejoin = "round",
    lineend = "round",
    color = "coral"
  ) +
  ggplot2::geom_hline(
    ggplot2::aes(yintercept = 1),
    linetype = "dashed",
    color = "gray"
  ) +
  ggplot2::labs(
    title = "Cumulative Pct Data Entry Errors in Sex Assigned at Birth",
    subtitle = "Iowa Trauma Registry | 2025-2026",
    x = "",
    y = "Record Error Count"
  ) +
  ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggthemes::theme_tufte() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

#### save the total errors plot ----
ggplot2::ggsave(
  filename = "cumulative_pct_errors.png",
  plot = cumulative_pct_errors_plot,
  path = error_path
)

### plot records over the months with years side by side ----
total_sex_errors_facet_plot <- total_sex_errors_months |>
  ggplot2::ggplot() +
  ggplot2::geom_line(
    ggplot2::aes(x = Month, y = total_errors, group = 1),
    linewidth = 1.5,
    linejoin = "round",
    lineend = "round",
    color = "green"
  ) +
  ggplot2::labs(
    title = "Data Entry Errors in Sex Assigned at Birth",
    subtitle = "Iowa Trauma Registry | 2025-2026",
    x = "",
    y = "Record Error Count"
  ) +
  ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  ggthemes::theme_tufte() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  ) +
  ggplot2::facet_wrap(~Year, scales = "free_x")

#### save the total errors plot ----
ggplot2::ggsave(
  filename = "total_sex_errors_facet_years.png",
  plot = total_sex_errors_facet_plot,
  path = error_path
)

## count errors per year and facility with proportions
total_sex_errors_per_facility <- trauma_2025_2026 |>
  dplyr::distinct(Year, Unique_Incident_ID, .keep_all = TRUE) |>
  dplyr::summarize(
    sex_assigned_at_birth_errors = sum(Patient_Gender == "Not Known/Not Recorded", na.rm = TRUE),
    
  )
