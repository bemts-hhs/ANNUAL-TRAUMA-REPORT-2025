###___________________________________________________________________________
# Trauma Events Section ----
###___________________________________________________________________________

####
# You must first run data_load.R and setup.R before running this script
####

###_____________________________________________________________________________
## Get counts ----
###_____________________________________________________________________________

### patients ----

#### get the current year's patient count ----
patient_count <- trauma_2025 |>
  injury_patient_count() |>
  dplyr::pull(n)

#### get the trend ----
patient_count_years <- trauma_data_clean |>
  injury_patient_count(Year, descriptive_stats = TRUE)

##### a gt table ----
patients_gt <- patient_count_years |>
  gt::gt() |>
  gt::cols_hide(prop_change) |>
  gt::cols_label(
    "n" ~ "Count",
    "change" ~ "Count Change",
    "prop_label" ~ "% Change"
  ) |>
  gt::sub_missing() |>
  gt::fmt_number(columns = 2:3, drop_trailing_zeros = TRUE) |>
  tab_style_hhs(
    message_text = NULL,
    border_cols = 2:last_col(),
    column_labels = 18,
    body = 16
  )

###### save the table to xlsx ----
patients_gt_xlsx <- openxlsx2::wb_workbook() |>
  openxlsx2::wb_add_worksheet(grid_lines = FALSE) |>
  gtxlsx::wb_add_gt(
    x = patients_gt
  )

###### write to the table to xlsx ----
openxlsx2::wb_save(
  wb = patients_gt_xlsx,
  file = paste0(output_folder, "/patients.xlsx"),
  overwrite = TRUE,
  flush = TRUE
)

###### save the table as png ----
gt::gtsave(
  patients_gt,
  filename = "patients_gt.png",
  path = output_folder
)

#### a line plot ----
patient_count_plot <- patient_count_years |>
  ggplot2::ggplot(ggplot2::aes(
    x = Year,
    y = n,
    label = ifelse(
      Year %in% c(2021, 2025),
      traumar::pretty_number(x = n, digits = 2),
      NA
    )
  )) +
  ggplot2::geom_line(
    linewidth = 1.75,
    color = "lightblue",
    lineend = "round",
    linejoin = "round"
  ) +
  ggrepel::geom_text_repel(
    nudge_y = -250,
    segment.color = "transparent",
    seed = 8182024,
    direction = "both",
    color = "darkblue",
    size = 10,
    fontface = "bold",
    family = "Work Sans"
  ) +
  traumar::theme_cleaner(base_family = "Work Sans") +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(size = 30)
  ) +
  ggplot2::scale_y_continuous(labels = function(x) {
    traumar::pretty_number(x, digits = 2)
  })

##### save the bar plot ----
ggplot2::ggsave(
  filename = "patient_count_plot.png",
  plot = patient_count_plot,
  path = plot_folder,
  height = 6,
  width = 6 * (16 / 9)
)

### injuries ----

#### get the current year's count of events ----
incident_count <- trauma_2025 |>
  injury_incident_count() |>
  dplyr::pull(n)

#### get the trend ----
incident_count_years <- trauma_data_clean |>
  injury_incident_count(Year, descriptive_stats = TRUE)

##### a line graph ----
incident_count_plot <- incident_count_years |>
  ggplot2::ggplot(ggplot2::aes(
    x = Year,
    y = n,
    label = ifelse(
      Year %in% c(2021, 2025),
      traumar::pretty_number(x = n, digits = 2),
      NA
    )
  )) +
  ggplot2::geom_line(
    linewidth = 1.75,
    color = "lightblue",
    lineend = "round",
    linejoin = "round"
  ) +
  ggrepel::geom_text_repel(
    nudge_y = ifelse(incident_count_years$Year == 2021, -300, 400),
    segment.color = "transparent",
    seed = 8182024,
    direction = "both",
    color = "darkblue",
    size = 15,
    fontface = "bold",
    family = "Work Sans"
  ) +
  traumar::theme_cleaner(base_family = "Work Sans", base_size = 30) +
  ggplot2::theme(
    axis.title = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank()
  ) +
  ggplot2::scale_y_continuous(labels = function(x) {
    traumar::pretty_number(x, digits = 2)
  })

###### save the incident plot ----
ggplot2::ggsave(
  filename = "incident_count_plot.png",
  plot = incident_count_plot,
  path = plot_folder,
  height = 6,
  width = 6 * (16 / 9)
)

##### generate the incident table ----
incidents_gt <- incident_count_years |>
  gt::gt() |>
  gt::cols_hide(columns = c(prop_change, Min_Reinjury:Q75_Reinjury)) |>
  gt::cols_label(
    "n" ~ "Count",
    "change" ~ "Count Change",
    "prop_label" ~ "% Change"
  ) |>
  gt::sub_missing() |>
  gt::fmt_number(columns = 2:3, drop_trailing_zeros = TRUE) |>
  tab_style_hhs(message_text = NULL, border_cols = 2:last_col())

###### save the incident table to xlsx
incidents_gt_xlsx <- openxlsx2::wb_workbook() |>
  openxlsx2::wb_add_worksheet(grid_lines = FALSE) |>
  gtxlsx::wb_add_gt(
    x = incidents_gt
  )

###### export xlsx to file ----
openxlsx2::wb_save(
  wb = incidents_gt_xlsx,
  file = paste0(output_folder, "/incidents.xlsx"),
  overwrite = TRUE,
  flush = TRUE
)

###### save the incident table to gt ----
gt::gtsave(
  incidents_gt,
  filename = "incidents_gt.png",
  path = output_folder
)

### cases ----

#### case counts ----
case_count <- trauma_2025 |>
  injury_case_count() |>
  dplyr::pull(n)

#### cases over time ----
case_count_years <- trauma_data_clean |>
  injury_case_count(Year, descriptive_stats = TRUE)

##### cases gt table ----
cases_gt <- case_count_years |>
  gt::gt() |>
  gt::cols_hide(prop_change) |>
  gt::cols_label(
    "n" ~ "Count",
    "change" ~ "Count Change",
    "prop_label" ~ "% Change"
  ) |>
  gt::sub_missing() |>
  gt::fmt_number(columns = 2:3, drop_trailing_zeros = TRUE) |>
  tab_style_hhs(message_text = NULL, border_cols = 2:last_col())

###### save the incident gt table to xlsx ----
cases_gt_xlsx <- openxlsx2::wb_workbook() |>
  openxlsx2::wb_add_worksheet(grid_lines = FALSE) |>
  gtxlsx::wb_add_gt(
    x = cases_gt
  )

###### save the incident xlsx to disk ----
openxlsx2::wb_save(
  wb = cases_gt_xlsx,
  file = paste0(output_folder, "/cases.xlsx"),
  overwrite = TRUE,
  flush = TRUE
)

###### save the cases gt table to png ----
gt::gtsave(
  cases_gt,
  filename = "cases_gt.png",
  path = output_folder
)
