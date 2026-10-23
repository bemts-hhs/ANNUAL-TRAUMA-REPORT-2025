###_____________________________________________________________________________
### Mechanism of Injury ----
###_____________________________________________________________________________

###
# In order to utilize this script, you must first run setup.R and then
# data_load.R in order to have the necessary custom functions and data.
###

# get counts and proportions of injury events by mechanism of injury ----
mechanism_of_injury_counts <- trauma_data_clean |>
  dplyr::filter(!is.na(CAUSE_OF_INJURY_AR_1)) |>
  injury_incident_count(
    Year,
    CAUSE_OF_INJURY_AR_1,
    descriptive_stats = TRUE,
    group = CAUSE_OF_INJURY_AR_1
  ) |>
  dplyr::mutate(valid_percent = n / sum(n), .by = Year) |>
  dplyr::arrange(CAUSE_OF_INJURY_AR_1)

# get a subset of the counts for the plot ----
mechanism_of_injury_counts_select <- mechanism_of_injury_counts |>
  dplyr::filter(Year == 2025)

# plot the distribution of mechanism ----
mechanism_of_injury_cols <- mechanism_of_injury_counts_select |>
  ggplot2::ggplot(ggplot2::aes(
    x = reorder(x = CAUSE_OF_INJURY_AR_1, n),
    y = n,
    label = prettyNum(x = n, big.mark = ","),
    fill = n
  )) +
  ggplot2::geom_col(
    width = 0.75,
    position = ggplot2::position_dodge(width = 0.25)
  ) +
  ggrepel::geom_text_repel(
    ggplot2::aes(y = ifelse(CAUSE_OF_INJURY_AR_1 == "Poisoning", 100, n)),
    family = "Work Sans",
    size = 18,
    fontface = "bold",
    direction = "x",
    color = ifelse(
      !mechanism_of_injury_counts_select$CAUSE_OF_INJURY_AR_1 %in%
        c("Poisoning", "Firearm", "Struck by/against"),
      "white",
      "black"
    ),
    nudge_y = ifelse(
      !mechanism_of_injury_counts_select$CAUSE_OF_INJURY_AR_1 %in%
        c("Poisoning", "Firearm", "Struck by/against"),
      -1,
      1
    ),
    segment.color = "transparent",
    seed = 12
  ) +
  ggplot2::guides(fill = "none") +
  traumar::theme_cleaner(
    base_size = 20,
    axis.text.x = ggplot2::element_blank()
  ) +
  ggplot2::labs(x = "", y = "") +
  ggplot2::coord_flip()

# save the treemap
ggplot2::ggsave(
  filename = "mechanism_of_injury_cols.png",
  plot = mechanism_of_injury_cols,
  path = plot_folder,
  height = 6.67,
  width = 6.67 * 1.78
)

# get quarterly counts and proportions of injury events by mechanism of injury ----
mechanism_of_injury_counts_quarterly <- trauma_data_clean |>
  dplyr::filter(!is.na(CAUSE_OF_INJURY_AR_1)) |>
  injury_incident_count(Admission_Quarter, CAUSE_OF_INJURY_AR_1) |>
  dplyr::mutate(valid_percent = n / sum(n), .by = Admission_Quarter) |>
  dplyr::group_by(Admission_Quarter) |>
  dplyr::arrange(desc(n), .by_group = TRUE) |>
  dplyr::ungroup()

# plot the distribution of mechanism ----
mechanism_of_injury_lines <- mechanism_of_injury_counts_quarterly |>
  ggplot2::ggplot(ggplot2::aes(
    x = Admission_Quarter,
    y = n,
    color = reorder(CAUSE_OF_INJURY_AR_1, -n)
  )) +
  ggplot2::geom_line(
    ggplot2::aes(alpha = 0.9),
    linewidth = 1.25,
    lineend = "round",
    linejoin = "round"
  ) +
  ggplot2::geom_point(
    ggplot2::aes(alpha = 0.9),
    size = 2
  ) +
  ggplot2::labs(x = "", y = "", color = "Mechanism") +
  ggplot2::scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y-%m",
    limits = c(as.Date("2021-01-01"), as.Date("2025-12-01"))
  ) +
  ggthemes::theme_tufte(base_size = 18, base_family = "Work Sans") +
  ggplot2::scale_y_continuous(
    labels = function(x) {
      x <- ifelse(x < 1000, x, traumar::pretty_number(x = x, digits = 2))
    }
  ) +
  paletteer::scale_color_paletteer_d(palette = "colorblindr::OkabeIto_black") +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 20, family = "Work Sans"),
    legend.text = ggplot2::element_text(family = "Work Sans", size = 20),
    legend.title = ggplot2::element_text(
      family = "Work Sans",
      face = "bold",
      size = 22
    ),
    axis.text.x = ggplot2::element_text(
      angle = 90
    ),
    legend.position = "right"
  ) +
  ggplot2::guides(alpha = "none")

# save the treemap
ggplot2::ggsave(
  filename = "mechanism_of_injury_lines.png",
  plot = mechanism_of_injury_lines,
  path = plot_folder,
  height = 6.67,
  width = 6.67 * 1.78
)

###___________________________________________________________________________
# Nature of injury ----
###___________________________________________________________________________

## get injury counts by nature of injury category ----
nature_of_injury_counts <- trauma_2025 |>
  dplyr::filter(
    !is.na(CAUSE_OF_INJURY_AR_1),
    !is.na(NATURE_OF_INJURY_DESCRIPTOR_1)
  ) |>
  dplyr::mutate(
    CAUSE_OF_INJURY_AR_1 = ifelse(
      grepl(pattern = "fall", x = CAUSE_OF_INJURY_AR_1, ignore.case = TRUE),
      "Falls",
      CAUSE_OF_INJURY_AR_1
    )
  ) |>
  injury_incident_count(CAUSE_OF_INJURY_AR_1, NATURE_OF_INJURY_DESCRIPTOR_1) |>
  tidyr::pivot_wider(
    id_cols = NATURE_OF_INJURY_DESCRIPTOR_1,
    names_from = CAUSE_OF_INJURY_AR_1,
    values_from = n
  ) |>
  dplyr::mutate(
    dplyr::across(-NATURE_OF_INJURY_DESCRIPTOR_1, ~ ifelse(is.na(.), 0, .))
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    Total = sum(
      dplyr::c_across(Falls:tidyselect::last_col()),
      na.rm = TRUE
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    Valid_Percent = scales::percent(Total / sum(Total), accuracy = 0.01)
  ) |>
  dplyr::arrange(desc(Total)) |>
  janitor::adorn_totals(where = "row")

## nature of injury gt table ----
# use data in this table for the narrative
nature_of_injury_gt <- nature_of_injury_counts |>
  dplyr::select(-Poisoning) |>
  dplyr::slice_head(n = 5) |>
  dplyr::mutate(
    dplyr::across(Falls:`Struck by/against`, ~ ifelse(. < 6, NA, .))
  ) |>
  gt::gt() |>
  gt::sub_missing(columns = 2:6) |>
  gt::cols_label(
    NATURE_OF_INJURY_DESCRIPTOR_1 ~ "Nature of Injury",
    Valid_Percent ~ "Percent"
  ) |>
  gt::fmt_number(columns = 2:7, drop_trailing_zeros = TRUE) |>
  tab_style_hhs(
    border_cols = 2:tidyselect::last_col(),
  ) |>
  gt::tab_footnote(
    footnote = small_count_message,
    locations = gt::cells_body(
      columns = Firearm,
      rows = 5
    )
  ) |>
  gt::tab_footnote(
    footnote = "Percentage excludes missing ICD-10 diagnosis and injury records.",
    locations = gt::cells_column_labels(columns = Valid_Percent)
  )

### save the nature of injury gt table ----
gt::gtsave(
  data = nature_of_injury_gt,
  filename = "nature_of_injury_table.png",
  path = output_folder
)

## create a nature of injury plot for the report ----
nature_of_injury_bars <- trauma_2025 |>
  injury_incident_count(NATURE_OF_INJURY_DESCRIPTOR_1) |>
  dplyr::filter(!is.na(NATURE_OF_INJURY_DESCRIPTOR_1)) |>
  dplyr::arrange(desc(n)) |>
  dplyr::slice_head(n = 10) |>
  dplyr::mutate(
    NATURE_OF_INJURY_DESCRIPTOR_1 = stringr::str_wrap(
      NATURE_OF_INJURY_DESCRIPTOR_1,
      width = 12
    )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = reorder(NATURE_OF_INJURY_DESCRIPTOR_1, n),
      y = n,
      label = traumar::pretty_number(x = n, digits = 2)
    )
  ) +
  ggplot2::geom_col(
    width = 0.75,
    position = "dodge",
    fill = "blue",
    alpha = 0.75
  ) +
  ggplot2::coord_flip() +
  ggplot2::guides(
    fill = "none",
    alpha = "none"
  ) +
  ggplot2::labs(
    x = "",
    y = ""
  ) +
  ggplot2::scale_y_continuous(labels = function(x) {
    traumar::pretty_number(x = x, digits = 2)
  }) +
  ggthemes::theme_gdocs(base_size = 18, base_family = "Work Sans") +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank()
  )

## save the nature of injury plot ----
ggplot2::ggsave(
  filename = "nature_of_injury_bars.png",
  plot = nature_of_injury_bars,
  path = plot_folder,
  height = 7.5 * (16 / 9),
  width = 7.5
)

###___________________________________________________________________________
# Body region ----
###___________________________________________________________________________

## get counts of injury events by body region ----
overall_body_region_counts <- trauma_2025 |>
  dplyr::filter(!is.na(BODY_REGION_CATEGORY_LEVEL_1_1)) |>
  dplyr::mutate(
    dplyr::across(
      c(CAUSE_OF_INJURY_AR_1, BODY_REGION_CATEGORY_LEVEL_1_1),
      ~ ifelse(
        is.na(.) | grepl(pattern = "unspec|unclass", x = ., ignore.case = TRUE),
        "Unspecified",
        .
      )
    ),
    BODY_REGION_CATEGORY_LEVEL_1_1 = ifelse(
      grepl(
        pattern = "head",
        x = BODY_REGION_CATEGORY_LEVEL_1_1,
        ignore.case = TRUE
      ),
      "Head and Neck all",
      BODY_REGION_CATEGORY_LEVEL_1_1
    )
  ) |>
  dplyr::distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE) |>
  janitor::tabyl(BODY_REGION_CATEGORY_LEVEL_1_1) |>
  dplyr::arrange(desc(n))

### export injury event counts by body region ----
readr::write_csv(
  x = overall_body_region_counts,
  file = paste0(output_folder, "/overall_body_region_counts.csv")
)

### make a gt table of the overall counts ----
overall_body_region_counts_gt <- overall_body_region_counts |>
  dplyr::rename_with(.cols = 2:3, ~ stringr::str_to_title(.)) |>
  dplyr::mutate(Percent = Percent * 100) |>
  gt::gt() |>
  gt::fmt_number(columns = "N", drop_trailing_zeros = TRUE) |>
  gt::cols_label(
    BODY_REGION_CATEGORY_LEVEL_1_1 ~ "Body Region"
  ) |>
  gtExtras::gt_plt_bar_pct(
    column = Percent,
    fill = "blue",
    labels = TRUE,
    scaled = TRUE,
    label_cutoff = 1,
    width = 110
  ) |>
  tab_style_hhs(border_cols = 2:3)

#### save the body region overall counts gt table ----
gt::gtsave(
  data = overall_body_region_counts_gt,
  filename = "overall_body_region_counts_gt.png",
  path = output_folder
)

## get counts of injury events by mechanism and body region ----
body_region_counts <- trauma_2025 |>
  dplyr::mutate(
    dplyr::across(
      c(CAUSE_OF_INJURY_AR_1, BODY_REGION_CATEGORY_LEVEL_1_1),
      ~ ifelse(
        is.na(.) | grepl(pattern = "unspec|unclass", x = ., ignore.case = TRUE),
        "Unspecified",
        .
      )
    ),
    CAUSE_OF_INJURY_AR_1 = ifelse(
      grepl(pattern = "fall", x = CAUSE_OF_INJURY_AR_1, ignore.case = TRUE),
      "All Falls",
      CAUSE_OF_INJURY_AR_1
    ),
    BODY_REGION_CATEGORY_LEVEL_1_1 = ifelse(
      grepl(
        pattern = "head",
        x = BODY_REGION_CATEGORY_LEVEL_1_1,
        ignore.case = TRUE
      ),
      "Head and Neck all",
      BODY_REGION_CATEGORY_LEVEL_1_1
    )
  ) |>
  dplyr::distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE) |>
  janitor::tabyl(BODY_REGION_CATEGORY_LEVEL_1_1, CAUSE_OF_INJURY_AR_1) |>
  janitor::adorn_totals(where = "col") |>
  dplyr::mutate(
    dplyr::across(`All Falls`:Unspecified, ~ ifelse(. > 0 & . < 6, NA, .))
  ) |>
  dplyr::arrange(desc(Total))

## a gt table of the body region vs mechanism counts ----
body_region_counts_gt <- body_region_counts |>
  gt::gt() |>
  gt::cols_label(
    BODY_REGION_CATEGORY_LEVEL_1_1 ~ "Body Region"
  ) |>
  gt::sub_missing(columns = tidyselect::everything()) |>
  gt::tab_footnote(
    footnote = small_count_message,
    locations = list(
      gt::cells_body(columns = "Firearm", rows = 5),
      gt::cells_body(columns = "Poisoning", rows = 1:3)
    )
  ) |>
  gt::fmt_number(
    columns = 2:tidyselect::last_col(),
    drop_trailing_zeros = TRUE
  ) |>
  tab_style_hhs(
    border_cols = 2:tidyselect::last_col()
  )

### export the body region gt table ----
gt::gtsave(
  data = body_region_counts_gt,
  filename = "body_region_counts_gt.png",
  path = output_folder
)

###___________________________________________________________________________
## Traumatic Brain Injury ----
###___________________________________________________________________________

### cases ----
tbi_related_cases <- trauma_data_clean |>
  injury_case_count(
    Year,
    TBI,
    descriptive_stats = TRUE,
    group = TBI
  ) |>
  dplyr::filter(TBI) |>
  dplyr::left_join(
    trauma_data_clean |>
      injury_case_count(Year),
    by = dplyr::join_by(Year),
    suffix = c("_tbi", "_total")
  ) |>
  dplyr::mutate(
    pct_tbi = n_tbi / n_total,
    pct_label = scales::percent(pct_tbi, accuracy = 0.01)
  )

#### write TBI related cases to disk ----
readr::write_csv(
  x = tbi_related_cases,
  file = paste0(output_folder, "/tbi_related_cases.csv")
)

### injury events ----
tbi_related_injuries <- trauma_data_clean |>
  dplyr::mutate(
    TBI = as.logical(max(TBI)),
    .by = c(Incident_Date, Unique_Patient_ID)
  ) |>
  injury_incident_count(
    Year,
    TBI,
    descriptive_stats = TRUE,
    group = TBI
  ) |>
  dplyr::filter(TBI == 1) |>
  dplyr::left_join(
    trauma_data_clean |>
      injury_incident_count(Year),
    suffix = c("_tbi", "_total"),
    by = dplyr::join_by(Year)
  ) |>
  dplyr::select(-tidyselect::matches("reinjury")) |>
  dplyr::mutate(
    pct_tbi = n_tbi / n_total,
    pct_label = scales::percent(pct_tbi, accuracy = 0.01)
  )

#### write the tbi injury events by year to disk ----
readr::write_csv(
  x = tbi_related_injuries,
  file = paste0(output_folder, "/tbi_related_injuries.csv")
)

### further analysis of TBI events ----
tbi_injury_event_data <- trauma_2025 |>
  dplyr::mutate(TBI = max(TBI), .by = c(Incident_Date, Unique_Patient_ID)) |>
  dplyr::filter(!is.na(CAUSE_OF_INJURY_AR_1)) |>
  dplyr::distinct(Incident_Date, Unique_Patient_ID, .keep_all = T) |>
  janitor::tabyl(CAUSE_OF_INJURY_AR_1, TBI) |>
  dplyr::arrange(desc(`1` / (`1` + `0`))) |>
  janitor::adorn_totals(where = "both") |>
  dplyr::rename(TBI = `1`, Non_TBI = `0`) |>
  dplyr::mutate(pct_tbi = TBI / Total, .before = Total)

#### write the TBI event data (no missing mechanism) to disk ----
readr::write_csv(
  x = tbi_injury_event_data,
  file = paste0(output_folder, "/tbi_injury_event_data.csv")
)

#### gt table of injury event counts by injury cause and TBI ----
tbi_injury_event_gt <- tbi_injury_event_data |>
  dplyr::mutate(non_tbi_pct = Non_TBI / Total, .after = pct_tbi) |>
  gt::gt() |>
  gt::fmt_number(columns = c(Non_TBI:TBI, Total), drop_trailing_zeros = TRUE) |>
  gt::fmt_percent(columns = pct_tbi:non_tbi_pct, decimals = 2) |>
  gt::cols_label(
    CAUSE_OF_INJURY_AR_1 ~ "Cause",
    Non_TBI ~ "Non-TBI"
  ) |>
  gt::cols_merge_n_pct(col_n = TBI, col_pct = pct_tbi) |>
  gt::cols_merge_n_pct(col_n = Non_TBI, col_pct = non_tbi_pct) |>
  gt::tab_footnote(
    footnote = "Format is n (% of total).",
    locations = gt::cells_column_labels(columns = Non_TBI:TBI)
  ) |>
  gt::tab_footnote(
    footnote = "Categories are in descending order of % TBI.",
    locations = gt::cells_column_labels(columns = CAUSE_OF_INJURY_AR_1)
  ) |>
  gt::tab_footnote(
    footnote = "Records with missing ICD-10CM data were removed.",
    locations = gt::cells_column_labels(columns = Total)
  ) |>
  tab_style_hhs(border_cols = 2:tidyselect::last_col())

##### save the gt table of injury event counts by injury cause and TBI to disk ----
gt::gtsave(
  data = tbi_injury_event_gt,
  filename = "tbi_injury_event_gt.png",
  path = output_folder
)

### create two time series objects related to TBI for a line plot ----
#### tbi injuries by mechanism monthly ----
tbi_injury_event_cause_monthly <- trauma_data_clean |>
  dplyr::filter(
    !is.na(CAUSE_OF_INJURY_AR_1),
    grepl(
      pattern = "fall|mvc|other|struck",
      x = CAUSE_OF_INJURY_AR_1,
      ignore.case = TRUE
    )
  ) |>
  dplyr::mutate(TBI = max(TBI), .by = c(Incident_Date, Unique_Patient_ID)) |>
  dplyr::distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE) |>
  dplyr::summarize(
    TBI_CAUSE = sum(TBI, na.rm = TRUE),
    .by = c(CAUSE_OF_INJURY_AR_1, Admission_Month)
  )

##### write the tbi injuries monthly by cause to disk ----
readr::write_csv(
  x = tbi_injury_event_cause_monthly,
  file = paste0(output_folder, "/tbi_injury_event_cause_monthly.csv")
)

#### tbi injury events overall monthly ----
tbi_injury_event_overall_monthly <- trauma_data_clean |>
  dplyr::mutate(TBI = max(TBI), .by = c(Incident_Date, Unique_Patient_ID)) |>
  dplyr::distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE) |>
  dplyr::summarize(
    TBI = sum(TBI, na.rm = TRUE),
    .by = Admission_Month
  )

#### tbi injury event plot ----
tbi_injury_event_plot <- tbi_injury_event_overall_monthly |>
  ggplot2::ggplot() +
  ggplot2::geom_line(
    ggplot2::aes(
      x = Admission_Month,
      y = TBI,
    ),
    color = "black",
    linewidth = 1.25,
    lineend = "round",
    linejoin = "round"
  ) +
  ggplot2::labs(
    x = "",
    y = ""
  ) +
  ggplot2::guides(color = "none") +
  ggplot2::scale_x_date(date_labels = "%Y-%m", date_breaks = "4 months") +
  ggthemes::theme_tufte(base_size = 14, base_family = "Work Sans") +
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(size = 25),
    axis.text.x = ggplot2::element_text(size = 25, angle = 90)
  )

##### save the tbi injury event plot to disk ----
ggplot2::ggsave(
  filename = "tbi_injury_event_plot.png",
  plot = tbi_injury_event_plot,
  path = plot_folder
)

### patients  ----
tbi_related_patients <- trauma_data_clean |>
  dplyr::mutate(TBI = as.logical(max(TBI)), .by = c(Year, Unique_Patient_ID)) |>
  injury_patient_count(
    Year,
    TBI,
    descriptive_stats = TRUE,
    group = TBI
  ) |>
  dplyr::filter(TBI) |>
  dplyr::left_join(
    trauma_data_clean |>
      injury_patient_count(Year, TBI) |>
      dplyr::mutate(
        percent = n / sum(n),
        percent_label = traumar::pretty_percent(percent, n_decimal = 2),
        .by = Year
      ) |>
      dplyr::filter(TBI) |>
      dplyr::select(-n),
    by = dplyr::join_by(Year, TBI)
  )

#### write patient data to disk ----
readr::write_csv(
  x = tbi_related_patients,
  file = paste0(output_folder, "/tbi_related_patients.csv")
)

###___________________________________________________________________________
# Special cases ----
###___________________________________________________________________________

## e-scooter and e-bike injuries ----

### identify errors in injury coding ----
escooter_ebike_errors <- trauma_data_clean |>
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("escooter|ebike|^tbi$"),
      ~ as.logical(max(., na.rm = TRUE))
    ),
    .by = c(Year, Incident_Date, Unique_Patient_ID)
  ) |>
  dplyr::distinct(Year, Incident_Date, Unique_Patient_ID, .keep_all = TRUE) |>
  dplyr::filter(All_EScooter & All_EBike) |>
  dplyr::select(Unique_Incident_ID, tidyselect::matches("escooter|ebike")) |>
  dplyr::pull(Unique_Incident_ID)

### data prep for escooter, ebike, and TBI analyses ----
# correct one record that had an e-bike and e-scooter code
# the record was clearly an e-scooter injury, keep as e-scooter
escooter_ebike_data <- trauma_data_clean |>
  dplyr::mutate(
    dplyr::across(
      tidyselect::matches("escooter|ebike|^tbi$"),
      ~ as.logical(max(., na.rm = TRUE))
    ),
    .by = c(Year, Incident_Date, Unique_Patient_ID)
  ) |>
  dplyr::distinct(Year, Incident_Date, Unique_Patient_ID, .keep_all = TRUE) |>
  dplyr::mutate(
    All_EBike = ifelse(
      Unique_Incident_ID == escooter_ebike_errors,
      FALSE,
      All_EBike
    )
  )

### estimate total counts and percentages ----
escooter_ebike_statistics <- escooter_ebike_data |>
  dplyr::summarize(
    dplyr::across(
      tidyselect::matches("escooter|ebike"),
      ~ sum(., na.rm = TRUE)
    ),
    .by = TBI
  ) |>
  tidyr::pivot_longer(
    cols = -TBI,
    names_to = "Micromobility",
    values_to = "n"
  ) |>
  tidyr::pivot_wider(
    id_cols = Micromobility,
    names_from = TBI,
    values_from = n
  ) |>
  dplyr::rename(
    No_TBI = `FALSE`,
    TBI = `TRUE`
  ) |>
  janitor::adorn_totals(where = "col") |>
  dplyr::mutate(
    percent = ifelse(Total > 0, TBI / Total, 0)
  ) |>
  dplyr::bind_rows(
    escooter_ebike_data |>
      injury_incident_count(TBI) |>
      tidyr::pivot_wider(names_from = TBI, values_from = n) |>
      dplyr::mutate(Micromobility = "All Injuries", .before = 1) |>
      dplyr::rename("No_TBI" = `FALSE`, TBI = `TRUE`) |>
      dplyr::mutate(
        Total = No_TBI + TBI,
        percent = ifelse(Total > 0, TBI / Total, 0)
      )
  ) |>
  dplyr::mutate(
    label = scales::percent(x = percent, accuracy = 0.01)
  ) |>
  dplyr::mutate(
    Micromobility = stringr::str_replace_all(
      string = stringr::str_replace_all(
        string = Micromobility,
        pattern = "E(Bike|Scooter)",
        replacement = "E-\\1"
      ),
      pattern = "_",
      replacement = " "
    ),
    Micromobility = factor(
      Micromobility,
      levels = c(
        "E-Scooter Strike Fall",
        "E-Scooter E-Bike MVC",
        "E-Bike MVC",
        "E-Scooter MVC",
        "E-Scooter Non MVC",
        "All E-Scooter",
        "All E-Bike",
        "All E-Scooter E-Bike",
        "All Injuries"
      )
    )
  ) |>
  dplyr::arrange(Micromobility)

#### make a gt table out of the escooter/ebike counts ----
escooter_ebike_statistics_gt <- escooter_ebike_statistics |>
  dplyr::select(-label) |>
  dplyr::mutate(
    dplyr::across(
      c(No_TBI, TBI, Total, percent),
      ~ ifelse(Total < 6, NA, .)
    )
  ) |>
  gt::gt() |>
  gt::sub_missing(columns = No_TBI:percent) |>
  gt::tab_footnote(
    footnote = small_count_message,
    locations = gt::cells_body(columns = No_TBI:percent, rows = 5)
  ) |>
  gt::tab_footnote(
    footnote = "Non-MVC e-scooter injuries are distinct from MVC and\nstrike/fall incidents, usually some form of crash.",
    location = gt::cells_body(columns = Micromobility, rows = 5)
  ) |>
  gt::fmt_number(columns = No_TBI:Total, drop_trailing_zeros = TRUE) |>
  gt::fmt_percent(columns = percent) |>
  gt::cols_label(
    No_TBI ~ "No TBI",
    percent ~ "Percent TBI"
  ) |>
  tab_style_hhs(border_cols = 2:tidyselect::last_col())

##### save the escooter ebike gt table ----
gt::gtsave(
  data = escooter_ebike_statistics_gt,
  filename = "escooter_ebike_statistics_gt.png",
  path = output_folder
)

### estimate total counts and percentages over years ----
escooter_ebike_statistics_years <- escooter_ebike_data |>
  dplyr::summarize(
    dplyr::across(
      tidyselect::matches("escooter|ebike"),
      ~ sum(., na.rm = TRUE)
    ),
    .by = c(Year, TBI)
  ) |>
  tidyr::pivot_longer(
    cols = -c(Year, TBI),
    names_to = "Micromobility",
    values_to = "n"
  ) |>
  tidyr::pivot_wider(
    id_cols = c(Year, Micromobility),
    names_from = TBI,
    values_from = n
  ) |>
  dplyr::rename(
    No_TBI = `FALSE`,
    TBI = `TRUE`
  ) |>
  janitor::adorn_totals(where = "col") |>
  dplyr::mutate(percent = ifelse(Total > 0, TBI / Total, 0)) |>
  dplyr::bind_rows(
    escooter_ebike_data |>
      injury_incident_count(Year, TBI) |>
      tidyr::pivot_wider(id_cols = Year, names_from = TBI, values_from = n) |>
      dplyr::mutate(Micromobility = "All Injuries", .before = 2) |>
      dplyr::rename("No_TBI" = `FALSE`, TBI = `TRUE`) |>
      dplyr::mutate(
        Total = No_TBI + TBI,
        percent = ifelse(Total > 0, TBI / Total, 0)
      )
  ) |>
  dplyr::mutate(
    label = scales::percent(x = percent, accuracy = 0.01)
  ) |>
  dplyr::arrange(Year, Micromobility) |>
  dplyr::mutate(
    Micromobility = stringr::str_replace_all(
      string = stringr::str_replace_all(
        string = Micromobility,
        pattern = "E(Bike|Scooter)",
        replacement = "E-\\1"
      ),
      pattern = "_",
      replacement = " "
    )
  )

#### get plot legend label order ----
escooter_ebike_legend_order <- escooter_ebike_statistics |>
  dplyr::filter(!grepl("^all|tbi", Micromobility, ignore.case = T)) |>
  dplyr::arrange(desc(TBI)) |>
  dplyr::pull(Micromobility)

#### plot the e-scooter/e-bike stats over time ----
escooter_ebike_statistics_plot_years <- escooter_ebike_statistics_years |>
  dplyr::filter(
    !grepl(
      pattern = "tbi|^all.*",
      x = Micromobility,
      ignore.case = TRUE
    )
  ) |>
  dplyr::mutate(
    Micromobility = stringr::str_replace_all(
      string = stringr::str_replace_all(
        string = Micromobility,
        pattern = "E(Bike|Scooter)",
        replacement = "E-\\1"
      ),
      pattern = "_",
      replacement = " "
    ),
    Micromobility = factor(Micromobility, levels = escooter_ebike_legend_order)
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_line(
    ggplot2::aes(
      x = factor(Year),
      y = TBI,
      color = Micromobility,
      group = Micromobility,
      alpha = 0.8
    ),
    linetype = "solid"
  ) +
  ggplot2::geom_line(
    ggplot2::aes(
      x = factor(Year),
      y = No_TBI,
      color = Micromobility,
      group = Micromobility,
      alpha = 0.8
    ),
    linetype = "dotted",
    linewidth = 1.25
  ) +
  ggplot2::labs(
    x = "",
    y = ""
  ) +
  ggplot2::guides(alpha = "none") +
  ggthemes::theme_tufte(base_size = 20, base_family = "Work Sans") +
  ggplot2::scale_y_continuous(labels = function(x) {
    traumar::pretty_number(x = x, digits = 2)
  }) +
  paletteer::scale_color_paletteer_d(
    palette = "colorblindr::OkabeIto_black"
  )

##### save the line graph of escooter ebike injuries over the years ----
ggplot2::ggsave(
  filename = "escooter_ebike_statistics_plot_years.png",
  plot = escooter_ebike_statistics_plot_years,
  path = plot_folder
)

### calculate quarterly escooter and ebike injury events ----
escooter_ebike_quarterly <- escooter_ebike_data |>
  dplyr::summarize(
    EBike = sum(All_EBike, na.rm = TRUE),
    EScooter = sum(All_EScooter, na.rm = TRUE),
    .by = Admission_Quarter
  ) |>
  tidyr::pivot_longer(
    cols = EBike:EScooter,
    names_to = "Micromobility",
    values_to = "n"
  )

#### plot the esccooter and ebike aggregates over the quarters ----
escooter_ebike_quarterly_plot <- escooter_ebike_quarterly |>
  dplyr::mutate(
    Micromobility = stringr::str_replace(
      Micromobility,
      "E(Bike|Scooter)",
      replacement = "E-\\1"
    )
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_col(
    ggplot2::aes(
      x = Admission_Quarter,
      y = n,
      fill = Micromobility,
      group = Micromobility
    ),
    position = "dodge"
  ) +
  ggplot2::labs(
    x = "",
    y = ""
  ) +
  ggthemes::theme_tufte(base_size = 20, base_family = "Work Sans") +
  paletteer::scale_fill_paletteer_d(palette = "colorblindr::OkabeIto") +
  ggplot2::scale_x_date(
    date_labels = "%Y-%m",
    date_breaks = "3 months",
    limits = c(as.Date("2021-01-01"), as.Date("2025-10-01"))
  ) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90),
    legend.position = "bottom"
  )

##### save the ebike/escooter quarterly plot to disk ----
ggplot2::ggsave(
  filename = "escooter_ebike_quarterly_plot.png",
  plot = escooter_ebike_quarterly_plot,
  path = plot_folder,
  width = 5 * (16 / 9),
  height = 5
)

## get statistics on special cases of injuries ----

### transform data to handle work and farm/ag entries ----
farm_ag_work_data <- trauma_data_clean |>
  dplyr::distinct(Year, Unique_Incident_ID, .keep_all = TRUE) |>
  dplyr::mutate(dplyr::across(
    c(Farm_Ag_Related, Financial_Work_Related),
    ~ ifelse(is.na(.), FALSE, ifelse(. == "Yes", TRUE, FALSE))
  ))

### work related and ag joint probabilities ----
farm_ag_work_joint_probs <- farm_ag_work_data |>
  dplyr::summarize(
    farm_ag = sum(Farm_Ag_Related),
    work = sum(Financial_Work_Related),
    farm_ag_work = sum(Farm_Ag_Related & Financial_Work_Related),
    n = dplyr::n(),
    .by = Year
  ) |>
  janitor::adorn_totals(where = "row") |>
  dplyr::mutate(
    farm_ag_pct_work = scales::percent(farm_ag_work / work, accuracy = 0.01),
    farm_ag_work_pct_farm_ag = scales::percent(
      farm_ag_work / farm_ag,
      accuracy = 0.01
    ),
    farm_ag_work_total_pct = scales::percent(farm_ag_work / n, accuracy = 0.01),
    farm_ag_pct = scales::percent(farm_ag / n, accuracy = 0.01),
    work_pct = scales::percent(work / n, accuracy = 0.01)
  ) |>
  dplyr::mutate(
    dplyr::across(
      farm_ag:n,
      ~ prettyNum(x = ., big.mark = ",")
    )
  )

#### save the join probabilities of work related and farm/ag to disk ----
readr::write_csv(
  x = farm_ag_work_joint_probs,
  file = paste0(output_folder, "/farm_ag_work_joint_probs.csv")
)

### make a work related farm/ag joint probs gt table ----
farm_ag_work_joint_probs_gt <- farm_ag_work_data |>
  dplyr::summarize(
    farm_ag = sum(Farm_Ag_Related),
    work = sum(Financial_Work_Related),
    farm_ag_work = sum(Farm_Ag_Related & Financial_Work_Related),
    n = dplyr::n(),
    .by = Year
  ) |>
  janitor::adorn_totals(where = "row") |>
  dplyr::mutate(
    farm_ag_pct_work = farm_ag_work / work,
    farm_ag_work_pct_farm_ag = farm_ag_work / farm_ag,
    farm_ag_work_total_pct = farm_ag_work / n,
    farm_ag_pct = farm_ag / n,
    work_pct = work / n
  ) |>
  gt::gt() |>
  gt::fmt_number(columns = farm_ag:n, drop_trailing_zeros = TRUE) |>
  gt::fmt_percent(columns = farm_ag_pct_work:work_pct, decimals = 1) |>
  gt::cols_merge_n_pct(col_n = farm_ag, col_pct = farm_ag_pct) |>
  gt::cols_merge_n_pct(col_n = work, col_pct = work_pct) |>
  gt::cols_merge(
    columns = c(farm_ag_work, farm_ag_pct_work, farm_ag_work_pct_farm_ag),
    pattern = "{1} ({2}, {3})"
  ) |>
  gt::cols_label(
    farm_ag ~ "Farm/Ag",
    farm_ag_work ~ "Farm and Work-Related",
    work ~ "Work-Related",
    n ~ "N"
  ) |>
  gt::cols_hide(columns = farm_ag_work_total_pct) |>
  gt::tab_footnote(
    footnote = "Format is n (% farm/ag & work of all work, % farm/ag & work of all farm/ag).",
    locations = gt::cells_column_labels(columns = farm_ag_work)
  ) |>
  tab_style_hhs(border_cols = 2:tidyselect::last_col())

#### save the farm_ag_work_joint_probs_gt object to disk ----
gt::gtsave(
  data = farm_ag_work_joint_probs_gt,
  filename = "farm_ag_work_joint_probs_gt.png",
  path = output_folder
)

### work related and ag annual case counts and percentages ---
work_related_agricultural <- farm_ag_work_data |>
  dplyr::summarize(
    work_related = sum(Financial_Work_Related, na.rm = TRUE),
    ag_related = sum(Farm_Ag_Related, na.rm = TRUE),
    n = dplyr::n(),
    percent_work = scales::percent(work_related / n, accuracy = 0.01),
    percent_ag = scales::percent(ag_related / n, accuracy = 0.01),
    .by = Year
  ) |>
  dplyr::mutate(
    dplyr::across(
      work_related:n,
      ~ prettyNum(x = ., big.mark = ",")
    )
  )

#### save the work related and farm/ag case counts to disk ----
readr::write_csv(
  x = work_related_agricultural,
  file = paste0(output_folder, "/work_related_agricultural.csv")
)

### farm and ag injury severity using trauma hospital cases ----
farm_ag_injury_severity <- trauma_data_clean |>
  dplyr::filter(Farm_Ag_Related == "Yes") |>
  injury_case_count(Year, ISS_Range) |>
  dplyr::mutate(
    ISS_Range = forcats::fct(
      x = stringr::str_replace_all(
        string = ISS_Range,
        pattern = " ",
        replacement = ""
      ),
      levels = c("1-8", "9-15", "16+")
    )
  ) |>
  dplyr::arrange(Year, ISS_Range) |>
  tidyr::pivot_wider(id_cols = Year, names_from = ISS_Range, values_from = n) |>
  janitor::adorn_totals(where = "both") |>
  dplyr::mutate(
    pct_mild = `1-8` / Total,
    pct_moderate = `9-15` / Total,
    pct_severe = `16+` / Total
  )

#### create a farm/ag gt table for the injury severity analysis ----
farm_ag_injury_severity_gt <- farm_ag_injury_severity |>
  gt::gt() |>
  gt::tab_spanner(
    label = "ISS Range",
    columns = `1-8`:`16+`
  ) |>
  gt::fmt_number(columns = `1-8`:Total, drop_trailing_zeros = TRUE) |>
  gt::fmt_percent(columns = pct_mild:pct_severe, drop_trailing_zeros = TRUE) |>
  gt::cols_merge_n_pct(col_n = `1-8`, col_pct = pct_mild) |>
  gt::cols_merge_n_pct(col_n = `9-15`, col_pct = pct_moderate) |>
  gt::cols_merge_n_pct(col_n = `16+`, col_pct = pct_severe) |>
  gt::tab_footnote(
    footnote = case_message,
    locations = gt::cells_column_labels(columns = Total)
  ) |>
  gt::tab_footnote(
    footnote = "Format is count (rowwise percent).",
    locations = gt::cells_column_labels(columns = `1-8`:`16+`)
  ) |>
  tab_style_hhs(border_cols = 2:tidyselect::last_col())

##### save the farm/ag gt injury severity gt table to disk ----
gt::gtsave(
  data = farm_ag_injury_severity_gt,
  filename = "farm_ag_injury_severity_gt.png",
  path = output_folder,
  zoom = 5,
  vwidth = 800,
  vheight = 700
)

### get farm/ag trauma center cases by nature and ISS range ----
farm_ag_injury_mechanism_iss <- trauma_data_clean |>
  dplyr::filter(Farm_Ag_Related == "Yes") |>
  injury_case_count(MECHANISM_1) |>
  dplyr::filter_out(is.na(MECHANISM_1)) |>
  dplyr::arrange(desc(n))

#### plot farm/ag injury severity by injury nature ----
farm_ag_injury_mechanism_iss_plot <- farm_ag_injury_mechanism_iss |>
  dplyr::slice_head(n = 15) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = reorder(MECHANISM_1, n),
      y = n,
      label = n
    )
  ) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge(width = 0.75),
    width = 0.6,
    fill = "cornflowerblue"
  ) +
  ggplot2::labs(
    x = "",
    y = ""
  ) +
  ggthemes::theme_tufte(base_size = 20, base_family = "Work Sans") +
  ggplot2::theme(
    axis.text = ggplot2::element_text(face = "bold")
  ) +
  ggplot2::ylim(c(0, 600)) +
  ggplot2::coord_flip()

##### save the farm/ag mechanism plot to disk ----
ggplot2::ggsave(
  filename = "farm_ag_injury_mechanism_iss_plot.png",
  plot = farm_ag_injury_mechanism_iss_plot,
  path = plot_folder,
  width = 7 * (16 / 9),
  height = 7
)

## reinjury section was removed for the 2025 report ----

# ### patients that were reinjured
# reinjury_patient_statistics <- trauma_data_clean |>
#   reinjury_patient_count(Year, descriptive_stats = TRUE) |>
#   dplyr::select(-c(9:14))

# ### injuries that occurred as reinjuries
# reinjury_injury_statistics <- trauma_data_clean |>
#   reinjury_injury_count(Year, descriptive_stats = TRUE) |>
#   dplyr::select(-c(8:13))

# ### prepare data for a gt table
# reinjury_data_prep <- reinjury_patient_statistics |>
#   dplyr::select(Year, reinjured_patients, n_patients, prop_reinjured) |>
#   dplyr::left_join(
#     reinjury_injury_statistics |>
#       dplyr::select(Year, Reinjury, injury_events = n, prop),
#     by = dplyr::join_by(Year)
#   )

# ### reinjury gt table
# reinjury_gt <- reinjury_data_prep |>
#   gt::gt() |>
#   gt::cols_label(
#     reinjured_patients ~ "Reinjured Pts",
#     n_patients ~ "Total Pts",
#     Reinjury ~ "Reinjury Events",
#     injury_events ~ "Injury Events",
#   ) |>
#   gt::fmt_number(
#     columns = c(reinjured_patients:n_patients, Reinjury:injury_events),
#     drop_trailing_zeros = TRUE
#   ) |>
#   gt::fmt_percent(columns = tidyselect::matches("prop")) |>
#   gt::cols_merge_n_pct(col_n = reinjured_patients, col_pct = prop_reinjured) |>
#   gt::cols_merge_n_pct(col_n = Reinjury, col_pct = prop) |>
#   tab_style_hhs(
#     border_cols = 2:tidyselect::last_col(),
#     column_labels = 18,
#     body = 16
#   )

# ### save the gt reinjury table
# gt::gtsave(data = reinjury_gt, filename = "reinjury_gt.png", path = plot_folder)
