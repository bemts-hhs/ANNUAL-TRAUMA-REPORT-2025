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
  injury_incident_count(Year, CAUSE_OF_INJURY_AR_1) |>
  dplyr::mutate(valid_percent = n / sum(n), .by = Year)

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
  ggplot2::labs(x = "", y = "", color = "Mechanism") +
  ggplot2::scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y-%m",
    limits = c(as.Date("2021-01-01"), as.Date("2025-12-01"))
  ) +
  ggthemes::theme_tufte(base_size = 18, base_family = "Work Sans") +
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
  gt::fmt_missing(columns = 2:6) |>
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
      columns = c(Firearm, `Struck by/against`),
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
  gt::fmt_missing(columns = tidyselect::everything()) |>
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
  dplyr::mutate(percent = ifelse(Total > 0, TBI / Total, 0)) |>
  dplyr::arrange(desc(TBI)) |>
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
    )
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
escooter_ebike_statistics_years |>
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

## get statistics on special cases of injuries ----

## work related and ag ---
work_related_agricultural <- trauma_data_clean |>
  dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE) |>
  dplyr::summarize(
    work_related = sum(Financial_Work_Related == "Yes", na.rm = TRUE),
    ag_related = sum(Farm_Ag_Related == "Yes", na.rm = TRUE),
    n = dplyr::n(),
    percent_work = work_related / n,
    percent_ag = ag_related / n,
    .by = Year
  )

## reinjury ----

### patients that were reinjured ----
reinjury_patient_statistics <- trauma_data_clean |>
  reinjury_patient_count(Year, descriptive_stats = TRUE) |>
  dplyr::select(-c(9:14))

### injuries that occurred as reinjuries ----
reinjury_injury_statistics <- trauma_data_clean |>
  reinjury_injury_count(Year, descriptive_stats = TRUE) |>
  dplyr::select(-c(8:13))

### prepare data for a gt table ----
reinjury_data_prep <- reinjury_patient_statistics |>
  dplyr::select(Year, reinjured_patients, n_patients, prop_reinjured) |>
  dplyr::left_join(
    reinjury_injury_statistics |>
      dplyr::select(Year, Reinjury, injury_events = n, prop),
    by = dplyr::join_by(Year)
  )

### reinjury gt table ----
reinjury_gt <- reinjury_data_prep |>
  gt::gt() |>
  gt::cols_label(
    reinjured_patients ~ "Reinjured Pts",
    n_patients ~ "Total Pts",
    Reinjury ~ "Reinjury Events",
    injury_events ~ "Injury Events",
  ) |>
  gt::fmt_number(
    columns = c(reinjured_patients:n_patients, Reinjury:injury_events),
    drop_trailing_zeros = TRUE
  ) |>
  gt::fmt_percent(columns = tidyselect::matches("prop")) |>
  gt::cols_merge_n_pct(col_n = reinjured_patients, col_pct = prop_reinjured) |>
  gt::cols_merge_n_pct(col_n = Reinjury, col_pct = prop) |>
  tab_style_hhs(
    border_cols = 2:tidyselect::last_col(),
    column_labels = 18,
    body = 16
  )

### save the gt reinjury table ----
gt::gtsave(data = reinjury_gt, filename = "reinjury_gt.png", path = plot_folder)
