###___________________________________________________________________________
# Trauma System Section for front matter of report ----
###___________________________________________________________________________

####
# You must first run data_load.R and setup.R before running this script
# Utilize trauma_data_prep as the file for analysis here
####

## calculate trauma center counts over time ----
trauma_facility_count_by_level_years <- trauma_data_prep |>
  dplyr::mutate(
    Year = lubridate::floor_date(
      x = ED_Acute_Care_Admission_Date,
      unit = "year"
    )
  ) |>
  dplyr::filter_out(is.na(Level)) |>
  dplyr::distinct(Year, `Current Facility Name`, .keep_all = TRUE) |>
  dplyr::select(Year, `Current Facility Name`, Level) |>
  dplyr::arrange(`Current Facility Name`) |>
  dplyr::count(Year, Level) |>
  tidyr::complete(Year, Level, fill = list(n = 0))

### create a plot of the change in trauma centers over time ----
trauma_facility_count_time_plot <- trauma_facility_count_by_level_years |>
  dplyr::filter_out(Level == "FSED") |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = Year,
      y = n,
      label = n
    )
  ) +
  ggplot2::geom_line(linewidth = 1.25, color = "lavenderblush2") +
  ggplot2::geom_point(color = "violet", size = 2.25) +
  ggplot2::geom_label(
    size = 5
  ) +
  traumar::theme_cleaner(
    base_size = 18,
    base_family = "Work Sans",
    facets = TRUE
  ) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90),
    strip.background = ggplot2::element_rect(fill = "violet")
  ) +
  ggplot2::scale_y_discrete(name = "") +
  ggplot2::scale_x_date(
    limits = c(as.Date("2018-01-01"), as.Date("2025-01-01")),
    date_labels = "%Y",
    date_breaks = "1 year"
  ) +
  ggplot2::facet_wrap(~Level, scales = "free_y") +
  ggplot2::labs(x = "")

#### save the trauma_facility_count_time_plot to disk ----
ggplot2::ggsave(
  filename = "trauma_facility_count_time_plot.png",
  plot = trauma_facility_count_time_plot,
  path = plot_folder,
  width = 6 * (16 / 9),
  height = 6
)

### Trauma facility count by trauma level df ----
trauma_facility_count_by_level <- trauma_2025 |>
  dplyr::distinct(Level, Facility_Name) |>
  dplyr::filter(!is.na(Level)) |>
  dplyr::count(Level, name = "Count")

#### Trauma facility count by trauma level plot ----
trauma_facility_count_by_level_plot <- trauma_facility_count_by_level |>
  ggplot2::ggplot(ggplot2::aes(Level, Count, fill = Level, label = Count)) +
  ggplot2::geom_col() +
  ggrepel::geom_text_repel(
    direction = "y",
    color = ifelse(
      trauma_facility_count_by_level$Level == "IV",
      "white",
      "black"
    ),
    nudge_y = ifelse(trauma_facility_count_by_level$Count > 10, -1, 1),
    size = 15,
    fontface = "bold",
    family = "Work Sans",
    segment.color = "transparent",
    seed = 10232015
  ) +
  ggplot2::guides(fill = "none") +
  traumar::theme_cleaner(
    base_size = 30,
    axis.text.y = ggplot2::element_blank()
  ) +
  ggplot2::labs(x = "", y = "") +
  paletteer::scale_fill_paletteer_d(
    palette = "colorblindr::OkabeIto_black",
    direction = 1
  )

##### save the facility count plot ----
ggplot2::ggsave(
  filename = "trauma_facility_count_by_level_plot.png",
  plot = trauma_facility_count_by_level_plot,
  path = plot_folder,
  height = 7,
  width = 7 * (16 / 9)
)

###_____________________________________________________________________________
## Deaths ----
###_____________________________________________________________________________

####
# You must first run data_load.R and setup.R before running this script
####

###_____________________________________________________________________________
## Plots for the death data ----
###_____________________________________________________________________________

## top causes of mortality in the US ----

### top 10 causes of death in the US ----
top_10_causes_us_plot <- death_cdc_wonder_nation_all_2020_2024_aggregate |>
  dplyr::mutate(
    name = gsub(
      x = `15 Leading Causes of Death`,
      pattern = "(nt|se|sm)s\\b",
      replacement = "\\1",
      perl = TRUE,
      ignore.case = TRUE
    ),
    name = gsub(
      pattern = "(chron)ic",
      x = name,
      ignore.case = TRUE,
      perl = TRUE,
      replacement = "\\1\\."
    ),
    name = gsub(
      pattern = "(resp)iratory",
      x = name,
      replacement = "\\1\\.",
      ignore.case = TRUE,
      perl = TRUE
    ),
    name = ifelse(
      grepl(
        pattern = "nephritis",
        x = name,
        ignore.case = TRUE
      ),
      "Nephritis, nephrosis, etc.",
      ifelse(
        grepl(
          pattern = "liver\\sdisease",
          x = name,
          ignore.case = TRUE
        ),
        "Chron. liver disease/cirrhosis",
        name
      )
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x = reorder(
      name,
      Deaths
    ),
    y = Deaths,
    fill = Deaths,
    label = traumar::pretty_number(Deaths, digits = 2)
  )) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge2(width = 0.5),
    width = 0.75
  ) +
  ggplot2::annotate(
    geom = "segment",
    x = "Accident",
    xend = "Accident",
    y = 1500000,
    yend = 1100000,
    arrow = ggplot2::arrow(type = "closed")
  ) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    x = "",
    y = "",
  ) +
  ggplot2::guides(color = "none") +
  ggplot2::scale_y_continuous(labels = function(x) traumar::pretty_number(x)) +
  paletteer::scale_fill_paletteer_c(
    palette = "ggthemes::Orange-Gold",
    direction = 1,
    labels = function(x) traumar::pretty_number(x)
  ) +
  traumar::theme_cleaner(
    base_size = 30,
    legend_position = "inside",
    legend.position.inside = c(0.75, 0.25)
  )

#### save the top causes of death in the US plot ----
ggplot2::ggsave(
  filename = "top_10_causes_us_plot.png",
  plot = top_10_causes_us_plot,
  path = plot_folder,
  height = 6,
  width = 6 * (16 / 9)
)

### gt() tbl of deaths among 1-44 age population in the U.S. ----
death_cdc_wisqars_all_1_44_tbl <- death_cdc_wonder_nation_1_44_2020_2024_aggregate |>
  dplyr::slice_max(Deaths, n = 5) |>
  dplyr::mutate(
    name = gsub(
      x = `15 Leading Causes of Death`,
      pattern = "(nt|se|sm)s\\b",
      replacement = "\\1",
      perl = TRUE,
      ignore.case = TRUE
    ),
    name = gsub(
      pattern = "(chron)ic",
      x = name,
      ignore.case = TRUE,
      perl = TRUE,
      replacement = "\\1\\."
    ),
    name = gsub(
      pattern = "(resp)iratory",
      x = name,
      replacement = "\\1\\.",
      ignore.case = TRUE,
      perl = TRUE
    ),
    name = ifelse(
      grepl(
        pattern = "nephritis",
        x = name,
        ignore.case = TRUE
      ),
      "Nephritis, nephrosis, etc.",
      ifelse(
        grepl(
          pattern = "liver\\sdisease",
          x = name,
          ignore.case = TRUE
        ),
        "Chron. liver disease/cirrhosis",
        name
      )
    ),
    .before = 1
  ) |>
  dplyr::select(
    -matches("15 leading|crude|population|code"),
    name,
    Deaths,
    `Age Adjusted Rate`,
    `Age Adjusted Rate Lower 95% Confidence Interval`,
    `Age Adjusted Rate Upper 95% Confidence Interval`
  ) |>
  gt::gt() |>
  gt::fmt_number(columns = Deaths, drop_trailing_zeros = TRUE) |>
  gtExtras::gt_duplicate_column(
    column = `Age Adjusted Rate`,
    after = `Age Adjusted Rate`,
    dupe_name = "Rate_Bar"
  ) |>
  gtExtras::gt_plt_bar(column = Rate_Bar, color = "coral") |>
  gt::cols_label(
    `Age Adjusted Rate` ~ "Age Adjusted Rate (95% CI)",
    Rate_Bar ~ "",
    name ~ "Cause of Death"
  ) |>
  gt::cols_merge(
    columns = c(
      `Age Adjusted Rate`,
      `Age Adjusted Rate Lower 95% Confidence Interval`,
      `Age Adjusted Rate Upper 95% Confidence Interval`
    ),
    pattern = "{1} ({2}&mdash;{3})"
  ) |>
  gt::cols_hide(columns = `Age Adjusted Rate Standard Error`) |>
  tab_style_hhs(
    border_cols = c(Deaths, `Age Adjusted Rate`),
    column_labels = 18,
    body = 16
  )

#### save the gt table for deaths ages 1-44 US ----
gt::gtsave(
  data = death_cdc_wisqars_all_1_44_tbl,
  filename = "death_cdc_wisqars_all_1_44_tbl.png",
  path = output_folder
)

## top causes of mortality in Iowa ----

### top 5 causes of death in Iowa
top_5_causes_iowa_plot <- death_cdc_wonder_iowa_all_2024 |>
  dplyr::slice_max(Deaths, n = 5) |>
  dplyr::mutate(
    name = gsub(
      x = `15 Leading Causes of Death`,
      pattern = "(nt|se|sm)s\\b",
      replacement = "\\1",
      perl = TRUE,
      ignore.case = TRUE
    ),
    name = gsub(
      pattern = "(chron)ic",
      x = name,
      ignore.case = TRUE,
      perl = TRUE,
      replacement = "\\1\\."
    ),
    name = gsub(
      pattern = "(resp)iratory",
      x = name,
      replacement = "\\1\\.",
      ignore.case = TRUE,
      perl = TRUE
    ),
    name = ifelse(
      grepl(
        pattern = "nephritis",
        x = name,
        ignore.case = TRUE
      ),
      "Nephritis, nephrosis, etc.",
      ifelse(
        grepl(
          pattern = "liver\\sdisease",
          x = name,
          ignore.case = TRUE
        ),
        "Chron. liver disease/cirrhosis",
        name
      )
    ),
    .before = 1
  ) |>
  dplyr::slice_head(n = 5) |>
  ggplot2::ggplot(ggplot2::aes(
    x = reorder(
      stringr::str_wrap(name, width = 10),
      -Deaths
    ),
    y = Deaths,
    fill = Deaths,
    label = traumar::pretty_number(x = Deaths, digits = 2)
  )) +
  ggplot2::geom_col(
    width = 0.75,
    position = ggplot2::position_dodge(width = 1)
  ) +
  ggrepel::geom_text_repel(
    direction = "y",
    nudge_y = ifelse(
      grepl(
        pattern = "heart",
        x = death_cdc_wonder_iowa_all_2023$`15 Leading Causes of Death`,
        ignore.case = TRUE
      ),
      300,
      10
    ),
    family = "Work Sans",
    size = 15,
    fontface = "bold",
    seed = 10232015,
    segment.color = "transparent"
  ) +
  ggplot2::annotate(
    geom = "segment",
    x = stringr::str_wrap("Accident", width = 10),
    xend = stringr::str_wrap("Accident", width = 10),
    y = 3500,
    yend = 2700,
    arrow = ggplot2::arrow(type = "closed")
  ) +
  paletteer::scale_fill_paletteer_c(
    palette = "ggthemes::Orange-Gold",
    direction = 1,
    labels = function(x) traumar::pretty_number(x)
  ) +
  ggplot2::labs(x = "", y = "") +
  traumar::theme_cleaner(
    base_size = 30,
    axis.text.y = ggplot2::element_blank(),
    legend_position = "inside",
    legend.position.inside = c(0.75, 0.75)
  )

#### save the top 10 causes of death in Iowa plot ----
ggplot2::ggsave(
  filename = "top_5_causes_iowa_plot.png",
  plot = top_5_causes_iowa_plot,
  path = plot_folder,
  height = 8,
  width = 8 * (16 / 9)
)

### gt() tbl of deaths among 1-44 age population in the U.S. ----
death_cdc_wisqars_ia_1_44_tbl <- death_cdc_wonder_iowa_1_44_2020_2024_aggregate |>
  dplyr::slice_max(Deaths, n = 6) |>
  dplyr::mutate(
    name = gsub(
      x = `15 Leading Causes of Death`,
      pattern = "(nt|se|sm)s\\b",
      replacement = "\\1",
      perl = TRUE,
      ignore.case = TRUE
    ),
    name = gsub(
      pattern = "(chron)ic",
      x = name,
      ignore.case = TRUE,
      perl = TRUE,
      replacement = "\\1\\."
    ),
    name = gsub(
      pattern = "(resp)iratory",
      x = name,
      replacement = "\\1\\.",
      ignore.case = TRUE,
      perl = TRUE
    ),
    name = ifelse(
      grepl(
        pattern = "nephritis",
        x = name,
        ignore.case = TRUE
      ),
      "Nephritis, nephrosis, etc.",
      ifelse(
        grepl(
          pattern = "liver\\sdisease",
          x = name,
          ignore.case = TRUE
        ),
        "Chron. liver disease/cirrhosis",
        name
      )
    ),
    .before = 1
  ) |>
  dplyr::select(
    -matches("15 lead|crude|population|code"),
    name,
    Deaths,
    `Age Adjusted Rate`,
    `Age Adjusted Rate Lower 95% Confidence Interval`,
    `Age Adjusted Rate Upper 95% Confidence Interval`
  ) |>
  gt::gt() |>
  gt::fmt_number(columns = Deaths, drop_trailing_zeros = TRUE) |>
  gtExtras::gt_duplicate_column(
    column = `Age Adjusted Rate`,
    after = `Age Adjusted Rate`,
    dupe_name = "Rate_Bar"
  ) |>
  gtExtras::gt_plt_bar(column = Rate_Bar, color = "coral") |>
  gt::cols_label(
    `Age Adjusted Rate` ~ "Age Adjusted Rate (95% CI)",
    Rate_Bar ~ "",
    name ~ "Cause of Death"
  ) |>
  gt::cols_merge(
    columns = c(
      `Age Adjusted Rate`,
      `Age Adjusted Rate Lower 95% Confidence Interval`,
      `Age Adjusted Rate Upper 95% Confidence Interval`
    ),
    pattern = "{1} ({2}&mdash;{3})"
  ) |>
  gt::cols_hide(columns = `Age Adjusted Rate Standard Error`) |>
  tab_style_hhs(
    border_cols = c(Deaths, `Age Adjusted Rate`),
    column_labels = 18,
    body = 16
  )

#### save the gt table for deaths ages 1-44 Iowa ----
gt::gtsave(
  data = death_cdc_wisqars_ia_1_44_tbl,
  filename = "death_cdc_wisqars_ia_1_44_tbl.png",
  path = output_folder
)
