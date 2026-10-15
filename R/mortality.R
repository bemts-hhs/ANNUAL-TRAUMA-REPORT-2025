###___________________________________________________________________________
# Mortality Section ----
###___________________________________________________________________________

# Iowa trauma deaths by intentionality ----
iowa_deaths_intentionality_plot <- iowa_deaths_intentionality |>
  dplyr::mutate(
    Intentionality = stringr::str_remove_all(
      Intentionality,
      pattern = "^\\d{2}-"
    ),
    labels = dplyr::if_else(
      Year %in% c(2021, 2025) & Deaths >= 6,
      traumar::pretty_number(Deaths),
      dplyr::if_else(
        Year %in% c(2021, 2025) & Deaths < 6,
        traumar::small_count_label(var = Deaths, cutoff = 6, replacement = "*"),
        NA_character_
      )
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x = factor(Year),
    y = Deaths,
    color = reorder(Intentionality, -Deaths),
    label = labels,
    group = Intentionality
  )) +
  ggplot2::geom_line(
    alpha = 0.5,
    linewidth = 3,
    lineend = "round",
    linejoin = "round"
  ) +
  ggrepel::geom_text_repel(
    segment.color = NA,
    family = "Work Sans",
    size = 8,
    color = "black",
    direction = "both"
  ) +
  ggplot2::labs(
    x = "",
    y = "",
    title = "Iowa Trauma Deaths by Intentionality",
    subtitle = "Source: Iowa Death Certificate Data | 2021-2025",
    color = "Intentionality",
    caption = "Note: Order of color legend follows descending order of lines.\n'*' indicates a masked value < 6 to protect confidentiality."
  ) +
  ggplot2::scale_y_continuous(
    labels = function(x) traumar::pretty_number(x)
  ) +
  paletteer::scale_color_paletteer_d(
    palette = "colorblindr::OkabeIto",
    direction = 1
  ) +
  traumar::theme_cleaner(
    base_size = 15,
    title_text_size = 20,
    subtitle_text_size = 18,
    vjust_title = 1.75,
    vjust_subtitle = 1,
    legend_position = "right"
  )

# save the Iowa trauma deaths by intentionality plot
ggplot2::ggsave(
  filename = "iowa_deaths_intentionality_plot.png",
  plot = iowa_deaths_intentionality_plot,
  path = plot_folder,
  height = 6,
  width = 8
)

# get fire/burn and drowning counts for caption in the following plot ----
fire_burn_drown <- iowa_deaths_cause |>
  dplyr::filter(
    Year == 2025,
    grepl(pattern = "fire|drown", x = Cause, ignore.case = TRUE)
  ) |>
  dplyr::select(-Year)

# unintentional trauma deaths by cause in Iowa plot
iowa_deaths_cause_plot <- iowa_deaths_cause |>
  dplyr::mutate(
    Cause = stringr::str_remove_all(Cause, pattern = "^\\d{2}-"),
    labels = dplyr::if_else(Year %in% c(2021, 2025), Deaths, NA),
    labels = dplyr::if_else(
      Year == 2025 &
        grepl(pattern = "fire|drown", x = Cause, ignore.case = TRUE),
      NA,
      labels
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x = factor(Year),
    y = Deaths,
    color = reorder(Cause, -Deaths),
    label = labels,
    group = Cause
  )) +
  ggplot2::geom_line(
    alpha = 0.5,
    linewidth = 3,
    lineend = "round",
    linejoin = "round"
  ) +
  ggrepel::geom_text_repel(
    segment.color = NA,
    family = "Work Sans",
    size = 8,
    color = "black",
    direction = "both",
    seed = 10232015
  ) +
  ggplot2::labs(
    x = "",
    y = "",
    title = "Iowa Unintentional Trauma Deaths by Cause",
    subtitle = "Source: Iowa Death Certificate Data | 2021-2025",
    color = "Cause of Death",
    caption = glue::glue(
      "Note: Order of color legend follows descending order of lines. || 2025 Fire/burn = {fire_burn_drown[1,2]}, 2025 Drowning = {fire_burn_drown[2,2]}"
    )
  ) +
  ggplot2::scale_y_continuous(
    labels = function(x) traumar::pretty_number(x)
  ) +
  paletteer::scale_color_paletteer_d(
    palette = "colorblindr::OkabeIto",
    direction = 1
  ) +
  traumar::theme_cleaner(
    base_size = 15,
    title_text_size = 20,
    subtitle_text_size = 18,
    vjust_title = 1.75,
    vjust_subtitle = 1,
    legend_position = "right",
    axis.text.y = ggplot2::element_blank()
  )

# save the iowa_deaths_cause_plot
ggplot2::ggsave(
  filename = "iowa_deaths_cause_plot.png",
  plot = iowa_deaths_cause_plot,
  path = plot_folder,
  height = 6,
  width = 8
)

# trauma suicides by cause plot
iowa_suicides_cause_plot <- iowa_suicides_cause |>
  dplyr::mutate(
    Cause = stringr::str_remove_all(Cause, pattern = "^\\d{2}-")
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x = factor(Year),
    y = Deaths,
    color = reorder(Cause, -Deaths),
    label = Deaths,
    group = Cause
  )) +
  ggplot2::geom_line(
    alpha = 0.5,
    linewidth = 3,
    lineend = "round",
    linejoin = "round"
  ) +
  ggrepel::geom_text_repel(
    segment.color = NA,
    family = "Work Sans",
    size = 8,
    color = "black",
    direction = "x"
  ) +
  ggplot2::labs(
    x = "",
    y = "",
    title = "Iowa Trauma Suicide Deaths by Cause",
    subtitle = "Source: Iowa Death Certificate Data | 2021-2025",
    color = "Cause",
    caption = "Note: Order of color legend follows descending order of lines."
  ) +
  ggplot2::scale_y_continuous(
    labels = function(x) traumar::pretty_number(x)
  ) +
  paletteer::scale_color_paletteer_d(
    palette = "colorblindr::OkabeIto",
    direction = 1
  ) +
  traumar::theme_cleaner(
    base_size = 15,
    title_text_size = 20,
    subtitle_text_size = 18,
    vjust_title = 1.75,
    vjust_subtitle = 1,
    legend_position = "right"
  )

# save trauma suicides by cause plot

ggplot2::ggsave(
  filename = "iowa_suicides_cause_plot.png",
  plot = iowa_suicides_cause_plot,
  path = plot_folder,
  height = 6,
  width = 8
)

# trends in causes of death with 5-year avg
iowa_death_trends_cause_tbl <- iowa_death_trends_cause |>
  dplyr::arrange(Year, desc(Deaths)) |>
  dplyr::mutate(Cause = stringr::str_remove_all(Cause, pattern = "^\\d{2}-")) |>
  tidyr::pivot_wider(
    id_cols = Cause,
    names_from = Year,
    values_from = Deaths,
    values_fill = NA_integer_
  ) |>
  dplyr::mutate(
    `Five Year Avg.` = mean(`2021`:`2025`, na.rm = TRUE),
    `% Diff. from Five Year Avg.` = (`2025` - `Five Year Avg.`) /
      `Five Year Avg.`,
    `2021-2025 Trend` = list(c(
      `2021`,
      `2022`,
      `2023`,
      `2024`,
      `2025`
    )),
    .by = Cause
  ) |>
  dplyr::arrange(desc(`Five Year Avg.`)) |>
  dplyr::select(Cause, `2025`:last_col()) |>
  gt::gt() |>
  gt::tab_header(
    title = "Iowa Trends in Causes of Traumatic Death",
    subtitle = "Source: Iowa Death Certificate Data | 2021-2025"
  ) |>
  gt::cols_label(`2025` = "# Deaths 2025") |>
  gt::fmt_number(
    columns = c(`2025`, `Five Year Avg.`),
    drop_trailing_zeros = TRUE
  ) |>
  gt::fmt_percent(
    columns = `% Diff. from Five Year Avg.`,
    drop_trailing_zeros = TRUE
  ) |>
  gt::tab_footnote(
    footnote = "Bars under cause of death track with count of deaths in 2025.",
    locations = gt::cells_column_labels(columns = Cause)
  ) |>
  gt::tab_footnote(
    footnote = "Five year avg. calculated from counts of each cause of death from 2021 through 2025.",
    locations = gt::cells_column_labels(columns = `Five Year Avg.`)
  ) |>
  gt::opt_footnote_marks(marks = "standard") |>
  gtExtras::gt_plt_dot(
    column = `2025`,
    category_column = Cause,
    palette = "colorblindr::OkabeIto"
  ) |>
  gtExtras::gt_plt_sparkline(
    column = `2021-2025 Trend`,
    type = "shaded",
    same_limit = FALSE,
    label = TRUE,
    fig_dim = c(8, 30)
  ) |>
  tab_style_hhs(border_cols = `2025`:`2021-2025 Trend`)

# save the table
gt::gtsave(
  data = iowa_death_trends_cause_tbl,
  filename = "iowa_death_trends_cause_tbl.png",
  path = plot_folder
)

# trends in unintentional and suicide poisonings
iowa_death_poisoning_plot <- iowa_death_poisoning |>
  dplyr::arrange(Cause, Year) |>
  dplyr::mutate(
    labels = dplyr::if_else(
      Year %in% c(2021, 2025),
      traumar::pretty_number(x = Deaths),
      NA_character_
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    x = Year,
    y = Deaths,
    color = Cause,
    label = labels,
    group = Cause
  )) +
  ggplot2::annotate(
    geom = "segment",
    x = 2021,
    xend = 2025,
    y = mean(iowa_death_poisoning$Deaths),
    yend = mean(iowa_death_poisoning$Deaths),
    linetype = "dashed",
    color = "#03617A",
    alpha = 0.75
  ) +
  ggplot2::annotate(
    geom = "text",
    x = 2023,
    xend = 2023,
    y = mean(iowa_death_poisoning$Deaths) + 15,
    yend = mean(iowa_death_poisoning$Deaths) + 15,
    color = "#03617A",
    alpha = 0.75,
    fontface = "bold",
    family = "Work Sans",
    size = 8,
    label = paste0("Avg.: ", round(mean(iowa_death_poisoning$Deaths)))
  ) +
  ggplot2::geom_line(
    linewidth = 2,
    lineend = "round",
    linejoin = "round",
    alpha = 0.9
  ) +
  ggrepel::geom_text_repel(
    direction = "y",
    segment.color = NA,
    color = "black",
    family = "Work Sans",
    size = 8,
    fontface = "bold"
  ) +
  ggplot2::labs(
    title = "Trends in Iowa Poisoning Deaths",
    subtitle = "Source: Iowa Death Certificate Data | 2021-2025",
    x = "",
    y = "# Deaths\n"
  ) +
  paletteer::scale_color_paletteer_d(palette = "colorblindr::OkabeIto") +
  traumar::theme_cleaner(
    base_size = 15,
    title_text_size = 20,
    subtitle_text_size = 18,
    vjust_title = 1.75,
    vjust_subtitle = 1
  )

# save the poisoning trends plot

ggplot2::ggsave(
  filename = "iowa_death_poisoning_plot.png",
  plot = iowa_death_poisoning_plot,
  path = plot_folder,
  height = 6,
  width = 8
)
