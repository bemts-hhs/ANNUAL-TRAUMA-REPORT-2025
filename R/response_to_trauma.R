###_____________________________________________________________________________
# Response to Trauma - Main report section ----
###_____________________________________________________________________________

####
# You must first run data_load.R and setup.R before running this script
####

# Count of cases by trauma facility level ----
trauma_cases_by_facility_level <- trauma_2025 |>
  dplyr::filter(!is.na(Level)) |>
  injury_case_count(Level) |>
  dplyr::mutate(
    percent = n / sum(n)
  ) |>
  dplyr::arrange(desc(n))

# plot Count of cases by trauma facility level ----
trauma_cases_by_facility_level_gt <- trauma_cases_by_facility_level |>
  gt::gt() |>
  gt::fmt_number(columns = n, drop_trailing_zeros = TRUE) |>
  gtExtras::gt_duplicate_column(column = n) |>
  gtExtras::gt_plt_bar(column = n_dupe) |>
  gt::fmt_percent(columns = percent) |>
  gt::cols_label(
    n ~ "Cases",
    percent ~ "% of Total",
    n_dupe ~ "Comparison"
  ) |>
  tab_style_hhs(border_cols = 2:4, column_labels = 18, body = 16)

# save the plot ----
gt::gtsave(
  filename = "trauma_cases_by_facility_level_gt.png",
  data = trauma_cases_by_facility_level_gt,
  path = output_folder
)

# document the % increase in case volume by level ----
trauma_case_level_increase <- trauma_data_clean |>
  dplyr::filter_out(is.na(Level)) |>
  injury_case_count(Year, Level, descriptive_stats = TRUE, group = Level) |>
  dplyr::arrange(Level) |>
  dplyr::filter(Year >= 2024)

# count of incidents by definitive care facility level ----
trauma_cases_by_def_care_level_2025 <- trauma_2025 |>
  dplyr::filter(Level %in% c("I", "II", "III", "IV"), Receiving == "Yes") |>
  injury_case_count(Level) |>
  dplyr::mutate(
    n_label = prettyNum(n, big.mark = ","),
    percent = n / sum(n),
    percent_label = traumar::pretty_percent(percent, n_decimal = 1)
  )

# count of incidents by definitive care facility level over the years ----
trauma_cases_by_def_care_level_years <- trauma_data_clean |>
  dplyr::filter(Level %in% c("I", "II", "III", "IV"), Receiving == "Yes") |>
  injury_case_count(Year, Level, descriptive_stats = TRUE, group = Level) |>
  dplyr::arrange(Level) |>
  dplyr::mutate(
    overall_change = n - dplyr::lag(x = n, n = 4),
    overall_pct_change = overall_change / dplyr::lag(x = n, n = 4),
    overall_pct_change_label = scales::percent(
      overall_pct_change,
      accuracy = 0.01
    ),
    .by = Level
  )

# plot the count of incidents by definitive care facility level ----
trauma_cases_by_def_care_level_plot <- trauma_cases_by_def_care_level |>
  ggplot2::ggplot(ggplot2::aes(
    x = reorder(Level, n),
    y = n,
    color = Level,
    label = paste0(n_label, " (", percent_label, ")")
  )) +
  ggplot2::geom_col(
    color = "white",
    fill = "darkslategray",
    width = 0.03,
    alpha = 0.75
  ) +
  ggplot2::geom_point(size = 8) +
  ggrepel::geom_text_repel(
    direction = "x",
    nudge_y = 0,
    nudge_x = -0.25,
    color = "black",
    fontface = "bold",
    family = "Work Sans",
    size = 15,
    seed = 10232015,
    segment.color = "transparent"
  ) +
  ggplot2::labs(
    x = "",
    y = "Count"
  ) +
  ggplot2::coord_flip() +
  ggplot2::guides(color = "none") +
  traumar::theme_cleaner(
    base_size = 30,
    axis.text.x = ggplot2::element_blank()
  ) +
  ggplot2::scale_y_continuous(
    labels = function(x) traumar::pretty_number(x, digits = 2)
  ) +
  paletteer::scale_color_paletteer_d(palette = "colorblindr::OkabeIto_black")

# save the definitive care case count plot ----
ggplot2::ggsave(
  filename = "trauma_cases_by_def_care_level_plot.png",
  plot = trauma_cases_by_def_care_level_plot,
  path = plot_folder,
  height = 6.67,
  width = 6.67 * (16 / 9)
)

## Trauma Team Activations section ----

# transport mode to facility df ----
transport_mode_to_facility <- trauma_data_clean |>
  dplyr::mutate(
    Transport_To_Your_Facility_By = dplyr::if_else(
      grepl(
        pattern = "not known|not applicable",
        x = Transport_To_Your_Facility_By,
        ignore.case = TRUE
      ),
      "Missing",
      dplyr::if_else(
        grepl(
          pattern = "ALS|BLS",
          x = Transport_To_Your_Facility_By,
          ignore.case = FALSE
        ),
        "Ground Ambulance",
        ifelse(
          grepl(
            pattern = "private",
            x = Transport_To_Your_Facility_By,
            ignore.case = TRUE
          ),
          "Private/Public/Walk-in",
          Transport_To_Your_Facility_By
        )
      )
    )
  ) |>
  injury_case_count(Admission_Quarter, Transport_To_Your_Facility_By) |>
  dplyr::arrange(Admission_Quarter, desc(n)) |>
  dplyr::mutate(
    percent = n / sum(n),
    .by = Admission_Quarter
  )

# transport mode to facility plot ----
transport_mode_to_facility_plot <- transport_mode_to_facility |>
  dplyr::filter_out(grepl(
    pattern = "ambulette|other|missing",
    x = Transport_To_Your_Facility_By,
    ignore.case = TRUE
  )) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = Admission_Quarter,
      y = n,
      label = traumar::pretty_number(x = n, digits = 2)
    )
  ) +
  ggplot2::geom_line(
    color = "thistle2",
    linewidth = 1.25,
    linejoin = "round",
    lineend = "round"
  ) +
  ggplot2::geom_point(color = "slategray", size = 2) +
  ggplot2::guides(color = "none") +
  ggplot2::labs(
    x = "",
    y = ""
  ) +
  traumar::theme_cleaner(
    base_size = 18,
    facets = TRUE
  ) +
  ggplot2::scale_y_continuous(labels = function(x) {
    traumar::pretty_number(x = x, digits = 2)
  }) +
  ggplot2::scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 90)
  ) +
  ggplot2::facet_wrap(~Transport_To_Your_Facility_By, scales = "free_y")

# save the transport mode plot ----
ggplot2::ggsave(
  filename = "transport_mode_to_facility_plot.png",
  plot = transport_mode_to_facility_plot,
  path = plot_folder,
  height = 7,
  width = 7 * (16 / 9)
)

# transport mode to facility among receiving facilities df ----
transport_mode_to_facility_receiving <- trauma_2025 |>
  dplyr::filter(Receiving == "Yes") |>
  dplyr::mutate(
    Transport_To_Your_Facility_By = dplyr::if_else(
      grepl(
        pattern = "not known|not applicable",
        x = Transport_To_Your_Facility_By,
        ignore.case = TRUE
      ),
      "Not Known/Not Recorded",
      dplyr::if_else(
        grepl(
          pattern = "ALS|BLS",
          x = Transport_To_Your_Facility_By,
          ignore.case = FALSE
        ),
        "Ground Ambulance",
        Transport_To_Your_Facility_By
      )
    )
  ) |>
  injury_case_count(Transport_To_Your_Facility_By) |>
  dplyr::arrange(desc(n)) |>
  dplyr::mutate(
    Transport_To_Your_Facility_By = stringr::str_wrap(
      Transport_To_Your_Facility_By,
      whitespace_only = FALSE,
      width = 20
    ),
    number_label = dplyr::if_else(
      n >= 6,
      prettyNum(
        traumar::small_count_label(
          var = n,
          cutoff = 6,
          replacement = NA_integer_
        ),
        big.mark = ","
      ),
      "*"
    ),
    percent = n / sum(n),
    percent_label = dplyr::if_else(
      number_label == "*",
      "*",
      traumar::pretty_percent(percent)
    )
  )

# transport mode to facility among receiving facilities plot ----
transport_mode_to_facility_receiving_plot <- transport_mode_to_facility_receiving |>
  ggplot2::ggplot(ggplot2::aes(
    x = reorder(Transport_To_Your_Facility_By, n),
    y = n,
    fill = n,
    label = ifelse(
      number_label == "*",
      "*",
      paste0(number_label, " (", percent_label, ")")
    )
  )) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggrepel::geom_text_repel(
    direction = "x",
    family = "Work Sans",
    size = 15,
    fontface = "bold",
    nudge_y = 1,
    seed = 10232015
  ) +
  ggplot2::guides(fill = "none") +
  ggplot2::labs(
    x = "",
    y = ""
  ) +
  traumar::theme_cleaner(
    base_size = 30,
    axis.text.x = ggplot2::element_blank()
  ) +
  paletteer::scale_fill_paletteer_c(
    palette = "ggthemes::Orange Light",
    direction = 1
  )

# save the transport mode plot ----
ggplot2::ggsave(
  filename = "transport_mode_to_facility_receiving_plot.png",
  plot = transport_mode_to_facility_receiving_plot,
  path = plot_folder,
  height = 7,
  width = 7 * (16 / 9)
)

# case count by ISS range ----
case_count_iss_range_df <- trauma_2025 |>
  dplyr::filter(!is.na(ISS_Range)) |>
  dplyr::mutate(
    ISS_Range = factor(
      ISS_Range,
      levels = c("1 - 8", "9 - 15", "16+"),
      labels = c("1-8", "9-15", "16+")
    )
  ) |>
  injury_case_count(ISS_Range, Level) |>
  dplyr::mutate(
    number_label = prettyNum(n, big.mark = ","),
    percent = n / sum(n),
    percent_label = traumar::pretty_percent(percent),
    full_label = ifelse(
      n >= 6,
      paste0(number_label, " (", percent_label, ")"),
      "*"
    ),
    .by = ISS_Range
  ) |>
  dplyr::arrange(ISS_Range, desc(n))

# case count by ISS range plot ----
case_count_iss_range_plot <- case_count_iss_range_df |>
  ggplot2::ggplot(ggplot2::aes(
    tidytext::reorder_within(x = Level, by = n, within = ISS_Range),
    y = n,
    fill = Level,
    label = full_label
  )) +
  tidytext::scale_x_reordered() +
  ggplot2::geom_col(alpha = 0.75) +
  ggrepel::geom_text_repel(
    direction = "x",
    family = "Work Sans",
    fontface = "bold",
    size = 10,
    color = "black",
    segment.color = "transparent",
    seed = 10232015
  ) +
  ggplot2::coord_flip() +
  ggplot2::facet_grid(
    rows = ggplot2::vars(ISS_Range),
    switch = "y",
    scales = "free_y"
  ) +
  ggplot2::labs(
    x = "",
    y = ""
  ) +
  ggplot2::guides(fill = "none") +
  traumar::theme_cleaner(
    base_size = 30,
    facet_text_size = 30,
    strip.placement = "outside",
    draw_panel_border = TRUE,
    facets = TRUE,
    axis.text.x = ggplot2::element_blank()
  ) +
  paletteer::scale_fill_paletteer_d(palette = "colorblindr::OkabeIto")

# save the iss range count by facility level plot ----
ggplot2::ggsave(
  filename = "case_count_iss_range_plot.png",
  plot = case_count_iss_range_plot,
  path = plot_folder,
  height = 7,
  width = 7 * (16 / 9)
)

# case count by ISS range and facility level at receiving facilities ----
case_count_iss_range_receiving_df <- trauma_2025 |>
  dplyr::filter(!is.na(ISS_Range), Receiving == "Yes") |>
  dplyr::mutate(
    ISS_Range = factor(
      ISS_Range,
      levels = c("1 - 8", "9 - 15", "16+"),
      labels = c("1-8", "9-15", "16+")
    )
  ) |>
  injury_case_count(ISS_Range, Level) |>
  dplyr::mutate(
    number_label = prettyNum(n, big.mark = ","),
    percent = n / sum(n),
    percent_label = traumar::pretty_percent(percent),
    full_label = ifelse(
      n >= 6,
      paste0(number_label, " (", percent_label, ")"),
      "*"
    ),
    .by = ISS_Range
  ) |>
  dplyr::arrange(ISS_Range, desc(n))

# case count by ISS range and facility level at receiving facilities plot ----
case_count_iss_range_receiving_plot <- case_count_iss_range_receiving_df |>
  ggplot2::ggplot(ggplot2::aes(
    tidytext::reorder_within(x = Level, by = n, within = ISS_Range),
    y = n,
    fill = Level,
    label = full_label
  )) +
  tidytext::scale_x_reordered() +
  ggplot2::geom_col(alpha = 0.75) +
  ggrepel::geom_text_repel(
    direction = "x",
    family = "Work Sans",
    fontface = "bold",
    size = 10,
    color = "black",
    segment.color = "transparent",
    seed = 10232015,
    nudge_y = 1
  ) +
  ggplot2::coord_flip() +
  ggplot2::facet_grid(
    rows = ggplot2::vars(ISS_Range),
    switch = "y",
    scales = "free_y"
  ) +
  ggplot2::labs(
    x = "",
    y = ""
  ) +
  ggplot2::guides(fill = "none") +
  traumar::theme_cleaner(
    base_size = 30,
    facet_text_size = 30,
    strip.placement = "outside",
    draw_panel_border = TRUE,
    facets = TRUE,
    axis.text.x = ggplot2::element_blank()
  ) +
  paletteer::scale_fill_paletteer_d(palette = "colorblindr::OkabeIto")

# save the plot ----
ggplot2::ggsave(
  filename = "case_count_iss_range_receiving_plot.png",
  plot = case_count_iss_range_receiving_plot,
  path = plot_folder
)

# cause of injury frequency collapsed categories ----
cause_of_injury_freq <- trauma_2025 |>
  dplyr::mutate(
    Level = factor(Level, levels = c("I", "II", "III", "IV", "FSED"))
  ) |>
  dplyr::filter(!is.na(Level), !is.na(CAUSE_OF_INJURY_AR_1)) |>
  injury_case_count(Level, CAUSE_OF_INJURY_AR_1) |>
  tidyr::complete(Level, CAUSE_OF_INJURY_AR_1, fill = list(n = 0)) |>
  dplyr::arrange(Level, desc(n)) |>
  dplyr::mutate(number_label = prettyNum(n, big.mark = ","))

# cause of injury frequency collapsed categories plot ----
cause_of_injury_freq_plot <- cause_of_injury_freq |>
  ggplot2::ggplot(ggplot2::aes(
    x = reorder(Level, n),
    y = n,
    fill = CAUSE_OF_INJURY_AR_1,
    label = dplyr::if_else(n < 200, "", number_label)
  )) +
  ggplot2::geom_col(alpha = 0.75, position = "stack") +
  ggplot2::geom_text(
    position = ggplot2::position_stack(vjust = 0.5),
    size = 10,
    color = "black",
    family = "Work Sans",
    fontface = "bold",
    angle = dplyr::if_else(cause_of_injury_freq$n < 700, 90, 0)
  ) +
  ggplot2::labs(
    x = "",
    y = "Case Count",
  ) +
  ggplot2::coord_flip() +
  traumar::theme_cleaner(
    base_size = 30,
    legend_position = "inside",
    legend.position.inside = c(.8, .3)
  ) +
  ggplot2::theme(legend.title = ggplot2::element_blank()) +
  paletteer::scale_fill_paletteer_d(palette = "colorblindr::OkabeIto_black") +
  ggplot2::scale_y_continuous(
    labels = function(x) traumar::pretty_number(x, digits = 1)
  )

# save the cause of injury by level plot ----
ggplot2::ggsave(
  filename = "cause_of_injury_freq_plot.png",
  plot = cause_of_injury_freq_plot,
  path = plot_folder,
  height = 7,
  width = 7 * (16 / 9)
)

# additional cause of injury frequency df ----
injuries_not_needed_pattern <- c("fall|motor|mvc|firearm|struck")

# cause of injury frequency among non-falls, non-mvc, non-firearm, non-struck by ----
cause_of_injury_additional_freq <- trauma_2025 |>
  dplyr::filter(
    !is.na(LEVEL_FALL1_1),
    !grepl(
      pattern = injuries_not_needed_pattern,
      x = LEVEL_FALL1_1,
      ignore.case = TRUE
    )
  ) |>
  injury_case_count(LEVEL_FALL1_1) |>
  dplyr::arrange(desc(n)) |>
  dplyr::mutate(
    LEVEL_FALL1_1 = dplyr::case_when(
      LEVEL_FALL1_1 == "Other Specified, Unintentional" ~ "Other-Unintentional",
      LEVEL_FALL1_1 == "Other Specified, Assault" ~ "Other-Assault",
      LEVEL_FALL1_1 == "Poisoning, Non-Drug" ~ "Poisoning Non-Drug",
      TRUE ~ LEVEL_FALL1_1
    ),
    number_label = traumar::small_count_label(
      var = n,
      cutoff = 6,
      replacement = "*"
    ),
    full_label = dplyr::if_else(
      n < 50,
      paste0(LEVEL_FALL1_1, " (", number_label, ")"),
      paste0(LEVEL_FALL1_1, "\n(", number_label, ")")
    )
  )


# additional cause of injury frequency treemap using treemapify package ----
cause_of_injury_additional_freq_plots <- cause_of_injury_additional_freq |>
  ggplot2::ggplot(ggplot2::aes(area = n, label = full_label, fill = n)) +
  treemapify::geom_treemap(
    color = "white",
    layout = "squarified",
    start = "bottomleft"
  ) +
  treemapify::geom_treemap_text(
    family = "Work Sans",
    size = 18,
    color = "white",
    fontface = "bold"
  ) +
  ggplot2::guides(fill = "none") +
  traumar::theme_cleaner(
    base_size = 30
  ) +
  viridis::scale_fill_viridis(option = "cividis", direction = -1)

# save the plot on additional cause of injury frequency ----
ggplot2::ggsave(
  filename = "cause_of_injury_additional_freq_plots.png",
  plot = cause_of_injury_additional_freq_plots,
  path = plot_folder,
  height = 7,
  width = 7 * (16 / 9)
)

# transfers out by trauma level df ----
transfers_out_by_trauma_lvl <- trauma_2025 |>
  dplyr::filter(Acute_Transfer_Out == "Yes") |>
  injury_case_count(Level) |>
  dplyr::arrange(desc(n)) |>
  dplyr::mutate(
    number_label = prettyNum(n, big.mark = ","),
    size_mod = log(n)
  )

# transfers out by trauma level plot ----
transfers_out_by_trauma_lvl_plot <- transfers_out_by_trauma_lvl |>
  ggplot2::ggplot(ggplot2::aes(
    x = reorder(x = Level, X = n),
    y = n,
    fill = Level,
    label = number_label
  )) +
  ggplot2::geom_point(
    shape = 21,
    color = "black",
    size = 5 * transfers_out_by_trauma_lvl$size_mod
  ) +
  ggrepel::geom_text_repel(
    direction = "x",
    nudge_y = ifelse(
      transfers_out_by_trauma_lvl$n > 4000,
      950,
      ifelse(
        transfers_out_by_trauma_lvl$n > 1000,
        800,
        ifelse(transfers_out_by_trauma_lvl$n > 200, 550, 400)
      )
    ),
    family = "Work Sans",
    size = 15,
    fontface = "bold",
    color = "black",
    seed = 10232015,
    segment.color = "transparent"
  ) +
  ggplot2::coord_flip() +
  ggplot2::guides(color = "none", size = "none", fill = "none") +
  ggplot2::labs(
    x = "",
    y = "Cases"
  ) +
  traumar::theme_cleaner(
    base_size = 30,
    axis.text.x = ggplot2::element_blank()
  ) +
  ggthemes::scale_fill_colorblind()

# save the plot of transfer cases ----
ggplot2::ggsave(
  filename = "acute_transfers_out_by_trauma_lvl_plot.png",
  plot = transfers_out_by_trauma_lvl_plot,
  path = plot_folder,
  height = 7,
  width = 7 * (16 / 9)
)

# transfer delays among patients being transferred out df ----

# source df to get reference values ----
transfer_delays_transfer_out <- trauma_2025 |>
  dplyr::filter(Acute_Transfer_Out == "Yes") |>
  tidyr::replace_na(list(Transfer_Delay_Reason = "Not Applicable")) |>
  dplyr::mutate(
    Transfer_Delay_Reason = dplyr::if_else(
      grepl(
        pattern = "select|not\\s(?:known|applicable|recorded|reporting|available|stated|performed)",
        x = Transfer_Delay_Reason,
        ignore.case = TRUE
      ),
      "Missing",
      Transfer_Delay_Reason
    )
  ) |>
  injury_case_count(Transfer_Delay_Reason) |>
  dplyr::arrange(desc(n)) |>
  dplyr::mutate(
    number_label = prettyNum(n, big.mark = ","),
    size_mod = log(n)
  )

# df for plotting ----
transfer_delays_transfer_out_main <- transfer_delays_transfer_out |>
  dplyr::filter(!Transfer_Delay_Reason %in% c("Missing", "Other"))

# get the 'missing' and 'other' values for the transfer delay reasons ----
missing_transfer_delays <- transfer_delays_transfer_out |>
  dplyr::filter(Transfer_Delay_Reason == "Missing") |>
  dplyr::pull(n) |>
  prettyNum(big.mark = ",")

other_transfer_delays <- transfer_delays_transfer_out |>
  dplyr::filter(Transfer_Delay_Reason == "Other") |>
  dplyr::pull(n) |>
  prettyNum(big.mark = ",")

# transfer delays among patients being transferred out plot ----
transfer_delays_transfer_out_plot <- transfer_delays_transfer_out_main |>
  dplyr::mutate(
    Transfer_Delay_Reason = dplyr::case_when(
      Transfer_Delay_Reason ==
        "Delayed identification that the patient needed trauma center resources" ~
        "Delayed identification of Pt. need",
      TRUE ~ Transfer_Delay_Reason
    ),
    Transfer_Delay_Reason = stringr::str_replace_all(
      Transfer_Delay_Reason,
      pattern = "[Pp]atient",
      replacement = "Pt."
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    reorder(Transfer_Delay_Reason, n),
    n,
    fill = n,
    label = traumar::small_count_label(var = n, cutoff = 6, replacement = "*")
  )) +
  ggplot2::geom_col(width = 0.5) +
  ggplot2::coord_flip() +
  ggplot2::guides(fill = "none") +
  ggplot2::labs(
    caption = glue::glue(
      "- There were { prettyNum(missing_transfer_delays, big.mark = ",
      ") } cases that were not delayed or were\nmissing a category, and { other_transfer_delays } marked as 'other.'"
    ),
    x = "",
    y = ""
  ) +
  viridis::scale_fill_viridis(option = "magma", direction = -1) +
  ggplot2::scale_y_continuous(labels = function(x) traumar::pretty_number(x)) +
  traumar::theme_cleaner(
    base_size = 18
  )

# save the transfer delay reason plot ----
ggplot2::ggsave(
  filename = "transfer_delays_transfer_out_plot.png",
  plot = transfer_delays_transfer_out_plot,
  path = plot_folder
)

# trauma team activations ----
tta_counts <- trauma_data_clean |>
  injury_case_count(
    Year,
    Trauma_Team_Activated,
    descriptive_stats = TRUE,
    group = Trauma_Team_Activated
  ) |>
  dplyr::arrange(Trauma_Team_Activated) |>
  dplyr::mutate(
    pct_total = n / sum(n),
    .by = Year,
    .after = n
  ) |>
  dplyr::mutate(
    overall_change = n - dplyr::lag(x = n, n = 4),
    overall_pct_change = overall_change / dplyr::lag(x = n, n = 4),
    overall_pct_change_label = scales::percent(
      x = overall_pct_change,
      accuracy = 0.01
    ),
    .by = Trauma_Team_Activated
  )

# TTAs among verification levels ----
tta_levels_counts <- trauma_data_clean |>
  dplyr::mutate(
    Trauma_Team_Activation_Level = dplyr::case_when(
      is.na(Trauma_Team_Activation_Level) |
        grepl(
          pattern = "not\\s|non-|consultation|level 3",
          x = Trauma_Team_Activation_Level,
          ignore.case = TRUE
        ) ~
        "Not Activated",
      TRUE ~ Trauma_Team_Activation_Level
    )
  ) |>
  injury_case_count(
    Year,
    Trauma_Team_Activation_Level,
    descriptive_stats = TRUE,
    group = Trauma_Team_Activation_Level
  ) |>
  dplyr::arrange(Trauma_Team_Activation_Level) |>
  dplyr::mutate(
    overall_change = n - dplyr::lag(x = n, n = 4),
    overall_pct_change = overall_change / dplyr::lag(x = n, n = 4),
    overall_pct_change_label = scales::percent(
      x = overall_pct_change,
      accuracy = 0.01
    ),
    .by = Trauma_Team_Activation_Level
  )

# acute transfers ----
acute_transfer_counts <- trauma_data_clean |>
  dplyr::mutate(
    Acute_Transfer_Out = ifelse(
      is.na(Acute_Transfer_Out),
      "No",
      Acute_Transfer_Out
    )
  ) |>
  injury_case_count(
    Year,
    Acute_Transfer_Out,
    descriptive_stats = TRUE,
    group = Acute_Transfer_Out
  ) |>
  dplyr::arrange(Acute_Transfer_Out) |>
  dplyr::mutate(
    overall_change = n - dplyr::lag(x = n, n = 4),
    overall_pct_change = overall_change / dplyr::lag(x = n, n = 4),
    overall_pct_change_label = scales::percent(
      x = overall_pct_change,
      accuracy = 0.01
    ),
    .by = Acute_Transfer_Out
  )

# median ED stay prior to transfer by ISS range df ----
# prep data ----
prep_trauma_data <- trauma_data_clean |>
  dplyr::mutate(
    Trauma_Team_Activation_Level = dplyr::case_when(
      is.na(Trauma_Team_Activation_Level) |
        grepl(
          pattern = "not\\s|non-|consultation|level 3",
          x = Trauma_Team_Activation_Level,
          ignore.case = TRUE
        ) ~
        "Not Activated",
      TRUE ~ Trauma_Team_Activation_Level
    )
  ) |>
  dplyr::filter(
    Acute_Transfer_Out == "Yes",
    !is.na(Trauma_Team_Activated),
    !is.na(ISS_Range)
  ) |>
  dplyr::distinct(Unique_Incident_ID, .keep_all = TRUE)

# use tidymodels to create a recipe to handle missing values and outliers
# create a recipe for imputing and outlier handling ----
trauma_recipe <- recipes::recipe(~., data = prep_trauma_data) |>
  recipes::step_mutate(
    Trauma_Team_Activated = factor(
      Trauma_Team_Activated,
      levels = c("Trauma Team Activated", "Trauma Team Not Activated"),
      labels = c("Activated", "Not Activated")
    ),
    ISS_Range = factor(
      ISS_Range,
      levels = c("1 - 8", "9 - 15", "16+"),
      labels = c("1-8", "9-15", "16+")
    )
  ) |>
  recipes::step_zv(recipes::all_predictors()) |>
  recipes::step_nzv(recipes::all_predictors()) |>
  recipes::step_impute_knn(Length_of_Stay, neighbors = 5) # KNN imputation

# prep and bake the recipe ----
trauma_data_processed <- withr::with_seed(
  10232015, # ensures reproducible KNN imputation
  trauma_recipe |>
    recipes::prep() |>
    recipes::bake(new_data = NULL) |>
    dplyr::mutate(
      # winsorize LOS within Year/ISS_Range groups AFTER imputation
      Length_of_Stay = traumar::impute(
        x = Length_of_Stay,
        focus = "skew",
        method = "winsorize",
        percentile = 0.95
      ),
      .by = c(Year, Trauma_Team_Activated, ISS_Range)
    )
)

# summarize lengths of stay ----
median_ed_stay_transfers_iss <- trauma_data_processed |>
  traumar::is_it_normal(
    Length_of_Stay,
    group_vars = c("Year", "Trauma_Team_Activated", "ISS_Range"),
    normality_test = NULL,
    include_plots = FALSE
  ) |>
  purrr::pluck(1) # get the descriptive statistics

# get raw differences ----
median_ed_stay_transfers_iss_diff <- median_ed_stay_transfers_iss |>
  dplyr::select(Year, Trauma_Team_Activated, ISS_Range, median, n_obs) |>
  tidyr::pivot_wider(
    id_cols = c(Year, ISS_Range),
    names_from = Trauma_Team_Activated,
    values_from = c(median, n_obs)
  ) |>
  janitor::clean_names(case = "snake") |>
  dplyr::mutate(
    raw_diff = abs(median_activated - median_not_activated),
    .after = median_not_activated
  )

# get overall raw difference in median LOS among TTA groups ----
median_diff_ed_los <- trauma_data_processed |>
  traumar::is_it_normal(
    Length_of_Stay,
    group_vars = c("Year", "Trauma_Team_Activated"),
    normality_test = NULL,
    include_plots = FALSE
  ) |>
  purrr::pluck(1) |>
  dplyr::select(Year, Trauma_Team_Activated, median) |>
  dplyr::arrange(Trauma_Team_Activated) |>
  tidyr::pivot_wider(
    names_from = Trauma_Team_Activated,
    values_from = median
  ) |>
  dplyr::mutate(raw_diff = abs(Activated - `Not Activated`))

# get observed difference in median LOS via infer ----
# for independent samples
# note guidance from the tidymodels team on the possibility of p values of 0
# being generated by the infer::get_p_value() function, given the
# simulation-based inference,
true_median_ed_stay_diff <- withr::with_seed(
  seed = 10232015,

  trauma_data_processed |>
    dplyr::group_by(Year, ISS_Range) |>
    tidyr::nest() |>
    dplyr::mutate(
      # Calculate observed difference in medians
      observed_diff = purrr::map(
        .x = data,
        ~ .x |>
          infer::specify(Length_of_Stay ~ Trauma_Team_Activated) |>
          infer::calculate(
            stat = "diff in medians",
            order = c("Activated", "Not Activated")
          )
      ),
      # Calculate observed difference in medians, get as vector of stats
      # useful in calculation of p values later
      observed_stats = purrr::map(
        .x = data,
        ~ .x |>
          infer::specify(Length_of_Stay ~ Trauma_Team_Activated) |>
          infer::calculate(
            stat = "diff in medians",
            order = c("Activated", "Not Activated")
          ) |>
          dplyr::pull(stat)
      ),
      # Calculate bootstrap confidence intervals using infer package
      infer_results = purrr::map(
        .x = data,
        ~ .x |>
          infer::specify(Length_of_Stay ~ Trauma_Team_Activated) |>
          infer::generate(reps = 1000, type = "bootstrap") |>
          infer::calculate(
            stat = "diff in medians",
            order = c("Activated", "Not Activated")
          ) |>
          infer::get_confidence_interval(level = 0.95, type = "percentile")
      ),
      # Generate null distribution and calculate p-value
      p_value = purrr::map(
        .x = data,
        ~ .x |>
          # Get the null distribution for p value calculation
          infer::specify(Length_of_Stay ~ Trauma_Team_Activated) |>
          infer::hypothesize(null = "independence") |>
          infer::generate(reps = 1000, type = "permute") |>
          infer::calculate(
            stat = "diff in medians",
            order = c("Activated", "Not Activated")
          ) |>
          # p value calculation for the group-wise difference in medians
          infer::get_p_value(
            obs_stat = unlist(observed_stats), # must unlist to get vector
            direction = "less"
          )
      )
    ) |>
    dplyr::select(-data) |>
    tidyr::unnest(c(observed_diff, p_value, infer_results)) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      median_ed_stay_transfers_iss_diff,
      by = dplyr::join_by(Year == year, ISS_Range == iss_range)
    ) |>
    dplyr::relocate(Year, .before = 1) |>
    dplyr::arrange(Year, ISS_Range)
)

# use of wilcox to estimate the stochastic superiority ----
true_diff_p_val <- trauma_data_processed |>
  dplyr::group_by(Year, ISS_Range) |>
  tidyr::nest() |>
  dplyr::mutate(
    wilcox = purrr::map(
      .x = data,
      ~ wilcox.test(
        Length_of_Stay ~ Trauma_Team_Activated,
        data = .x,
        exact = FALSE,
        conf.int = TRUE,
        conf.level = 0.95,
        alternative = "less"
      )
    ),
    wilcox = purrr::map(.x = wilcox, ~ broom::tidy(x = .x))
  ) |>
  dplyr::select(-data) |>
  tidyr::unnest(wilcox) |>
  dplyr::ungroup() |>
  dplyr::left_join(
    median_ed_stay_transfers_iss_diff,
    by = dplyr::join_by(Year == year, ISS_Range == iss_range)
  ) |>
  dplyr::relocate(Year, .before = 1) |>
  dplyr::arrange(Year, ISS_Range)

# get true overall difference in median LOS ----
true_median_ed_stay_diff_overall <- trauma_data_processed |>
  dplyr::group_by(Year) |>
  tidyr::nest() |>
  dplyr::mutate(
    # Calculate observed difference in medians
    observed_diff = purrr::map(
      .x = data,
      ~ .x |>
        infer::specify(Length_of_Stay ~ Trauma_Team_Activated) |>
        infer::calculate(
          stat = "diff in medians",
          order = c("Activated", "Not Activated")
        )
    ),
    # Calculate observed difference in medians, get as vector of stats
    # useful in calculation of p values later
    observed_stats = purrr::map(
      .x = data,
      ~ .x |>
        infer::specify(Length_of_Stay ~ Trauma_Team_Activated) |>
        infer::calculate(
          stat = "diff in medians",
          order = c("Activated", "Not Activated")
        ) |>
        dplyr::pull(stat)
    ),
    # Calculate bootstrap confidence intervals using infer package
    infer_results = purrr::map(
      .x = data,
      ~ .x |>
        infer::specify(Length_of_Stay ~ Trauma_Team_Activated) |>
        infer::generate(reps = 1000, type = "bootstrap") |>
        infer::calculate(
          stat = "diff in medians",
          order = c("Activated", "Not Activated")
        ) |>
        infer::get_confidence_interval(level = 0.95, type = "percentile")
    ),
    # Generate null distribution and calculate p-value
    p_value = purrr::map(
      .x = data,
      ~ .x |>
        # Get the null distribution for p value calculation
        infer::specify(Length_of_Stay ~ Trauma_Team_Activated) |>
        infer::hypothesize(null = "independence") |>
        infer::generate(reps = 1000, type = "permute") |>
        infer::calculate(
          stat = "diff in medians",
          order = c("Activated", "Not Activated")
        ) |>
        # p value calculation for the group-wise difference in medians
        infer::get_p_value(
          obs_stat = unlist(observed_stats), # must unlist to get vector
          direction = "less"
        )
    )
  ) |>
  dplyr::select(-data) |>
  tidyr::unnest(c(observed_diff, p_value, infer_results)) |>
  dplyr::ungroup()

# gt table to show median ED LOS difference based on TTA ----
true_median_ed_stay_diff_gt <- true_median_ed_stay_diff |>
  dplyr::mutate(
    n_obs = n_obs_activated + n_obs_not_activated,
    .after = n_obs_not_activated
  ) |>
  dplyr::select(c(Year:upper_ci, n_obs)) |>
  dplyr::filter(Year >= 2023) |>
  tidyr::pivot_wider(
    id_cols = ISS_Range,
    names_from = Year,
    values_from = c(stat, lower_ci, upper_ci, n_obs)
  ) |>
  gt::gt() |>
  gt::fmt_number(
    columns = tidyselect::matches("stat|_ci|n_obs"),
    drop_trailing_zeros = TRUE
  ) |>
  gt::cols_merge(
    columns = c(
      stat_2023,
      lower_ci_2023,
      upper_ci_2023,
      n_obs_2023
    ),
    pattern = "{1} (CI: {2}, {3}) n={4}"
  ) |>
  gt::cols_merge(
    columns = c(
      stat_2024,
      lower_ci_2024,
      upper_ci_2024,
      n_obs_2024
    ),
    pattern = "{1} (CI: {2}, {3}) n={4}"
  ) |>
  gt::cols_merge(
    columns = c(
      stat_2025,
      lower_ci_2025,
      upper_ci_2025,
      n_obs_2025
    ),
    pattern = "{1} (CI: {2}, {3}) n={4}"
  ) |>
  gt::cols_label_with(
    columns = tidyselect::matches("stat"),
    ~ stringr::str_remove(string = ., pattern = "stat_")
  ) |>
  gt::cols_label(
    ISS_Range ~ "ISS Range"
  ) |>
  tab_style_hhs(
    border_cols = 2:4,
    column_labels = 18,
    body = 16,
    source_note = 16
  )

# save the gt table for median differences in ED LOS over the years ----
gt::gtsave(
  filename = "true_median_ed_stay_diff_gt.png",
  data = true_median_ed_stay_diff_gt,
  path = output_folder
)

# create intermediate objects for the plot ----
raw_median_times <- trauma_data_processed |>
  traumar::is_it_normal(Length_of_Stay, group_vars = "Year") |>
  purrr::pluck(1)

raw_median_times_pivot <- median_diff_ed_los |>
  tidyr::pivot_longer(
    cols = c(Activated, `Not Activated`),
    names_to = "var",
    values_to = "val"
  )

# overall median difference in lengths of stay during the study period ----
overall_median_ed_los_difference <- median(median_diff_ed_los$raw_diff)

# plot the change in the median ED LOS prior to transfer over time ----
# as a combination plot
longitudinal_median_ed_los_plot <-
  ggplot2::ggplot() +
  ggplot2::geom_col(
    data = raw_median_times,
    ggplot2::aes(x = Year, y = median),
    width = 0.5,
    alpha = 0.35,
    fill = "#B9E1DA"
  ) +
  ggplot2::geom_text(
    data = raw_median_times,
    ggplot2::aes(x = Year, y = 10, label = median),
    color = "black",
    size = 12,
    family = "Work Sans",
    fontface = "bold"
  ) +
  ggplot2::geom_line(
    data = raw_median_times_pivot,
    ggplot2::aes(x = Year, y = val, color = var),
    linewidth = 1.75
  ) +
  ggrepel::geom_text_repel(
    data = raw_median_times_pivot,
    ggplot2::aes(
      x = Year,
      y = ifelse(Year %in% c(2021, 2025), val, NA),
      label = val
    ),
    color = "black",
    fontface = "bold",
    family = "Work Sans",
    size = 12,
    direction = "y",
    nudge_y = 5,
    seed = 10232015
  ) +
  ggplot2::guides(
    fill = "none",
    color = ggplot2::guide_legend(title = "")
  ) +
  ggplot2::labs(x = "", y = "") +
  ggplot2::scale_color_manual(values = c("#4D4D4F", "#FDD304")) +
  traumar::theme_cleaner(base_size = 30, axis.text.y = ggplot2::element_blank())

# save the longitudinal analysis of medians plot ----
ggplot2::ggsave(
  filename = "longitudinal_median_ed_los_plot.png",
  plot = longitudinal_median_ed_los_plot,
  height = 7,
  width = 7 * (16 / 9),
  path = plot_folder
)
