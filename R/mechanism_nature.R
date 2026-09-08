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
  dplyr::mutate(percent = n / sum(n), .by = Year)

# get a subset of the counts for the plot ----
mechanism_of_injury_counts_select <- mechanism_of_injury_counts |>
  dplyr::filter(Year == 2024)

# plot the age distribution within the IPOP database ----
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

# get statistics on special cases of injuries ----

# work related and ag ---
work_related_agricultural <- trauma_data_clean |>
  dplyr::distinct(Incident_Date, Unique_Patient_ID, .keep_all = TRUE) |>
  dplyr::summarize(
    work_related = sum(Financial_Work_Related == "Yes", na.rm = TRUE),
    ag_related = sum(Farm_Ag_Related == "Yes", na.rm = TRUE),
    n = dplyr::n(),
    percent_work = work_related / n,
    percent_ag = ag_related / n,
    .by = Year
  )

# reinjury ----

# patients that were reinjured ----
reinjury_patient_statistics <- trauma_data_clean |>
  reinjury_patient_count(Year, descriptive_stats = TRUE) |>
  dplyr::select(-c(9:14))

# injuries that occurred as reinjuries ----
reinjury_injury_statistics <- trauma_data_clean |>
  reinjury_injury_count(Year, descriptive_stats = TRUE) |>
  dplyr::select(-c(8:13))

# prepare data for a gt table ----
reinjury_data_prep <- reinjury_patient_statistics |>
  dplyr::select(Year, reinjured_patients, n_patients, prop_reinjured) |>
  dplyr::left_join(
    reinjury_injury_statistics |>
      dplyr::select(Year, Reinjury, injury_events = n, prop),
    by = dplyr::join_by(Year)
  )

# reinjury gt table ----
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

# save the gt reinjury table ----
gt::gtsave(data = reinjury_gt, filename = "reinjury_gt.png", path = plot_folder)

# get counts of the nature of injury and body regions ----

###_____________________________________________________________________________
## Body Region ----
###_____________________________________________________________________________

####
# You must first run data_load.R and setup.R before running this script
####

### IPOP nature of injury frequency ----
ipop_nature_injury_freq <- ipop_data_clean |>
  dplyr::filter(Year == 2024) |>
  dplyr::mutate(
    NATURE_OF_INJURY_DESCRIPTOR = dplyr::if_else(
      NATURE_OF_INJURY_DESCRIPTOR %in% c("Unspecified", "Other specified"),
      "Other",
      NATURE_OF_INJURY_DESCRIPTOR
    )
  ) |>
  ipop_case_count(
    NATURE_OF_INJURY_DESCRIPTOR,
    which = "Inpatient"
  ) |>
  dplyr::arrange(desc(n)) |>
  tidyr::replace_na(list(NATURE_OF_INJURY_DESCRIPTOR = "Missing")) |>
  dplyr::mutate(
    mod = sqrt(n),
    angle = 2 * pi * rank(mod) / dplyr::n(),
    angle_mod = cos(angle)
  )

#### plot nature of injury frequency via area chart ----
# if this plot saves dark, you can load in Paint, edit size down, and it will turn to white background
ipop_nature_injury_freq_plot <- ipop_nature_injury_freq |>
  ggplot2::ggplot(ggplot2::aes(
    x = reorder(
      stringr::str_wrap(NATURE_OF_INJURY_DESCRIPTOR, width = 5),
      -mod
    ),
    y = mod,
    fill = mod,
    label = ifelse(
      n >= 6,
      traumar::pretty_number(n, n_decimal = 2),
      traumar::small_count_label(var = n, cutoff = 6, replacement = "*")
    )
  )) +
  ggplot2::geom_col(position = "dodge2", show.legend = TRUE, alpha = 0.9) +
  # First segment: from the top of the bar to just before the text
  ggplot2::geom_segment(
    ggplot2::aes(
      x = reorder(stringr::str_wrap(NATURE_OF_INJURY_DESCRIPTOR, 5), -mod),
      xend = reorder(stringr::str_wrap(NATURE_OF_INJURY_DESCRIPTOR, 5), -mod),
      y = mod, # Start at the top of the bar
      yend = dplyr::if_else(
        NATURE_OF_INJURY_DESCRIPTOR == "Fracture",
        mod,
        max(mod) - 2
      ) # End just before the text
    ),
    linetype = "dashed",
    color = "#F27026"
  ) +
  ggplot2::geom_label(
    fill = dplyr::if_else(
      ipop_nature_injury_freq$NATURE_OF_INJURY_DESCRIPTOR == "Fracture",
      "transparent",
      "white"
    ),
    nudge_y = dplyr::if_else(
      ipop_nature_injury_freq$NATURE_OF_INJURY_DESCRIPTOR == "Fracture",
      -10,
      20
    ),
    color = dplyr::if_else(
      ipop_nature_injury_freq$NATURE_OF_INJURY_DESCRIPTOR == "Fracture",
      "transparent",
      "white"
    )
  ) +
  ggplot2::geom_text(
    nudge_y = dplyr::if_else(
      ipop_nature_injury_freq$NATURE_OF_INJURY_DESCRIPTOR == "Fracture",
      -12,
      20
    ),
    family = "Work Sans",
    fontface = "bold",
    color = dplyr::if_else(
      ipop_nature_injury_freq$NATURE_OF_INJURY_DESCRIPTOR == "Fracture",
      "white",
      "black"
    ),
    size = 15
  ) +
  ggplot2::coord_radial(clip = "off", inner.radius = 0.15) +
  ggplot2::labs(
    x = "",
    y = "",
    fill = stringr::str_wrap("Orange to blue High to low count", width = 15)
  ) +
  paletteer::scale_fill_paletteer_c(
    palette = "ggthemes::Orange-Blue Diverging",
    direction = -1
  ) +
  traumar::theme_cleaner(
    base_size = 30,
    axis.text.y = ggplot2::element_blank(),
    legend_position = "right"
  ) +
  ggplot2::theme(
    legend.text = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(
      fill = "transparent",
      color = "transparent"
    )
  )

#### save the nature of injury frequency plot for the IPOP data ----
ggplot2::ggsave(
  filename = "ipop_nature_injury_freq_plot.png",
  plot = ipop_nature_injury_freq_plot,
  path = plot_folder,
  height = 15,
  width = 15 * 1.78
)

### IPOP body region injury frequency table ----
ipop_body_region <- ipop_data_clean |>
  dplyr::filter(Year == 2024) |>
  tidyr::replace_na(list(
    BODY_REGION_CATEGORY_LEVEL_1 = "Unclassifiable by body region"
  )) |>
  ipop_case_count(
    BODY_REGION_CATEGORY_LEVEL_1,
    which = "Inpatient"
  ) |>
  dplyr::arrange(desc(n)) |>
  dplyr::mutate(
    BODY_REGION_CATEGORY_LEVEL_1 = stringr::str_replace(
      string = BODY_REGION_CATEGORY_LEVEL_1,
      pattern = "&",
      replacement = "/"
    ),
    BODY_REGION_CATEGORY_LEVEL_1 = dplyr::if_else(
      grepl(
        pattern = "unclass",
        x = BODY_REGION_CATEGORY_LEVEL_1,
        ignore.case = TRUE
      ),
      "Unclassifiable",
      dplyr::if_else(
        grepl(
          pattern = "head and neck",
          x = BODY_REGION_CATEGORY_LEVEL_1,
          ignore.case = TRUE
        ),
        "Head and neck",
        BODY_REGION_CATEGORY_LEVEL_1
      )
    ),
    mod = sqrt(n),
    angle = 2 * pi * rank(mod) / dplyr::n(),
    angle_mod = cos(angle)
  )

#### make the circular bar plot ----
ipop_body_region_plot <- ipop_body_region |>
  ggplot2::ggplot(ggplot2::aes(
    x = reorder(BODY_REGION_CATEGORY_LEVEL_1, -mod),
    y = mod,
    fill = mod,
    label = traumar::pretty_number(n, n_decimal = 2)
  )) +
  ggplot2::geom_col(position = "dodge2", width = 0.5) +
  ggplot2::geom_text(
    family = "Work Sans",
    size = 15,
    color = c(rep("white", 2), rep("black", 4)),
    fontface = "bold",
    nudge_y = c(-15, -15, 9, 8, 10, 8)
  ) +
  #ylim(-1, 9.1) +
  ggplot2::labs(
    x = "",
    y = "",
    fill = stringr::str_wrap("Dark to light red High to low count", width = 19)
  ) +
  paletteer::scale_fill_paletteer_c(
    palette = "grDevices::Reds",
    direction = -1
  ) +
  ggplot2::coord_radial(start = 0, clip = "off") +
  traumar::theme_cleaner(
    base_size = 30,
    axis.text.y = ggplot2::element_blank(),
    legend_position = "top"
  ) +
  ggplot2::theme(
    legend.text = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(
      fill = "transparent",
      color = "transparent"
    )
  )

##### save the body region injury frequency plot ----
ggplot2::ggsave(
  filename = "ipop_body_region_plot.png",
  plot = ipop_body_region_plot,
  path = plot_folder,
  height = 15,
  width = 15 * 1.78
)

###___________________________________________________________________________
### Traumatic Brain Injury ----
###___________________________________________________________________________

#### cases ----
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
      injury_case_count(Year, TBI) |>
      dplyr::mutate(
        percent = n / sum(n),
        percent_label = traumar::pretty_percent(percent, n_decimal = 2),
        .by = Year
      ) |>
      dplyr::filter(TBI) |>
      dplyr::select(-n),
    by = dplyr::join_by(Year, TBI)
  )

#### injury events ----
tbi_related_injuries <- trauma_data_clean |>
  injury_incident_count(
    Year,
    TBI,
    descriptive_stats = TRUE,
    group = TBI
  ) |>
  dplyr::filter(TBI) |>
  dplyr::left_join(
    trauma_data_clean |>
      injury_incident_count(Year, TBI) |>
      dplyr::mutate(
        percent = n / sum(n),
        percent_label = traumar::pretty_percent(percent, n_decimal = 2),
        .by = Year
      ) |>
      dplyr::filter(TBI) |>
      dplyr::select(-n),
    by = dplyr::join_by(Year, TBI)
  ) |>
  dplyr::select(-tidyselect::matches("reinjury"))

##### extract annual tbi percentages of total injury events for caption ----
tbi_pct_labels <- tbi_related_injuries |> dplyr::pull(percent_label)

##### take pct labels and make it into a caption ----
tbi_caption <- paste0(
  "[",
  paste0(tbi_pct_labels[1:length(tbi_pct_labels) - 1], collapse = ", "),
  ", and ",
  tbi_pct_labels[length(tbi_pct_labels)],
  "]"
)

##### tbi injury event plot ----
tbi_injury_event_plot <- tbi_related_injuries |>
  ggplot2::ggplot() +
  ggplot2::geom_col(
    ggplot2::aes(x = Year, y = n),
    position = "dodge",
    width = 0.75,
    color = "transparent",
    fill = "coral"
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      x = Year,
      y = n * 0.05,
      label = prettyNum(n, big.mark = ",")
    ),
    family = "Work Sans",
    color = "white",
    fontface = "bold",
    size = 6
  ) +
  ggplot2::guides(color = "none", fill = "none") +
  ggplot2::labs(
    title = "Iowa TBI Injury Event Counts by Year",
    subtitle = "Source: Iowa Trauma Registry || Years: 2021-2025",
    x = "",
    y = ""
  ) +
  ggthemes::theme_tufte(base_size = 14, base_family = "Work Sans") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 18),
    plot.subtitle = ggplot2::element_text(size = 16),
    axis.text.x = ggplot2::element_text(size = 14),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )

###### save the tbi injury event plot to disk ----
ggplot2::ggsave(
  filename = "tbi_injury_event_plot.png",
  plot = tbi_injury_event_plot,
  path = plot_folder
)

#### patients  ----
tbi_related_patients <- trauma_data_clean |>
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
