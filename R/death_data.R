###_____________________________________________________________________________
# Death data ----
###_____________________________________________________________________________

###_____________________________________________________________________________
# State-wide deaths and CDC WONDER National Center for Health Statistics
# This needs to be updated each year to add on new years that are finalized
# Go to Current Final Multiple Cause of Death Data and make data request
# Use the query to group by UCD 15 leading causes of death
# Include crude rates and age adjusted rates and download all confidence interaval
# and standard error estimations
# You have to download each file year after year. Previous data are finalized
# so no need to download those again unless there are documented updates from CDC
# Select ICD-10 113 Cause List and MCD - ICD-10 113 Cause List options
# URL below:
# https://wonder.cdc.gov/mcd.html
# Use the same process above to get ages 1-44 data for the entire US as well as
# the all ages US data
# For the Iowa specific data, use the same process to get all ages and ages 1-44
# datasets
###_____________________________________________________________________________
# For Josh Jungling's breakdown, go here and choose the most current year's report
# https://hhs.iowa.gov/public-health/health-statistics
# Go to the most recent year's Vital Statistics of Iowa Annual Report
# Use the chart LEADING CAUSES OF DEATH BY NUMBER AND PERCENT OF TOTAL DEATHS, BY GENDER
# Copy / paste the text data from the last 5 years into Copilot and ask it to
# put the data in data.frame format and it will.
# for other estimates, utilize the Tableau workbook at
# https://data.idph.state.ia.us/#/site/IDPH-Data/views/TraumaDeaths/TraumaRequest
###_____________________________________________________________________________

# CDC WONDER ALL UNITED STATES all ages ----

# All US and all ages 2020 ----
death_cdc_wonder_nation_all_2020 <- readr::read_delim(
  file = death_us_all_2020_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2020) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# All US and all ages 2021 ----
death_cdc_wonder_nation_all_2021 <- readr::read_delim(
  file = death_us_all_2021_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2021) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# All US and all ages 2022 ----
death_cdc_wonder_nation_all_2022 <- readr::read_delim(
  file = death_us_all_2022_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2022) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# All US and all ages 2023 ----
death_cdc_wonder_nation_all_2023 <- readr::read_delim(
  file = death_us_all_2023_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2023) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# All US and all ages 2024 ----
death_cdc_wonder_nation_all_2024 <- readr::read_delim(
  file = death_us_all_2024_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2024) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# All US and all ages 2020-2025 ----
death_cdc_wonder_nation_all_2020_2024_aggregate <- readr::read_delim(
  file = death_us_all_2020_2024_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::select(-Notes) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# All US deaths 2020 - 2024 ----
death_cdc_wonder_nation_all_2020_2024_detail <- dplyr::bind_rows(
  death_cdc_wonder_nation_all_2020,
  death_cdc_wonder_nation_all_2021,
  death_cdc_wonder_nation_all_2022,
  death_cdc_wonder_nation_all_2023,
  death_cdc_wonder_nation_all_2024
) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# Download top 10 deaths yearly detail file to .csv for future reference ----
readr::write_csv(
  death_cdc_wonder_nation_all_2020_2024_detail,
  file = paste0(death_path, "death_cdc_wonder_nation_all_2020_2024_detail.csv")
)

# Download top 10 deaths aggregate file to .csv for future reference ----
readr::write_csv(
  death_cdc_wonder_nation_all_2020_2024_aggregate,
  file = paste0(
    death_path,
    "death_cdc_wonder_nation_all_2020_2024_aggregate.csv"
  )
)

###
# Iowa CDC WONDER all ages ----
###

# Iowa and all ages 2020 ----
death_cdc_wonder_iowa_all_2020 <- readr::read_delim(
  file = death_ia_all_2020_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2020) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# Iowa and all ages 2021 ----
death_cdc_wonder_iowa_all_2021 <- readr::read_delim(
  file = death_ia_all_2021_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2021) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# Iowa and all ages 2022 ----
death_cdc_wonder_iowa_all_2022 <- readr::read_delim(
  file = death_ia_all_2022_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2022) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# Iowa and all ages 2023 ----
death_cdc_wonder_iowa_all_2023 <- readr::read_delim(
  file = death_ia_all_2023_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2023) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# Iowa and all ages 2024 ----
death_cdc_wonder_iowa_all_2024 <- readr::read_delim(
  file = death_ia_all_2024_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2024) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# Iowa and all ages 2020-2025 ----
death_cdc_wonder_iowa_all_2020_2024_aggregate <- readr::read_delim(
  file = death_ia_all_2020_2024_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::select(-Notes) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# All US deaths 2020 - 2024 ----
death_cdc_wonder_iowa_all_2020_2024_detail <- dplyr::bind_rows(
  death_cdc_wonder_iowa_all_2020,
  death_cdc_wonder_iowa_all_2021,
  death_cdc_wonder_iowa_all_2022,
  death_cdc_wonder_iowa_all_2023,
  death_cdc_wonder_iowa_all_2024
) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# Download top 10 deaths yearly detail file to .csv for future reference ----
readr::write_csv(
  death_cdc_wonder_iowa_all_2020_2024_detail,
  file = paste0(death_path, "death_cdc_wonder_iowa_all_2020_2024_detail.csv")
)

# Download top 10 deaths aggregate file to .csv for future reference ----
readr::write_csv(
  death_cdc_wonder_iowa_all_2020_2024_aggregate,
  file = paste0(
    death_path,
    "death_cdc_wonder_iowa_all_2020_2024_aggregate.csv"
  )
)

###
# US National CDC WONDER ages 1-44 ----
###

# CDC ages 1-44 2020 ----
death_cdc_wonder_nation_1_44_2020 <- readr::read_delim(
  file = death_us_1_44_2020_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2020) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# CDC ages 1-44 2021 ----
death_cdc_wonder_nation_1_44_2021 <- readr::read_delim(
  file = death_us_1_44_2021_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2021) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# CDC ages 1-44 2022 ----
death_cdc_wonder_nation_1_44_2022 <- readr::read_delim(
  file = death_us_1_44_2022_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2022) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# CDC ages 1-44 2023 ----
death_cdc_wonder_nation_1_44_2023 <- readr::read_delim(
  file = death_us_1_44_2023_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2023) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# CDC ages 1-44 2024 ----
death_cdc_wonder_nation_1_44_2024 <- readr::read_delim(
  file = death_us_1_44_2024_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2024) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# CDC ages 1-44 2020-2025 ----
death_cdc_wonder_nation_1_44_2020_2024_aggregate <- readr::read_delim(
  file = death_us_1_44_2020_2024_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::select(-Notes) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# All US ages 1-44 deaths 2020 - 2024
death_cdc_wonder_nation_1_44_2020_2024_detail <- dplyr::bind_rows(
  death_cdc_wonder_nation_1_44_2020,
  death_cdc_wonder_nation_1_44_2021,
  death_cdc_wonder_nation_1_44_2022,
  death_cdc_wonder_nation_1_44_2023,
  death_cdc_wonder_nation_1_44_2024
) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# Download top 10 deaths ages 1-44 yearly detail file to .csv for future
# reference ----
readr::write_csv(
  death_cdc_wonder_nation_1_44_2020_2024_detail,
  file = paste0(death_path, "death_cdc_wonder_nation_1_44_2020_2024_detail.csv")
)

# Download top 10 deaths ages 1-44 aggregate file to .csv for future reference ----
readr::write_csv(
  death_cdc_wonder_nation_1_44_2020_2024_aggregate,
  file = paste0(
    death_path,
    "death_cdc_wonder_nation_1_44_2020_2024_aggregate.csv"
  )
)

###
# Iowa CDC WONDER ages 1-44 ----
###

# Iowa CDC ages 1-44 2020 ----
death_cdc_wonder_iowa_1_44_2020 <- readr::read_delim(
  file = death_ia_1_44_2020_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2020) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# Iowa CDC ages 1-44 2021 ----
death_cdc_wonder_iowa_1_44_2021 <- readr::read_delim(
  file = death_ia_1_44_2021_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2021) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# Iowa CDC ages 1-44 2022 ----
death_cdc_wonder_iowa_1_44_2022 <- readr::read_delim(
  file = death_ia_1_44_2022_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2022) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# Iowa CDC ages 1-44 2023 ----
death_cdc_wonder_iowa_1_44_2023 <- readr::read_delim(
  file = death_ia_1_44_2023_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2023) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# Iowa CDC ages 1-44 2024 ----
death_cdc_wonder_iowa_1_44_2024 <- readr::read_delim(
  file = death_ia_1_44_2024_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::rename(Year = Notes) |>
  dplyr::mutate(Year = 2024) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# Iowa CDC ages 1-44 2020-2025 ----
death_cdc_wonder_iowa_1_44_2020_2024_aggregate <- readr::read_delim(
  file = death_ia_1_44_2020_2024_path,
  delim = "\t",
  n_max = 10
) |>
  dplyr::select(-Notes) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# All Iowa  ages 1-44 deaths 2020 - 2024
death_cdc_wonder_iowa_1_44_2020_2024_detail <- dplyr::bind_rows(
  death_cdc_wonder_iowa_1_44_2020 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("rate"),
      ~ as.numeric(stringr::str_remove_all(., "Unreliable"))
    )),
  death_cdc_wonder_iowa_1_44_2021 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("rate"),
      ~ as.numeric(stringr::str_remove_all(., "Unreliable"))
    )),
  death_cdc_wonder_iowa_1_44_2022 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("rate"),
      ~ as.numeric(stringr::str_remove_all(., "Unreliable"))
    )),
  death_cdc_wonder_iowa_1_44_2023 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("rate"),
      ~ as.numeric(stringr::str_remove_all(., "Unreliable"))
    )),
  death_cdc_wonder_iowa_1_44_2024 |>
    dplyr::mutate(dplyr::across(
      tidyselect::matches("rate"),
      ~ as.numeric(stringr::str_remove_all(., "Unreliable"))
    ))
) |>
  dplyr::mutate(
    `15 Leading Causes of Death` = stringr::str_replace_all(
      `15 Leading Causes of Death`,
      pattern = "^[#]|\\s\\(.+\\)|\\s\\(.+\\)\\s\\(.+\\)",
      ""
    )
  )

# Iowa Download top 10 deaths ages 1-44 yearly detail file to .csv for future
# reference ----
readr::write_csv(
  death_cdc_wonder_iowa_1_44_2020_2024_detail,
  file = paste0(death_path, "death_cdc_wonder_iowa_1_44_2020_2024_detail.csv")
)

# Iowa Download top 10 deaths ages 1-44 aggregate file to .csv for future reference ----
readr::write_csv(
  death_cdc_wonder_iowa_1_44_2020_2024_aggregate,
  file = paste0(
    death_path,
    "death_cdc_wonder_iowa_1_44_2020_2024_aggregate.csv"
  )
)

###
# Data for section Trends in Causes of Death ----
###

# for the Iowa trauma deaths by intentionality plot ----
iowa_deaths_intentionality <- readxl::read_excel(
  path = p15_path
) |>
  dplyr::rename(
    Intentionality = `_TR P15`
  ) |>
  tidyr::pivot_longer(
    cols = -1,
    names_to = "Year",
    values_to = "Deaths"
  )

# for the Iowa unintentional trauma deaths by cause plot ----
iowa_deaths_cause <- readxl::read_excel(
  path = p16_1_path
) |>
  dplyr::rename(
    Cause = `_TR P16-1`
  ) |>
  tidyr::pivot_longer(
    cols = -1,
    names_to = "Year",
    values_to = "Deaths"
  )

# for the Iowa trauma suicides by cause plot ----
iowa_suicides_cause <- readxl::read_excel(
  path = p16_2_path
) |>
  dplyr::rename(
    Cause = `_TR P16-2`
  ) |>
  tidyr::pivot_longer(
    cols = -1,
    names_to = "Year",
    values_to = "Deaths"
  )

# for the trends in causes of death table ----
iowa_death_trends_cause <- readxl::read_excel(
  path = p17_path
) |>
  dplyr::rename(Cause = `_TR P17`) |>
  tidyr::pivot_longer(
    cols = -1,
    names_to = "Year",
    values_to = "Deaths"
  )

# for Iowa unintentional falls trends plot ----
iowa_death_unintentional_falls <- readxl::read_excel(
  path = p30_path
) |>
  dplyr::rename(Cause = `_TR P30`) |>
  tidyr::pivot_longer(
    cols = -1,
    names_to = "Year",
    values_to = "Deaths"
  )

# for Iowa poisoning death trends plot ----
# unintentional poisoning
iowa_death_unintentional_poisoning <- readxl::read_excel(
  path = p33_1_path
) |>
  dplyr::rename(
    Cause = `_TR P33-1`
  ) |>
  tidyr::pivot_longer(
    cols = -1,
    names_to = "Year",
    values_to = "Deaths"
  )

# suicide poisoning ----
iowa_death_suicide_poisoning <- readxl::read_excel(
  path = p33_2_path
) |>
  dplyr::rename(
    Cause = `_TR P33-2`
  ) |>
  tidyr::pivot_longer(
    cols = -1,
    names_to = "Year",
    values_to = "Deaths"
  )

#union the poisoning tables ----
iowa_death_poisoning <- dplyr::bind_rows(
  iowa_death_unintentional_poisoning,
  iowa_death_suicide_poisoning
)
