###_____________________________________________________________________________
# census bureau standard pops 2020-2025 census ----
# documentation here:
# https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/
###_____________________________________________________________________________

# 2020-2025 Census Bureau County population data ----
# get years for each county population
county_pops_all <- readr::read_csv(
  file = iowa_county_pops_path
)

# get columns of interest for the county-level population data ----
county_pops_select <- county_pops_all |>
  dplyr::filter(STATE == "19", COUNTY != "000") |>
  dplyr::select(County = CTYNAME, tidyselect::matches("popestimate\\d{4}$")) |>
  tidyr::pivot_longer(
    cols = -County,
    names_to = "Year",
    values_to = "County_Population"
  ) |>
  dplyr::mutate(
    Year = stringr::str_remove(string = Year, pattern = "POPESTIMATE"),
    Year = forcats::as_factor(as.numeric(Year)),
    County = stringr::str_squish(stringr::str_remove_all(
      County,
      pattern = "\\sCounty"
    )),
    County = stringr::str_to_title(County),
    County = dplyr::if_else(
      grepl(pattern = "o[']brien", x = County, ignore.case = TRUE),
      "O'Brien",
      County
    )
  )

# Iowa county pops by age group ----

# ingest data
age_group_pops <- readr::read_csv(iowa_county_age_pops_path)

# 2020-2025 data Iowa county population data by age group
age_group_pops_final <- age_group_pops |>
  dplyr::select(
    CTYNAME,
    YEAR,
    tidyselect::matches(
      "age(04|59|1014|1519|2024|2529|3034|3539|4044|4549|5054|5559|6064|6569|7074|7579|8084|85plus)_tot",
      ignore.case = TRUE
    ),
    POPESTIMATE
  ) |>

  # Year #1 here is the base year at 4/1/2020
  dplyr::filter(YEAR != 1) |> # 7/1/2020 - 7/1/2025 pop estimates

  # Add 2018 to each year so that 2 == 2020, 3 == 2021, 4 == 2022, 5 == 2023,
  # and 6 == 2024, and so on, which is the intended meaning
  dplyr::mutate(YEAR = 2018 + YEAR) |>
  dplyr::rename(Year = YEAR) |>
  tidyr::pivot_longer(
    cols = AGE04_TOT:AGE85PLUS_TOT,
    names_to = "Age_Group",
    values_to = "County_Age_Population"
  ) |>
  dplyr::mutate(
    Age_Group = stringr::str_extract(Age_Group, pattern = "\\d+"),
    Age_Group = dplyr::if_else(
      Age_Group == "85",
      "85+",
      dplyr::if_else(
        nchar(Age_Group) == 2,
        paste0(
          stringr::str_sub(Age_Group, 1, 1),
          "-",
          stringr::str_sub(Age_Group, 2, 2)
        ),
        dplyr::if_else(
          nchar(Age_Group) == 4,
          paste0(
            stringr::str_sub(Age_Group, 1, 2),
            "-",
            stringr::str_sub(Age_Group, 3, 4)
          ),
          "Missing"
        )
      )
    ),
    CTYNAME = stringr::str_remove_all(CTYNAME, pattern = "(?:\\sCounty)*")
  ) |>
  dplyr::rename(County = CTYNAME, County_Population = POPESTIMATE) |>
  dplyr::relocate(County_Population, .after = County_Age_Population)

###_____________________________________________________________________________
# age group populations for Iowa at the state (not county) level ----
# work with the sc-est[year]-agesex-civ.csv files for this via
# https://www2.census.gov/programs-surveys/popest/datasets/2020-2025/state/asrh/
###_____________________________________________________________________________

# Iowa age groups at the state level for 2018-2022
# clean Iowa age group populations
# these are NOT standard populations
us_state_age_pops <- readr::read_csv(file = iowa_state_age_pops_path)

# get Iowa state age populations
state_age_group_pops <- us_state_age_pops |>

  # Iowa == State #19, and SEX == 0 is for totals not sex-specific
  dplyr::filter(STATE == 19, SEX == 0) |>
  dplyr::select(
    -c(SUMLEV, REGION, DIVISION, STATE, NAME, SEX, ESTBASE2020_CIV)
  ) |>
  tidyr::pivot_longer(
    cols = -AGE,
    names_to = "Year",
    values_to = "Population"
  ) |>
  dplyr::mutate(
    Year = as.numeric(stringr::str_extract(string = Year, pattern = "\\d+")),
    Age_Group = dplyr::if_else(
      AGE < 5,
      "0-4",
      dplyr::if_else(
        AGE >= 5 & AGE < 10,
        "5-9",
        dplyr::if_else(
          AGE >= 10 & AGE < 15,
          "10-14",
          dplyr::if_else(
            AGE >= 15 & AGE < 20,
            "15-19",
            dplyr::if_else(
              AGE >= 20 & AGE < 25,
              "20-24",
              dplyr::if_else(
                AGE >= 25 & AGE < 30,
                "25-29",
                dplyr::if_else(
                  AGE >= 30 & AGE < 35,
                  "30-34",
                  dplyr::if_else(
                    AGE >= 35 & AGE < 40,
                    "35-39",
                    dplyr::if_else(
                      AGE >= 40 & AGE < 45,
                      "40-44",
                      dplyr::if_else(
                        AGE >= 45 & AGE < 50,
                        "45-49",
                        dplyr::if_else(
                          AGE >= 50 & AGE < 55,
                          "50-54",
                          dplyr::if_else(
                            AGE >= 55 & AGE < 60,
                            "55-59",
                            dplyr::if_else(
                              AGE >= 60 & AGE < 65,
                              "60-64",
                              dplyr::if_else(
                                AGE >= 65 & AGE < 70,
                                "65-69",
                                dplyr::if_else(
                                  AGE >= 70 & AGE < 75,
                                  "70-74",
                                  dplyr::if_else(
                                    AGE >= 75 & AGE < 80,
                                    "75-79",
                                    dplyr::if_else(
                                      AGE >= 80 & AGE < 85,
                                      "80-84",
                                      dplyr::if_else(
                                        AGE >= 85 & AGE < 999,
                                        "85+",
                                        dplyr::if_else(
                                          AGE >= 999,
                                          "Total",
                                          "Error",
                                          missing = "Error"
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    Age_Group = factor(
      Age_Group,
      levels = c(
        "0-4",
        "5-9",
        "10-14",
        "15-19",
        "20-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50-54",
        "55-59",
        "60-64",
        "65-69",
        "70-74",
        "75-79",
        "80-84",
        "85+",
        "Total",
        "Error"
      )
    )
  ) |>
  dplyr::select(Year, Age_Group, Population) |>
  dplyr::summarize(
    State_Population = sum(Population, na.rm = TRUE),
    .by = c(Year, Age_Group)
  )

# standard US populations
us_age_pops <- readr::read_tsv(
  file = "https://seer.cancer.gov/stdpopulations/stdpop.18ages.txt",
  col_names = FALSE
)

# clean US standard populations
us_age_pops_clean <- us_age_pops |>
  dplyr::filter(grepl(pattern = "^204", x = X1)) |>
  dplyr::mutate(
    Age_Group = stringr::str_sub(X1, start = 4, end = 6),
    Population = stringr::str_sub(X1, start = 7, end = 14),
    Population = as.numeric(stringr::str_remove(Population, pattern = "^0")),
    Weight = Population / sum(Population)
  ) |>
  dplyr::select(-X1) |>
  dplyr::mutate(
    Age_Group = c(
      "0-4",
      "5-9",
      "10-14",
      "15-19",
      "20-24",
      "25-29",
      "30-34",
      "35-39",
      "40-44",
      "45-49",
      "50-54",
      "55-59",
      "60-64",
      "65-69",
      "70-74",
      "75-79",
      "80-84",
      "85+"
    ),
    Age_Group = factor(
      Age_Group,
      levels = c(
        "0-4",
        "5-9",
        "10-14",
        "15-19",
        "20-24",
        "25-29",
        "30-34",
        "35-39",
        "40-44",
        "45-49",
        "50-54",
        "55-59",
        "60-64",
        "65-69",
        "70-74",
        "75-79",
        "80-84",
        "85+"
      )
    )
  ) |>
  dplyr::rename(US_Population = Population)
