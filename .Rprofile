source("renv/activate.R")

# install pak ----
if ("pak" %notin% installed.packages()) {
  install.packages("pak")
  message("`pak` is now attached!")
} else {
  message("`pak` was already attached.")
}

# install renv ----
if ("renv" %notin% installed.packages()) {
  pak::pak("renv")
  message("`renv` is now attached!")
} else {
  message("`renv` was already attached.")
}

# install cli ----
if ("cli" %notin% installed.packages()) {
  pak::pak("cli")
  message("`cli` is now attached!")
} else {
  message("`cli` was already attached.")
}

# messaging about package installation ----
cli::cli_h1(
  "Package Installation"
)

cli::cli_alert_info(
  "Ensure that all required packages are attached."
)

# dynamically attach packages as needed ----
source("./R/packages.R")

# new section ----
cli::cli_text("\n")
cli::rule()
cli::cli_text("\n")

# get environment variables section ----
cli::cli_h1(
  "Load Environment variables"
)

cli::cli_alert_info(
  "Environment variables loaded into memory to make data ingestion secure."
)

# get environment variable paths into memory ----
source("./R/environment_variables.R")

# new section ----
cli::cli_text("\n")
cli::rule()
cli::cli_text("\n")

# project setup section ----
cli::cli_h1(
  "Project Setup"
)

cli::cli_alert_info(
  "Custom functions and utilities."
)

# run the project setup script
source("./R/setup.R")

# new section ----
cli::cli_text("\n")
cli::rule()
cli::cli_text("\n")

# project setup section ----
cli::cli_h1(
  "Helper files and data for utilities"
)

cli::cli_alert_info(
  "Load custom data for different utilities."
)

# get helper files into memory
source("./R/helpers.R")

# new section ----
cli::cli_text("\n")
cli::rule()
cli::cli_text("\n")

# project setup section ----
cli::cli_h1(
  "US Census Bureau Data"
)

cli::cli_alert_info(
  "Load US Census Bureau data to assist with rate calculations"
)
# load census bureau data
source("./R/census_bureau_data.R")

# new section ----
cli::cli_text("\n")
cli::rule()
cli::cli_text("\n")

# project setup section ----
cli::cli_h1(
  "Mortality data"
)

cli::cli_alert_info(
  "Load data on traumatic deaths from Iowa HHS Bureau of Health Statistics and CDC"
)

# load death data
source("./R/death_data.R")
