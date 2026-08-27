# Utilize the air package for code formatting by default in Positron
# renv is loaded by Positron if you elect to do so (that was done here)
# renv is initialized by Positron if you elect to do so (that was done here)

## packages ----

# define target packages
required_packages <- c(
  'renv',
  'usethis',
  'devtools',
  'tidyverse',
  'tidymodels',
  'tidytext',
  'traumar',
  'nemsqar',
  'naniar',
  'cli',
  'ggrepel',
  'ggthemes',
  'treemapify',
  'janitor',
  'gt',
  'gtsummary',
  'gtExtras',
  'webshot2',
  'svglite'
)

# these packages are utilized in this project and must be installed ----
if (any(required_packages %notin% installed.packages())) {
  # missing packages
  missing_packages <- required_packages[
    required_packages %notin% installed.packages()
  ]

  # only install those needed
  lapply(missing_packages, pak::pak)

  # print a message to the console reporting that work is done
  print_message <- paste(
    "The following missing packages were attached via `pak::pak()`: ",
    paste(missing_packages, collapse = ", ")
  )
} else {
  # otherwise print a message to the console stating all is well
  message("All required packages are attached.")
}
