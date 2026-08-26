# Utilize the air package for code formatting by default in Positron
# renv is loaded by Positron if you elect to do so (that was done here)
# renv is initialized by Positron if you elect to do so (that was done here)

## packages ----

# install pak
if (!require("pak")) {
  install.packages("pak")
}

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
  lapply(missing_packages, pak::pak, character.only = TRUE)
} else {
  message("All required packages are attached.")
}
