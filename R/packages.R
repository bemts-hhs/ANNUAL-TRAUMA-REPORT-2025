# Utilize the air package for code formatting by default in Positron
# renv is loaded by Positron if you elect to do so (that was done here)
# renv is initialized by Positron if you elect to do so (that was done here)

## packages ----

# install pak
install.packages("pak")

# these packages are utilized in this project and must be installed ----
pak::pak(c(
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
))
