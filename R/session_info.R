###___________________________________________________________________________
# Export session info to the outputs folder
###___________________________________________________________________________

# use sessioninfo::platform_info() ----
session_data <- sessioninfo::platform_info() |>
  as.data.frame() |>
  tidyr::pivot_longer(
    cols = tidyselect::everything(),
    names_to = "setting",
    values_to = "value"
  )

# save the data to disk ----
readr::write_csv(
  x = session_data,
  file = paste0(output_folder, "/session_data.csv")
)
