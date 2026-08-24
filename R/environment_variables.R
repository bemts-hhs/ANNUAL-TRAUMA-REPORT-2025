### Get environment variables ----

# paths for outputs
plot_folder <- Sys.getenv("plot_path")
output_folder <- Sys.getenv("output_path")
death_path <- Sys.getenv("death_folder")

# clinical data

# trauma environment variables ----
trauma_data_path_2021 <- Sys.getenv("trauma_data_2021")
trauma_data_path_2022 <- Sys.getenv("trauma_data_2022")
trauma_data_path_2023 <- Sys.getenv("trauma_data_2023")
trauma_data_path_2024 <- Sys.getenv("trauma_data_2024")
trauma_data_path_2025 <- Sys.getenv("trauma_data_2025")

# ems environment variables ----
ems_data_path <- Sys.getenv("ems_data_folder")

# ipop inpatient environment variables ----
ipop_ip_data_path_2021 <- Sys.getenv("ipop_ip_data_2021")
ipop_ip_data_path_2022 <- Sys.getenv("ipop_ip_data_2022")
ipop_ip_data_path_2023 <- Sys.getenv("ipop_ip_data_2023")
ipop_ip_data_path_2024 <- Sys.getenv("ipop_ip_data_2024")
ipop_ip_data_path_2025 <- Sys.getenv("ipop_ip_data_2025")

# ipop outpatient environment variables ----
ipop_op_data_path_2021 <- Sys.getenv("ipop_op_data_2021")
ipop_op_data_path_2022 <- Sys.getenv("ipop_op_data_2022")
ipop_op_data_path_2023 <- Sys.getenv("ipop_op_data_2023")
ipop_op_data_path_2024 <- Sys.getenv("ipop_op_data_2024")
ipop_op_data_path_2025 <- Sys.getenv("ipop_op_data_2025")

# deaths environment variables ----
# nationwide, all ages environment variables ----
death_us_all_2020_path <- Sys.getenv("death_us_all_2020_folder")
death_us_all_2021_path <- Sys.getenv("death_us_all_2021_folder")
death_us_all_2022_path <- Sys.getenv("death_us_all_2022_folder")
death_us_all_2023_path <- Sys.getenv("death_us_all_2023_folder")
death_us_all_2024_path <- Sys.getenv("death_us_all_2024_folder")
death_us_all_2020_2024_path <- Sys.getenv("death_us_all_2020_2024_folder")

# nationwide, ages 1-44 environment variables ----
death_us_1_44_2020_path <- Sys.getenv("death_us_1_44_2020_folder")
death_us_1_44_2021_path <- Sys.getenv("death_us_1_44_2021_folder")
death_us_1_44_2022_path <- Sys.getenv("death_us_1_44_2022_folder")
death_us_1_44_2023_path <- Sys.getenv("death_us_1_44_2023_folder")
death_us_1_44_2024_path <- Sys.getenv("death_us_1_44_2024_folder")
death_us_1_44_2020_2024_path <- Sys.getenv("death_us_1_44_2020_2024_folder")

# iowa, all ages environment variables ----
death_ia_all_2020_path <- Sys.getenv("death_ia_all_2020_folder")
death_ia_all_2021_path <- Sys.getenv("death_ia_all_2021_folder")
death_ia_all_2022_path <- Sys.getenv("death_ia_all_2022_folder")
death_ia_all_2023_path <- Sys.getenv("death_ia_all_2023_folder")
death_ia_all_2024_path <- Sys.getenv("death_ia_all_2024_folder")
death_ia_all_2020_2024_path <- Sys.getenv("death_ia_all_2020_2024_folder")

# iowa, ages 1-44 environment variables ----
death_ia_1_44_2020_path <- Sys.getenv("death_ia_1_44_2020_folder")
death_ia_1_44_2021_path <- Sys.getenv("death_ia_1_44_2021_folder")
death_ia_1_44_2022_path <- Sys.getenv("death_ia_1_44_2022_folder")
death_ia_1_44_2023_path <- Sys.getenv("death_ia_1_44_2023_folder")
death_ia_1_44_2024_path <- Sys.getenv("death_ia_1_44_2024_folder")
death_ia_1_44_2020_2024_path <- Sys.getenv("death_ia_1_44_2020_2024_folder")

# iowa, health statistics data environment variables ----
deathpop_data_path <- Sys.getenv("deathpop_data_folder")
p15_path <- Sys.getenv("p15_folder")
p16_1_path <- Sys.getenv("p16_1_folder")
p16_2_path <- Sys.getenv("p16_2_folder")
p17_path <- Sys.getenv("p17_folder")
p30_path <- Sys.getenv("p30_folder")
p33_1_path <- Sys.getenv("p33_1_folder")
p33_2_path <- Sys.getenv("p33_2_folder")

# files for classification environment variables ----
mech_injury_path <- Sys.getenv("mech_injury_map")
injury_matrix_path <- Sys.getenv("injury_matrix")
iowa_counties_districts_path <- Sys.getenv("iowa_counties_districts")
hospital_data_path <- Sys.getenv("hospital_data_folder")

# population files environment variables ----
iowa_county_pops_path <- Sys.getenv("IOWA_COUNTY_POPS")
iowa_county_age_pops_path <- Sys.getenv("IOWA_COUNTY_AGE_POPS")
us_standard_pops_path <- Sys.getenv("US_STANDARD_POPS")
iowa_state_age_pops_path <- Sys.getenv("IOWA_STATE_POPS")
