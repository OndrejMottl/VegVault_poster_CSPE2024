#----------------------------------------------------------#
#
#
#                VegVault poster CSPE2024
#
#                     Plot example 1
#
#
#                       O. Mottl
#                         2024
#
#----------------------------------------------------------#

# Plotting spatiotemporal patterns of the Picea genus across North America
#   for modern data and since the Last Glacial Maximum

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

source(
  here::here("R/00_Config_file.R")
)


#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_altitude <-
  get_altitude_data(
    countires_vec = c("CAN", "USA", "MEX"),
    sel_path = here::here("Data/Input/Terrain/"),
    aggregate_fact = 10,
    dowload_de_novo = FALSE
  ) %>%
  tidyr::drop_na() %>%
  dplyr::filter(alt >= 0)

data_na_plots_picea <-
  # Access the VegVault
  vaultkeepr::open_vault(
    path = path_to_vegvault # [config]
  ) %>%
  # Start by adding dataset information
  vaultkeepr::get_datasets() %>%
  # Select both modern and paleo plot data
  vaultkeepr::select_dataset_by_type(
    sel_dataset_type = c(
      "vegetation_plot",
      "fossil_pollen_archive"
    )
  ) %>%
  # Limit data to North America
  vaultkeepr::select_dataset_by_geo(
    lat_lim = c(22, 60),
    long_lim = c(-135, -60)
  ) %>%
  # Add samples
  vaultkeepr::get_samples() %>%
  # Limit the samples by age
  vaultkeepr::select_samples_by_age(
    age_lim = c(0, 12e3)
  ) %>%
  # Add taxa & classify all data to a genus level
  vaultkeepr::get_taxa(classify_to = "genus") %>%
  # Extract only Picea data
  vaultkeepr::select_taxa_by_name(sel_taxa = "Picea") %>%
  vaultkeepr::extract_data()


#----------------------------------------------------------#
# 2. build figure -----
#----------------------------------------------------------#

time_step <- 2500

data_to_plot <-
  data_na_plots_picea %>%
  dplyr::filter(value > 0) %>%
  dplyr::distinct(
    dataset_type, dataset_id, coord_long, coord_lat, sample_id, age
  ) %>%
  dplyr::mutate(
    age_bin = floor(age / time_step) * time_step
  ) %>%
  dplyr::mutate(
    age_bin_class = dplyr::case_when(
      .default = paste("paleo:", age_bin, "cal yr BP"),
      dataset_type == "vegetation_plot" ~ "modern",
    ),
    age_bin_class = factor(age_bin_class,
      levels = c(
        "modern",
        paste(
          "paleo:",
          seq(0, 10e3, time_step),
          "cal yr BP"
        )
      )
    )
  )

fig_na_plots_picea <-
  data_to_plot %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = coord_long,
      y = coord_lat,
    )
  ) +
  ggplot2::coord_quickmap(
    xlim = c(-135, -60),
    ylim = c(22, 60)
  ) +
  ggplot2::scale_fill_gradient2(
    low = col_bronw_light, # [config]
    mid = col_beige_dark, # [config]
    high = "white",
    midpoint = 5.8,
    guide = "none"
  ) +
  ggplot2::labs(
    subtitle =  "Each point is a presence of the Picea genus at a given time",
    x = "Latitude",
    y = "Longitude"
  ) +
  ggplot2::borders(
    fill = col_bronw_light, # [config]
    col = NA
  ) +
  ggplot2::geom_raster(
    data = data_altitude,
    mapping = ggplot2::aes(
      x = long,
      y = lat,
      fill = log1p(alt)
    ),
    alpha = 1
  ) +
  ggplot2::borders(
    fill = NA,
    col = col_blue_dark, # [config]
    size = line_size # [config]
  ) +
  ggplot2::geom_point(
    size = point_size, # [config]
    col = col_blue_light # [config]
  ) +
  ggplot2::geom_point(
    size = 0.1,
    col = col_green_light # [config]
  ) +
  ggplot2::facet_wrap(
    ~age_bin_class
  )


ggplot2::ggsave(
  filename = here::here(
    "Outputs/Figures/fig_na_plots_picea.png"
  ),
  plot = fig_na_plots_picea,
  width = image_width, # [config]
  height = image_height, # [config]
  units = image_units # [config]
)
