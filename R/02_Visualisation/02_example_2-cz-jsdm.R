#----------------------------------------------------------#
#
#
#                VegVault poster CSPE2024
#
#                     Plot example 2
#
#
#                       O. Mottl
#                         2024
#
#----------------------------------------------------------#

# Plotting data for SPD using all plant taxa in the Czech Republic.
#   We will extract modern plot-based data and Mean Annual temprature.

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

data_cz_altitude <-
  get_altitude_data(
    countires_vec = "CZE",
    sel_path = here::here("Data/Input/Terrain/"),
    aggregate_fact = 1,
    dowload_de_novo = FALSE
  )

data_cz_jsdm <-
  # Acess the VegVault file
  vaultkeepr::open_vault(
    path = path_to_vegvault # [config]
  ) %>%
  # Add the dataset information
  vaultkeepr::get_datasets() %>%
  # Select modern plot data and climate
  vaultkeepr::select_dataset_by_type(
    sel_dataset_type = c(
      "vegetation_plot",
      "gridpoints"
    )
  ) %>%
  # Limit data to Czech Republic
  vaultkeepr::select_dataset_by_geo(
    lat_lim = c(48.5, 51.1),
    long_lim = c(12, 18.9)
  ) %>%
  # Add samples
  vaultkeepr::get_samples() %>%
  # select only modern data
  vaultkeepr::select_samples_by_age(
    age_lim = c(0, 0)
  ) %>%
  # Add abiotic data
  vaultkeepr::get_abiotic_data() %>%
  # Select only Mean Anual Temperature (bio1)
  vaultkeepr::select_abiotic_var_by_name(sel_var_name = "bio1") %>%
  # add taxa
  vaultkeepr::get_taxa() %>%
  vaultkeepr::extract_data()


#----------------------------------------------------------#
# 2. build figure -----
#----------------------------------------------------------#

data_cz_climate <-
  data_cz_jsdm %>%
  dplyr::filter(dataset_type == "gridpoints") %>%
  dplyr::select(
    sample_id_link, abiotic_value
  ) %>%
  tidyr::drop_na() %>%
  dplyr::distinct()

data_cz_plots <-
  data_cz_jsdm %>%
  dplyr::filter(dataset_type == "vegetation_plot") %>%
  dplyr::select(
    dataset_id, coord_long, coord_lat, sample_id, taxon_id
  ) %>%
  dplyr::distinct() %>%
  dplyr::group_by(dataset_id, coord_long, coord_lat, sample_id) %>%
  dplyr::summarise(
    .groups = "drop",
    n_taxa = dplyr::n()
  ) %>%
  dplyr::filter(n_taxa > 1)

data_cz_plots_with_climate <-
  data_cz_plots %>%
  dplyr::left_join(
    data_cz_climate,
    by = c("sample_id" = "sample_id_link")
  )

data_cz_altitude_sf <-
  sf::st_as_sf(
    data_cz_altitude,
    coords = c("long", "lat"),
    crs = 4326
  )

poly_cz_border <-
  giscoR::gisco_get_countries(
    country = "CZ",
    resolution = "01"
  )

data_cz_altitude_filter <-
  sf::st_filter(
    data_cz_altitude_sf,
    poly_cz_border
  )

data_cz_altitude_filter_df <-
  sf::st_coordinates(data_cz_altitude_filter) %>%
  as.data.frame() %>%
  tibble::as_tibble() %>%
  rlang::set_names(nm = c("long", "lat")) %>%
  dplyr::mutate(
    alt = data_cz_altitude_filter$alt
  )

fig_cz_jsdm <-
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = coord_long,
      y = coord_lat
    )
  ) +
  ggplot2::coord_quickmap(
    xlim = c(12, 18.9),
    ylim = c(48.5, 51.1)
  ) +
  ggplot2::labs(
    x = "Longtitude",
    y = "Latitude",
    colour = "Temperature (°C)",
    size = "Species richness",
    subtitle = "Each point represents one vegetation plot"
  ) +
  ggplot2::scale_fill_gradient2(
    low = col_bronw_light, # [config]
    mid = col_beige_dark, # [config]
    high = "white",
    midpoint = 5.8,
    guide = "none"
  ) +
  ggplot2::scale_colour_steps(
    low = col_blue_dark, # [config]
    high = col_blue_light # [config]
  ) +
  ggplot2::scale_size_continuous(
    breaks = scales::pretty_breaks(n = 5),
    range = c(
      0.2,
      point_size * 2 # [config]
    )
  ) +
  ggplot2::theme(
    legend.position = "right"
  ) +
  ggplot2::borders(
    fill = col_bronw_light, # [config]
    col = NA
  ) +
  ggplot2::geom_vline(
    xintercept = seq(12, 18, 2),
    linewidth = line_size,
    colour = col_white
  ) +
  ggplot2::geom_hline(
    yintercept = seq(48.5, 51, 0.5),
    linewidth = line_size,
    colour = col_white
  ) +
  ggplot2::geom_raster(
    data = data_cz_altitude_filter_df,
    mapping = ggplot2::aes(
      x = long,
      y = lat,
      fill = log(alt)
    ),
    alpha = 1
  ) +
  ggplot2::borders(
    fill = NA,
    col = col_blue_dark, # [config]
    size = line_size # [config]
  ) +
  ggplot2::geom_point(
    data = data_cz_plots_with_climate,
    mapping = ggplot2::aes(
      x = coord_long,
      y = coord_lat,
      col = abiotic_value,
      size = n_taxa
    )
  ) +
  ggplot2::geom_point(
    data = data_cz_plots_with_climate,
    col = col_green_light, # [config]
    size = 0.1
  )


ggplot2::ggsave(
  filename = here::here(
    "Outputs/Figures/fig_cz_jsdm.png"
  ),
  plot = fig_cz_jsdm,
  width = image_width, # [config]
  height = image_height, # [config]
  units = image_units # [config]
)
