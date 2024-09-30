#----------------------------------------------------------#
#
#
#                VegVault poster CSPE2024
#
#                     Plot example 3
#
#
#                       O. Mottl
#                         2024
#
#----------------------------------------------------------#

# Plotting data temporal trends of  CWM for Latin ame.

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

data_la_traits <-
  # Acess the VegVault file
  vaultkeepr::open_vault(
    path = path_to_vegvault # [config]
  ) %>%
  # Add the dataset information
  vaultkeepr::get_datasets() %>%
  # Select modern plot data and climate
  vaultkeepr::select_dataset_by_type(
    sel_dataset_type = c(
      "fossil_pollen_archive",
      "traits"
    )
  ) %>%
  # Limit data to South and Central America
  vaultkeepr::select_dataset_by_geo(
    lat_lim = c(-53, 28),
    long_lim = c(-110, -38),
    sel_dataset_type = c(
      "fossil_pollen_archive",
      "traits"
    )
  ) %>%
  # Add samples
  vaultkeepr::get_samples() %>%
  # Limit to 6-12 ka yr BP
  vaultkeepr::select_samples_by_age(
    age_lim = c(6e3, 12e3)
  ) %>%
  # add taxa & clasify all data to a genus level
  vaultkeepr::get_taxa(classify_to = "genus") %>%
  # add trait information & clasify all data to a genus level
  vaultkeepr::get_traits(classify_to = "genus") %>%
  # Only select the plant height
  vaultkeepr::select_traits_by_domain_name(sel_domain = "Plant heigh") %>%
  vaultkeepr::extract_data()


#----------------------------------------------------------#
# 2. Build figure -----
#----------------------------------------------------------#

data_la_datasets <-
  data_la_traits %>%
  dplyr::filter(dataset_type == "fossil_pollen_archive") %>%
  dplyr::distinct(dataset_id, sample_id, age) %>%
  dplyr::group_by(dataset_id) %>%
  dplyr::summarise(
    .groups = "drop",
    age_min = min(age),
    age_mean = mean(age),
    age_max = max(age)
  )

data_la_taxa <-
  data_la_traits %>%
  dplyr::filter(dataset_type == "fossil_pollen_archive") %>%
  dplyr::distinct(dataset_id, sample_id, taxon_id) %>%
  dplyr::group_by(dataset_id, sample_id) %>%
  dplyr::count() %>%
  dplyr::ungroup()


data_la_height <-
  data_la_traits %>%
  dplyr::filter(dataset_type == "traits") %>%
  dplyr::distinct(dataset_id, taxon_id_trait, .keep_all = TRUE) %>%
  tidyr::drop_na(taxon_id_trait) %>%
  dplyr::select(taxon_id_trait, trait_value) %>%
  dplyr::group_by(taxon_id_trait) %>%
  dplyr::summarise(
    mean_value = mean(trait_value)
  ) %>%
  tidyr::drop_na(mean_value)

fig_la_datasets <-
  data_la_datasets %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      y = reorder(dataset_id, -age_mean),
      yend = reorder(dataset_id, -age_mean),
      x = age_min,
      xend = age_max
    )
  ) +
  ggplot2::geom_segment(
    linewidth = 2,
    col = col_blue_dark
  ) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = age_min
    ),
    col = col_green_light,
    size = point_size
  ) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(
      x = age_max
    ),
    col = col_green_dark,
    size = point_size
  ) +
  ggplot2::theme(
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.position = "none",
    panel.grid.major.y = ggplot2::element_blank()
  ) +
  ggplot2::scale_x_continuous(
    transform = "reverse",
    breaks = seq(12e3, 6e3, -2e3),
    labels = seq(12, 6, -2)
  ) +
  ggplot2::labs(
    x = "Age (ka cal yr BP)",
    y = "Records"
  ) +
  ggplot2::coord_cartesian(
    #  xlim = c(min(limits), max(limits))
  )

fig_la_taxa <-
  data_la_taxa %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = 1,
      y = n
    )
  ) +
  ggplot2::geom_violin(
    fill = col_blue_light,
    col = col_blue_dark
  ) +
  ggplot2::geom_boxplot(
    width = 0.2,
    outlier.shape = NA,
    fill = col_white,
    col = col_blue_dark
  ) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.position = "none"
  ) +
  ggplot2::labs(
    y = "N genera per sample"
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::label_number(),
    breaks = c(1, 5, 10, 20, 30, 50, 70),
    limits = c(0, 60)
  )

fig_la_height <-
  data_la_height %>%
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = 1,
      y = mean_value
    )
  ) +
  ggplot2::geom_violin(
    fill = col_green_light,
    col = col_green_dark
  ) +
  ggplot2::geom_boxplot(
    width = 0.2,
    outlier.shape = NA,
    fill = col_white,
    col = col_green_dark
  ) +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.position = "none"
  ) +
  ggplot2::labs(
    y = "Average Plant heigh per genera"
  ) +
  ggplot2::scale_y_continuous(
    transform = scales::transform_pseudo_log(),
    labels = scales::label_number(),
    breaks = c(0, 1, 5, 10, 20, 30, 50, 70),
    limits = c(0, 80)
  )

fig_la_merge <-
  cowplot::plot_grid(
    fig_la_datasets,
    fig_la_taxa,
    fig_la_height,
    nrow = 1,
    align = "h",
    axis = "bt",
    rel_widths = c(2, 1, 1)
  )

ggplot2::ggsave(
  filename = here::here(
    "Outputs/Figures/fig_la_cwm.png"
  ),
  plot = fig_la_merge,
  width = image_width, # [config]
  height = image_height, # [config]
  units = image_units # [config]
)

