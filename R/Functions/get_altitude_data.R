get_altitude_data <- function(
    countires_vec,
    sel_path = here::here("Data/Input/Terrain/"),
    aggregate_fact = 1,
    dowload_de_novo = FALSE) {
  `%>%` <- magrittr::`%>%`

  # get raster data for all countries and save them
  # !!! This takes time once !!!
  purrr::walk(
    .progress = "getting raster data",
    .x = countires_vec,
    .f = ~ {
      sel_county <- .x

      # skip if alredy downloaded
      if (
        isFALSE(dowload_de_novo) &&
          list.files(
            sel_path
          ) %>%
            stringr::str_detect(., sel_county) %>%
            any()
      ) {
        return()
      }

      # get the altitude data
      ras <-
        geodata::elevation_30s(
          country = sel_county,
          path = sel_path,
          mask = FALSE
        )

      # some countries has more than have more than 1 raster files.
      #   Loop through the list and save each individualy
      if (
        is.list(ras)
      ) {
        purrr::walk(
          .x = ras,
          .f = ~ try(
            terra::writeRaster(
              .x,
              filename = paste0(
                paste0(
                  sel_path, "/",
                  names(.x) %>%
                    stringr::str_extract(., paste0(eval(sel_county), ".*")) %>%
                    stringr::str_replace(., ".grd", ""),
                  ".tif"
                )
              ),
              overwrite = dowload_de_novo
            ),
            silent = TRUE
          )
        )
      }

      try(
        terra::writeRaster(
          ras,
          filename = paste0(
            sel_path,
            "/",
            names(ras) %>%
              stringr::str_extract(., paste0(eval(sel_county), ".*")) %>%
              stringr::str_replace(., ".grd", ""),
            ".tif"
          ),
          overwrite = dowload_de_novo
        ),
        silent = TRUE
      )
    }
  )

  #----------------------------------------------------------#
  # 3. Merge and lower quality  -----
  #----------------------------------------------------------#

  # Create list with all exported raster files
  ras_lst <-
    countires_vec %>%
    purrr::map(
      .f = ~ list.files(
        sel_path,
        full.names = TRUE,
        pattern = ".tif"
      ) %>%
        stringr::str_subset(pattern = "aux", negate = TRUE) %>%
        stringr::str_subset(pattern = .x)
    ) %>%
    unlist() %>%
    unique()


  assertthat::assert_that(
    length(countires_vec) == length(ras_lst),
    msg = "There are diffent number of rasters than the expected countries"
  )


  list_rast <-
    purrr::map(
      .progress = "loading rasters",
      .x = ras_lst,
      .f = ~ terra::rast(.x)
    )

  if (
    aggregate_fact > 1
  ) {
    list_rast <-
      list_rast %>%
      purrr::map(
        .progress = "lowering resolution",
        .f = ~ terra::aggregate(.x, fact = aggregate_fact)
      )
  }

  # merge all raster together
  alt_raster <-
    purrr::reduce(
      .x = list_rast,
      .f = ~ terra::merge(.x, .y)
    )

  # turn into data.frame
  data_altitude_raw <-
    terra::crds(alt_raster) %>%
    data.frame() %>%
    tibble::as_tibble() %>%
    dplyr::bind_cols(
      terra::values(alt_raster, na.rm = TRUE)
    ) %>%
    purrr::set_names(nm = c("long", "lat", "alt"))

  return(data_altitude_raw)
}
