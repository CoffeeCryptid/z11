#' Create 1km and 100m INSPIRE IDs
#'
#' Create 1 km² and 100m X 100m INSPIRE IDs from coordinates
#'
#' @param data Object of class ```sf``` containing point geometries
#' @param type Character string for the requested ID type
#' @param column_name Name that the newly created INSPIRE ID column should have,
#' as a character string
#' @param combine Should the inspire ID be appended to the dataset?
#' Defaults to FALSE.
#' @return tibble
#'
#' @importFrom magrittr %>%
#' @importFrom sf st_coordinates st_transform st_crs
#' @importFrom purrr map_dfc
#' @importFrom tibble as_tibble
#'
#' @export

z11_create_inspire_ids <- function(
  data,
  type = c("1km", "100m"),
  column_name = "Gitter_ID_",
  combine = FALSE
) {

  if (sf::st_crs(data)$epsg != 3035) {
    data <- data %>% sf::st_transform(3035)
  }

  coordinate_pairs <-
    data %>%
    sf::st_coordinates() %>%
    tibble::as_tibble()

  id_name <- paste0(column_name, type)

  if (length(type) == 2) {
    names(type) <- id_name
    inspire <- purrr::map_dfc(type, get_inspire_id, coordinate_pairs = coordinate_pairs)
  } else if (length(type) == 1) {
    inspire <- get_inspire_id(type, coordinate_pairs)
  } else {
    stop("Invalid type")
  }

  if (isTRUE(combine)) {
    return(
      dplyr::bind_cols(data, !!id_name[1] := inspire)
    )
  } else {
    return(inspire)
  }
}

get_inspire_id <- function(type, coordinate_pairs) {
  if (type == "1km") {
    sprintf(
      "1kmN%sE%s",
      substr(coordinate_pairs$Y %>% as.character(), 1, 4),
      substr(coordinate_pairs$X %>% as.character(), 1, 4)
    ) %>% as.character()
  } else if (type == "100m") {
    sprintf(
      "100mN%sE%s",
      substr(coordinate_pairs$Y %>% as.character(), 1, 5),
      substr(coordinate_pairs$X %>% as.character(), 1, 5)
    ) %>% as.character()
  } else {
    stop("Not a valid type!")
  }
}

utils::globalVariables(c(":="))
