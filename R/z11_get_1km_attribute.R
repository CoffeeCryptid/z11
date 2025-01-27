#' Retrieve 1km^2 attribute of the German Census 2011
#'
#' This function retrieves an attribute with a raster resolution of 1km². You
#' can either choose to have it converted directly into the raster format
#' (default) or you can return it as a \code{sf} tibble object with point
#' geometries.
#'
#' @param attribute attribute name as definded in
#' \code{z11::z11_list_1km_attributes}
#' @param data_source Either a DBI connection or a character string containing
#' a file path to the downloaded census data https://github.com/StefanJuenger/z11data;
#' default is NULL - data are downloaded from the internet
#' @param geometry logical; should coordinates be extracted from the INSPIRE ID?
#' @param as_raster logical; shall the attribute be returned as raster or sf
#' object
#'
#' @return Raster or sf
#'
#' @importFrom magrittr %>%
#' @importFrom sf st_as_sf
#' @importFrom stars st_rasterize
#' @importFrom dplyr bind_cols
#' @importFrom DBI dbSendQuery dbFetch dbClearResult
#' @importFrom methods as
#'
#' @export

#Generic Function
z11_get_1km_attribute <- function(
  attribute, data_source, geometry = TRUE, as_raster = TRUE
) UseMethod("z11_get_1km_attribute", data_source)

# DBIConnection Method
#' @rdname z11_get_1km_attribute
#' @export
z11_get_1km_attribute.DBIConnection <-
  function(attribute, data_source, geometry = TRUE, as_raster = TRUE) {
    # Get attribute from database
    message("Fetch attribute from database...")
    tab <- ifelse(grepl("\\_cat$", attribute), "klassiert1km", "spitz1km")
    query <- sprintf('SELECT "Gitter_ID_1km", "%s" FROM %s;', attribute, tab)
    res <- DBI::dbSendQuery(data_source, query)
    requested_attribute <- DBI::dbFetch(res)
    DBI::dbClearResult(res)

    # Extract coordinates from inspire ID
    if (isTRUE(geometry)) {
      requested_attribute <- requested_attribute %>%
        dplyr::bind_cols(., z11_extract_inspire_coordinates(.$Gitter_ID_1km)) %>%
        sf::st_as_sf(coords = c("X", "Y"), crs = 3035)

      #Transform to raster
      if (isTRUE(as_raster)) {
        requested_attribute <- stars::st_rasterize(requested_attribute, dx = 1000, dy = 1000) %>%
          methods::as("Raster")
      }
    }

    return(requested_attribute)
}

#Default Method
#' @rdname z11_get_1km_attribute
#' @export
z11_get_1km_attribute.default <-
  function(attribute, data_source, geometry = TRUE, as_raster = TRUE) {

    # Load data in session
    if (is.null(data_source)) {
      requested_attribute <-
        paste0(
          "https://github.com/StefanJuenger/z11data/raw/main/1km/",
          attribute,
          ".rds"
        ) %>%
        url("rb") %>%
        readRDS()
    } else {
      requested_attribute <- paste0(attribute, ".rds") %>%
        file.path(data_source, "1km", .) %>%
        readRDS()
    }

    # Extract coordinates from inspire ID
    if (isTRUE(geometry)) {
      requested_attribute <- requested_attribute %>%
        dplyr::bind_cols(., z11_extract_inspire_coordinates(.$Gitter_ID_1km)) %>%
        sf::st_as_sf(coords = c("X", "Y"), crs = 3035)

      #Transform to raster
      if (isTRUE(as_raster)) {
        requested_attribute <- stars::st_rasterize(requested_attribute, dx = 1000, dy = 1000) %>%
          methods::as("Raster")
      }
    }

    return(requested_attribute)
}

utils::globalVariables(c("."))
