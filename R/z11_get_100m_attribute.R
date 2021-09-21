#' Retrieve 1 hectare attribute of the German Census 2011
#'
#' This function retrieves an attribute with a raster resolution of 1 hectare.
#' You can either choose to have it converted directly into the raster format
#' (default) or you can return it as a `sf` tibble object with point
#' geometries.
#'
#' @param attribute attribute name as definded in
#' \code{z11::z11_list_100m_attributes}
#' @param as_raster logical; shall the attribute be returned as raster or sf
#' object
#' @param data_source character string; location of the downloaded census data
#' from https://github.com/StefanJuenger/z11data; default is NULL - data are
#' downloaded from the internet
#'
#' @return Raster or sf
#'
#' @importFrom magrittr %>%
#' @importFrom sf st_as_sf
#' @importFrom stars st_rasterize
#' @importFrom dplyr bind_cols
#'
#' @export

# Generic function
setGeneric("z11_get_100m_attribute",
           function(attribute, data_source, geometry = TRUE, as_raster = TRUE) {
             standardGeneric("z11_get_100m_attribute")
           }
)

# Method for DBI Connection
setMethod("z11_get_100m_attribute",
  signature(data_source = "DBIConnection"),
  function(attribute, data_source, geometry = TRUE, as_raster = TRUE) {
    # Get attribute from database
    message("Fetch attribute from database...")
    table <- switch(substring(attribute, 1, 3),
                    Ein = "bevoelkerung100m", DEM = "demographie100m", HAU = "haushalte100m",
                    FAM = "familien100m", GEB = "gebaeude100m", WOH = "wohnungen100m")
    query <- sprintf('SELECT "Gitter_ID_100m", "%s" FROM %s WHERE "%s" IS NOT NULL;', attribute, table, attribute)
    res <- DBI::dbSendQuery(con, query)
    requested_attribute <- DBI::dbFetch(res)
    DBI::dbClearResult(res)

    # Extract coordinates from inspire ID
    message("Extract coordinates from inspire ID...")
    requested_attribute <- requested_attribute %>%
      dplyr::bind_cols(
        z11_extract_inspire_coordinates(.$Gitter_ID_100m)
      ) %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = 3035)

    #Transform to raster
    if (isTRUE(as_raster)) {
      message("Transform to raster...")
      requested_attribute <- requested_attribute %>%
        stars::st_rasterize(dx = 100, dy = 100) %>%
        as("Raster")
    }

    return(requested_attribute)
  }
)

# Method for any class
setMethod("z11_get_100m_attribute",
  signature(data_source = "ANY"),
  function(attribute, data_source, geometry = TRUE, as_raster = TRUE) {

    # Load data in session
    if (is.null(data_source)) {
    requested_attribute <-
      paste0(
        "https://github.com/StefanJuenger/z11data/raw/main/100m/",
        attribute,
        ".rds"
      ) %>%
      url("rb") %>%
      readRDS()
    } else {
      requested_attribute <-
        file.path(data_source, "/100m/", paste0(attribute, ".rds")) %>%
        readRDS()
    }

    # Extract coordinates from inspire ID
    if (isTRUE(geometry)) {
      requested_attribute <- requested_attribute %>%
        dplyr::bind_cols(
          z11_extract_inspire_coordinates(.$Gitter_ID_100m)
        ) %>%
        sf::st_as_sf(coords = c("X", "Y"), crs = 3035)

      #Transform to raster
      if (isTRUE(as_raster)) {
        requested_attribute <- requested_attribute %>%
          stars::st_rasterize(dx = 100, dy = 100) %>%
          as("Raster")
      }
    }

    return(requested_attribute)
  }
)


