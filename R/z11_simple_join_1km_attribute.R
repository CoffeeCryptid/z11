#' Simple join of data with census 1 km attributes
#'
#' Merge input data with census 1 km data by simple matching of INSPIRE IDs
#'
#' @param df input data
#' @param inspire_column Column name in input data containing the inspire ID
#' @param data_source Either a DBI connection, or a character string containing
#' a file path to the data location
#' @param attributes A character or character vector containing the name of the
#' Census attribute to be merged with the input data.
#' If no attribute name is given, all available attributes will be merged.
#'
#' @examples
#' \dontrun{
#' joined <- z11_simple_join_1km_attribute(df, inspire_1km, con,
#'   c("Frauen_A", "Frauen_A_cat"))
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom data.table data.table setDT setnames
#' @importFrom DBI dbWriteTable dbSendQuery dbFetch dbClearResult
#' @importFrom dplyr select bind_cols
#' @importFrom rlang .data enquo as_label
#'
#' @export

#Generic function

z11_simple_join_1km_attribute <- function(
  df, inspire_column, data_source = NULL, attributes = NULL
  ) UseMethod("z11_simple_join_1km_attribute", data_source)

# Method for DBI Connections
#' @rdname z11_simple_join_1km_attribute
#' @export
z11_simple_join_1km_attribute.DBIConnection <-
  function(df, inspire_column, data_source, attributes = NULL) {
    inspire_column <- rlang::enquo(inspire_column) %>% rlang::as_label()

    message("Prepare for joining...")
    input <- data.frame(Gitter_ID_1km = df[[inspire_column]], order_id = 1:nrow(df))

    DBI::dbWriteTable(data_source, "temp", input, temporary = TRUE, overwrite = TRUE)

    message("Join data...")
    if (is.null(attributes)) {
      #Join all 1km variables
      query <- 'SELECT * from temp
      LEFT JOIN spitz1km USING ("Gitter_ID_1km")
      LEFT JOIN klassiert1km USING ("Gitter_ID_1km")
      ORDER BY order_id;'
    } else {
      #Only join select 1km variables
      tables <- ifelse(grepl("\\_cat$", attributes), "klassiert1km", "spitz1km")
      tables_query <- paste("LEFT JOIN", unique(tables), 'USING ("Gitter_ID_1km")',
                            sep = " ", collapse = "\n")
      vars_query <- paste(attributes, collapse = '", "')
      query <- sprintf('SELECT "Gitter_ID_1km", "%s", "order_id" from temp %s ORDER BY order_id;', vars_query, tables_query)
    }

    res <- DBI::dbSendQuery(data_source, query)
    output <- DBI::dbFetch(res) %>%
      dplyr::select(-.data$Gitter_ID_1km, -.data$order_id)
    DBI::dbClearResult(res)

    message("Done!")
    return(
      dplyr::bind_cols(df, output)
    )
}

# Default method
#' @rdname z11_simple_join_1km_attribute
#' @export
z11_simple_join_1km_attribute.default <-
  function(df, inspire_column, data_source, attributes = NULL) {
    inspire_column <- rlang::enquo(inspire_column) %>% rlang::as_label()

    linked_data <- df

    #All attributes
    if (is.null(attributes)) {attributes <- z11::z11_list_1km_attributes()}

    for (i in attributes) {
      message(sprintf("Joining variable %s...", i))

      attribute <- z11::z11_get_1km_attribute(i, data_source, geometry = FALSE, as_raster = FALSE)
      data.table::setDT(attribute)
      data.table::setnames(attribute, old = "Gitter_ID_1km", new = inspire_column)

      linked_data <- merge(linked_data, attribute, on = inspire_column, all.x = TRUE, sort = FALSE)
    }

    return(linked_data)
}

utils::globalVariables(c("order"))
