#' Simple join of data with census 100m attributes
#'
#' Merge input data with census 100m data by simple matching of INSPIRE IDs
#'
#' @param df input data
#' @param inspire_column Column name in input data containing the inspire ID
#' @param data_source Either a DBI connection, or a character string containing
#' a file path to the data location
#' @param attributes A character or character vector containing the name of the
#' Census attribute to be merged with the input data. If no attribute name is
#' given, all available attributes will be merged.
#'
#' @examples
#' \dontrun{joined <- z11_simple_join_100m_attribute(df, inspire_100m, con)}
#'
#' @importFrom magrittr %>%
#' @importFrom data.table data.table setDT setnames
#' @importFrom DBI dbWriteTable dbSendQuery dbFetch dbClearResult
#' @importFrom dplyr select bind_cols
#' @importFrom rlang .data enquo as_label
#'
#' @export
z11_simple_join_100m_attribute <- function(
  df, inspire_column, data_source = NULL, attributes = NULL
) UseMethod("z11_simple_join_100m_attribute", data_source)

# Method for DBI Connection
#' @rdname z11_simple_join_100m_attribute
#' @export
z11_simple_join_100m_attribute.DBIConnection <-
  function(df, inspire_column, data_source, attributes = NULL) {
    inspire_column <- rlang::enquo(inspire_column) %>% rlang::as_label()

    message("Prepare for joining...")
    input <- data.frame(Gitter_ID_100m = df[[inspire_column]], order_id = 1:nrow(df))

    DBI::dbWriteTable(data_source, "temp", input, temporary = TRUE, overwrite = TRUE)

    message("Join data...")
    if (is.null(attributes)) {
      #Join all 100m variables
      query <- 'SELECT * FROM temp
LEFT JOIN bevoelkerung100m USING ("Gitter_ID_100m")
LEFT JOIN demographie100m USING ("Gitter_ID_100m")
LEFT JOIN haushalte100m USING ("Gitter_ID_100m")
LEFT JOIN familien100m USING ("Gitter_ID_100m")
LEFT JOIN gebaeude100m USING ("Gitter_ID_100m")
LEFT JOIN wohnungen100m USING ("Gitter_ID_100m")
ORDER BY order_id;'
    } else {
      # Only join select 100m variables
      tables <- vapply(substring(attributes, 1, 3), FUN.VALUE =  character(1),
                       function(x) switch(x,
                                          Ein = "bevoelkerung100m",
                                          DEM = "demographie100m",
                                          HAU = "haushalte100m",
                                          FAM = "familien100m",
                                          GEB = "gebaeude100m",
                                          WOH = "wohnungen100m"))

      tables_query <- paste("LEFT JOIN", unique(tables), 'USING ("Gitter_ID_100m")', sep = " ", collapse = "\n")
      vars_query <- paste(attributes, collapse = '", "')
      query <- sprintf('SELECT "Gitter_ID_100m", "%s", "order_id" FROM temp %s ORDER BY order_id;', vars_query, tables_query)
    }

    output <- DBI::dbGetQuery(data_source, query) %>%
      dplyr::select(-.data$Gitter_ID_100m, -.data$order_id)

    message("Done!")
    return(
      dplyr::bind_cols(df, output)
    )
}

#Method for other classes
#' @rdname z11_simple_join_100m_attribute
#' @export
z11_simple_join_100m_attribute.default <-
  function(df, inspire_column, data_source, attributes = NULL) {

    inspire_column <- rlang::enquo(inspire_column) %>% rlang::as_label()

    linked_data <- data.table(df)

    #All attributes
    if (is.null(attributes)) {attributes <- z11::z11_list_100m_attributes()}

    for (i in attributes) {

      message(sprintf("Joining variable %s...", i))

      #Get attribute data
      attribute <- z11::z11_get_100m_attribute(i, data_source, geometry = FALSE, as_raster = FALSE)
      data.table::setDT(attribute)
      data.table::setnames(attribute, old = "Gitter_ID_100m", new = inspire_column)

      #Merge
      linked_data <- merge(linked_data, attribute, on = inspire_column, all.x = TRUE, sort = FALSE)
    }

    return(linked_data)
}

