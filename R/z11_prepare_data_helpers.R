#' Helper functions for z11_prepare_data
#' 
#' 
#' Locale for readr
#' @importFrom readr locale
#' @noRd
readr_locale <- readr::locale(
  encoding = "Latin1",
  decimal_mark = ",",
  grouping_mark = "."
)

#' Processing function for 100m data
#' @importFrom rlang .data
#' @importFrom dplyr mutate select
#' @importFrom DBI dbWriteTable
#' @noRd
append_to_sql <- function(con, table_name, prefix) {
  function(x, pos) {
    x <- as.data.frame(x) %>%
      mutate(Variable = paste(prefix, .data$Merkmal, .data$Auspraegung_Code, sep = "_")) %>%
      select(.data$Gitter_ID_100m, .data$Variable, .data$Anzahl)
    DBI::dbWriteTable(con, name = table_name, value = x, append = TRUE)
  }
}

#' Processing function for 100m inhabitants data
#' @importFrom dplyr filter rename
#' @importFrom rlang .data
#' @importFrom DBI dbWriteTable
#' @noRd
append_inh_to_sql <- function(con, table_name) {
  function(x, pos) {
    x <- as.data.frame(x) %>%
      filter(.data$Einwohner != -1) %>%
      rename(Einwohner_100m = .data$Einwohner)
    DBI::dbWriteTable(con, name = table_name, value = x, append = TRUE)
  }
}

#' Helper function: check if table exists in database,
#' ask if it should be overwritten
#' importFrom DBI dbRemoveTable
#' @noRd
table_exists <- function(con, table_name) {
  if (dbExistsTable(con, table_name)) {
    prompt <- sprintf("Table %s already exists. Overwrite? (y/n): ", table_name)
    user_input <- readline(prompt = prompt) %>% trimws() %>% tolower()
    
    if (user_input != "y") {
      message(sprintf("Not overwriting table %s, aborting.", table_name))
      return(TRUE)
    } else {
      dbRemoveTable(con, table_name)
    }
  }
  return(FALSE)
}