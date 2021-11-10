#' Prepare 1km grid data, database
#'
#' @importFrom readr read_delim
#' @importFrom dplyr rename_with ends_with contains
#' @importFrom DBI dbWriteTable
#' @noRd
prepare_1km_data_db <- function(data_location, file, table_name) {
  # If table already exists and user doesn't want to overwrite it,
  # end execution of function
  if (table_exists(data_location, table_name)) {
    return(invisible(FALSE))
  }

  message("Reading file...")
  df <- readr::read_delim(
    file,
    locale = readr_locale,
    col_select = !ends_with("_mp_1km"),
    show_col_types = FALSE)

  # If this is categorized data, rename all variables
  if (table_name == "klassiert1km") {
    df <- rename_with(df,
                      ~paste(.x, "cat", sep = "_"),
                      !contains("Gitter_ID_1km")
    )
  }

  message(sprintf("    Writing table %s to database...", table_name))
  dbWriteTable(data_location, name = table_name, value = df)
}

prepare_1km_data_r <- function(data_location, file, table_name) {
  message(sprintf("Reading file %s...", table_name))
  df <- data.table::fread(file, encoding = "Latin-1", dec = ",",
                          drop = c("x_mp_1km", "y_mp_1km"))

  #Create subdirectory if it doesn't exist
  if (!dir.exists(file.path(data_location, "1km"))) {
    dir.create(file.path(data_location, "1km"))
  }

  # Select and rename columns
  if (table_name == "klassiert1km") {
    colns <- colnames(df)
    colns <- c(colns[1], paste(colns[2:length(colns)], "cat", sep = "_"))
    setnames(df, colns)
  }

  #Vector of column names to loop over
  colns <- colnames(df)[-1]

  #Select columns, rename and save
  message("Save data...")
  purrr::walk(colns,
    ~df[, .(Gitter_ID_1km, get(.x))][
      , data.table::setnames(.SD, c("Gitter_ID_1km", .x))] %>%
      saveRDS(file = file.path(data_location, "1km", paste0(.x, ".rds")))
  )
}
