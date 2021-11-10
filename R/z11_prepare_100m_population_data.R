#' Prepare 100m population data
#' 
#' @importFrom DBI dbCreateTable
#' @importFrom readr read_delim_chunked cols_only col_character col_integer
#' @noRd
prepare_100m_pop_data_db <- function(data_location, file) {
  # If table already exists and user doesn't want to overwrite it,
  # end execution of function
  if (table_exists(data_location, "einwohner100m")) {
    return(invisible(FALSE))
  }
  
  #Create table
  dbCreateTable(data_location, "einwohner100m", 
    fields = c("Gitter_ID_100m" = "TEXT", "Einwohner_100m" = "INTEGER")
  )
  
  message("Writing table einwohner100m to database...")
  read_delim_chunked(file,
                     delim = ";",
                     locale = readr_locale,
                     trim_ws = TRUE,
                     chunk_size = 10000,
                     callback = append_inh_to_sql(data_location, "einwohner100m"),
                     col_types = cols_only(Gitter_ID_100m = col_character(),
                                           Einwohner = col_integer()
                     )
  )
}

#' @importFrom data.table fread setnames
#' @noRd
prepare_100m_pop_data_r <- function(data_location, file) {
  message("Reading file bevoelkerung100m...")
  df <- data.table::fread(file, encoding = "Latin-1", dec = ",",
                          drop = c("x_mp_100m", "y_mp_100m"))
  
  #Create subdirectory if it doesn't exist
  if (!dir.exists(file.path(data_location, "100m"))) {
    dir.create(file.path(data_location, "100m"))
  }
  
  message("    Filter and rename data...")
  df <- df[Einwohner != -1]
  data.table::setnames(df, old = "Einwohner", new = "Einwohner_100m")
  
  message("    Save data...")
  saveRDS(df, file = file.path(data_location, "100m", "Einwohner_100m.rds"))
}