#' Prepare 100m data
#'
#' @importFrom DBI dbExistsTable dbRemoveTable dbCreateTable dbGetQuery dbExecute
#' @importFrom readr read_delim_chunked cols_only col_character col_integer
#' @noRd
prepare_100m_data_db <- function(data_location, file, table_name) {
  # End execution of function if table already exists and
  # user doesn't want to overwrite it
  if (table_exists(data_location, table_name)) {
    return(invisible(data_location))
  }

  # Create temporary table
  if (dbExistsTable(data_location, "temp")) {
    dbRemoveTable(data_location, "temp")
  }
  dbCreateTable(data_location, "temp", temporary = TRUE,
    fields = c("Gitter_ID_100m" = "TEXT", "Variable" = "TEXT", "Anzahl" = "INTEGER")
  )

  # Set delimiter
  delim <- if (table_name == "demographie100m") ";" else ","
  prefix <- toupper(substring(table_name, 1, 3))

  message(sprintf("Writing table %s to database...", table_name))
  read_delim_chunked(file,
     delim = delim,
     locale = readr_locale,
     trim_ws = TRUE,
     chunk_size = 10000,
     callback = append_to_sql(data_location, "temp", prefix),
     col_types = cols_only(Gitter_ID_100m = col_character(),
                           Merkmal = col_character(),
                           Auspraegung_Code = col_integer(),
                           Anzahl = col_integer()
     )
  )

  # Get list of variables in dataset
  vars <- dbGetQuery(data_location, 'SELECT DISTINCT "Variable" FROM temp;') %>%
    unlist()

  # Generate statement to transform to wide format
  var_subq <- vapply(vars, function(x)
    sprintf("MAX(CASE WHEN \"Variable\" = '%s' THEN \"Anzahl\" ELSE NULL END) \"%s\"", x, x),
    character(1)
  )
  statement <- sprintf('CREATE TABLE %s AS
  SELECT "Gitter_ID_100m", %s
  FROM temp
  GROUP BY "Gitter_ID_100m"
  ORDER BY "Gitter_ID_100m";',
    table_name,
    paste(var_subq, collapse = ",\n")
  )

  message("    Transform to wide format...")
  dbExecute(data_location, statement)
}

#' @importFrom data.table fread setorder setnames
#' @importFrom purrr walk
#' @noRd
prepare_100m_data_r <- function(data_location, file, table_name) {
  message(sprintf("Reading file %s...", table_name))
  df <- data.table::fread(file, encoding = "Latin-1", dec = ",",
    select = c("Gitter_ID_100m", "Merkmal", "Auspraegung_Code", "Anzahl"))

  #Create subdirectory if it doesn't exist
  if (!dir.exists(file.path(data_location, "100m"))) {
    dir.create(file.path(data_location, "100m"))
  }

  #Prefix for variable names ("GEB", "FAM", etc.)
  prefix <- substring(table_name, 1, 3) %>% toupper()

  #Revome trailing whitespaces, paste Merkmal and Auspraegung_Code columns together
  message("Transform and save data...")
  df <- df[, merkm := paste(prefix, trimws(Merkmal), Auspraegung_Code, sep = "_")]
  data.table::setorder(df, merkm)

  purrr::walk(unique(df$merkm),
    ~df[merkm == .x, .(Gitter_ID_100m, Anzahl)][
      , data.table::setnames(.SD, new = c("Gitter_ID_100m", .x))] %>%
      saveRDS(file = file.path(data_location, "100m", paste0(.x, ".rds")))
  )
}
