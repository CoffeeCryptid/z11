#' Data Preparation
#'
#' @description
#' Reads in 2011 Census .csv files, splits it into smaller chunks, and saves it
#' locally.
#'
#' @export
z11_prepare_data <- function(file, data_location) {
  if (dir.exists(file)) {
    #If file path is directory, read in all .csv files from directory
    files <- list.files(file, full.names = TRUE, pattern = "\\.csv$|\\.CSV$")
    for (f in files) {
      z11::z11_prepare_file(f, data_location)
      invisible(gc())
    }

  } else if (file.exists(file)) {
    z11_prepare_file(file, data_location)
  } else {
    stop("Not a valid file or directory!")
  }
}

#' Read, transform and save one .csv file
#' @importFrom data.table fread
#' @export
#' @noRd
z11_prepare_file <- function(file, data_location) {
  # Read first 50 rows of dataset to determine structure and content
  sample_df <- data.table::fread(file, encoding = "Latin-1", dec = ",", nrows = 50)
  is_sql <- is(data_location, "DBIConnection")

  if (ncol(sample_df) == 7) {
    # 100m grid data
    table_name <- switch(sample_df$Merkmal[3],
                         ALTER_KURZ = "demographie100m",
                         FAMGROESS_KLASS = "familien100m",
                         ZAHLWOHNGN_HHG = "gebaeude100m",
                         WOHNEIGENTUM = "wohnungen100m",
                         HHTYP_LEB = "haushalte100m")

    if (is_sql) {
      prepare_100m_data_db(data_location, file, table_name)
    } else {
      prepare_100m_data_r(data_location, file, table_name)
    }

  } else if ("x_mp_100m" %in% colnames(sample_df)) {
    # 100m population data

    if (is_sql) {
      prepare_100m_pop_data_db(data_location, file)
    } else {
      prepare_100m_pop_data_r(data_location, file)
    }

  } else if ("x_mp_1km" %in% colnames(sample_df)) {
    # 1km grid data

    # Spitz oder klassiert?
    table_name <- if (sample_df[49, "Einwohner"] == 8) {
      "spitz1km"
    } else {
      "klassiert1km"
    }

    if (is_sql) {
      prepare_1km_data_db(data_location, file, table_name)
    } else {
      prepare_1km_data_r(data_location, file, table_name)
    }

  } else {
    message("Error! This doesn't seem to be a census .csv file :(")
  }
}

