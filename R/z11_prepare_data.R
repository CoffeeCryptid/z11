#' Data Preparation
#'
#' @description
#' Reads in 2011 Census .csv files, splits it into smaller chunks, and saves it
#' locally.
#'
#' @param file Path to a either a census .csv file or a directory where the
#' .csv files are saved
#' @param data_location A DBI connection or a character string containing a path
#' to the directory where the data should be saved.
#'
#' @details
#' The data files are very large and use a lot of RAM, so use with caution.
#' Don't run other things in the background.
#'
#' @examples
#' \dontrun{
#' z11_prepare_data("/home/yourname/Haushalte100m.csv", "/home/yourname/z11data")
#'
#' con <- DBI::dbConnect(RSQLite::SQLite(), "/home/yourname/z11data.sqlite3")
#' z11_prepare_data("/home/yourname/census_data", con)
#' DBI::dbDisconnect(con)
#' }
#'
#' @importFrom purrr walk
#' @importFrom magrittr %>%
#' @importFrom data.table fread setDT setnames setorder dcast
#' @importFrom purrr walk
#' @importFrom DBI dbWriteTable
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

#' @export
#' @noRd
z11_prepare_file <- function(file, data_location) {
  UseMethod("z11_prepare_file", data_location)
}

# Method: character
#' @export
#' @noRd
z11_prepare_file.character <- function(file, data_location) {
  #Read data
  message("Reading file...")
  df <- data.table::fread(file, encoding = "Latin-1", dec = ",")

  #100m grid data
  if (ncol(df) == 7) {
    identifier <- df$Merkmal[3]
    name <- switch(identifier, ALTER_KURZ = "demographie100m", FAMGROESS_KLASS = "familien100m",
                   ZAHLWOHNGN_HHG = "gebaeude100m", WOHNEIGENTUM = "wohnungen100m", HHTYP_LEB = "haushalte100m")

    #Revome trailing whitespaces, paste Mermal and Auspraegung_Code columns together
    message("Clean up strings...")
    df <- df[, merkm := paste(trimws(Merkmal), Auspraegung_Code, sep = "_")]

    message("Transform and save data...")
    #Create subdirectory if it doesn't exist
    if (!dir.exists(file.path(data_location, "100m"))) {
      dir.create(file.path(data_location, "100m"))
    }
    #Prefix for variable names ("GEB", "FAM", etc.)
    prefix <- substring(name, 1, 3) %>% toupper() %>% paste0("_")
    #Subset data, rename Variables, save
    purrr::walk(unique(df$merkm),
                ~df[merkm == .x, .(Gitter_ID_100m, Anzahl)][, data.table::setnames(.SD, new = c("Gitter_ID_100m", paste0(prefix, .x)))] %>%
                  saveRDS(file = file.path(data_location, "100m", paste0(prefix, .x, ".rds")))
    )
  } else if ("x_mp_100m" %in% colnames(df)) {
    #100m population data

    #Create subdirectory if it doesn't exist
    if (!dir.exists(file.path(data_location, "100m"))) {
      dir.create(file.path(data_location, "100m"))
    }

    message("Select and rename columns...")
    df <- df[Einwohner != -1, .SD, .SDcols = !c('x_mp_100m', 'y_mp_100m')]
    data.table::setnames(df, old = "Einwohner", new = "Einwohner_100m")

    message("Save data...")
    saveRDS(df, file = file.path(data_location, "100m", "Einwohner_100m.rds"))

  } else if ("x_mp_1km" %in% colnames(df)) {
    #1km grid data

    #Create subdirectory if it doesn't exist
    if (!dir.exists(file.path(data_location, "1km"))) {
      dir.create(file.path(data_location, "1km"))
    }

    message("Select and rename columns")
    df <- df[,.SD, .SDcols = !c('x_mp_1km', 'y_mp_1km')]
    if (df[Wohnfl_Whg_D >= 0, .N] > 100000) {
      colns <- colnames(df)
      colns <- c(colns[1], paste(colns[2:length(colns)], "cat", sep = "_"))
      setnames(df, colns)
    }

    #Create vector of column names to loop over, get suffix for variable names
    colns <- colnames(df)[-1]

    #Select columns, rename and save
    message("Save data...")
    purrr::walk(colns,
                ~df[, .(Gitter_ID_1km, get(.x))][, data.table::setnames(.SD, c("Gitter_ID_1km", .x))] %>%
                  saveRDS(file = file.path(data_location, "1km", paste0(.x, ".rds"))))
  } else {
    message("Error! This doesn't seem to be a census .csv file :(")
  }

  message("Cleaning up...")
  rm(df)
}

#Method: DBI Connection
#' @export
#' @noRd
z11_prepare_file.DBIConnection <- function(file, data_location) {
  #Read data
  message("Reading file...")
  df <- data.table::fread(file, encoding = "Latin-1", dec = ",")

  #Prepare data
  message("Transforming data...")
  if (ncol(df) == 7) {
    identifier <- df$Merkmal[3]
    name <- switch(identifier, ALTER_KURZ = "demographie100m", FAMGROESS_KLASS = "familien100m",
                   ZAHLWOHNGN_HHG = "gebaeude100m", WOHNEIGENTUM = "wohnungen100m", HHTYP_LEB = "haushalte100m")

    message("    Transform to wide format")
    df <- data.table::dcast(df, Gitter_ID_100m ~ Merkmal + Auspraegung_Code, value.var = "Anzahl")

    message("    Change variable names")
    prefix <- toupper(substring(name, 1, 3))
    colns <- colnames(df) %>% trimws()
    colns <- c(colns[1], paste(prefix, colns[2:length(colns)], sep = "_"))
    setnames(df, colns)

    message("    Order by INSPIRE ID")
    data.table::setorder(df, Gitter_ID_100m)

  } else if ("x_mp_100m" %in% colnames(df)) {
    # 100m Bevoelkerung
    name <- "bevoelkerung100m"
    #message("    Transform INSPIRE ID to numeric")
    #df[, Gitter_ID_100m := as.numeric(gsub("^100m|[^0-9]", "", Gitter_ID_100m))]

    message("    Select and rename columns")
    df <- df[Einwohner != -1, .(Gitter_ID_100m, Einwohner)]
    data.table::setnames(df, old = "Einwohner", new = "Einwohner_100m")

    message("    Order by INSPIRE ID")
    data.table::setorder(df, Gitter_ID_100m)

  } else if ("x_mp_1km" %in% colnames(df)) {
    # 1km data

    message("    Select and rename columns")
    df <- df[, .SD, .SDcols = !c('x_mp_1km', 'y_mp_1km')]
    name <- "spitz1km"
    if (df[Wohnfl_Whg_D >= 0, .N] > 100000) {
      colns <- colnames(df)
      colns <- c(colns[1], paste(colns[2:length(colns)], "cat", sep = "_"))
      setnames(df, colns)
      name <- "klassiert1km"
    }

    message("    Order by INSPIRE ID")
    data.table::setorder(df, Gitter_ID_1km)

  } else {
    stop("Something went wrong :( Is this a correct 2011 Census .csv file?")
  }

  message(sprintf("Writing %s to database...", name))
  DBI::dbWriteTable(data_location, name, df, overwrite = TRUE)

  message("Cleaning up...")
  rm(df)
}

utils::globalVariables(c("Gitter_ID_100m", ".", ".SD", ".N",
                         "Wohnfl_Whg_D", "Gitter_ID_1km", "merkm",
                         "Merkmal", "Auspraegung_Code", "Einwohner"))
