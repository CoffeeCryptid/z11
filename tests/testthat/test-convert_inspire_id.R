# Get filename or connection to db
source("data_source.R")

# Retrieve attribute, convert INSPIRE ID to coordinates,
# and get inspire ID again
test_that("converting 1km INSPIRE ID to coordinates and vice versa works correctly", {
  suppressMessages(df <- z11_get_1km_attribute("Alter_D", src, geometry = FALSE, as_raster = FALSE))
  coords <- z11_extract_inspire_coordinates(df$Gitter_ID_1km)
  coords <- sf::st_as_sf(coords, coords = c("X", "Y"), crs = 3035)
  inspire <- z11_create_inspire_ids(coords, type = "1km")
  expect_vector(inspire, ptype = character(), size = nrow(df))
  expect_equal(df$Gitter_ID_1km, inspire)
  expect_match(inspire[1], "1kmN\\d{4}E\\d{4}") # Check format of INSPIRE IDs
})

test_that("converting 100m INSPIRE ID to coordinates and vice versa works correctly", {
  suppressMessages(df <- z11_get_100m_attribute("FAM_FAMTYP_KIND_2", src, geometry = FALSE, as_raster = FALSE))
  coords <- z11_extract_inspire_coordinates(df$Gitter_ID_100m)
  coords <- sf::st_as_sf(coords, coords = c("X", "Y"), crs = 3035)
  inspire <- z11_create_inspire_ids(coords, type = "100m")
  expect_vector(inspire, ptype = character(), size = nrow(df))
  expect_equal(df$Gitter_ID_100m, inspire)
  expect_match(inspire[1], "100mN\\d{5}E\\d{5}")
})

test_that("converting to 1km AND 100m INSPIRE ID yields dataframe containing INSPIRE IDs of the correct formats", {
  suppressMessages(df <- z11_get_1km_attribute("Alter_D", src, geometry = FALSE, as_raster = FALSE))
  coords <- z11_extract_inspire_coordinates(df$Gitter_ID_1km)
  coords <- sf::st_as_sf(coords, coords = c("X", "Y"), crs = 3035)
  inspire <- z11_create_inspire_ids(coords, type = c("1km", "100m"))
  expect_s3_class(inspire, "data.frame")
  expect_equal(ncol(inspire), 2)
  expect_match(inspire[[1]][1], "1kmN\\d{4}E\\d{4}")
  expect_match(inspire[[2]][1], "100mN\\d{5}E\\d{5}")
  expect_named(inspire, c("Gitter_ID_1km", "Gitter_ID_100m"))
})

test_that("converting to 1km INSPIRE ID with combine = TRUE yields data.frame with two named columns", {
  suppressMessages(df <- z11_get_1km_attribute("Alter_D", src, geometry = FALSE, as_raster = FALSE))
  coords <- z11_extract_inspire_coordinates(df$Gitter_ID_1km)
  coords <- sf::st_as_sf(coords, coords = c("X", "Y"), crs = 3035)
  combined <- z11_create_inspire_ids(coords, type = "1km", column_name = "inspire", combine = TRUE)
  expect_named(combined, c("geometry", "inspire1km"))
  expect_match(combined[[2]][2], "1kmN\\d{4}E\\d{4}")
  expect_equal(ncol(combined), 2)
})

test_that("converting to 1km and 100m INSPIRE ID with combine = TRUE yields data.frame with three named columns", {
  suppressMessages(df <- z11_get_1km_attribute("Alter_D", src, geometry = FALSE, as_raster = FALSE))
  coords <- z11_extract_inspire_coordinates(df$Gitter_ID_1km)
  coords <- sf::st_as_sf(coords, coords = c("X", "Y"), crs = 3035)
  combined <- z11_create_inspire_ids(coords, type = c("1km", "100m"), column_name = "inspire", combine = TRUE)
  expect_named(combined, c("geometry", "inspire1km", "inspire100m"))
  expect_match(combined[[2]][1], "1kmN\\d{4}E\\d{4}")
  expect_match(combined[[3]][1], "100mN\\d{5}E\\d{5}")
  expect_equal(ncol(combined), 3)
})
