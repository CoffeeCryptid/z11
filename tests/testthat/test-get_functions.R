#Testing z11_get_1km_attribute, z11_get_100m_attribute

# Get filename or connection to db
source("data_source.R")

# Retrieve attribute as raster
test_that("retrieved 1km attribute is raster", {
  suppressMessages(df <- z11::z11_get_1km_attribute("Frauen_A", src, geometry = TRUE, as_raster = TRUE))
  expect_s4_class(df, "RasterLayer") #Check if class is raster
})

test_that("retrieved 100m attribute is raster", {
  suppressMessages(df <- z11::z11_get_100m_attribute("Einwohner_100m", src, geometry = TRUE, as_raster = TRUE))
  expect_s4_class(df, "RasterLayer") #Check if class is raster
})

# Retrieve 1km attribute, no raster
test_that("retrieved 1km attribute (no raster) has correct class and shape", {
  suppressMessages(df <- z11_get_1km_attribute("Frauen_A", src, geometry = TRUE, as_raster = FALSE))
  expect_s3_class(df, "data.frame") #Result is of class data.frame
  expect_equal(ncol(df), 3) #Data frame has three columns
  expect_true(nrow(df) > 0) #Data frame is not empty
  expect_s3_class(df$geometry, "sfc") #Check if geometry column is of class sfc
  expect_vector(df[[2]], ptype = double()) #Check if attribute is double
})

test_that("retrieved 100m attribute (no raster) has correct class and shape", {
  suppressMessages(df <- z11_get_100m_attribute("DEM_ALTER_10JG_1", src, geometry = TRUE, as_raster = FALSE))
  expect_s3_class(df, "data.frame") #Result is of class data.frame
  expect_equal(ncol(df), 3) #Data frame has three columns
  expect_true(nrow(df) > 0) #Data frame is not empty
  expect_s3_class(df$geometry, "sfc") #Check if geometry column is of class sfc
  expect_vector(df[[2]], ptype = integer()) #Check if attribute is integer
})

