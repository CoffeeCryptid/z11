# Test simple join functions

# Get filename or connection to db
source("data_source.R")

# Retrieve 1km attribute, randomly select 3000 cases, and merge ONE 1km attribute
test_that("1km merge with one 1km attribute yields correct results", {
  suppressMessages(df <- z11_get_1km_attribute("unter18_A", src, geometry = FALSE, as_raster = FALSE))
  df <- df[sample(nrow(df), 3000), ]
  names(df)[2] <- "var_alt"
  suppressMessages(joined <- z11_simple_join_1km_attribute(df, Gitter_ID_1km, data_source = src, attributes = "unter18_A"))
  # Check if new column is equal to old column
  expect_equal(joined$unter18_A, joined$var_alt)
  # Check if data frame has correct number of rows and columns
  expect_equal(dim(joined), c(3000, 3))
})

test_that("100m merge with one 100m attribute yields correct results", {
  suppressMessages({
    df <- z11_get_100m_attribute("GEB_BAUJAHR_MZ_1", src,
                                 geometry = FALSE, as_raster = FALSE)
    df <- df[sample(nrow(df), 3000), ]
    names(df)[2] <- "var_alt"
    joined <- z11_simple_join_100m_attribute(df, Gitter_ID_100m,
      data_source = src, attributes = "GEB_BAUJAHR_MZ_1")
  })
  # Check if new column is equal to old column
  expect_equal(joined$GEB_BAUJAHR_MZ_1, joined$var_alt)
  # Check if data frame has correct number of rows and columns
  expect_equal(dim(joined), c(3000, 3))
})

# Retrieve attribute, randomly select 3000 cases, and merge SEVERAL attributes
test_that("1km merge with several 1km attributes yields correct results", {
  suppressMessages({
    df <- z11_get_1km_attribute("Leerstandsquote", src, geometry = FALSE,
                                as_raster = FALSE)
    df <- df[sample(nrow(df), 1000), ]
    names(df)[2] <- "var_alt"
    joined <- z11_simple_join_1km_attribute(df, Gitter_ID_1km,
      data_source = src, attributes = c("unter18_A", "Leerstandsquote"))
  })
  # Check if new column is equal to old column
  expect_equal(joined$Leerstandsquote, joined$var_alt)
  # Check if data frame has correct number of rows and columns
  expect_equal(dim(joined), c(1000, 4))
})

test_that("100m merge with several 100m attributes yields correct results", {
  suppressMessages({
    df <- z11_get_100m_attribute("GEB_HEIZTYP_4", src, geometry = FALSE,
                                 as_raster = FALSE)
    df <- df[sample(nrow(df), 1000), ]
    names(df)[2] <- "var_alt"
    joined <- z11_simple_join_100m_attribute(df, Gitter_ID_100m,
      data_source = src, attributes = c("GEB_HEIZTYP_4", "WOH_HEIZTYP_4"))
  })
  # Check if new column is equal to old column
  expect_equal(joined$GEB_HEIZTYP_4, joined$var_alt)
  # Check if data frame has correct number of rows and columns
  expect_equal(dim(joined), c(1000, 4))
})

# Retrieve 1km attribute, randomly select 3000 cases, and merge ALL 1km attributes
test_that("1km merge with ALL attributes yields correct results", {
  suppressMessages({
    df <- z11_get_1km_attribute("Wohnfl_Bew_D", src, geometry = FALSE,
                                as_raster = FALSE)
    df <- df[sample(nrow(df), 500), ]
    names(df)[2] <- "var_alt"
    joined <- z11_simple_join_1km_attribute(df, Gitter_ID_1km, data_source = src)
  })
  # Check if new column is equal to old column
  expect_equal(joined$Wohnfl_Bew_D, joined$var_alt)
  # Check if data frame has correct number of rows and columns
  expect_equal(dim(joined), c(500, 22))
})

test_that("100m merge with ALL attributes yields correct results", {
  suppressMessages({
    df <- z11_get_100m_attribute("HAU_HHTYP_LEB_2", src, geometry = FALSE,
                                 as_raster = FALSE)
    df <- df[sample(nrow(df), 100), ]
    names(df)[2] <- "var_alt"
    joined <- z11_simple_join_100m_attribute(df, Gitter_ID_100m, data_source = src)
  })
  # Check if new column is equal to old column
  expect_equal(joined$HAU_HHTYP_LEB_2, joined$var_alt)
  # Check if data frame has correct number of rows and columns
  expect_equal(dim(joined), c(100, 231))
})
