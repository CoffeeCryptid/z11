# Test z11_list_*_attributes functions

test_that("z11_list_1km_attributes creates a character vector of length 20", {
  expect_vector(z11_list_1km_attributes(), ptype = character(), size = 20)
})

test_that("z11_list_100m_attributes creates a character vector of length 229", {
  expect_vector(z11_list_100m_attributes(), ptype = character(), size = 229)
})
