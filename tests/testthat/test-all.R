context("Test all functionalities of the package")

source("utils.R")

# Dummy test to get around CRAN check
test_that("This test block executes on CRAN", {
  expect_true(TRUE)
})

test_succeeds("scaffold_py_function_wrapper() works correctly", {
  actual_output <- capture.output(scaffold_py_function_wrapper("tf$nn$top_k"))
  expected_output <- readLines("resources/expected_scaffolded_wrapper.R", warn = FALSE)
  expect_equal(actual_output, expected_output)
})

test_succeeds("custom_scaffold_py_function_wrapper() works correctly", {
  actual_output <- capture.output(custom_scaffold_py_function_wrapper("tf$nn$top_k"))
  expected_output <- readLines("resources/expected_custom_scaffolded_wrapper.R", warn = FALSE)
  expect_equal(actual_output, expected_output)
})
