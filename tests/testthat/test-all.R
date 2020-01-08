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
  skip_if_not_installed("stringr")
  library(stringr)
  process_int_param_fn <- function(param, docs) {
    # Extract the list of parameters that have integer values as default
    int_params <- gsub(
      " = [-]?[0-9]+L",
      "",
      str_extract_all(docs$signature, "[A-z]+ = [-]?[0-9]+L")[[1]])
    # Explicitly cast parameter in the list obtained above to integer
    if (param %in% int_params) {
      param <- paste0("as.integer(", param, ")")
    }
    param
  }

  actual_output <- capture.output(
    custom_scaffold_py_function_wrapper(
      "tf$nn$top_k",
      r_function = "top_k",
      process_param_fn = process_int_param_fn,
      postprocess_fn = function() { "return(python_function_result)" })
  )
  expected_output <- readLines("resources/expected_custom_scaffolded_wrapper.R", warn = FALSE)
  expect_equal(actual_output, expected_output)
})
