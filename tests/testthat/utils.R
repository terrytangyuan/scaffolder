skip_if_no_tensorflow <- function() {
  if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    skip("Skip tests on CRAN due to lack of proper Python setup")
  }
  if (!reticulate::py_module_available("tensorflow_io"))
    skip("tensorflow Python module is not available for testing")
}

test_succeeds <- function(desc, expr) {
  test_that(desc, {
    skip_if_no_tensorflow()
    expect_error(force(expr), NA)
  })
}
