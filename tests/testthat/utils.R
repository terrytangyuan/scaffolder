skip_if_no_tensorflow <- function() {
  if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    skip("Skip tests on CRAN due to lack of proper Python setup")
  }
  if (Sys.info()[["user"]] == "yuan.tang")
    reticulate::use_python("/usr/bin/python3")
  if (!reticulate::py_module_available("tensorflow"))
    skip("tensorflow Python module is not available for testing")
  else
    tf <<- reticulate::import("tensorflow")
}

test_succeeds <- function(desc, expr) {
  test_that(desc, {
    skip_if_no_tensorflow()
    expect_error(force(expr), NA)
  })
}
