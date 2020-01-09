#' Scaffolding Interfaces to Packages in Other Programming Languages
#'
#' This package provides a comprehensive set of tools to scaffold
#' interfaces to modules, classes, functions, and
#' documentations written in other programming languages.
#'
#' @docType package
#' @name scaffolder
NULL

#' @importFrom reticulate py_available import
#' @importFrom utils capture.output
NULL

.onLoad <- function(libname, pkgname) {
  
  if (!on_cran()) {
    if (!reticulate::py_available()) {
      python_not_available_message()
    }
  }
}

python_not_available_message <- function() {
  # TODO: Debug why this is not detecting Python successfully
  # packageStartupMessage(
  #   paste0("Python may not be available. ",
  #          "Please install it and try load it again."))
}

on_cran <- function() {
  !identical(Sys.getenv("NOT_CRAN"), "true")
}

.onUnload <- function(libpath) {
  
}

.onAttach <- function(libname, pkgname) {
  
}

.onDetach <- function(libpath) {
  
}