#' @description Finds values and indices of the `k` largest entries for the last dimension.
#' @title top_k
#' 
#' @param  input 1-D or higher `Tensor` with last dimension at least `k`.
#' @param  k 0-D `int32` `Tensor`. Number of top elements to look for along the last dimension (along each row for matrices).
#' @param  sorted If true the resulting `k` elements will be sorted by the values in descending order.
#' @param  name Optional name for the operation.
top_k <- function(input, k = 1L, sorted = TRUE, name = NULL) {

  python_function_result <- tf$nn$top_k(
    input = input,
    k = as.integer(k),
    sorted = sorted,
    name = name
  )
  return(python_function_result)
}