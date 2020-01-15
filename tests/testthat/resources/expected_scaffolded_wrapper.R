#' @title top_k
#'
#' @description Finds values and indices of the `k` largest entries for the last dimension.
#'
#' @details If the input is a vector (rank=1), finds the `k` largest entries in the vector
#' and outputs their values and indices as vectors. Thus `values[j]` is the
#' `j`-th largest entry in `input`, and its index is `indices[j]`. For matrices (resp. higher rank input), computes the top `k` entries in each
#' row (resp. vector along the last dimension). Thus, values.shape = indices.shape = input.shape[:-1] + [k] If two elements are equal, the lower-index element appears first.
#'
#' @param input 1-D or higher `Tensor` with last dimension at least `k`.
#' @param k 0-D `int32` `Tensor`. Number of top elements to look for along the last dimension (along each row for matrices).
#' @param sorted If true the resulting `k` elements will be sorted by the values in descending order.
#' @param name Optional name for the operation.
#'
#' @return values: The `k` largest elements along each last dimensional slice. indices: The indices of `values` within the last dimension of `input`.
#'
#' @export
top_k <- function(input, k = 1L, sorted = TRUE, name = NULL) {

  python_function_result <- tf$nn$top_k(
    input = input,
    k = k,
    sorted = sorted,
    name = name
  )

}
