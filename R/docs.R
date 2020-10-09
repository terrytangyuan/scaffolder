#' @importFrom reticulate py_help_handler
py_function_docs <- function(python_function) {
  
  # eval so that python loads
  eval(parse(text = python_function))
  
  # get components
  components <- strsplit(python_function, "\\$")[[1]]
  topic <- components[[length(components)]]
  source <- paste(components[1:(length(components)-1)], collapse = "$")
  
  # get function docs
  function_docs <- reticulate::py_help_handler(type = "completion", topic, source)
  
  # get parameter docs
  parameter_docs <- reticulate::py_help_handler(type = "parameter", NULL, python_function)
  
  # create a named list with parameters
  parameters <- parameter_docs$arg_descriptions
  names(parameters) <- parameter_docs$args
  
  # create a new list with all doc info
  list(name = function_docs$title,
       qualified_name = python_function,
       description = function_docs$description,
       details = function_docs$details,
       signature = function_docs$signature,
       parameters = parameters,
       sections = function_docs$sections,
       returns = function_docs$returns)
}
