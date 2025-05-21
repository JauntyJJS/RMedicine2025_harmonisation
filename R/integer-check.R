#' @title Is Integer Value
#' @description Function to check if the input
#' value is an integer.
#' @param input_value The input value
#' @param allow_na If true, NA values
#' are ignored and output is considered TRUE.
#' Default: FALSE
#' @return A boolean indicating TRUE
#' when the input value is an integer and
#' FALSE otherwise.
#' @examples
#'
#' # An integer
#' is_integer_value(1)
#'
#' # Not an integer
#' is_integer_value(1.1)
#'
#' # Not numeric
#' is_integer_value("1")
#'
#' # NA cases
#' is_integer_value(NA, allow_na = FALSE)
#' is_integer_value(NA, allow_na = TRUE)
#'
#' @rdname is_integer_value
#' @export
is_integer_value <- function(input_value,
                             allow_na = FALSE) {

  boolean_result <- FALSE

  # When input value is NA
  if (is.na(input_value)) {
    if (isTRUE(allow_na)) {
      boolean_result <- TRUE
      return(boolean_result)
    } else {
      return(boolean_result)
    }
  }

  # When input value is not numeric
  if (isTRUE(!is.numeric(input_value))) {
    return(boolean_result)
  }

  # When input value is numeric
  boolean_result <- isTRUE(input_value %% 1 == 0)

  return(boolean_result)
}

#' @title Is Integer Vector
#' @description Function to check if the input
#' vector contains only integers.
#' @param input_vector The input vector
#' @param allow_na If true, NA values
#' are ignored and output is considered TRUE.
#' Default: FALSE
#' @return A boolean vector indicating TRUE
#' when the input element is an integer and
#' FALSE otherwise.
#' @details
#' We assume the input vector is numeric.
#' If it is not, all elements will be given
#' a FALSE value.
#' @examples
#'
#' # An integer
#' is_integer_vector(c(1, 2, 3))
#'
#' # Not an integer
#' is_integer_vector(c(1.1, 2, 3))
#'
#' # Not numeric vector
#' # R converts c(1, 2, "3") to c("1", "2", "3")
#' is_integer_vector(c(1, 2, "3"))
#'
#' # NA cases
#' is_integer_vector(c(1, NA, 3), allow_na = FALSE)
#' is_integer_vector(c(1, NA, 3), allow_na = TRUE)
#'
#' @rdname is_integer_vector
#' @export
is_integer_vector <- function(input_vector,
                              allow_na = FALSE) {

  boolean_results <- input_vector |>
    purrr::map_lgl(
      .f = is_integer_value,
      allow_na = allow_na
    )
  return(boolean_results)
}
