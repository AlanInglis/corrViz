#' matrix2long
#'
#' Convert a Matrix to Long Format.
#'
#' This function converts a matrix into a long format data frame.
#' The resulting data frame contains four columns: row, column, value, and id. The 'id' column
#' assigns a unique identifier to each column group, making it easier to identify and analyze
#' the data by column groups.
#'
#' @param mat A matrix to be converted into long format.
#'
#' @return A data frame in long format with columns: row, column, value, and id.
#'
#' @examples
#' # Create a matrix
#' mat <- matrix(data = 1:9,
#'               nrow = 3,
#'               ncol = 3,
#'               dimnames = list(c("A", "B", "C"),
#'                               c("X", "Y", "Z")))
#'
#' long_format <- matrix2long(mat)
#' long_format
#'
#' # Using correlation matrix
#' matrix2long(cor(mtcars))
#'
#' @export


matrix2long <- function(mat) {
  # Ensure the input is a matrix
  if (!is.matrix(mat)) {
    stop("Input must be a matrix.")
  }

  # Get the dimensions and row/column names of the input matrix
  nrows <- nrow(mat)
  ncols <- ncol(mat)
  rownames <- rownames(mat)
  colnames <- colnames(mat)

  # If row/column names are NULL, assign default names
  if (is.null(rownames)) {
    rownames <- 1:nrows
  }
  if (is.null(colnames)) {
    colnames <- 1:ncols
  }

  # Create a data frame with 4 columns: row, column, value, and id
  long_format <- data.frame(
    row_name = rep(rownames, times = ncols),
    col_name = rep(colnames, each = nrows),
    value = as.vector(mat),
    id = rep(1:ncols, each = nrows)
  )

  return(long_format)
}
