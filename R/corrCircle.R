#' corrCircle
#'
#' This function creates a circular plot of correlations between variables in a dataset.
#'
#' @param data A dataframe containing the data to be analyzed.
#' @param method A character string specifying the correlation method. One of
#'   "pearson", "kendall", or "spearman". Default is "pearson".
#' @param threshold A numeric value indicating the minimum absolute correlation
#'  value to display in the plot.
#' @param ticks A logical value indicating whether to display ticks (TRUE) or not (FALSE),
#'  default is FALSE.
#'
#' @return A circular chord plot object displaying the correlations between variables.
#'
#' @details When using a large amount of data, this plot can quickly become over
#' complicated. It is recommended to filter the correlations using the \code{threshold}
#' argument to simplify the visualisation.
#'
#'
#' @importFrom circlize chordDiagram
#' @importFrom circlize colorRamp2
#' @importFrom stats cor
#'
#' @examples
#' corrCircle(data = mtcars,
#'           method = 'pearson',
#'           threshold = 0.8)
#'
#'
#' @export
#'



corrCircle <- function(data,
                       method = c("pearson", "kendall", "spearman"),
                       threshold = 0,
                       ticks = FALSE) {
  # Compute the correlation matrix
  cor_matrix <- cor(data, method = method)

  # Set lower triangular matrix to zero to avoid duplicated links
  cor_matrix[lower.tri(cor_matrix)] <- 0

  # Set diagonal elements to zero
  diag(cor_matrix) <- 0

  # Filter the correlations based on a threshold
  cor_matrix[abs(cor_matrix) < threshold] <- 0

  cols <- colorRamp2(c(-1, 0, 1), c("blue", "white", "red"), transparency = 0.5)

  if (ticks) {
    chordDiagram(cor_matrix,
      col = cols
    )
  } else {
    chordDiagram(cor_matrix,
      col = cols,
      annotationTrack = c("grid", "name")
    )
  }
}
