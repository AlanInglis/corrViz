#' corrCircle
#'
#' Creates a circular chord plot displaying correlations.
#'
#'
#' @param data A data frame.
#' @param method Which correlation coefficient (or covariance) is to be computed.
#' One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#' @param threshold Filter correlations with an absolute value greater than selected value.
#' #'
#' @return A circular chord plot displaying correlations.
#'
#' @details DEtS hEre!!!
#'
#' @importFrom circlize chordDiagram
#' @importFrom circlize colorRamp2
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
