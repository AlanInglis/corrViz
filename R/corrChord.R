#' corrChord
#'
#' This function creates a chord plot of correlations between variables in a dataset.
#'
#' @param data A dataframe containing the data to be analyzed.
#' @param method A character string specifying the correlation method. One of
#'   "pearson", "kendall", or "spearman". Default is "pearson".
#' @param threshold A numeric value indicating the minimum absolute
#' correlation value to display in the plot.
#' @param circle A logical value indicating whether to use a circular
#' layout (TRUE) or linear layout (FALSE), default is FALSE.
#'
#' @return A chord plot displaying correlations.
#'
#' @details When using a large amount of data, this plot can quickly become over
#' complicated. It is recommended to filter the correlations using the \code{threshold}
#' argument to simplify the visualisation.
#'
#' @importFrom igraph graph_from_data_frame
#' @importFrom stats cor
#' @import ggplot2
#' @import ggraph
#'
#' @examples
#' corrChord(data = mtcars,
#'           method = 'pearson',
#'           threshold = 0.8)
#'
#' corrChord(data = mtcars,
#'           method = 'pearson',
#'           threshold = 0.8,
#'           circle = TRUE)
#'
#' @export
#'

corrChord <- function(data,
                      method = c("pearson", "kendall", "spearman"),
                      threshold = 0,
                      circle = FALSE) {

  # Decalre global vars
  value <- NULL

  # Compute the correlation matrix
  cor_matrix <- cor(data, method = method)

  # Set lower triangular matrix to zero to avoid duplicated links
  cor_matrix[lower.tri(cor_matrix)] <- 0

  # Set diagonal elements to zero
  diag(cor_matrix) <- 0

  # Filter the correlations based on a threshold
  cor_matrix[abs(cor_matrix) < threshold] <- 0

  # Convert the correlation matrix to a 'long' format
  long_cor_matrix <- matrix2long(cor_matrix)

  # Remove zero correlations
  long_cor_matrix <- long_cor_matrix[long_cor_matrix$value != 0, ]

  # add colours
  long_cor_matrix$col <- ifelse(long_cor_matrix$value <= 0, 'blue', 'red')

  # Find rows where names match entries in column1 or column2
  nam <- unique(c(long_cor_matrix$row_name, long_cor_matrix$col_name))
  df <- long_cor_matrix

  # Find matching names in column1 and column2
  matched_names_column1 <- unique(df$row_name[df$row_name %in% nam])
  matched_names_column2 <- unique(df$col_name[df$col_name %in% nam])
  nam_total <- unique(c(matched_names_column1, matched_names_column2))


  # Create an igraph object
  graph <- graph_from_data_frame(long_cor_matrix, directed = FALSE)

  # nudge if linear
  if(circle){
    ny = 0
  }else{
    ny = -0.3
  }

  ggraph(graph, layout = 'linear', circular = circle) +
    geom_edge_arc(aes(edge_width = abs(value),
                      edge_color = col,
                      alpha = abs(value)),
                  show.legend = F) +
    scale_edge_color_identity() +
    coord_fixed() +
    theme_void() +
    geom_node_label(aes(label=nam_total), size = 3.5, nudge_y = ny)

}


