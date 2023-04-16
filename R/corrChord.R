#' corrChord
#'
#' Creates an chord plot displaying correlations.
#'
#'
#' @param data A data frame.
#' @param method Which correlation coefficient (or covariance) is to be computed.
#' One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#' @param threshold Filter correlations with an absolute value greater than selected value.
#' @param circle If TRUE then plot is displayed as a circle.
#'
#' @return A chord plot displaying correlations.
#'
#' @details DEtS hEre!!!
#'
#' @importFrom igraph graph_from_data_frame
#' @importFrom reshape2 melt
#' @import ggplot2
#' @import ggraph
#'
#' @export
#'

corrChord <- function(data,
                      method = c("pearson", "kendall", "spearman"),
                      threshold = 0,
                      circle = FALSE) {

  # Compute the correlation matrix
  cor_matrix <- cor(data, method = method)

  # Set lower triangular matrix to zero to avoid duplicated links
  cor_matrix[lower.tri(cor_matrix)] <- 0

  # Set diagonal elements to zero
  diag(cor_matrix) <- 0

  # Filter the correlations based on a threshold
  cor_matrix[abs(cor_matrix) < threshold] <- 0

  # Convert the correlation matrix to a 'long' format
  long_cor_matrix <- reshape2::melt(cor_matrix)

  # Remove zero correlations
  long_cor_matrix <- long_cor_matrix[long_cor_matrix$value != 0, ]

  # add colours
  long_cor_matrix$col <- ifelse(long_cor_matrix$value <= 0, 'blue', 'red')

  # Find rows where names match entries in column1 or column2
  nam <- levels(long_cor_matrix$Var1)
  df <- long_cor_matrix

  # Find matching names in column1 and column2
  matched_names_column1 <- unique(df$Var1[df$Var1 %in% nam])
  matched_names_column2 <- unique(df$Var2[df$Var2 %in% nam])
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


