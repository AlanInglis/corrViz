#' corrNetwork
#'
#' Creates an interactive Correlation Network Visualization
#'
#' @param data A data frame or matrix containing the input data for the correlation calculation.
#' @param method A character string specifying the correlation method. One of
#'   "pearson", "kendall", or "spearman". Default is "pearson".
#' @param threshold A numeric value indicating the minimum absolute correlation
#'  value to display in the plot.
#' @param layout Any \code{igraph} layout to display the network.
#' @param width The width of the viewing window.
#' @param height The height of the viewing window.
#' @param physics A logical value indicating whether to use physics-based layout. Default is TRUE.
#'
#' @return A network plot displaying correlations.
#'
#' @details Each node in the network represents a variable where the width of
#' the connecting edges represent the absolute value of the correlation. Positive
#' correlations have red coloured edges whereas negative correlations have blue coloured
#' edges.
#'
#' @importFrom visNetwork visNetwork
#' @importFrom visNetwork visOptions
#' @importFrom visNetwork visInteraction
#' @importFrom visNetwork visLegend
#' @importFrom visNetwork visEdges
#' @importFrom visNetwork visNodes
#' @importFrom visNetwork visIgraphLayout
#' @importFrom visNetwork visPhysics
#' @importFrom stats cor
#' @importFrom stats na.omit
#'
#' @examples
#' corrNetwork(data = iris[,1:4], threshold = 0.5)
#'
#' corrNetwork(data = mtcars,
#'            threshold = 0.8,
#'            method = "pearson",
#'            layout = 'layout_on_grid',
#'            physics = FALSE)
#' @export

corrNetwork <- function(data,
                        method = c("pearson", "kendall", "spearman"),
                        threshold = 0,
                        layout = "layout_nicely",
                        width = "100%",
                        height = "400px",
                        physics = TRUE) {

  # declare global vars
  value <- val <- col_name <- row_name <- NULL

  if (threshold > 1 | threshold < -1) {
    stop("threshold must be in the range [-1,1]")
  }

  # Calculate correlation matrix
  cor_matrix <- cor(data, method = method)
  diag(cor_matrix) <- NA

  # Convert matrix to data frame
  df_data <- matrix2long(cor_matrix)

  # # Add row and column names as separate columns
  # df_data$row_name <- NULL
  # df_data$row_name <- row.names(cor_matrix)
  # df_data$col_name <- NULL
  # df_data$col_name <- colnames(cor_matrix)
  #
  # # Reshape data frame to long format
  # df_data_long <- reshape(df_data,
  #   direction = "long",
  #   varying = list(colnames(cor_matrix)),
  #   v.names = "value",
  #   timevar = "col_name",
  #   times = colnames(cor_matrix)
  # )

  # turn into long df
  #new_df <- df_data_long
  new_df <- na.omit(df_data)
  new_df <- subset(new_df, !duplicated(value)) #NOTE POSSIBLE ERROR HERE IF  2 VARS HAVE SAME COR

  nodes <- data.frame(id = 1:length(names(data)), label = names(data))

  # edges <- new_df |>
  #   filter(value < -threshold | value > threshold) |>
  #   mutate(
  #     from = match(col_name, nodes$label),
  #     to = match(row_name, nodes$label),
  #     val = abs(value),
  #     value = value,
  #     width = abs(val) * 10
  #   )


  filtered_df <- subset(new_df, value < -threshold | value > threshold)

  # Add new columns 'from', 'to', 'val', 'value', and 'width'
  edges <- transform(filtered_df,
                     from = match(col_name, nodes$label),
                     to = match(row_name, nodes$label),
                     val = abs(value),
                     value = value,
                     width = abs(value) * 10
  )

  rownames(edges) <- NULL
  edges$id <- NULL
  edges$title <- paste0("Corr: ", round(edges$value, 2))

  edges$color <- NA
  edges$color <- ifelse(edges$value <= 0, "blue", "red")
  edges$value <- abs(edges$value)
  edges$highlight <- 'pink'


  # Create the interactive network plot
  visNetwork::visNetwork(nodes, edges, width = width, height = height) |>
    visNetwork::visOptions(
      highlightNearest = list(enabled = T, hover = T),
      nodesIdSelection = T
    ) |>
    visNetwork::visInteraction(navigationButtons = T) |>
    visNetwork::visLegend(width = 0.15, position = "left") |>
    visNetwork::visEdges(smooth = FALSE) |>
    visNetwork::visNodes(color =  list(background = "skyblue",
                                       border = "black",
                                       highlight = list(background = "yellow",
                                                        border = 'black'),
                                       hover = list(background = "pink",
                                                    border = 'black'))) |>
    visNetwork::visIgraphLayout(layout = layout, physics = physics) |>
    visNetwork::visPhysics(
      solver = "forceAtlas2Based",
      forceAtlas2Based = list(gravitationalConstant = -500)
    )
}
