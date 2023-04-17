#' corrNetwork
#'
#' Creates an interactive network plot displaying correlations.
#'
#'
#' @param data A data frame.
#' @param method Which correlation coefficient (or covariance) is to be computed.
#' One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#' @param threshold Filter correlations with an absolute value lower than selected value.
#' @param layout Use an \code{igraph} layout to display network.
#' @param width The width of the viewing window.
#' @param height The height of the viewing window.
#' @param physics If TRUE (the default) then physics is enabled on nodes. This may
#' affect the selected layout.
#' @return A network plot displaying correlations.
#'
#' @details Each node in the network represents a variable where the width of
#' the connecting edges represent the absolute value of the correlation. Positive
#' correlations have red coloured edges whereas negative correlations have blue coloured
#' edges.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
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
#' @importFrom stats reshape
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
  value <- val <- NULL

  if (threshold > 1 | threshold < -1) {
    stop("threshold must be in the range [-1,1]")
  }

  # Calculate correlation matrix
  cor_matrix <- cor(data, method = method)
  diag(cor_matrix) <- NA

  # Convert matrix to data frame
  df_data <- data.frame(cor_matrix)

  # Add row and column names as separate columns
  df_data$row_name <- NULL
  df_data$row_name <- row.names(cor_matrix)
  df_data$col_name <- NULL
  df_data$col_name <- colnames(cor_matrix)

  # Reshape data frame to long format
  df_data_long <- reshape(df_data,
    direction = "long",
    varying = list(colnames(cor_matrix)),
    v.names = "value",
    timevar = "col_name",
    times = colnames(cor_matrix)
  )

  # turn into long df
  new_df <- df_data_long
  new_df <- na.omit(new_df)
  new_df <- subset(new_df, !duplicated(value))

  nodes <- data.frame(id = 1:length(names(data)), label = names(data))

  edges <- new_df |>
    filter(.data$value < -threshold | .data$value > threshold) |>
    mutate(
      from = match(.data$col_name, nodes$label),
      to = match(.data$row_name, nodes$label),
      val = abs(.data$value),
      value = .data$value,
      width = abs(val) * 10
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
