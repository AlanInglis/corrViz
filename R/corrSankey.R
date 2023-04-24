#' corrSankey
#'
#' Create an interactive Sankey diagram to visualize correlations
#'
#' This function generates a Sankey diagram for a given data frame, correlation method, and
#' correlation threshold, with an optional colour parameter.
#'
#' @param data A data frame containing the input data for calculating correlations.
#' @param method A character string specifying the correlation method. One of
#'   "pearson", "kendall", or "spearman". Default is "pearson".
#' @param threshold A numeric value indicating the minimum absolute correlation value to
#'  include in the diagram.
#'                  Default is 0 (include all correlations).
#' @param colour A logical value indicating whether to color the links based on positive or
#' negative correlation. Default is FALSE (links are grey).
#'
#' @importFrom plotly plot_ly
#'
#' @return A plotly Sankey diagram object.
#'
#' @examples
#' corrSankey(data = mtcars, method = 'pearson', threshold = 0.6)
#' corrSankey(data = mtcars, method = 'pearson', threshold = 0.8, colour = TRUE)
#'
#' @export


corrSankey <- function(data,
                       method = c("pearson", "kendall", "spearman"),
                       threshold = 0,
                       colour = FALSE){

  correlation_matrix <- cor(data, method = method)

  # Create a data frame of correlations above a certain threshold

  correlations <- data.frame(row = integer(), col = integer(), value = numeric())

  for (i in 1:(ncol(correlation_matrix) - 1)) {
    for (j in (i + 1):ncol(correlation_matrix)) {
      if (abs(correlation_matrix[i, j]) > threshold) {
        correlations <- rbind(correlations, data.frame(row = i, col = j, value = correlation_matrix[i, j]))
      }
    }
  }

  # Prepare the nodes and links data frames for the Sankey diagram
  nodes <- data.frame(name = colnames(correlation_matrix))
  links <- data.frame(source = correlations$row - 1,
                      target = correlations$col - 1,
                      value = abs(correlations$value),
                      correlation = correlations$value,
                      act_val = round(correlations$value,2))

  if(colour){
   col <-  ifelse(links$correlation > 0, "rgba(255,0,0,0.7)", "rgba(0,0,255,0.7)")
  }else{
    col <- 0
  }

  # plot
  p <- plotly::plot_ly(
    type = "sankey",
    valueformat = ".2f",
    node = list(
      label = nodes$name,
      customdata = nodes$name,
      hovertemplate = 'Node: %{label}<extra></extra>'
    ),
    link = list(
      source = links$source,
      target = links$target,
      value = links$value,
      customdata = links$act_val,
      hovertemplate = paste('Correlation: %{customdata}<br />',
                            'From: %{source.customdata}<br />',
                            'To: %{target.customdata}<extra></extra>'),
      color = col
    )
  )

  return(p)
}

corrSankey(mtcars, method = 'p', threshold = 0.6)
corrSankey(mtcars, method = 'p', threshold = 0.8, colour = T)





