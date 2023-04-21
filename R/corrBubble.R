#' corrBubble
#'
#' This function creates an interactive bubble plot of
#' correlations between variables in a dataset.
#'
#' @param data A dataframe containing the data to be analyzed.
#' @param method A character string specifying the correlation method. One of
#'   "pearson", "kendall", or "spearman". Default is "pearson".
#' @param display A character vector specifying which part of the correlation
#' matrix to display: 'all', 'upper', or 'lower', default is 'all'.
#' @param pal A color palette for the bubble plot.
#'
#' @return An interactive bubble plot displaying correlations.
#'
#' @details Creates an interactive bubble plot displaying correlation values. By hovering
#' mouse over a cell, the variables and correlation value is shown.
#'
#' @importFrom plotly ggplotly
#' @importFrom stats cor
#' @importFrom grDevices colorRampPalette
#' @import ggplot2
#'
#' @examples
#' corrBubble(data = mtcars,
#'            method = 'pearson',
#'            display = 'all')
#'
#'
#' @export

corrBubble <- function(data,
                        method = c("pearson", "kendall", "spearman"),
                        display = c('all', 'upper', 'lower'),
                        pal = colorRampPalette(c("cornflowerblue", 'white', 'tomato'))(100)){

  # declare global vars
  row_name <- col_name <- correlation <- NULL
  correlations <- cor(data, method = method)
  diag(correlations) <- NA

  # choose upper or lower
  display <- match.arg(display)

  switch(display,
         "lower" = {
           correlations[upper.tri(correlations, diag = TRUE)] <- NA
         },
         "upper" = {
           correlations[lower.tri(correlations, diag = TRUE)] <- NA
         },
         "all" = {
           correlations <- correlations
         })


  # turn into df
  dfm <- matrix2long(correlations)
  colnames(dfm)[3] <- "correlation"
  dfm$col <- ifelse(dfm$correlation <= 0, "blue", "red")



  # order factors
  label_names <- colnames(correlations)
  dfm$row_name <- factor(dfm$row_name, levels = label_names)
  dfm$col_name <- factor(dfm$col_name, levels = label_names)
  dfm$correlation <- round(dfm$correlation, 3)

  # create plot
  bubble_chart <-  ggplot() +
    geom_point(mapping = aes(x = col_name, y = row_name, color = correlation),
               data = dfm,  size = abs(dfm$correlation)*10) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(limits = rev(levels(dfm$col_name))) +
    scale_color_gradientn(colors = pal,
                          na.value = 'black',
                          guide = 'colorbar',
                          aesthetics = 'color',
                          limits = c(-1,1)) +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(legend.position = 'none')



  # create interactive plot
  p <- ggplotly(bubble_chart,  tooltip = c("row_name", "col_name", 'correlation'))

  return(p)
}



