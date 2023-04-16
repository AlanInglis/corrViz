#' corrBubble
#'
#' Creates an interactive bubble plot displaying correlations.
#'
#' @param data A data frame.
#' @param method Which correlation coefficient (or covariance) is to be computed.
#' One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#' @param display Choose to display the upper, lower, or full martix of vaules using
#' "upper", "lower", or "all", respectively.
#' @param pal The colour palette to use for displaying values.
#'
#' @return An interactive bubble plot displaying correlations.
#'
#' @details Creates an interactive bubble plot displaying correlation values. By hovering
#' mouse over a cell, the variables and correlation value is shown.
#'
#' @importFrom plotly ggplotly
#' @importFrom reshape2 melt
#' @import ggplot2
#'
#' @examples
#' corrBubble(data = mtcars, display = 'all')
#'
#'
#' @export

corrBubble <- function(data,
                        method = c("pearson", "kendall", "spearman"),
                        display = c('all', 'upper', 'lower'),
                        pal = colorRampPalette(c("cornflowerblue", 'white', 'tomato'))(100)){

  correlations <- cor(data, method = method)
  diag(correlations) <- NA

  # choose upper or lower
  display <- match.arg(display)

  switch(display,
         "lower" = {
           correlations[lower.tri(correlations, diag = TRUE)] <- NA
         },
         "upper" = {correlations[upper.tri(correlations, diag = TRUE)] <- NA
         },
         "all" = {
           correlations <- correlations
         })


  # turn into df
  dfm <- reshape2::melt(correlations)
  colnames(dfm)[3] <- "correlation"
  dfm$col <- ifelse(dfm$correlation <= 0, "blue", "red")



  # order factors
  label_names <- colnames(correlations)
  dfm$Var1 <- factor(dfm$Var1, levels = label_names)
  dfm$Var2 <- factor(dfm$Var2, levels = label_names)
  dfm$correlation <- round(dfm$correlation, 3)

  # create plot
  bubble_chart <-  ggplot() +
    geom_point(mapping = aes(x = Var1, y = Var2, color = correlation),
               data = dfm,  size = abs(dfm$correlation)*10) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(limits = rev(levels(dfm$Var2))) +
    scale_color_gradientn(colors = pal,
                          na.value = 'black',
                          guide = 'colorbar',
                          aesthetics = 'color',
                          limits = c(-1,1)) +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(legend.position = 'none')



  # create interactive plot
  p <- ggplotly(bubble_chart,  tooltip = c("Var1", "Var2", 'correlation'))

  return(p)
}



