#' corrHeatmap
#'
#' Creates an interactive heatmap plot displaying correlations.
#'
#' @param data A data frame.
#' @param method Which correlation coefficient (or covariance) is to be computed.
#' One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#' @param display Choose to display the upper, lower, or full martix of vaules using
#' "upper", "lower", or "all", respectively.
#' @param pal The colour palette to use for displaying values.
#'
#' @return An interactive heatmap plot displaying correlations.
#'
#' @details Creates an interactive heatmap displaying correlation values. By hovering
#' mouse over a cell, the variables and correlation value is shown.
#'
#' @importFrom plotly ggplotly
#' @importFrom reshape2 melt
#' @import ggplot2
#'
#' @examples
#' corrHeatmap(data = mtcars, display = 'all')
#'
#'
#' @export

corrHeatmap <- function(data,
                        method = c("pearson", "kendall", "spearman"),
                        display = c('all', 'upper', 'lower'),
                        pal = colorspace::diverging_hcl(palette = "Blue-Red", n = 100)){


  # get correlations
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
  dfm$correlation <- round(dfm$correlation, 3)

  # order factors
  labelNames <- colnames(correlations)
  dfm$Var1 <- factor(dfm$Var1, levels = labelNames)
  dfm$Var2 <- factor(dfm$Var2, levels = labelNames)

  # Plot heatmap using ggplot2
  p <- ggplot(dfm, aes(x = Var1, y = Var2, fill = correlation)) +
    geom_tile() +
    scale_x_discrete(position = "top") +
    scale_y_discrete(limits = rev(levels(dfm$Var2))) +
    scale_fill_gradientn(
      colors = pal, limits = c(-1,1), name = "Correlation",
      guide = guide_colorbar(
        order = 1,
        frame.colour = "black",
        ticks.colour = "black"
      ), oob = scales::squish
    ) +
    labs(x = "", y = "") +
    theme_bw()


  interactive_heatmap <-  ggplotly(p)
  return(interactive_heatmap)

}


