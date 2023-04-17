#' corrHeatmap
#'
#' Creates an interactive heatmap plot displaying correlations.
#'
#' @param data A data frame.
#' @param method Which correlation coefficient (or covariance) is to be computed.
#' One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#' @param display Choose to display the upper, lower, or full matrix of values using
#' "upper", "lower", or "all", respectively.
#' @param reorder If TRUE (the default) then the heatmap is reordered to group correlations.
#' @param pal The colour palette to use for displaying values.
#'
#' @return An interactive heatmap plot displaying correlations.
#'
#' @details Creates an interactive heatmap displaying correlation values. By hovering
#' mouse over a cell, the variables and correlation value is shown.
#'
#' @importFrom plotly ggplotly
#' @importFrom reshape2 melt
#' @importFrom DendSer dser
#' @import ggplot2
#' @importFrom stats as.dist
#' @importFrom stats cor
#' @importFrom stats na.omit
#' @importFrom stats reshape
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#' corrHeatmap(data = mtcars,
#'              method = 'pearson',
#'              display = 'all')
#'
#'
#' @export

corrHeatmap <- function(data,
                        method = c("pearson", "kendall", "spearman"),
                        display = c('all', 'upper', 'lower'),
                        reorder = TRUE,
                        pal = colorRampPalette(c("darkblue", 'white', 'darkred'))(100)){

  # Declare global vars
  Var1 <- Var2 <- correlation <- NULL
  # get correlations
  correlations <- cor(data, method = method)
  diag(correlations) <- NA

  if(reorder){
    corrReorder <- function(d) {
      d1 <- d
      d1[is.na(d)] <- 0
      correl <- as.dist(d1)
      rcorrel <- range(correl)
      if (rcorrel[2] != rcorrel[1]) {
        correl <- (correl - rcorrel[1]) / (rcorrel[2] - rcorrel[1])
      }
      score <- apply(as.matrix(correl), 1, max)
      o <- DendSer::dser(-correl, -score, cost = DendSer::costLS)
      res <- d[o, o]
      class(res) <- class(d)
      res
    }

    correlations <- corrReorder(correlations)
  }

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
  label_names <- colnames(correlations)
  dfm$Var1 <- factor(dfm$Var1, levels = label_names)
  dfm$Var2 <- factor(dfm$Var2, levels = label_names)

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


