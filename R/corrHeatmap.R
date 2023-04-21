#' corrHeatmap
#'
#' This function creates an interactive heatmap of correlations between variables in a dataset.
#'
#' @param data A dataframe containing the data to be analyzed.
#' @param method A character string specifying the correlation method. One of
#'   "pearson", "kendall", or "spearman". Default is "pearson".
#' @param display A character vector specifying which part of the correlation matrix to
#' display: 'all', 'upper', or 'lower', default is 'all'.
#' @param reorder A logical value indicating whether to reorder the heatmap based
#' on hierarchical clustering, default is TRUE.
#' @param pal A color palette for the heatmap.
#'
#' @return An interactive heatmap plot displaying correlations.
#'
#' @details Creates an interactive heatmap displaying correlation values. By hovering
#' mouse over a cell, the variables and correlation value is shown.
#'
#' @importFrom plotly ggplotly
#' @importFrom DendSer dser
#' @import ggplot2
#' @importFrom stats as.dist
#' @importFrom stats cor
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
  row_name <- col_name <- correlation <- NULL
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
  dfm <- matrix2long(correlations)
  colnames(dfm)[3] <- "correlation"
  dfm$correlation <- round(dfm$correlation, 3)

  # order factors
  label_names <- colnames(correlations)
  dfm$row_name <- factor(dfm$row_name, levels = label_names)
  dfm$col_name <- factor(dfm$col_name, levels = label_names)

  # Plot heatmap using ggplot2
  p <- ggplot(dfm, aes(x = row_name, y = col_name, fill = correlation)) +
    geom_tile() +
    scale_x_discrete(position = "top") +
    scale_y_discrete(limits = rev(levels(dfm$col_name))) +
    scale_fill_gradientn(
      colors = pal,
      limits = c(-1,1),
      name = "Correlation",
      aesthetics = 'fill',
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


