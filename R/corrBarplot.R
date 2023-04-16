#' corrBarplot
#'
#' Creates either a static or interactive bar plot displaying correlations.
#'
#' @param data A data frame.
#' @param method Which correlation coefficient (or covariance) is to be computed.
#' One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#' @param interactive If TRUE then an interactive version of the barplot is displayed.
#' @param pal The colour palette to use for displaying values.
#'
#' @return A static or interactive bar plot displaying correlations.
#'
#' @details Creates a static or interactive bar plot displaying correlation values. By hovering
#' mouse over a bar, the variables and correlation value is shown.
#'
#' @importFrom plotly ggplotly
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr as_tibble
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr as_tibble
#' @importFrom reshape2 melt
#' @importFrom forcats fct_reorder
#' @import ggplot2
#'
#' @examples
#' corrHeatmap(data = mtcars, display = 'all')
#'
#'
#' @export


corrBarplot <-  function(data,
                         method = c("pearson", "kendall", "spearman"),
                         interactive = FALSE,
                         pal = colorRampPalette(c("cornflowerblue", 'white', 'tomato'))(100)
                         ){


  # get triangular correlations
  triangle_correlations <- cor(data) * lower.tri(cor(data))

  # reshape and add name column
  correlations <- triangle_correlations |>
    as_tibble(rownames = 'variable1') |>
    pivot_longer(cols = -1, names_to = 'variable2', values_to = 'correlation') |>
    filter(abs(correlation) > 0) |>
    mutate(
      pair = paste(variable1, variable2, sep = ' + '),
      pair = forcats::fct_reorder(pair, correlation)
    )

  correlations$correlation <- round(correlations$correlation, 3)

  plotFun <- function(nudge){
    pp <- correlations  |>
      ggplot(aes(y = pair, x = correlation, fill = correlation)) +
      geom_col() +
      scale_fill_gradientn(
        colors = pal, limits = c(-1,1), name = "Correlation",
        guide = guide_colorbar(
          order = 1,
          frame.colour = "black",
          ticks.colour = "black"
        ), oob = scales::squish
      ) +
      geom_text(
        aes(x = 0, label = pair),
        size = 2.5,
        hjust = ifelse(correlations$correlation > 0, 1, 0),
        nudge_x = ifelse(correlations$correlation > 0, -nudge, nudge)
      ) +
      labs(x = 'Correlation', y = '') +
      theme_bw() +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    return(pp)
  }



  if(interactive){
    pp <- ggplotly(plotFun(nudge = 0.2), tooltip = c('x', 'y'))
  }else{
    pp <- plotFun(nudge = 0.01)
  }

  return(pp)
}
