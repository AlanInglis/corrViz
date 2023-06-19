#' corrBarplot
#'
#' This function creates a either a static or interactive
#' bar plot of correlations between variables in a dataset.
#'
#' @param mat A square correlation matrix to visualise.
#' @param interactive A logical value specifying whether to create an interactive ggplotly plot,
#' default is TRUE
#' @param pal A colour palette for the bar plot, default is colorRampPalette(c("cornflowerblue", "white", "tomato"))(100).
#'
#' @return A static or interactive bar plot displaying correlations.
#'
#' @details Creates a static or interactive bar plot displaying correlation values. By hovering
#' mouse over a bar, the variables and correlation value is shown.
#'
#' @importFrom plotly ggplotly
#' @import ggplot2
#' @importFrom stats cor
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#' cm <- cor(mtcars)
#'
#' corrBarplot(mat = cm,
#'            interactive = TRUE)
#'
#'
#' @export


corrBarplot <-  function(mat,
                         interactive = TRUE,
                         pal = colorRampPalette(c("cornflowerblue", 'white', 'tomato'))(100)
                         ){


  # declare global vars
  correlation <- variable1 <- variable2 <- pair <- value <- row_name <- col_name <- NULL
  # get triangular correlations
  triangle_correlations <- (mat) * lower.tri(mat)

  # reshape and add name column
  correlations_long <- matrix2long(triangle_correlations)


  # Filter rows with an absolute 'correlation' value greater than 0
  correlations_filtered <- subset(correlations_long, abs(value) > 0)

  # Add a new column 'pair' and reorder the data frame based on the 'correlation' column
  correlations_transformed <- transform(correlations_filtered,
                                        pair = paste(row_name, col_name, sep = " + "))


  correlations_transformed$pair <- factor(correlations_transformed$pair,
                                          levels = correlations_transformed$pair[order(correlations_transformed$value)])

  # Assign the result to correlations
  correlations <- correlations_transformed

  # Rename columns
  names(correlations)[1:3] <- c('variable1', 'variable2', 'correlation')

  # Round values
  correlations$correlation <- round(correlations$correlation, 3)

  # Plot function
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
  }



  if(interactive){
    pp <- ggplotly(plotFun(nudge = 0.2), tooltip = c('x', 'y'))
  }else{
    pp <- plotFun(nudge = 0.01)
  }

  return(pp)
}
