#' Plot a correlation grid
#'
#' @description Create a correlation grid plot to visualize correlations among the columns of a dataset
#'
#' @param data A data.frame, with numeric columns to compute correlations.
#' @param method A character string, specifying the correlation method,
#' one of "pearson", "kendall", or "spearman" (default: "pearson").
#' @param display A character string, specifying the display type,
#' one of "all", "upper", or "lower" (default: "all").
#' @param type A character string, specifying the shape of the correlation
#' coefficients, one of "square", "circle", or "text" (default: "square").
#' @param showDiag A logical value, if TRUE (default), the diagonal of the
#' correlation matrix is shown.
#' @param pal A color palette function, used for the correlation
#' coefficient colors.
#'
#' @return A correlation grid plot
#'
#' @import ggplot2
#' @importFrom purrr pmap
#'
#' @examples
#' data(mtcars)
#' corr_grid_plot <- corrGrid(data = mtcars,
#'                            type = 'square')
#
#' @export


corrGrid <- function(data,
                    method = c("pearson", "kendall", "spearman"),
                    display = c('all', 'upper', 'lower'),
                    type = c('square', 'circle', 'text', 'pie'),
                    showDiag = 'TRUE',
                    pal = colorRampPalette(c("darkblue", 'white', 'darkred'))(100)
){
  # Declare global vars
  row_name <- col_name <- correlation <- NULL
  # get correlations
  correlations <- cor(data, method = method)


  # if (type == "pie" && ((display == "upper" || display == "lower") || showDiag == FALSE)){
  #   stop('Error: pie plot can only be used when display = \"all\"
  #        and showDiag = \"TRUE\".')
  # }

  if(showDiag == FALSE){
    diag(correlations) <- NA
    remove_diag <- TRUE
  }else{
    remove_diag <- FALSE
  }

  # choose upper or lower
  display <- match.arg(display)

    switch(display,
           "lower" = {
             correlations[lower.tri(correlations, diag = remove_diag)] <- NA
           },
           "upper" = {correlations[upper.tri(correlations, diag = remove_diag)] <- NA
           },
           "all" = {
             correlations <- correlations
           })




  # turn into df
  dfm <- matrix2long(correlations)
  colnames(dfm)[3] <- "correlation"
  dfm$correlation <- round(dfm$correlation, 3)
  dfm$abs <- abs(dfm$correlation)

  # order factors
  label_names <- colnames(correlations)
  dfm$row_name <- factor(dfm$row_name, levels = label_names)
  dfm$col_name <- factor(dfm$col_name, levels = label_names)

  if(type == 'square'){
    class(type) <- 'square'
  }else if(type == 'circle'){
    class(type) <- 'circle'
  }else if(type == 'text'){
    class(type) <- 'text'
  }else if(type == 'pie'){
    class(type) <- 'pie'
  }

  pp <- corrp(data = dfm,
              method = method,
              display = display,
              type = type,
              showDiag = showDiag,
              pal = pal)

  suppressWarnings(
    print(pp)
  )
}


# -------------------------------------------------------------------------


# Main plot function:
corrp <- function(data,
                  type = c('square', 'circle', 'text', 'pie'),
                  pal = colorRampPalette(c("darkblue", 'white', 'darkred'))(100),
                  ...) {
    UseMethod("corrp", type)
  }




# Square ------------------------------------------------------------------

corrp.square <- function(data,
                         type = c('square', 'circle', 'text', 'pie'),
                         pal = colorRampPalette(c("darkblue", 'white', 'darkred'))(100),
                         ...){
  # plot
  p <- ggplot(data, aes(x = row_name, y = col_name, fill = correlation)) +
    geom_tile(aes(width = 1, height = 1), fill = NA, color = "grey50") +
    geom_tile(aes(width = tile_size(abs), height = tile_size(abs))) +
    scale_x_discrete(position = "top", expand = c(0, 0)) +
    scale_y_discrete(limits = rev(levels(data$row_name)), expand = c(0, 0)) +
    scale_fill_gradientn(
      colors = pal,
      limits = c(-1, 1),
      guide = guide_colorbar(
        order = 1,
        frame.colour = "black",
        ticks.colour = "black"
      ), oob = scales::squish
    ) +
    labs(x = "", y = "") +
    theme_bw() +
    theme(
      aspect.ratio = 1,
      panel.grid = element_blank()
    )

  return(p)
}


# Circle ------------------------------------------------------------------

corrp.circle <- function(data,
                         type = c('square', 'circle', 'text', 'pie'),
                         pal = colorRampPalette(c("darkblue", 'white', 'darkred'))(100),
                         ...){
  # plot
  # plot
  if(length(unique(data$row_name)) < 8){
    const <- 1/ncol(data) * 70
  }else if(length(unique(data$row_name)) >=8 && length(unique(data$row_name)) <= 10){
    const <- 1/ncol(data) * 60
  }else if(length(unique(data$row_name)) >=11 && length(unique(data$row_name)) <= 12){
    const <- 1/ncol(data) * 50
  }else{
    const <- 1/ncol(data) * 25
  }



  p <- ggplot(data, aes(x = row_name, y = col_name, color = correlation)) +
    geom_tile(aes(width = 1, height = 1), fill = NA, color = "grey50") +
    geom_point(size = tile_size(data$abs)*const) +
    scale_x_discrete(position = "top", expand = c(0, 0)) +
    scale_y_discrete(limits = rev(levels(data$row_name)), expand = c(0, 0)) +
    scale_color_gradientn(
      colors = pal,
      limits = c(-1, 1),
      guide = guide_colorbar(
        order = 1,
        frame.colour = "black",
        ticks.colour = "black"
      ), oob = scales::squish
    ) +
    labs(x = "", y = "") +
    theme_bw() +
    theme(
      aspect.ratio = 1,
      panel.grid = element_blank()
    )

  return(p)
}

# Text -----------------------------------------------------------------


corrp.text <- function(data,
                       type = c('square', 'circle', 'text', 'pie'),
                       pal = colorRampPalette(c("darkblue", 'white', 'darkred'))(100),
                         ...){
  # plot
  p <- ggplot(data, aes(x = row_name, y = col_name, color = correlation)) +
    geom_tile(aes(width = 1, height = 1), fill = NA, color = "grey50") +
    geom_text(aes(label = round(correlation, 2))) +
    scale_x_discrete(position = "top", expand = c(0, 0)) +
    scale_y_discrete(limits = rev(levels(data$row_name)), expand = c(0, 0)) +
    scale_color_gradientn(
      colors = pal,
      limits = c(-1, 1),
      guide = guide_colorbar(
        order = 1,
        frame.colour = "black",
        ticks.colour = "black"
      ), oob = scales::squish
    ) +
    labs(x = "", y = "") +
    theme_bw() +
    theme(
      aspect.ratio = 1,
      panel.grid = element_blank()
    )

  return(p)

}

# Pie ------------------------------------------------------------------

corrp.pie <- function(data,
                     type = c('square', 'circle', 'text', 'pie'),
                     pal = colorRampPalette(c("darkblue", 'white', 'darkred'))(100),
                     ...){
  # plot
  ggrid <- ggplot(data, aes(x = row_name, y = col_name, fill = correlation)) +
    geom_tile(aes(width = 1, height = 1), fill = NA, color = "grey50") +
    geom_point(shape=22, alpha=0) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_fill_gradientn(
      colors = pal,
      limits = c(-1, 1),
      guide = guide_colorbar(
        order = 1,
        frame.colour = "black",
        ticks.colour = "black"
      ), oob = scales::squish
    ) +
    theme(
      aspect.ratio = 1,
      panel.grid = element_blank()
    ) +
    theme_bw() +
    labs(x = "", y = "") +
    theme(
      aspect.ratio = 1,
      panel.grid = element_blank()
    )


  # -------------------------------------------------------------------------

  # create multiple df for plotting pie charts
  dft <- NULL
  for(i in 1:nrow(data)){
    dft[[i]] <- data.frame(group = c(data[i,1], data[i, 2]),
                           value = c(data[i,3]),
                           absVal = c(data[i,5], 1 - data[i,5]))

    dft[[i]]$group <- sort(dft[[i]]$group)
  }

  # create plot list
  plotList <- list()
  plotFun <- function(data){

    for(i in 1:length(dft)){
      plotList[[i]] <- ggplot(data[[i]], aes(x="", y=absVal, fill=value)) +
        geom_bar(stat="identity", width=1, color = 'black') +
        scale_fill_gradientn(
          colors = pal,
          limits = c(-1, 1),
          guide = guide_colorbar(
            order = 1,
            frame.colour = "black",
            ticks.colour = "black"
          ), oob = scales::squish
        )+
        coord_polar("y", start=0) +
        theme_void()
    }
    return(plotList)
  }

  all_plots <- plotFun(dft)

  # -------------------------------------------------------------------------

  build <- NULL
  final <- NULL
  for(i in 1:length(all_plots)){
    build[[i]] <- ggplot_build(all_plots[[i]])$data
    build[[i]][[1]]$y[2] <- 1- build[[i]][[1]]$y[1]

    if(dft[[i]]$value[1] < 0){
      build[[i]][[1]]$fill[1] <- build[[i]][[1]]$fill[2]

    }
    build[[i]][[1]]$fill[2] <- '#ffffff'

    final[[i]] <- ggplot(build[[i]][[1]], aes(x="", y=y, fill= dft[[i]]$group)) +
      geom_col(color = 'black')  +
      coord_polar("y", start=0) +
      scale_fill_manual(values = build[[i]][[1]]$fill) +
      theme_void() +
      theme(legend.position = 'none')
  }

  # change this to make it work for some unknown reason
  i = 2


  # plot pies on a grid
  pie_grid <- expand.grid(
    as.numeric(factor(unique(data$row_name))),
    as.numeric(factor(unique(data$col_name)))
  )
  pie_grid$plot <- final


  pp<-  ggrid +
    purrr::pmap(pie_grid, function(Var1, Var2, plot) {
      annotation_custom(
        grob = ggplotGrob(plot),
        xmin = Var1 - .5, xmax = Var1 + .5,
        ymin = Var2 - .5, ymax = Var2 + .5
      )
    })

  return(pp)
}




# tile size ---------------------------------------------------------------

tile_size <- function(x, min_size = 0.5, max_size = 1) {
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  rescaled_x <- ((x - min_x) / (max_x - min_x)) * (max_size - min_size) + min_size
  return(rescaled_x)
}

