#' corrPairs
#'
#' This function creates a pairwise correlation plot with annotated correlation
#' coefficients and optional coloring by a specified variable. The plot can be
#' interactive or static.
#'
#' @param data A data frame containing the variables to be plotted.
#' @param method A character string specifying the correlation method. One of
#'   "pearson", "kendall", or "spearman". Default is "pearson".
#' @param interactive A logical value indicating whether the output plot should
#'   be interactive (TRUE) or static (FALSE). Default is TRUE.
#' @param col_by An optional character string specifying the name of the
#'   column in the data frame to be used for coloring points. Default is NULL.
#'
#' @return A ggplotly object (if interactive = TRUE) or a ggplot object (if
#'   interactive = FALSE) displaying the pairwise correlation plot with
#'   annotated correlation coefficients and optional coloring by the specified
#'   variable.
#'
#' @import ggplot2
#' @importFrom GGally wrap
#' @importFrom GGally ggpairs
#' @importFrom GGally eval_data_col
#' @importFrom plotly ggplotly
#' @importFrom plotly style
#' @importFrom stats cor.test
#' @importFrom grDevices rainbow
#'
#' @examples
#' corrPairs(data = mtcars[,1:4],
#'          method = "pearson",
#'          interactive = TRUE,
#'          col_by = "cyl")
#'
#' @export



corrPairs <- function(data,
               method = c("pearson", "kendall", "spearman"),
               interactive = TRUE,
               col_by = NULL) {

  # declare globals
  cor_fun <- dot_fun <- NULL

  # correlation function
  cor_fun <- function(data, mapping, method = "pearson", ndp = 2, sz = 5, stars = TRUE, ...) {
    x <- eval_data_col(data, mapping$x)
    y <- eval_data_col(data, mapping$y)

    corr <- cor.test(x, y, method = method)
    est <- corr$estimate
    lb.size <- sz * abs(est)

    if (stars) {
      stars <- c("***", "**", "*", "")[findInterval(corr$p.value, c(0, 0.001, 0.01, 0.05, 1))]
      lbl <- paste0(round(est, ndp), stars)
    } else {
      lbl <- round(est, ndp)
    }

    if (est < 0) {
      ggplot(data = data, mapping = mapping) +
        annotate("text",
          x = mean(x, na.rm = TRUE), y = mean(y, na.rm = TRUE),
          label = lbl, color = "blue", size = lb.size, ...
        ) +
        theme_bw() +
        theme(panel.grid = element_blank())
    } else {
      ggplot(data = data, mapping = mapping) +
        annotate("text",
          x = mean(x, na.rm = TRUE), y = mean(y, na.rm = TRUE),
          label = lbl, color = "red", size = lb.size, ...
        ) +
        theme_bw() +
        theme(panel.grid = element_blank())
    }
  }

  dot_fun <- function(data, mapping, col_by = col_by) {
    x <- eval_data_col(data, mapping$x)
    y <- eval_data_col(data, mapping$y)

    if (cor(x, y) <= 0) {
      line_colour <- "blue"
    } else {
      line_colour <- "red"
    }

    if (!is.null(col_by)) {
      colnum <- which(colnames(data) == col_by)
      nam <- names(data)[colnum]

      colnum <- which(colnames(data) == col_by)
      col_by1 <- levels(as.factor(data[, colnum]))
      colours <- c("purple", "green", "yellow", "pink", 'skyblue', 'darkred')
      if (length(col_by1) <= length(colours)) {
        data$colour <- colours[factor(data[, colnum])]
      } else {
        data$colour <- rainbow(length(data[, colnum]))
      }
      textt <- paste0(nam, ":", data[, colnum])
    } else {
      data$colour <- "black"
      col_by1 <- 1
      textt <- ""
    }

    ggplot(data = data, mapping = mapping) +
      geom_point(aes(x = x, y = y, text = textt),
        color = data$colour
      ) +
      geom_smooth(aes(x = x, y = y),
        formula = y ~ x,
        method = "loess",
        colour = line_colour,
        se = F
      )
  }


  p <- GGally::ggpairs(data,
    lower = list(continuous = wrap(dot_fun, col_by = col_by)),
    diag = list(continuous = wrap("densityDiag", fill = "skyblue")),
    upper = list(continuous = wrap(cor_fun))
  )


  if(interactive){
    w <- ggplotly(p)


    # Remove some plotly hover text -------------------------------------------

    # This function finds the index number
    # of the trend line and turns off its
    # hover text
    seq_fun_line <- function(size) {
      a <- ((size - 1):1)
      b <- seq(from = 4, length = length(a), by = 1)
      w <- seq(from = 3, length.out = a[1], by = 2)

      res <- vector("list", length(a))
      res[[1]] <- w


      for (i in 1:(length(a) - 1)) {
        res[[i + 1]] <- seq(from = res[[i]][length(res[[i]])] + b[i], length.out = a[i + 1], by = 2)
      }

      unlist(res)
    }

    # This function finds the index number
    # of the correlation text and turns off its
    # hover text
    seq_fun_cor <- function(size) {
      num_groups <- size - 1
      seq_group <- 1:num_groups
      first_gap <- num_groups * 2
      all_gaps <- seq(from = first_gap, to = 0, by = -2)
      final_gaps <- all_gaps[c(1:(num_groups - 1))]

      position1 <- size * 2
      pos <- vector("list", num_groups)
      pos[[1]] <- position1

      gcn <- function(starting_number, size) {
        result_vector <- seq(from = starting_number, length.out = size)
        return(result_vector)
      }

      posv <- NULL
      for (i in 1:(num_groups - 1)) {
        posv[[i]] <- pos[[i]][[length(pos[[i]])]] + final_gaps[[i]]
        pos[[i + 1]] <- gcn(posv[[i]], seq_group[i + 1])
      }

      unlist(pos)
    }

    # Find index of trend line
    seq_off_line <- seq_fun_line(size = ncol(data))

    # Find index of correlation text
    seq_off_corr <- seq_fun_cor(size = ncol(data))

    seq_off <- c(seq_off_line, seq_off_corr)

    # Turn trend line text off
    style(w, hoverinfo = "none", traces = c(seq_off))
  }else{
    return(p)
  }

}

