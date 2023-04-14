#' animSolar
#'
#' Creates an animated solar system correlation plot.
#'
#' @param data A data frame.
#' @param method Which correlation coefficient (or covariance) is to be computed.
#' One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#' @param sun A variable name to be chosen as the dependent variable.
#' @param path Path of the directory to save plot to. Defaults to the working directory.
#' @param gifName File name to create on disk. should be in the format "myfile.gif"
#' @param fps The frame rate of the animation in frames/sec. default is 60 fps.
#'
#' @return An animated solar system plot displaying correlations.
#'
#' @details In a solar system correlation plot, the dependent variable of
#' interest is positioned at the center, represented as the sun.
#' The explanatory variables are depicted as planets orbiting
#' around the sun, with their distance from the sun corresponding
#' to the absolute value of their correlation with the dependent variable.
#' Therefore, the greater the distance of a planet from the sun,
#' the weaker the correlation between the explanatory variable
#' and the dependent variable.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr arrange
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom dplyr n
#' @importFrom ggforce geom_circle
#' @importFrom gganimate transition_manual
#' @importFrom gganimate animate
#' @importFrom gganimate anim_save
#' @importFrom gganimate gifski_renderer
#' @import ggplot2
#'
#' @export
animSolar <- function(data,
                      method = c("pearson", "kendall", "spearman"),
                      sun = NULL,
                      path = NULL,
                      gifName = 'solar_system.gif',
                      fps = 60) {

  if (!requireNamespace("gifski", quietly = TRUE)){
   stop('ERROR: gifski package is required to render animation.
        Please use: install.packages("gifski")')
  }
  # Calculate correlation matrix
  cor_matrix <- cor(data, method = method)
  diag(cor_matrix) <- NA

  # Convert matrix to data frame
  df_data <- data.frame(cor_matrix)

  # Add row and column names as separate columns
  df_data$row_name <- row.names(cor_matrix)
  df_data$col_name <- colnames(cor_matrix)

  # Reshape data frame to long format
  df_data_long <- reshape(df_data,
    direction = "long",
    varying = list(colnames(cor_matrix)),
    v.names = "value",
    timevar = "col_name",
    times = colnames(cor_matrix)
  )

  correlations <- df_data_long |>
    filter(col_name == sun)  |>
    arrange(desc(value)) |>
    rename(
      r = value,
      x = col_name,
      y = row_name
    )



  # Assign orbit radius based on absolute, rounded correlation values
  correlations$r <- round(correlations$r, 1)
  correlations <- correlations  |>
    mutate(
      orbit_radius = 1 - round(abs(r), 1),
      angle = 2 * pi * row_number() / n()
    )

  # add correlation colour
  correlations$col <- ifelse(correlations$r <= 0, "blue", "red")
  correlations <- na.omit(correlations)

  # create animated plot
  nframes <- 100
  seqFrames <- (ncol(data) - 1)

  #ang <- rep(seq(0, 2 * pi, length.out = seqFrames), 10)
  ang <- rep(seq(0, 2 * pi, length.out = nframes), seqFrames)
  ang <- ang + rep(correlations$angle, each = nframes)

  solar_system <- data.frame(
    y = rep(correlations$y, each = nframes),
    x = rep(correlations$x, each = nframes),
    r = rep(correlations$r, each = nframes),
    id = rep(correlations$id, each = nframes),
    orbit_radius = rep(correlations$orbit_radius, each = nframes),
    angle = ang,
    col = rep(correlations$col, each = nframes),
    frame = rep(1:nframes, seqFrames)
  )


  p <- ggplot(solar_system, aes(x = orbit_radius * cos(angle), y = orbit_radius * sin(angle))) +
    ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = orbit_radius, color = stage(y, after_scale = alpha(color, 0.5))),
      linetype = "solid", alpha = 0.5
    ) +
    geom_point(aes(size = orbit_radius, color = y), alpha = 0.8) +
    geom_text(aes(
      x = orbit_radius * cos(angle),
      y = orbit_radius * sin(angle),
      label = y
    ), hjust = -0.5, vjust = 0.5) +
    geom_point(aes(x = 0, y = 0), size = 6, color = "yellow") +
    geom_text(aes(x = 0, y = 0, label = sun), hjust = -0.5, vjust = -1) +
    geom_text(
      data = correlations, aes(x = 0, y = orbit_radius, label = round(abs(r), 1)),
      size = 3.5, alpha = 0.3
    ) +
    coord_fixed() +
    theme_void() +
    theme(legend.position = "none") +
    transition_manual(frame)


  animation <- animate(p,
    nframes = 500, fps = fps, end_pause = 0,
    width = 800, height = 800, units = "px",
    renderer = gifski_renderer()
  )
  anim_save(gifName, animation, path = path)
}



