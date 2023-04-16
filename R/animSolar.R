#' animSolar
#'
#' Creates an animated solar system correlation plot.
#'
#' @param data A data frame.
#' @param method Which correlation coefficient (or covariance) is to be computed.
#' One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#' @param sun A variable name to be chosen as the dependent variable.
#' @param export If TRUE, then the animation is exported to the users machine.
#' If FALSE (default), then the animation is played in the viewer window.
#' @param num_frames The number of frames.
#' @param path Path of the directory to save plot to. Defaults to the working directory.
#' @param gif_name File name to create on disk. Must be in the format "myFile.gif"
#' @param fps The frames rate of the exported animation in frames/sec. Default is 60 fps and
#' is only used when exporting a gif via \code{export = TRUE}.
#'
#' @details the \code{num_frames} argument is used the number of frames.
#' Setting this to a low value will produce the plot
#' quicker, however having a low number of frames will result in the "planets" jumping
#' as the frames transition. Additionally, a low values of   \code{num_frames} will affect the
#' orbit of the animation when setting \code{export = FALSE}. This differs from the
#' \code{fps} argument which sets the number of frames to play per second for use
#' when exporting a gif.
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
                      export = FALSE,
                      num_frames = 100,
                      path = NULL,
                      gif_name = "solar_system.gif",
                      fps = 60) {
  if(export){
    if (!requireNamespace("gifski", quietly = TRUE)) {
      stop('ERROR: gifski package is required to render animation.
        Please use: install.packages("gifski")')
    }
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
    filter(col_name == sun) |>
    arrange(desc(value)) |>
    rename(
      r = value,
      x = col_name,
      y = row_name
    )
  # check if any correaltions would round to 1 and change to 0.9
  correlations$r <- ifelse(correlations$r > 0.9, 0.9, correlations$r)


  # Assign orbit radius based on absolute, rounded correlation values
  correlations$r <- round(correlations$r, 1)
  correlations <- correlations |>
    mutate(
      orbit_radius = 1 - round(abs(r), 1),
      angle = 2 * pi * row_number() / n()
    )

  # add correlation colour
  correlations$col <- ifelse(correlations$r <= 0, "blue", "red")
  correlations <- na.omit(correlations)

  # create animated plot
  nframes <- num_frames
  seqFrames <- (ncol(data) - 1)

  # ang <- rep(seq(0, 2 * pi, length.out = seqFrames), 10)
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


  # Function to generate points along the circumference of a circle
  circle_points <- function(radius, n_points = 100) {
    tibble::tibble(
      x = radius * cos(seq(0, 2 * pi, length.out = n_points)),
      y = radius * sin(seq(0, 2 * pi, length.out = n_points)),
      r = radius
    )
  }


  # Modify the data.frame to generate circle points for each orbit_radius
  circle_data <- dplyr::bind_rows(lapply(unique(correlations$orbit_radius), circle_points), .id = "id")


  # create df of circle names to display
  circle_name <- data.frame(
    nam = unique(round(abs(correlations$r), 1)),
    nam2 = unique(round(abs(correlations$orbit_radius), 1))
  )

  suppressWarnings({
    p <- ggplot() +
      geom_path(data = circle_data, aes(x = x, y = y, group = id, color = id), linetype = "solid", alpha = 0.5) +
      geom_point(data = solar_system, aes(
        x = orbit_radius * cos(angle),
        y = orbit_radius * sin(angle),
        size = 3,
        color = y,
        frame = frame,
        label = r
      ), alpha = 0.8) +
      geom_text(data = solar_system, aes(
        x = orbit_radius * cos(angle),
        y = orbit_radius * sin(angle),
        label = y,
        frame = frame
      ), hjust = -0.5, vjust = 0.5) +
      geom_point(data = solar_system, aes(x = 0, y = 0), size = 6, color = "yellow") +
      geom_text(data = solar_system, aes(x = 0, y = 0, label = sun), hjust = -0.5, vjust = -1) +
      geom_text(
        data = correlations, aes(x = 0, y = orbit_radius, label = round(abs(r), 1)),
        size = 3.5, alpha = 0.3
      ) +
      coord_fixed() +
      theme_void() +
      theme(legend.position = "none") +
      transition_manual(frame)
  })

  if (export) {
    animation <- animate(p,
      nframes = 500, fps = fps, end_pause = 0,
      width = 800, height = 800, units = "px",
      renderer = gifski_renderer()
    )
    anim_save(gif_name, animation, path = path)
  } else {
    ggplotly(p, tooltip = c("label"))
  }
}
