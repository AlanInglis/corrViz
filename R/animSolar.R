#' animSolar
#'
#' This function creates an animated solar system plot of correlations between
#' variables in a dataset.
#'
#' @param data A dataframe containing the data to be analyzed.
#' @param method A character string specifying the correlation method. One of
#'   "pearson", "kendall", or "spearman". Default is "pearson".
#' @param sun A character string specifying the column name in the dataset to be treated
#' as the 'sun' in the solar system plot.
#' @param export A logical value specifying whether to export the animation as a GIF file, default is FALSE.
#' @param num_frames An integer specifying the number of frames in the animation, default is 100.
#' @param path A character string specifying the directory path where the GIF file will be saved, default is NULL.
#' @param gif_name A character string specifying the name of the GIF file. Must be in the format "myFile.gif"
#' @param fps An integer specifying the frames per second for the animation.
#' Default is 60 and is only used when exporting a gif via \code{export = TRUE}.
#'
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
#' @details The \code{num_frames} argument is used to select the number of frames.
#' Setting this to a low value will produce the plot
#' quicker, however having a low number of frames will result in the "planets" jumping
#' as the frames transition. Additionally, a low values of   \code{num_frames} will affect the
#' orbit of the animation when setting \code{export = FALSE}. This differs from the
#' \code{fps} argument which sets the number of frames to play per second for use
#' when exporting a gif.
#'
#' @return An animated solar system plot displaying correlations.
#'
#'
#' @importFrom gganimate transition_manual
#' @importFrom gganimate animate
#' @importFrom gganimate anim_save
#' @importFrom gganimate gifski_renderer
#' @importFrom stats cor
#' @importFrom stats na.omit
#' @import ggplot2
#'
#' @examples
#' animSolar(data = mtcars,
#'           method = 'pearson',
#'           sun = 'mpg',
#'           export = FALSE,
#'           num_frames = 25)
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

  # declare global vars
  colr <- col_name <- NULL

  # Calculate correlation matrix
  cor_matrix <- cor(data, method = method)
  diag(cor_matrix) <- NA

  # Convert matrix to data frame
  df_data_long <- matrix2long(cor_matrix)

  # Filter rows where col_name equals 'sun'
  df_data_filtered <- subset(df_data_long, col_name == sun)

  # Order rows by 'value' column in descending order
  df_data_ordered <- df_data_filtered[order(df_data_filtered$value, decreasing = TRUE),]

  # Rename columns
  names(df_data_ordered)[names(df_data_ordered) == "value"] <- "r"
  names(df_data_ordered)[names(df_data_ordered) == "col_name"] <- "x"
  names(df_data_ordered)[names(df_data_ordered) == "row_name"] <- "y"

  # Assign the result to correlations
  correlations <- df_data_ordered

  # check if any correaltions would round to 1 and change to 0.9
  correlations$r <- ifelse(correlations$r > 0.9, 0.9, correlations$r)


  # Assign orbit radius based on absolute, rounded correlation values
  correlations$r <- round(correlations$r, 1)
  # Calculate new columns 'orbit_radius' and 'angle'
  correlations <- transform(correlations,
                            orbit_radius = 1 - round(abs(r), 1),
                            angle = 2 * pi * seq_len(nrow(correlations)) / nrow(correlations)
  )


  # add correlation colour
  correlations$colr <- ifelse(correlations$r <= 0, "blue", "red")
  correlations <- na.omit(correlations)

  # create animated plot
  nframes <- num_frames
  seqFrames <- (ncol(data) - 1)

  # ang <- rep(seq(0, 2 * pi, length.out = seqFrames), 10)
  ang <- rep(seq(0, 2 * pi, length.out = nframes), seqFrames)
  ang <- ang + rep(correlations$angle, each = nframes)

  y <- x <- r <- id <- orbit_radius <- angle <- frame <- NULL
  solar_system <- data.frame(
    y = rep(correlations$y, each = nframes),
    x = rep(correlations$x, each = nframes),
    r = rep(correlations$r, each = nframes),
    id = rep(correlations$id, each = nframes),
    orbit_radius = rep(correlations$orbit_radius, each = nframes),
    angle = ang,
    colr = rep(correlations$colr, each = nframes),
    frame = rep(1:nframes, seqFrames)
  )


  # Function to generate points along the circumference of a circle
  circle_points <- function(radius, n_points = 100) {
    data.frame(
      x = radius * cos(seq(0, 2 * pi, length.out = n_points)),
      y = radius * sin(seq(0, 2 * pi, length.out = n_points)),
      r = radius
    )
  }

  # Define a function to apply circle_points and assign an 'id' to each element in the list
  create_circle_points_df <- function(radius, id) {
    df <- circle_points(radius)
    df$id <- id
    return(df)
  }

  # Use lapply to apply the create_circle_points_df function to each unique orbit_radius
  circle_data_list <- mapply(create_circle_points_df,
                             unique(correlations$orbit_radius),
                             seq_along(unique(correlations$orbit_radius)),
                             SIMPLIFY = FALSE)

  # Combine the resulting list of data frames into a single data frame using base R's 'do.call' and 'rbind'
  circle_data <- do.call(rbind, circle_data_list)
  circle_data$id <- as.character(circle_data$id)



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
        color = colr,
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
