#' corrSolar
#'
#' Creates a solar system correlation plot.
#'
#' @param data A data frame.
#' @param method Which correlation coefficient (or covariance) is to be computed.
#' One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
#' @param sun A variable name to be chosen as the dependent variable.
#'
#' @return An solar system plot displaying correlations.
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
#' @importFrom dplyr desc
#' @importFrom dplyr tibble
#' @importFrom stats cor
#' @importFrom stats reshape
#' @importFrom stats na.omit
#' @import ggplot2
#'
#' @examples
#' corrSolar(data = mtcars,
#'           method = 'pearson',
#'           sun = 'mpg')
#'
#'
#' @export


corrSolar <- function(data,
                      method = c("pearson", "kendall", "spearman"),
                      sun = NULL){


  # declare global vars
  r <- x <- y <- id <- orbit_radius <- angle <- nam <- nam2 <- NULL

  # Calculate correlation matrix
  cor_matrix <- cor(data, method = method)
  diag(cor_matrix) <- NA

  # Convert matrix to data frame
  df_data <- data.frame(cor_matrix)

  # Add row and column names as separate columns
  df_data$row_name <- NULL
  df_data$row_name <- row.names(cor_matrix)
  df_data$col_name <- NULL
  df_data$col_name <- colnames(cor_matrix)

  # Reshape data frame to long format
  df_data_long <- reshape(df_data,
                          direction = "long",
                          varying = list(colnames(cor_matrix)),
                          v.names = "value",
                          timevar = "col_name",
                          times = colnames(cor_matrix))

  correlations <- df_data_long |>
    filter(.data$col_name == sun) |>
    arrange(desc(.data$value)) |>
    rename(r = .data$value,
           x = .data$col_name,
           y = .data$row_name)

  # check if any correaltions would round to 1 and change to 0.9
  correlations$r <- ifelse(correlations$r > 0.9, 0.9, correlations$r)

  # Assign orbit radius based on absolute, rounded correlation values
  correlations$r <- round(correlations$r, 1)
  correlations <- correlations |>
    mutate(orbit_radius = 1 - round(abs(r), 1),
           angle = 2 * pi * row_number() / n())

  # add correlation colour
  correlations$col <- ifelse(correlations$r <= 0, "blue", "red")
  correlations <- na.omit(correlations)

  # Function to generate points along the circumference of a circle
  circle_points <- function(radius, n_points = 100) {
    tibble(
      x = radius * cos(seq(0, 2 * pi, length.out = n_points)),
      y = radius * sin(seq(0, 2 * pi, length.out = n_points)),
      r = radius
    )
  }


  # Modify the data.frame to generate circle points for each orbit_radius
  circle_data <- dplyr::bind_rows(lapply(unique(correlations$orbit_radius), circle_points), .id = "id")


  # create df of circle names to display
  circle_name <- data.frame(nam =  unique(round(abs(correlations$r), 1)),
                            nam2 = unique(round(abs(correlations$orbit_radius), 1)))


  # Create the correlation orbit plot with different colors for each line and point
  correlation_orbit_plot <- ggplot(circle_data, aes(x = x, y = y)) +
    geom_path(aes(group = id, color = id), linetype = "solid", alpha = 0.5) +
    geom_point(data = correlations, aes(x = orbit_radius * cos(angle), y = orbit_radius * sin(angle)), color = correlations$col, size = 4) +
    geom_text(data = correlations, aes(x = orbit_radius * cos(angle), y = orbit_radius * sin(angle), label = y), hjust = -0.5, vjust = 0.5) +
    geom_point(data = correlations, aes(x = 0, y = 0), size = 6, color = "yellow") +
    geom_text(data = correlations, aes(x = 0, y = 0, label = sun), hjust = -0.5, vjust = -1) +
    geom_text(data = circle_name, aes(x = 0, y = nam2,
                                      label = nam),
              size = 3.5, color = 'black', alpha = 0.3) +
    theme_void() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    coord_fixed()


  return(correlation_orbit_plot)
}
