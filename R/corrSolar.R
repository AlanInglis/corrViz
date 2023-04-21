#' corrSolar
#'
#' This function creates a solar system plot of correlations between
#' variables in a dataset.
#'
#' @param data A dataframe containing the data to be analyzed.
#' @param method A character string specifying the correlation method. One of
#'   "pearson", "kendall", or "spearman". Default is "pearson".
#' @param sun A character string specifying the column name in the dataset to
#' be treated as the 'sun' in the solar system plot.
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
  r <- x <- y <- id <- orbit_radius <- angle <- nam <- nam2 <- col_name <- NULL

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
  correlations <- transform(correlations,
                             orbit_radius = 1 - round(abs(r), 1),
                             angle = 2 * pi * seq_len(nrow(correlations)) / nrow(correlations)
  )

  # add correlation colour
  correlations$col <- ifelse(correlations$r <= 0, "blue", "red")
  correlations <- na.omit(correlations)

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
