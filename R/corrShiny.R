#' corrShiny
#'
#' Correlation Explorer Shiny App. This function creates
#' a Shiny application to explore the correlation between
#' variables in a given dataset.
#'
#' @param data A data frame with the variables to be analyzed.
#' @param x_var The name of the variable to be plotted on the X-axis.
#' @param y_var The name of the variable to be plotted on the Y-axis.
#' @param color_var The name of the variable to be used for coloring the points on the scatter plot.
#' @param size_var The name of the variable to be used for sizing the points on the scatter plot.
#' @param correlation_method The method to be used for computing the correlation coefficient, must be one of "pearson", "spearman" or "kendall".
#' @import shiny
#'
#' @import shiny
#' @importFrom ggplot2 aes_string geom_point labs geom_smooth annotate scale_color_discrete scale_color_continuous scale_size_discrete scale_size_continuous
#'
#' @return A Shiny app that displays a scatter plot and the correlation coefficient between two variables.
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' corrShiny(data = mtcars,
#'           x_var = "wt",
#'           y_var = "mpg",
#'           color_var = "cyl",
#'           size_var = "hp")
#'}
#' @export

corrShiny <- function(data, x_var, y_var, color_var = NULL, size_var = NULL, correlation_method = "pearson") {
  # Define UI
  ui <- fluidPage(
    titlePanel("Correlation Explorer"),

    sidebarLayout(
      sidebarPanel(
        selectInput("x_var", "Choose X-axis variable:", choices = colnames(data), selected = x_var),
        selectInput("y_var", "Choose Y-axis variable:", choices = colnames(data), selected = y_var),
        helpText("Select two variables to display their correlation"),
        checkboxInput("add_best_fit_line", "Add line of best fit", FALSE),
        selectInput("line_fit_method", "Line fit method:", choices = c("lm", "loess")),
        checkboxInput("show_confidence_interval", "Show confidence interval", FALSE),
        checkboxInput("show_cor_on_plot", "Show correlation coefficient on plot", FALSE),
        selectInput("correlation_method", "Choose correlation method:", choices = c("pearson", "spearman", "kendall"), selected = correlation_method),
        selectInput("color_var", "Choose color variable:", choices = c("", colnames(data)), selected = color_var),
        selectInput("size_var", "Choose size variable:", choices = c("", colnames(data)), selected = size_var),
        checkboxInput("show_obs_labels", "Display observation labels", FALSE)
      ),

      mainPanel(
        plotOutput("scatter_plot"),
        textOutput("correlation_text")
      )
    )
  )

  # Define server logic
  server <- function(input, output) {
    output$scatter_plot <- renderPlot({
      plot <- ggplot(data, aes_string(x = input$x_var, y = input$y_var, color = if (input$color_var != "") input$color_var else NULL, size = if (input$size_var != "") input$size_var else NULL)) +
        geom_point() +
        labs(x = input$x_var, y = input$y_var)

      if (input$add_best_fit_line) {
        plot <- plot + geom_smooth(method = input$line_fit_method, se = input$show_confidence_interval, color = "blue", linetype = "solid")
      }

      if (input$show_cor_on_plot) {
        correlation <- round(cor(data[[input$x_var]], data[[input$y_var]], method = input$correlation_method), 2)
        plot <- plot + annotate("text", x = Inf, y = -Inf, label = paste("Corr =", correlation), vjust = -1, hjust = 1, size = 4, color = "black")
      }

      if (input$show_obs_labels) {
        plot <- plot + geom_text(aes(label = row.names(data)), nudge_x = 0.05, nudge_y = 0.05, check_overlap = TRUE, size = 3)
      }

      if (input$color_var != "") {
        if (is.factor(data[[input$color_var]])) {
          plot <- plot + scale_color_discrete(name = input$color_var)
        } else {
          plot <- plot + scale_color_continuous(name = input$color_var)
        }
      }

      if (input$size_var != "") {
        if (is.factor(data[[input$size_var]])) {
          plot <- plot + scale_size_discrete(name = input$size_var)
        } else {
          plot <- plot + scale_size_continuous(name = input$size_var)
        }
      }

      return(plot)
    })


    output$correlation_text <- renderText({
      correlation <- cor(data[[input$x_var]], data[[input$y_var]], method = input$correlation_method)
      paste("Correlation coefficient between", input$x_var, "and", input$y_var, "using", input$correlation_method, "correlation:", round(correlation, 2))
    })
  }

  # Run the application
  shinyApp(ui = ui, server = server)
}


