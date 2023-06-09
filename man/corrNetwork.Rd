% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corrNetwork.R
\name{corrNetwork}
\alias{corrNetwork}
\title{corrNetwork}
\usage{
corrNetwork(
  mat,
  threshold = 0,
  layout = "layout_nicely",
  width = "100\%",
  height = "400px",
  physics = TRUE
)
}
\arguments{
\item{mat}{A square correlation matrix to visualise.}

\item{threshold}{A numeric value indicating the minimum absolute correlation
value to display in the plot.}

\item{layout}{Any \code{igraph} layout to display the network.}

\item{width}{The width of the viewing window.}

\item{height}{The height of the viewing window.}

\item{physics}{A logical value indicating whether to use physics-based layout. Default is TRUE.}
}
\value{
A network plot displaying correlations.
}
\description{
Creates an interactive Correlation Network Visualization
}
\details{
Each node in the network represents a variable where the width of
the connecting edges represent the absolute value of the correlation. Positive
correlations have red coloured edges whereas negative correlations have blue coloured
edges.
}
\examples{
ci <- cor(iris[1:4])
corrNetwork(mat = ci, threshold = 0.5)

# Another example
cm <- cor(mtcars)

corrNetwork(mat = cm,
           threshold = 0.8,
           layout = 'layout_on_grid',
           physics = FALSE)
}
