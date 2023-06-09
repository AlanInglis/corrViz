% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corrHeatmap.R
\name{corrHeatmap}
\alias{corrHeatmap}
\title{corrHeatmap}
\usage{
corrHeatmap(
  mat,
  display = c("all", "upper", "lower"),
  reorder = TRUE,
  pal = colorRampPalette(c("darkblue", "white", "darkred"))(100)
)
}
\arguments{
\item{mat}{A square correlation matrix to visualise.}

\item{display}{A character vector specifying which part of the correlation matrix to
display: 'all', 'upper', or 'lower', default is 'all'.}

\item{reorder}{A logical value indicating whether to reorder the heatmap based
on hierarchical clustering, default is TRUE.}

\item{pal}{A color palette for the heatmap.}
}
\value{
An interactive heatmap plot displaying correlations.
}
\description{
This function creates an interactive heatmap of correlations between variables in a dataset.
}
\details{
Creates an interactive heatmap displaying correlation values. By hovering
mouse over a cell, the variables and correlation value is shown.
}
\examples{
cm <- cor(mtcars)

corrHeatmap(mat = cm,
           display = 'all')


}
