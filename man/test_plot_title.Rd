% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_plot_title.R
\name{test_plot_title}
\alias{test_plot_title}
\title{Test how text displays on a plot}
\usage{
test_plot_title(text)
}
\arguments{
\item{text}{"string": The plot title to test}
}
\description{
Using functionality like bquote never really makes sense.
  Often we only figure that out after running slow code, and getting back a
  plot with an incorrect title (or just an error). This function simply
  creates a basic plot, and shows how the text is displayed.
}
\examples{
\dontrun{
  test_plot_title(bquote("W"~m^2~ "(magic)"))
}
}
