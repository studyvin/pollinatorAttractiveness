% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stackedBars.R
\name{stackedBars}
\alias{stackedBars}
\title{Create a stacked bar chart of means bounded below by zero.}
\usage{
stackedBars(df, fileSave, title, xlab, ylab, dir, height, width, text = 11)
}
\arguments{
\item{df}{A \code{data.frame} containing columns for \code{FlowerType},
\code{Variety}, \code{b0}, \code{lo2.5}, and \code{hi97.5},
\code{Captured}, and \code{Observed}.  Typically the output from function
\code{getPoissonMeans}.}

\item{fileSave}{A character string used to name the plot file.}

\item{title}{A character string used to entitle the plot.}

\item{xlab}{A character string used to label the \eqn{x}-axis.}

\item{ylab}{A character string used to label the \eqn{y}-axis.}

\item{dir}{A character string used to identify the directory into which
output is to be saved.  The directory should have a trailing slash
"\code{/}."}

\item{height}{A number indicating the height of the resulting \code{png}, in
inches.}

\item{width}{A number indicating the width of the resulting \code{png}, in
inches.}

\item{text}{The size of the output font.  Larger means bigger.  Default is
\code{11}.}
}
\value{
A \code{ggplot2}-styled stacked bar chart, with bars oriented horizontally.
}
\description{
Create a stacked bar chart of means bounded below by zero.
}
\details{
This function creates a \code{png} via \code{ggplot2}.
}
\seealso{
\code{ggplot2::ggplot} and friends, \code{forcats}.
}
\author{
Jason Mitchell
}
