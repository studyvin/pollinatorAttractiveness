% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitFT.R
\name{fitFT}
\alias{fitFT}
\title{Conduct an analysis-of-deviance for the MSU pollinator analysis.}
\usage{
fitFT(df, year, offsetVar = NULL, CNMax = 10)
}
\arguments{
\item{df}{A \code{data.frame} containing columns for \code{TotalPollinatorVisits},
\code{Cultivar}, \code{SamplingPeriods}, and \code{FlowerType}.  Typically,
\code{Cultivar} is nested within \code{FlowerType}.}

\item{year}{The numeric four-digit year.  Used to handle slight differences
in recorded data layout.}

\item{offsetVar}{The variable to be utilized as an offset, submitted as a
character string.  Typically \code{SamplingPeriods}.  Default is
\code{NULL}, or no offset.}

\item{CNMax}{The maximum condition number to serve as a cutoff, beyond which
models are not considered.}
}
\value{
A \code{list} containing results of the original fit, adjusted
  Tukey-like pairwise multiple comparisons, and the analysis-of-deviance.
  The four constituent objects are \enumerate{
}
}
\description{
Conduct an analysis-of-deviance for the MSU pollinator analysis
  assuming a Poisson-distributed outcome and varying exposure.
}
\details{
Function \code{fitFT} serves as a preparation and wrapper function
  for function \code{fits} which conducts the statistical fitting via either
  of \code{glm} or \code{MASS::glm.nb}.

  Processing includes the creation of a binary explanatory variables for
  consideration of different cutpoints.  Cutpoints are binary, in that
  cultivar records greater than or equal to a predefined number of
  \code{TotalPollinatorVisits} are grouped as \code{1}, while all others (so
  those less than \code{TotalPollinatorVisits}) are \code{zero}.  All
  possible binary outcomes from \code{1} through \code{10} are considered.

  Following the creation of appropriate data sets, R-type formula strings are
  then created, possibly including \code{Cultivar} as an additional
  explanatory covariate via function \code{modelStrings}.  Up to 5
  explanatory covariates are allowed, with possibly two interactions.

  Next, given the model strings, design matrices are constructed so that
  condition numbers can be calculated for each.  The condition number of the
  underlying variance-covariance matrix of the centered design matrix allows
  for interpretation of the multicollinearity of the variables whose columns
  form the original design matrix.  Condition numbers are ratios of the
  largest eigenvalue to the smallest eigenvalue, which themselves are
  estimates of variance in rotated dimensions in which they matter the most.
  More practically, the presence of a large condition number implies a more
  ellipsoidal data cloud when compared to clouds tied to smaller condition
  numbers.  The ellipsoidal nature of the cloud is very similar to
  ellipsoidal clouds observed in two-dimensional data plots used to assess
  two-dimensional correlation.  The condition-number approach is the
  generalization of the typical approach used for two dimensions to assess
  correlation.

  Those model strings with a sufficiently low condition number are then
  passed to the \code{fits} function, which then conducts the actual
  fitting of count statistical models.
}
\seealso{
\code{stats::glm}, \code{stats::anova}, \code{stats::confint},
  \code{multcomp::glht}
}
\author{
Jason Mitchell, based on original work by Zoe Gustafson.
}
