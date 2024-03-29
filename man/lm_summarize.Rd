% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lm_summarize.R
\name{lm_summarize}
\alias{lm_summarize}
\title{More detailed output for linear models}
\usage{
lm_summarize(
  lm,
  std = T,
  semi = T,
  partial = T,
  CI = F,
  CL = 0.95,
  verbose = F,
  correlations = F,
  warn = T
)
}
\arguments{
\item{lm}{An object created from the `lm()` function.}

\item{std}{Logical. If `True` (default) the standardized regression coefficients will be calculated and returned.}

\item{semi}{Logical. If `True` (default) the semi-partial correlation coefficients will be calculated and returned.}

\item{partial}{Logical. If `True` (default) the partial correlation coefficients will be calculated and returned.}

\item{CI}{Logical. If `True` confidence intervals will calculated and returned for the regression coefficients to be returned. Defaults to `False`.}

\item{CL}{Numeric. The confidence level of the confidence intervals to be returned. The value must be between 0 and 1. Defaults to .95, corresponding to a 95\% confidence level.}

\item{verbose}{Logical. If `True` the R^2 for the model and test of significance will be output to the console while the function runs. This will not be saved, as it is already part of the `lm` object and cam be accessed from there. Defaults to `False`.}

\item{correlations}{Logical. If `True` the zero-order correlation matrix will be output to the console while the function is running. Defaults to `False`.}

\item{warn}{Logical. If `True` (default) warnings are displayed.}
}
\value{
A data.frame with the corresponding summary values.
}
\description{
Output summaries for lms with more detail. This function returns a `data.frame` with the details regarding the regression coefficients. By default the columns include the unstandardized and standardized regression coefficients, the standard error of the unstandardized coefficients, *t*-values, significance, and partial and semi-partial correlation coefficients. Confidence intervals for the coefficients can also be output.
}
