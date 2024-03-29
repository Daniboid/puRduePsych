% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/z_independent_cors_test.R
\name{z_independent_cors_test}
\alias{z_independent_cors_test}
\title{Test for comparing two correlations from independent samples.}
\usage{
z_independent_cors_test(
  r1 = NULL,
  r2 = NULL,
  n1 = NULL,
  n2 = NULL,
  z_val = NULL,
  alpha = 0.05,
  two_tailed = T
)
}
\arguments{
\item{r1}{Numeric. The first correlation to be compared. Must be between -1 and 1.}

\item{r2}{Numeric. The second correlation to be compared. Must be between -1 and 1.}

\item{n1}{Numeric. The sample size from which `r1` was generated. Must be a positive value.}

\item{n2}{Numeric. The sample size from which `r2` was generated. Must be a positive value.}

\item{z_val}{Numeric. A z-score to be tested for significance.}

\item{alpha}{Numeric. A confidence level for the test. Defaults to .05, corresponding with a 95\% confidence level.}

\item{two_tailed}{Logical. If `True` (default) the test for significance is done in a two-tailed manner.}
}
\value{
Outputs the test for significance to the console and returns both a *z*-value and *p*-value for the test.
}
\description{
Test for comparing two correlations from independent samples.
}
