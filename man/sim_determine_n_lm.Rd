% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_determine_n_lm.R
\name{sim_determine_n_lm}
\alias{sim_determine_n_lm}
\title{Power analysis Via Simulation to determine the N required for a linear regression}
\usage{
sim_determine_n_lm(
  lm,
  effect,
  n_sims = 10000,
  alpha = 0.05,
  n_step = 1,
  desired_power = 0.8,
  verbose = T
)
}
\arguments{
\item{lm}{An `lm()` object. The regression model for which to simulate power.}

\item{effect}{Charater (String). The name of the predictor for which to estimate the required sample size.}

\item{n_sims}{Integer. The number of simulations to run per sample size step. Defaults to 10,000.}

\item{alpha}{Float (Numeric) between 0 and 1. The significance level of the test. Defaults to 0.05.}

\item{n_step}{Integer. A multiple by which to increase sample size between runs. Defaults to 1.}

\item{desired_power}{Float (Numeric) between 0 and 1. The desired power to achieve. Defaults to .8.}

\item{verbose}{Logical (Boolean). Should the output be shown while running each step? Defaults to True.}
}
\description{
See title
}
