#' Steiger's z*-bar (Steiger, 1980)
#'
#' @description
#' This code allows you to test for a significant difference in the correlations between two variables and a third.
#'
#' @param r_x1_y Numeric. The correlation value between the first predictor and the outcome.
#' @param r_x2_y Numeric. The correlation value between the second predictor and the outcome.
#' @param r_x1_x2 Numeric. The correlation value between the first and second predictors.
#' @param n Numeric. The sample size used to calculate the correlations.
#' @param z_val Numeric. A z-score to be tested for significance.
#' @param alpha Numeric. A confidence level for the test. Defaults to .05, corresponding with a 95\% confidence level.
#' @param two_tailed Logical. If `True` (default) the test for significance is done in a two-tailed manner.
#'
#' @returns Outputs the test for significance to the console and returns both a *z*-value and *p*-value for the test.

z_dependent_cors_test = function(r_x1_y=NULL,
                                 r_x2_y=NULL,
                                 r_x1_x2=NULL,
                                 n=NULL,
                                 z_val = NULL,
                                 alpha=.05,
                                 two_tailed = T){
  if(!is.null(r_x1_y) & !is.null(r_x2_y) & !is.null(r_x1_x2) & !is.null(n) & is.null(z_val)){
    z_val = z_dependent_cors(r_x1_y, r_x2_y, r_x1_x2, n)
  } else if (is.null(z_val)) {
    stop("You must supply either a z-value to test for significance, or all of the following: \n\t correlations between x1 and y, x2 and y, x1 and x2, and the sample size.")
  }

  for_return = z_test(z_val)
  names(for_return) = c("z_val", "p_val")

  return(for_return)
}

