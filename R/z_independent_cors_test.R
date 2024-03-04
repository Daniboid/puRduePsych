#' Test for comparing two correlations from independent samples.
#'
#' @param r1 Numeric. The first correlation to be compared. Must be between -1 and 1.
#' @param r2 Numeric. The second correlation to be compared. Must be between -1 and 1.
#' @param n1 Numeric. The sample size from which `r1` was generated. Must be a positive value.
#' @param n2 Numeric. The sample size from which `r2` was generated. Must be a positive value.
#' @param z_val Numeric. A z-score to be tested for significance.
#' @param alpha Numeric. A confidence level for the test. Defaults to .05, corresponding with a 95\% confidence level.
#' @param two_tailed Logical. If `True` (default) the test for significance is done in a two-tailed manner.
#'
#' @returns Outputs the test for significance to the console and returns both a *z*-value and *p*-value for the test.
#'
#'

z_independent_cors_test = function(r1=NULL,
                                 r2=NULL,
                                 n1=NULL,
                                 n2=NULL,
                                 z_val = NULL,
                                 alpha=.05,
                                 two_tailed = T){
  if(!is.null(r1) & !is.null(r2) & !is.null(n1) & !is.null(n2) & is.null(z_val)){
    z_val = z_independent_cors(r1, r2, n1, n2)
  } else if(is.null(z_val)) {
    stop("You must supply either a z-value to test for significance, or all of the following: \n\t the two corellation valuse and the sample sizes from which they were generated.")
  }

  for_return = z_test(z_val)
  names(for_return) = c("z_val", "p_val")

  return(for_return)
}

