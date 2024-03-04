#' Calculate the statistic for a test of significant differences between two independent correlations.
#'
#' @param r1 Numeric. The first correlation to be compared. Must be between -1 and 1.
#' @param r2 Numeric. The second correlation to be compared. Must be between -1 and 1.
#' @param n1 Numeric. The sample size from which `r1` was generated. Must be a positive value.
#' @param n2 Numeric. The sample size from which `r2` was generated. Must be a positive value.
#'
#' @returns `z_independent_cors()` returns a z-score that can be tested using the `z_independent_cors_test()` or `z_test()` function.

z_independent_cors = function(r1,
                              r2,
                              n1,
                              n2){
  z1 = fishers_z(r1)
  z2 = fishers_z(r2)

  z_val = (z1-z2) / sqrt(1/(n1-3) + 1/(n2-3))

  return(z_val)
}
