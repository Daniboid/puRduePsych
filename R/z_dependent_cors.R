#' Calculate the *z*-value comparing two dependent correlations
#'
#' @param r_x1_y Numeric. The correlation value between the first predictor and the outcome.
#' @param r_x2_y Numeric. The correlation value between the second predictor and the outcome.
#' @param r_x1_x2 Numeric. The correlation value between the first and second predictors.
#' @param n Numeric. The sample size used to calculate the correlations.
#'
#' @returns Returns a z-score that can be tested using the `z_dependent_cors_test()` or `z_test()` function.

z_dependent_cors = function(r_x1_y,
                            r_x2_y,
                            r_x1_x2,
                            n){
  # abs.R = 1 - r_x1_y^2 - r_x2_y^2 - r_x1_x2^2 +2 * r_x1_y * r_x2_y * r_x1_x2
  r_bar = (r_x1_y + r_x2_y)/2
  psy_hat = (r_x1_x2*(1-2*(r_bar^2)))-(0.5*((r_bar^2*(1-2*(r_bar^2)-(r_x1_x2^2)))))
  s_bar = psy_hat/(1-r_bar^2)^2
  z = (fishers_z(r_x1_y) - fishers_z(r_x2_y)) * sqrt((n-3)/(2-2*s_bar))
}
