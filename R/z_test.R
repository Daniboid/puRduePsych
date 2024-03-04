#' A simple method for outputting the results of a *z*-test
#'
#' @param z_val Numeric. The *z*-value to be tested for statistical significance.
#' @param alpha Numeric. The significance level criterion for the test. Must be between 0 and 1. Defaults to .05, corresponding with a confidence level of 95\%.
#' @param two_tailed Logical. If `True` (defualt) the test will be conducted in a two-tailed manner. If `False`, the test is conducted in a one-tailed manner. (Indication of a specific directional hypothesis is not currently supported.)
#'
#' @returns returns a list containing the `z_val` initially provided and the corresponding `p_val` (i.e., the probability of obtaining the specified value, or one more extreme, assuming the null hypothesis is true). The function also prints out the results of the test.
#'
#'
#' @export z_test

z_test = function(z_val = NULL,
                  alpha=.05,
                  two_tailed = T){
    p_val = stats::pnorm(abs(z_val), lower.tail = F)

    if (two_tailed){
      p_val = 2*p_val
      if (p_val < alpha) {
        cat("The absolute value of observed z (",
            round(z_val,3), ") is greater than  the critical z (", round(stats::qnorm(1-alpha/2),3),
            "). \nThe test is statistically significant.  \n    p = ", p_val, ".\n\n")
      } else {
        cat("The absolute value of observed z (",
            round(z_val,3), ") is less than  the critical z (", round(stats::qnorm(1-alpha/2),3),
            "). \nThe test is not statistically significant.  \n   p = ", p_val, ".\n\n")
      }
    } else {
      if (p_val < alpha) {
        cat("The absolute value of observed z (", z_val, ") is greater than  the critical z (", stats::qnorm(1-alpha),
            "). The test is statistically significant.  \n    p = ", p_val, ".\n\n")
      } else {
        cat("The absolute value of observed z (", z_val, ") is less than  the critical z (", stats::qnorm(1-alpha),
            "). The test is not statistically significant.  \n   p = ", p_val, ".\n\n")
      }
    }
    for_return = c(z_val, p_val)
    names(for_return) = c("z_val", "p_val")

    return(for_return)
}
