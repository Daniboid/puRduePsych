#' Calculate Fisher's *z* from a correlation value
#'
#' @description
#' `fishers_z` is a function designed to take correlation values and return a Fisher's *z* form that value.
#'
#' @param r Numeric. The correlation to be converted to a *z*-score
#' @param n Numeric (Optional). if a sample size is provided, a confidence interval (defined by `alpha`) will be returned along with the *z* value
#' @param alpha The corresponding alpha level for the confidence interval to be returned if `n` is not `NULL`
#' @param return_se Logical. If `True`, the standard error of the *z*-score will be returned along side of the confidence interval. Only used if `n` is provided. Defaults to `False`.
#'
#' @returns If `n` is not provided, the function will only return the calculated *z*-score. If `n` is provided, a list will be returned: the first item will be the *z*-score, the second item will be the lower-bound of the confidence interval computed, the third will be the corresponding upper-bound. An optional 4th value is the standard error.
#'

fishers_z = function(r,
                     n = NULL,
                     alpha = .05,
                     return_se = F) {
  if(is.null(n) & return_se) stop("Standard error cannot be returned if sample size is not specified.")

  if(is.null(r) | r < -1 | r > 1) {
    stop("You must provide a (valid) correlation value.")
  } else{
    z_val = .5*(log(1+r)-log(1-r))
  }

  if(!is.null(n) & is.numeric(n)){
    if (n < 0) stop("Sample size must be a positive value.")
    se = 1/sqrt(n-3)
    LB = z_val - stats::qnorm(alpha/2, lower.tail = F)
    UB = z_val + stats::qnorm(alpha/2, lower.tail = F)

    if(return_se) {
      for_return = c(z_val, LB, UB, se)
      names(for_return) = c("z_val", "LB", "UB", "se")
    } else {
      for_return = c(z_val, LB, UB)
      names(for_return) = c("z_val", "LB", "UB")
    }
    return(for_return)
  } else if(is.null(n)) {
    return(z_val)
  } else stop("The sample size provided is invalid.")
}
