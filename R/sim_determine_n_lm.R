#' Power analysis Via Simulation to determine the N required for a linear regression
#'
#' @description
#' See title
#'
#'
#' @param lm An `lm()` object. The regression model for which to simulate power.
#' @param effect Charater (String). The name of the predictor for which to estimate the required sample size.
#' @param n_sims Integer. The number of simulations to run per sample size step. Defaults to 10,000.
#' @param alpha Float (Numeric) between 0 and 1. The significance level of the test. Defaults to 0.05.
#' @param n_step Integer. A multiple by which to increase sample size between runs. Defaults to 1.
#' @param desired_power Float (Numeric) between 0 and 1. The desired power to achieve. Defaults to .8.
#' @param verbose Logical (Boolean). Should the output be shown while running each step? Defaults to True.
#'
#'
#' @import parallel
#' @import doSNOW
#' @import stats
#' @import utils
#' @export sim_determine_n_lm
#'



sim_determine_n_lm = function(lm,
                          effect,#=NULL,
                          n_sims=10000,
                          alpha=.05,
                          n_step=1,
                          desired_power=.8,
                          verbose=T) {
  if(!exists("effect")) stop("Effect not defined.")
  if(!exists("lm")) stop("lm not defined.")

  current_power = puRduePsych::post_hoc_sim_pow_lm(lm)[effect]

  base::cat(base::paste("\nCurrent power for", base::names(current_power),"based on existing data is:", current_power, "\n\n"))
  if (all(current_power >= desired_power)) {
    base::cat("No need to calculate further\n\n")
    return(nrow(lm$model))
  } else {
    n_obs = base::nrow(lm$model)
    while (all(current_power < desired_power)){
      n_obs = n_obs + n_step
      if(verbose) base::cat(base::paste("\n\tChecking if a sample of", n_obs, "observations is enough to meet the deisred power...\n"))
      current_power = puRduePsych::post_hoc_sim_pow_lm(lm, n_obs)[effect]
    }

    if(verbose) base::cat(base::paste("\nA sample of", n_obs, "should be sufficient to power for the effect of",
                          effect, "at a power of", desired_power, ".\n"))
    return(n_obs)
  }
}
