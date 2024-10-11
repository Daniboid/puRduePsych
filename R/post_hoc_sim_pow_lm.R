#' Simulate Power for A Linear Model
#'
#' @description
#' see title
#'
#' @param lm An `lm()` object. The regression model for which to simulate power.
#' @param n_forSim Integer. The number of observations to be used in the linear model simulations.
#' @param n_sims Integer. The number of simulations to run.
#' @param alpha Float between 0 and 1. The significance level of the test.
#'
#'
#' @import parallel
#' @import doSNOW
#' @export post_hoc_sim_pow_lm
#'



post_hoc_sim_pow_lm = function (lm,
                                n_forSim=NULL,
                                n_sims=10000,
                                alpha=.05){

  cl = parallel::makeCluster(parallel::detectCores())
  doSNOW::registerDoSNOW(cl)
  pb = utils::txtProgressBar(min = 1, max = n_sims, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  signif = foreach::foreach (s = 1:n_sims, .combine = rbind, .options.snow=opts
  ) %dopar% {

    fake = puRduePsych::sim_dat_lm(lm, n_forSim)    #generate a fake dataset
    lm_tmp = base::suppressWarnings(suppressMessages(lm(lm$call$formula,
                                                        data = fake)))

    coefs = base::data.frame(stats::summary.lm(lm_tmp)$coefficients)
    base::colnames(coefs) = c("est", "se","t","p")
    for_return = coefs$p < alpha
    names(for_return) = base::rownames(coefs)
    return(for_return)
  }
  close(pb)
  parallel::stopCluster(cl)
  return(colMeans(signif))
}
