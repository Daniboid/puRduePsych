#' Simulate Power for ANOVA
#'
#' @description
#' see title
#'
#' @param aov_in An `aov()` object. The ANOVA model for which to simulate power.
#' @param n_forSim Integer. The number of observations to be used in the linear model simulations. Defaults to the n (and ratios between groups) of the original data from the `aov_in`
#' @param n_sims Integer. The number of simulations to run. Defaults to 10k.
#' @param alpha Float between 0 and 1. The significance level of the test.
#'
#'
#' @import parallel
#' @import doSNOW
#' @import foreach
#' @export post_hoc_sim_pow_lm
#'





post_hoc_sim_pow_anova = function(aov_in,
                                  type,
                                  n_forSim = NULL,
                                  n_sims = 10000,
                                  alpha = .05){

  cl = parallel::makeCluster(parallel::detectCores())
  doSNOW::registerDoSNOW(cl)
  pb = utils::txtProgressBar(min = 1, max = n_sims, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  signif = foreach::foreach (s = 1:n_sims, .combine = rbind, .options.snow=opts
  ) %dopar% {

    fake = puRduePsych::sim_dat_anova(aov_in, n_forSim)    #generate a fake dataset
    tmp_form = as.character(aov_in$call$formula)
    tmp_form = paste(tmp_form[2], tmp_form[1], tmp_form[3])
    aov_tmp = base::suppressWarnings(suppressMessages(stats::aov(eval(formula(tmp_form)),
                                                        data = fake)))

    aovtab = stats::summary.aov(aov_tmp)[[1]]
    for_return = aovtab$`Pr(>F)`[1:(nrow(aovtab)-1)] < alpha
    names(for_return) = base::rownames(aovtab)[1:(nrow(aovtab)-1)]
    return(for_return)
  }
  close(pb)
  parallel::stopCluster(cl)
  return(colMeans(signif))
}
