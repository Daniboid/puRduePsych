#' Power Analysis via Simulation for Linear Mixed Effects Model
#'
#' @description
#' NOT YET FUNCTIONAL
#'
#'
#'
#' @import parallel
#' @import doSNOW
#' @import lmerTest
#' @import stats
#' @import utils
#' @export sim_pow_lmer
#'


sim_pow_lmer = function (J=68,
                         K=32,
                         n.sims=10000,       #default to 10000 simulations if not specified
                         cor.PnR.supp=.21){
  stop("This function doesn't work yet...")
  cl = parallel::makeCluster(parallel::detectCores())
  doSNOW::registerDoSNOW(cl)
  #pb = txtProgressBar(min = 1, max = n.sims, style = 3)
  #progress <- function(n) setTxtProgressBar(pb, n)
  #opts <- list(progress=progress)
  signif = foreach (s = 1:n.sims, .combine = rbind #, .options.snow=opts
  ) %dopar% {

    fake = puRduePsych::sim.dat(J,K,cor.PnR.supp)     #generate a fake dataset
    lme.power = base::suppressWarnings(suppressMessages(lmerTest::lmer(anx ~ (1 + lanx + rec || person)
                                                             + lanx + phase + prov + rec + prov*phase + rec*phase,
                                                             data=fake)))         #analyze it
    est = lme4::fixef(lme.power)               #save the parameter estimate
    se = arm::se.fixef(lme.power)             #save the standard error
    (abs(est)-1.96*se)>0               #calculate significance - returns TRUE/FALSE

  }
  # close(pb)
  parallel::stopCluster(cl)
  return(base::colMeans(signif))
}
