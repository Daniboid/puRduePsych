#' Simulate new data based on a Linear Mixed Effects Model
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
#' @export sim_dat_lmer
#'




sim_dat_lmer = function (J,K,cor.PnR.Supp=.21){
  stop("This function doesn't work yet...")
  time   = rep(seq(1,K,length=K),J) # K measurements per person
  person = rep(1:J, each=K)         # J person IDs
  phase  = ifelse(time>25,.5,-.5)   # effect code phase
  lanx <- stats::rnorm(J*K,0,.5)           # time level lag anxiety
  prov <- stats::rnorm(J*K,0,.24)          # time level provision
  rec  <- sqrt(.25)*((1-(cor.PnR.Supp^2))*
                       stats::rnorm(J*K,0,1)+sqrt(cor.PnR.Supp)*prov) # time level receipt

  #fixed effects
  b0 = 1.831    # true intercept value
  b1 =  .529    # true lagged anxiety slope
  b2 = -.132    # true phase slope
  b3 =  .125    # true received supp. slope
  b4 = -.122    # true provided supp. slope
  b5 = -.143    # true received supp. x phase interaction
  b6 =  .147    # true provided supp. x phase interaction

  #random effects
  vsub.b0 = .096    # true variance in the intercept (i.e. positive affect)
  vsub.b1 = .015    # true variance in lagged anxiety slope
  vsub.b3 = .016    # true variance in received supp. slope
  vresid  = .420    # true variance of the residuals

  #combine fixed and random effects per person
  b0.int    <- stats::rnorm(J,b0,sqrt(vsub.b0))  #generate an intercept for each person
  b1.laganx <- stats::rnorm(J,b1,sqrt(vsub.b1))  #generate a slope for lag anxiety for each person
  b3.rec    <- stats::rnorm(J,b3,sqrt(vsub.b3))  #generate a slope for receipt for each person
  anx <- stats::rnorm(J*K, b0.int[person]            #use the person's intercept
               +b1.laganx[person]*lanx   #lag anxiety slope
               +b2*phase                 #average phase effect
               +b3.rec[person]*rec       #average plus person-specific receipt effect
               +b4*prov                  #average provision effect
               +b5*phase*rec             #average phase by receipt effect
               +b6*phase*prov            #average phase by provided effect
               ,sqrt(vresid))            #residual

  return(data.frame(person,time,phase,prov,rec,lanx,anx))
}
