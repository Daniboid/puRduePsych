#' Data Simulation and Power Analysis for MLM
#'
#' @description
#' see title
#'
#'
#'
#' @import parallel
#' @import doSNOW
#' @import lme4
#' @import lmerTest
#' @export sim.dat_lmer
#' @export sim.pow.lmer
#'



sim_dat_lmer = function (J,K,cor.PnR.Supp=.21){
  stop("This function doesn't work yet...")
  time   = rep(seq(1,K,length=K),J) # K measurements per person
  person = rep(1:J, each=K)         # J person IDs
  phase  = ifelse(time>25,.5,-.5)   # effect code phase
  lanx <- rnorm(J*K,0,.5)           # time level lag anxiety
  prov <- rnorm(J*K,0,.24)          # time level provision
  rec  <- sqrt(.25)*((1-(cor.PnR.Supp^2))*
                       rnorm(J*K,0,1)+sqrt(cor.PnR.Supp)*prov) # time level receipt

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
  b0.int    <- rnorm(J,b0,sqrt(vsub.b0))  #generate an intercept for each person
  b1.laganx <- rnorm(J,b1,sqrt(vsub.b1))  #generate a slope for lag anxiety for each person
  b3.rec    <- rnorm(J,b3,sqrt(vsub.b3))  #generate a slope for receipt for each person
  anx <- rnorm(J*K, b0.int[person]            #use the person's intercept
               +b1.laganx[person]*lanx   #lag anxiety slope
               +b2*phase                 #average phase effect
               +b3.rec[person]*rec       #average plus person-specific receipt effect
               +b4*prov                  #average provision effect
               +b5*phase*rec             #average phase by receipt effect
               +b6*phase*prov            #average phase by provided effect
               ,sqrt(vresid))            #residual

  return(data.frame(person,time,phase,prov,rec,lanx,anx))
}

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
    sim.dat = function (J,K,cor.PnR.Supp=.21){

      time   = rep(seq(1,K,length=K),J) # K measurements per person
      person = rep(1:J, each=K)         # J person IDs
      phase  = ifelse(time>25,.5,-.5)   # effect code phase
      lanx <- rnorm(J*K,0,.5)           # time level lag anxiety
      prov <- rnorm(J*K,0,.24)          # time level provision
      rec  <- sqrt(.25)*((1-(cor.PnR.Supp^2))*
                           rnorm(J*K,0,1)+sqrt(cor.PnR.Supp)*prov) # time level receipt

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
      b0.int    <- rnorm(J,b0,sqrt(vsub.b0))  #generate an intercept for each person
      b1.laganx <- rnorm(J,b1,sqrt(vsub.b1))  #generate a slope for lag anxiety for each person
      b3.rec    <- rnorm(J,b3,sqrt(vsub.b3))  #generate a slope for receipt for each person
      anx <- rnorm(J*K, b0.int[person]            #use the person's intercept
                   +b1.laganx[person]*lanx   #lag anxiety slope
                   +b2*phase                 #average phase effect
                   +b3.rec[person]*rec               #average plus person-specific receipt effect
                   +b4*prov                  #average provision effect
                   +b5*phase*rec             #average phase by receipt effect
                   +b6*phase*prov            #average phase by provision effect
                   ,sqrt(vresid))            #residual

      return(data.frame(person,time,phase,prov,rec,lanx,anx))
    }

    fake = sim.dat(J,K,cor.PnR.supp)               #generate a fake dataset
    lme.power = suppressWarnings(suppressMessages(lme4::lmer(anx ~ (1 + lanx + rec || person)
                                                             + lanx + phase + prov + rec + prov*phase + rec*phase,
                                                             data=fake)))         #analyze it
    est = lme4::fixef(lme.power)               #save the parameter estimate
    se = arm::se.fixef(lme.power)             #save the standard error
    (abs(est)-1.96*se)>0               #calculate significance - returns TRUE/FALSE

  }
  # close(pb)
  parallel::stopCluster(cl)
  return(colMeans(signif))
}
