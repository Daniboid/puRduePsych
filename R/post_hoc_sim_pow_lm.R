#' Simulate Power for A Linear Model
#'
#' @description
#' see title
#'
#'
#'
#'
#' @import parallel
#' @import doSNOW
#' @export post_hoc_sim_pow_lm
#' @export determine_n_lm
#'
#'



post_hoc_sim_pow_lm = function (lm,
                                n_forSim=NULL,
                                n_sims=10000,
                                alpha=.05){

  cl = parallel::makeCluster(parallel::detectCores())
  doSNOW::registerDoSNOW(cl)
  pb = txtProgressBar(min = 1, max = n_sims, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  signif = foreach (s = 1:n_sims, .combine = rbind, .options.snow=opts
  ) %dopar% {
    sim_dat_lm = function (lm,
                           n_forSim=NULL){
      warning("This function doesn't work yet, and has not been fully tested...")
      warning("This function assumes all numeric variables in the model are normally distributed, and the same number of observations of all factor (nominal and ordinal) variables.")

      if(is.null(n_forSim)) n_forSim = nrow(lm$model)

      coefs = data.frame(summary(lm)$coefficients)
      colnames(coefs) = c("est", "se","t","p")
      coefs$sd = coefs$se * sqrt(nrow(lm$model))

      dv = as.character(lm$terms)[2]
      x_term = as.character(lm$terms)[3]
      ivs = unlist(strsplit(x_term, " \\+ "))


      observation = rep(1:n_forSim)     # Create a number of rows
      df_sim = data.frame(observation)

      for (iv in ivs){ # For each IV...
        if (is.numeric(lm$model[,iv])) { # if it is numeric...
          tmp_mu = mean(lm$model[,iv]) # calculate the mean...
          tmp_sd = sd(lm$model[,iv])   # and standard deviation...
          df_sim[,iv] = rnorm(n_forSim, tmp_mu, tmp_sd) # use those to generate a simulated sample...
        } else {
          tmp_ratio = table(lm$model[,iv])/sum(table(lm$model[,iv]))
          tmp_vect  = rep(names(tmp_ratio), round(tmp_ratio*n_forSim))
          if (length(tmp_vect) < nrow(df_sim)) tmp_vect = c(tmp_vect, rep(names(tmp_ratio[tmp_ratio == max(tmp_ratio)]),
                                                                          nrow(df_sim) - length(tmp_vect)))
          if(length(tmp_vect) > nrow(df_sim)) tmp_vect = tmp_vect[-length(tmp_vect)]
          df_sim[,iv] = factor(tmp_vect)
        } # otherwise assume the same number of nominal and ordinal observations in the simulated sample.
      }

      df_sim$perf_pred_dv = predict(lm, newdata=df_sim)

      sd_resid  = sd(lm$residuals)

      df_sim[,dv] <- rnorm(n_forSim,
                           mean(df_sim$perf_pred_dv),
                           sd_resid)

      return(df_sim)
    }

    fake = sim_dat_lm(lm, n_forSim)               #generate a fake dataset
    lm_tmp = suppressWarnings(suppressMessages(lm(lm$call$formula,
                                                  data = fake)))

    coefs = data.frame(summary(lm_tmp)$coefficients)
    colnames(coefs) = c("est", "se","t","p")
    for_return = coefs$p < alpha
    names(for_return) = rownames(coefs)
    return(for_return)
  }
  close(pb)
  parallel::stopCluster(cl)
  return(colMeans(signif))
}

determine_n_lm = function(lm,
                          effect,#=NULL,
                          n_sims=10000,
                          alpha=.05,
                          n_step=1,
                          desired_power=.8,
                          verbose=T) {
  if(!exists("effect")) stop("Effect not defined.")
  if(!exists("lm")) stop("lm not defined.")
  post_hoc_sim_pow_lm = function (lm,
                                  n_forSim=NULL,
                                  n_sims=10000,
                                  alpha=.05){

    cl = parallel::makeCluster(parallel::detectCores())
    doSNOW::registerDoSNOW(cl)
    pb = txtProgressBar(min = 1, max = n_sims, style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress=progress)
    signif = foreach (s = 1:n_sims, .combine = rbind, .options.snow=opts
    ) %dopar% {
      sim_dat_lm = function (lm,
                             n_forSim=NULL){
        warning("This function doesn't work yet, and has not been fully tested...")
        warning("This function assumes all numeric variables in the model are normally distributed, and the same number of observations of all factor (nominal and ordinal) variables.")

        if(is.null(n_forSim)) n_forSim = nrow(lm$model)

        coefs = data.frame(summary(lm)$coefficients)
        colnames(coefs) = c("est", "se","t","p")
        coefs$sd = coefs$se * sqrt(nrow(lm$model))

        dv = as.character(lm$terms)[2]
        x_term = as.character(lm$terms)[3]
        ivs = unlist(strsplit(x_term, " \\+ "))
        # if(is.null(effect)) effect = ivs


        observation = rep(1:n_forSim)     # Create a number of rows
        df_sim = data.frame(observation)

        for (iv in ivs){ # For each IV...
          if (is.numeric(lm$model[,iv])) { # if it is numeric...
            tmp_mu = mean(lm$model[,iv]) # calculate the mean...
            tmp_sd = sd(lm$model[,iv])   # and standard deviation...
            df_sim[,iv] = rnorm(n_forSim, tmp_mu, tmp_sd) # use those to generate a simulated sample...
          } else {
            tmp_ratio = table(lm$model[,iv])/sum(table(lm$model[,iv]))
            tmp_vect  = rep(names(tmp_ratio), round(tmp_ratio*n_forSim))
            if (length(tmp_vect) < nrow(df_sim)) tmp_vect = c(tmp_vect, rep(names(tmp_ratio[tmp_ratio == max(tmp_ratio)]),
                                                                            nrow(df_sim) - length(tmp_vect)))
            if(length(tmp_vect) > nrow(df_sim)) tmp_vect = tmp_vect[-length(tmp_vect)]
            df_sim[,iv] = factor(tmp_vect)
          } # otherwise assume the same number of nominal and ordinal observations in the simulated sample.
        }

        df_sim$perf_pred_dv = predict(lm, newdata=df_sim)

        sd_resid  = sd(lm$residuals)

        df_sim[,dv] <- rnorm(n_forSim,
                             mean(df_sim$perf_pred_dv),
                             sd_resid)

        return(df_sim)
      }

      fake = sim_dat_lm(lm, n_forSim)               #generate a fake dataset
      lm_tmp = suppressWarnings(suppressMessages(lm(lm$call$formula,
                                                    data = fake)))

      coefs = data.frame(summary(lm_tmp)$coefficients)
      colnames(coefs) = c("est", "se","t","p")
      for_return = coefs$p < alpha
      names(for_return) = rownames(coefs)
      return(for_return)
    }
    close(pb)
    cat("\n")
    parallel::stopCluster(cl)
    return(colMeans(signif))
  }

  current_power = post_hoc_sim_pow_lm(lm)[effect]

  cat(paste("\nCurrent power for", names(current_power),"based on existing data is:", current_power, "\n\n"))
  if (all(current_power >= desired_power)) {
    cat("No need to calculate further\n\n")
    return(nrow(lm$model))
  } else {
    n_obs = nrow(lm$model)
    while (all(current_power < desired_power)){
      n_obs = n_obs + n_step
      if(verbose) cat(paste("\n\tChecking if a sample of", n_obs, "observations is enough to meet the deisred power...\n"))
      current_power = post_hoc_sim_pow_lm(lm, n_obs)[effect]
    }

    if(verbose) cat(paste("\nA sample of", n_obs, "should be sufficient to power for the effect of",
                          effect, "at a power of", desired_power, ".\n"))
    return(n_obs)
  }
}
