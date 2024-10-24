#' Simulate new data based on an extant ANOVA
#'
#' @description
#' see title
#'
#' @param aov_in Object of type `aov`. The object from which to generate a new data-set.
#' @param n_forSim Integer Scalar. The number of observations to create in the new data. If `NULL` the same number of observations as in the original `lm` will be used.
#'
#'
#' @import stats
#' @import utils
#' @import psych
#' @export sim_dat_anova
#'


sim_dat_anova = function(aov_in,
                         n_forSim = NULL){

  if(is.null(n_forSim)) n_forSim = nrow(aov_in$model)


  dv = as.character(aov_in$terms)[2]
  x_term = as.character(aov_in$terms)[3]
  ivs = unlist(strsplit(x_term," [\\+|\\*] "))

  observation = rep(1:n_forSim)     # Create a number of rows
  df_sim = base::data.frame(observation)

  for (iv in ivs){ # For each IV...
    if (is.numeric(aov_in$model[,iv])) { # if it is numeric...
      tmp_mu = stats::mean(aov_in$model[,iv]) # calculate the mean...
      tmp_sd = stats::sd(aov_in$model[,iv])   # and standard deviation...
      df_sim[,iv] = stats::rnorm(n_forSim, tmp_mu, tmp_sd) # use those to generate a simulated sample...
    } else {
      if (n_forSim == nrow(aov_in$model)) df_sim[,iv] = aov_in[,iv] else {
        tmp_ratio = table(aov_in$model[,iv])/sum(table(aov_in$model[,iv]))
        tmp_vect  = rep(names(tmp_ratio), round(tmp_ratio*n_forSim))
        if (length(tmp_vect) < nrow(df_sim)) tmp_vect = c(tmp_vect, rep(names(tmp_ratio[tmp_ratio == max(tmp_ratio)]),
                                                                        nrow(df_sim) - length(tmp_vect)))
        if(length(tmp_vect) > nrow(df_sim)) tmp_vect = tmp_vect[-length(tmp_vect)]
        df_sim[,iv] = sample(factor(tmp_vect))
      }
    } # otherwise assume the same ratio of nominal and ordinal observations in the simulated sample, but randomize them.
  }

  df_sim$perf_pred_dv = stats::predict.lm(aov_in, newdata=df_sim)

  sd_resid  = stats::sd(aov_in$residuals)

  df_sim[,dv] <- df_sim$perf_pred_dv + stats::rnorm(n_forSim,
                                                    0,
                                                    sd_resid)

  return(df_sim)
}
