#' Simulate new data based on a Linear Regression Model
#'
#' @description
#' see title
#'
#' @param lm Object of type `lm`. The object from which to generate a new data-set.
#' @param n_forSim Integer Scalar. The number of observations to create in the new data. If `NULL` the same number of observations as in the original `lm` will be used.
#'
#'
#' @import stats
#' @import utils
#' @export sim_dat_lm
#'

sim_dat_lm = function (lm,
                       n_forSim=NULL){
  warning("This function has not been fully tested...")
  warning("This function assumes all numeric variables in the model are normally distributed, and the same ratio of observations of all factor (nominal and ordinal) variables.")

  if(is.null(n_forSim)) n_forSim = nrow(lm$model)

  coefs = data.frame(summary(lm)$coefficients)
  colnames(coefs) = c("est", "se","t","p")
  coefs$sd = coefs$se * sqrt(nrow(lm$model))

  dv = as.character(lm$terms)[2]
  x_term = as.character(lm$terms)[3]
  ivs = unlist(strsplit(x_term, " \\+ "))


  observation = rep(1:n_forSim)     # Create a number of rows
  df_sim = base::data.frame(observation)

  for (iv in ivs){ # For each IV...
    if (is.numeric(lm$model[,iv])) { # if it is numeric...
      tmp_mu = stats::mean(lm$model[,iv]) # calculate the mean...
      tmp_sd = stats::sd(lm$model[,iv])   # and standard deviation...
      df_sim[,iv] = stats::rnorm(n_forSim, tmp_mu, tmp_sd) # use those to generate a simulated sample...
    } else {
      tmp_ratio = table(lm$model[,iv])/sum(table(lm$model[,iv]))
      tmp_vect  = rep(names(tmp_ratio), round(tmp_ratio*n_forSim))
      if (length(tmp_vect) < nrow(df_sim)) tmp_vect = c(tmp_vect, rep(names(tmp_ratio[tmp_ratio == max(tmp_ratio)]),
                                                                      nrow(df_sim) - length(tmp_vect)))
      if(length(tmp_vect) > nrow(df_sim)) tmp_vect = tmp_vect[-length(tmp_vect)]
      df_sim[,iv] = factor(tmp_vect)
    } # otherwise assume the same number of nominal and ordinal observations in the simulated sample.
  }

  df_sim$perf_pred_dv = stats::predict.lm(lm, newdata=df_sim)

  sd_resid  = stats::sd(lm$residuals)

  df_sim[,dv] <- stats::rnorm(n_forSim,
                              stats::mean(df_sim$perf_pred_dv),
                              sd_resid)

  return(df_sim)
}

