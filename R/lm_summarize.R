#' More detailed output for linear models
#'
#' @description
#' Output summaries for lms with more detail. This function returns a `data.frame` with the details regarding the regression coefficients. By default the columns include the unstandardized and standardized regression coefficients, the standard error of the unstandardized coefficients, *t*-values, significance, and partial and semi-partial correlation coefficients. Confidence intervals for the coefficients can also be output.
#'
#' @param lm An object created from the `lm()` function.
#' @param std Logical. If `True` (default) the standardized regression coefficients will be calculated and returned.
#' @param semi Logical. If `True` (default) the semi-partial correlation coefficients will be calculated and returned.
#' @param partial Logical. If `True` (default) the partial correlation coefficients will be calculated and returned.
#' @param CI Logical. If `True` confidence intervals will calculated and returned for the regression coefficients to be returned. Defaults to `False`.
#' @param CL Numeric. The confidence level of the confidence intervals to be returned. The value must be between 0 and 1. Defaults to .95, corresponding to a 95\% confidence level.
#' @param verbose Logical. If `True` the R^2 for the model and test of significance will be output to the console while the function runs. This will not be saved, as it is already part of the `lm` object and cam be accessed from there. Defaults to `False`.
#' @param correlations Logical. If `True` the zero-order correlation matrix will be output to the console while the function is running. Defaults to `False`.
#'
#' @returns A data.frame with the corresponding summary values.
#'
#' @export lm_summarize

lm_summarize = function(lm,
                        std = T,
                        semi = T,
                        partial = T,
                        CI = F,
                        CL = .95,
                        verbose = F,
                        correlations = F) { #= NULL){
  # if( (!is.null(lm) & ( !is.null(data) | !is.null(formula) )) |
  #     (is.null(data) & !is.null(formula)) | (!is.null(data) & is.null(formula)) ) stop("You must either provide an lm object or data and a formula.")

  if(!is.numeric(CL) | CL > 1 | CL <= 0) stop("Confidence Level (CL) needs to be a numeric value between 0 and 1.")

  dv = as.character(lm$call$formula)[2]


  summ_lm = summary(lm)

  lm_coef = data.frame(summ_lm$coefficients)
  names(lm_coef) = c("B", "S.E.", "t", "p.val")

  cors = stats::cor(lm$model)


  if(std) {
    lm_stdzd = summary(lm(lm$call$formula, data.frame(scale(lm$model))))
    lm_coef$Beta = data.frame(lm_stdzd$coefficients)$Estimate
    lm_coef$Beta[1] = NA
    lm_coef = lm_coef[,c("B", "S.E.", "Beta", "t", "p.val")]
  }

  if(semi & nrow(lm_coef) > 2) {
    lm_coef$semi = NA_real_
    for(x in 2:nrow(lm_coef)) {
      lm_coef$semi[x] = sqrt(summ_lm$r.squared -
                               summary(stats::lm(stats::as.formula(paste(dv, "~ .")), data = lm$model[,-x]))$r.squared)
      if(lm_coef$B[x] < 0 ) lm_coef$semi[x] = -1*lm_coef$semi[x]
    }
  } else if(semi) warning("Semi-partial correlations can only be obtained if more than one predictor is in the model. \n\t Semi-partial correlations were not computed...")



  if(partial & nrow(lm_coef) > 2) {
    lm_coef$partial = NA_real_
    for(x in 2:nrow(lm_coef)) {
      lm_coef$partial[x] = sqrt((summ_lm$r.squared -
                                   summary(stats::lm(stats::as.formula(paste(dv, "~ .")), data = lm$model[,-x]))$r.squared)/
                                  (1-summary(stats::lm(stats::as.formula(paste(dv, "~ .")), data = lm$model[,-x]))$r.squared)
                                )
      if(lm_coef$B[x] < 0 ) lm_coef$partial[x] = -1*lm_coef$partial[x]
    }
  } else if(partial) warning("Partial correlations can only be obtained if more than one predictor is in the model. \n\t Partial correlations were not computed...")

  if(CI){
    df = nrow(lm$model) - ncol(lm$model)
    t_crit = stats::qt(CL, df)

    lm_coef$CI.LB = lm_coef$B - (t_crit * lm_coef$S.E)
    lm_coef$CI.UB = lm_coef$B + (t_crit * lm_coef$S.E.)

    lm_coef = lm_coef[,c(1:2, ncol(lm_coef)-1, ncol(lm_coef), 3:(ncol(lm_coef)-2))]

    if(std){
      lm_coef$std.CI.LB = lm_stdzd$coefficients[,1] - (t_crit * lm_stdzd$coefficients[,2])
      lm_coef$std.CI.UB = lm_stdzd$coefficients[,1] + (t_crit * lm_stdzd$coefficients[,2])

      lm_coef = lm_coef[,c(1:5, ncol(lm_coef)-1, ncol(lm_coef), 6:(ncol(lm_coef)-2))]
    }
  }


  if (verbose) {
    cat(paste("\n\nR-Squared: ", round(summ_lm$r.squared,4), "\t\tAdj. R-Squared: ", round(summ_lm$adj.r.squared,4),"\n\t",
              "F (", summ_lm$fstatistic[2], ", ", summ_lm$fstatistic[3], ") = ", round(summ_lm$fstatistic[1],3), "\tp = ", signif(stats::pf(summ_lm$fstatistic[1],
                                                                                                                                    summ_lm$fstatistic[2],
                                                                                                                                    summ_lm$fstatistic[3],
                                                                                                                                    lower.tail = F),3),
              "\n\n",   sep = "")
        )

    print(lm_coef)
    cat("\n\n")
  }

  if (correlations) {
    cat("Correlations:\n")
    print(cors)
    cat("\n\n")
  }

  # return(list(table = c(lm_coef), rsquared = summ_lm$r.squared))
  return(lm_coef)
}
