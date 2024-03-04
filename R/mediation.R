#' Simple Mediation analyses
#'
#' @description
#' The `psych::mediate()` function is recommended over this, as, at present, this function
#' handles only simple mediation models (i.e., one IV and one mediator). The function from the
#' `psych` package is able to much more sophisticated models and will output a diagram for you.
#'
#' @param mediator Character. The name of the mediator to be modeled.
#' @param y_xm_reg An object generated from the `lm()` function. This regression should be for the outcome variable regressed on both the primary predictor and the mediator.
#' @param std Logical. If `True` (default) the function will output rows for both the unstandardized and standardized mediation effects.
#'
#' @returns Returns a `data.frame` with the mediation effect and test of significance.

lm_mediation = function(mediator,
                        y_xm_reg,
                        std = T) {
  if (ncol(y_xm_reg$model) != 3) stop("The outcome sohuld be regressed on exactly two variables")
  if (!mediator %in% colnames(y_xm_reg$model)) stop("The specified mediator is not included in the regression output")

  str_form = as.character(y_xm_reg$call$formula)

  y     = str_form[2]
  preds = strsplit(str_form[3], " + ", fixed = T)[[1]]
  x     = preds[preds != mediator]

  m_x_reg = stats::lm(stats::formula(paste(mediator, "~", x)), y_xm_reg$model)
  y_x_reg = stats::lm(stats::formula(paste(y,        "~", x)), y_xm_reg$model)

  y_xm_coef = summary(y_xm_reg)$coefficients
  y_x_coef  = summary(y_x_reg)$coefficients
  m_x_coef  = summary(m_x_reg)$coefficients

  c       = y_x_coef[x, "Estimate"]
  c_prime = y_xm_coef[x, "Estimate"]

  a = m_x_coef[x, "Estimate"]
  b = y_xm_coef[mediator, "Estimate"]

  effect = c - c_prime

  se = sqrt((m_x_coef[x, "Std. Error"]^2) * (y_xm_coef[mediator, "Estimate"]^2) +
              (m_x_coef[x, "Estimate"]^2) * (y_xm_coef[mediator, "Std. Error"]^2) +
              (m_x_coef[x, "Std. Error"]^2) * (y_xm_coef[mediator, "Std. Error"]^2))

  z_val = effect/se

  p_val = stats::pnorm(z_val)

  for_return = data.frame(c, c_prime, a, b, effect, se, z_val, p_val)

  if(std){
    y_xm_std_reg = stats::lm(y_xm_reg$call$formula,               data.frame(scale(y_xm_reg$model)))
    y_x_std_reg  = stats::lm(stats::as.formula(paste(y, "~", x)),        data.frame(scale(y_xm_reg$model)))
    m_x_std_reg  = stats::lm(stats::as.formula(paste(mediator, "~", x)), data.frame(scale(y_xm_reg$model)))


    y_xm_coef = summary(y_xm_std_reg)$coefficients
    y_x_coef  = summary(y_x_std_reg)$coefficients
    m_x_coef  = summary(m_x_std_reg)$coefficients

    tau       = y_x_coef[x, "Estimate"]
    tau_prime = y_xm_coef[x, "Estimate"]

    alpha = m_x_coef[x, "Estimate"]
    beta  = y_xm_coef[mediator, "Estimate"]

    std_effect = tau - tau_prime

    se = sqrt((m_x_coef[x, "Std. Error"]^2) * (y_xm_coef[mediator, "Estimate"]^2) +
                (m_x_coef[x, "Estimate"]^2) * (y_xm_coef[mediator, "Std. Error"]^2) +
                (m_x_coef[x, "Std. Error"]^2) * (y_xm_coef[mediator, "Std. Error"]^2))

    z_val = std_effect/se

    p_val = stats::pnorm(z_val)

    for_return = rbind(for_return,
                       data.frame(c=tau, c_prime=tau_prime, a=alpha, b=beta, effect = std_effect, se, z_val, p_val))
    rownames(for_return) = c("Unstandardized", "Standardized")
  }

  return(for_return)
}
