#' Determine N required to have a significant *t*-test or one-way ANOVA
#'
#' @description See Title
#'
#'
#'
#' @param groups Positive Integer Scalar. The number of groups to perform the test with.
#' @param means Numeric Vector. The theoretical means for each group.
#' @param sds Positive Numeric Integer or Vector. The theoretical standard deviation(s) for the group(s).
#' @param desired_power Numeric (Float) between 0 and 1. The desired power of the test. Defaults to .8
#' @param alpha Positive Scalar between 0 and 1. The significance threshold for the test. Defaults to .05
#' @param alternative Character (String). The alternative hypothesis to be used by a t-test. Values of "two.sided" (default), "less", or "greater" are valid. Ignored if there are 3 or more groups.
#' @param mu Integer Scalar. Null mean to test against for a one-sample test. Defaults to 0. Ignored if groups are greater than 1.
#' @param eq_var Logical (Boolean). Should a two-sample t-test assume equal variance (i.e., perform Student's *t*-test). Defaults to FALSE. Ignored if only 1 group or more than 2.
#' @param paired Logical (Boolean). Should a two-sample t-test be conducted in a pairwise manner. Defaults to FALSE. Ignored if only 1 group or more than 2.
#' @param anova_type Integer Scalar from 1 to 3. The type of Sums of Squared Deviations to be used for the one-way ANOVA. Ignored if less than 3 groups.
#' @param effect_size Logical (Boolean). Should the standardized effect size (Cohen's *d* or *f2*) be returned? Defaults to `FALSE`
#'
#'
#' @import utils
#' @import rlang
#' @import stats
#' @import ggplot2
#' @import ggpubr
#' @export determine_n_t_anova
#'

determine_n_t_anova = function(groups,
                               means,
                               sds,
                               alpha = .05,
                               desired_power = .8,
                               alternative = NULL,
                               eq_var = NULL,
                               mu = NULL,
                               paired = F,
                               anova_type = NULL,
                               effect_size = F) {

  if(length(groups) > 1 | !all.equal(groups, as.integer(groups)) | groups <= 0) stop("'groups' must be a single positive integer.")
  if(desired_power < 0 | desired_power > 1 | !is.numeric(desired_power)) stop("`desired_power` must be a number between 0 and 1.")
  if(length(means) != groups) stop("Number of means provided does not match the number of groups.")
  if(length(sds) != 1 & length(sds) != groups) stop("'sds' must be either a single standard deviation value or a vector of values specifying the sd for each group")
  if(!all(sds > 0)) stop("All standard deviation values must be positive.")
  if(groups == 2 & paired & length(n) > 1 & !all(n==n[1]) ) stop("Paired samples *t*-tests need equal group sizes.")

  n = 2
  tmp_pow = puRduePsych::pow_t_anova(groups, n, means, sds, alpha, alternative, eq_var, mu, paired, anova_type)

  while(tmp_pow$power < desired_power){
    n = n+1
    tmp_pow = puRduePsych::pow_t_anova(groups, n, means, sds, alpha, alternative, eq_var, mu, paired, anova_type, effect_size)
  }

  print(paste("n per group for the desired power:", n))

  return_list = list(n=n)
  if(effect_size & groups < 3) return_list$d = tmp_pow$d else if (effect_size) return_list$f2 = tmp_pow$f2
  return(return_list)
}

