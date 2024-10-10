#' Simulated power tests for a single Categorical IV and a Continuous DV
#'
#' @description
#' These functions will estimate power via simulation for one- and two-sample *t*-tests, *z*-test, and one-way ANOVA.
#' As these tests assume a normal distribution from the population, all simulated samples are drawn from such.
#'
#'
#' @param groups Positive Integer Scalar. The number of groups to perform the test with.
#' @param n Positive Integer Scalar or Vector. The number of observations per group. If a scalar is provided, all groups will have that number of observations. If a vector is provided, the there must be a number of integers equal to `groups`; these will be the numbers of observations in a given group.
#' @param means Numeric Vector. The theoretical means for each group.
#' @param sds Positive Numeric Integer or Vector. The theoretical standard deviation(s) for the group(s).
#' @param n_sims Positive Integer Scalar. The number of simulations to base the estimate on. Default is 10,000.
#' @param alpha Positive Scalar between 0 and 1. The significance threshold for the test. Defaults to .05
#' @param alternative Character (String). The alternative hypothesis to be used by a t-test. Values of "two.sided" (default), "less", or "greater" are valid. Ignored if there are 3 or more groups.
#' @param mu Integer Scalar. Null mean to test against for a one-sample test. Defaults to 0. Ignored if groups are greater than 1.
#' @param eq_var Logical (Boolean). Should a two-sample t-test assume equal variance (i.e., perform Student's *t*-test). Defaults to FALSE. Ignored if only 1 group or more than 2.
#' @param paired Logical (Boolean). Should a two-sample t-test be conducted in a pairwise manner. Defaults to FALSE. Ignored if only 1 group or more than 2.
#' @param anova_type Integer Scalar from 1 to 3. The type of Sums of Squared Deviations to be used for the one-way ANOVA. Ignored if less than 3 groups.
#'
#'
#' @import parallel
#' @import doSNOW
#' @import car
#' @export sim_pow_t_anova
#'

sim_pow_t_anova = function(groups,
                             n,
                             means,
                             sds,
                             n_sims=10000,
                             alpha = NULL,
                             alternative = NULL,
                             eq_var = NULL,
                             mu = NULL,
                             paired = NULL,
                             anova_type = NULL){
  if(length(groups) > 1 & typeof(groups) == "character") {
    group_names = groups
    groups = length(groups)
  }

  if(length(groups) > 1 | !all.equal(groups, as.integer(groups)) | groups <= 0) stop("'groups' must be a single positive integer.")
  if(length(n) != 1 & length(n) != groups) stop("'n' must be either a single positive integer or a vector of positive integers
                                                with the same length as the number of groups specified.")
  if(length(means) != groups) stop("Number of means provided does not match the number of groups.")
  if(length(sds) != 1 & length(sds) != groups) stop("'sds' must be either a single standard deviation value or
                                                    a vector of values specifying the sd for each group")
  if(!all(sds > 0)) stop("All standard deviation values must be positive.")


  requireNamespace("parallel")
  requireNamespace("doSNOW")
  requireNamespace("car")

  # Default Alpha
  if(is.null(alpha)) alpha = .05

  if(groups == 1){ # Is it a one-sample test?
    # notify about ignoring
    if(!is.null(anova_type)) warn("`anova_type` provided for a one-sample test; this argument will be ignored...")
    if(!is.null(paired)) warn("`paired` provided for a one sample test; this argument will be ignored...")
    if(!is.null(eq_var)) warn("`eq_var` provided for a one sample test; this argument will be ignored...")

    # set defaults
    if (is.null(mu)) mu = 0
    if (is.null(alternative)) alternative = "two.sided"

  } else if (groups == 2){ # Is it a two-sample test?
    # notify about ignoring
    if(!is.null(anova_type)) warn("`anova_type` provided for a two-sample test; this argument will be ignored...")
    if(!is.null(mu)) warn("`mu` provided for a two-sample test; this argument will be ignored...")

    # set defaults
    if(is.null(alternative)) alternative = "two.sided"
    if(is.null(eq_var)) eq_var = F
    if(is.null(paired)) paired = F
  } else { # Is it a one-way ANOVA?
    # notify about ignoring
    if(!is.null(mu)) warn("`mu` provided for an ANOVA; this argument will be ignored...")
    if(!is.null(alternative)) warn("`alternative` provided for a test with 3 or more groups. One-Way ANOVA is always right-tailed; this argument will be ignored...")
    if(!is.null(paired)) warn("`paired` provided for a test with 3 or more groups. This function cannot currently handle repeated
                              measures ANOVA; this argument will be ignored...")

    # set defaults
    if(is.null(anova_type)) anova_type = 3
  }


  cl = parallel::makeCluster(parallel::detectCores())
  doSNOW::registerDoSNOW(cl)
  pb = txtProgressBar(min = 1, max = n_sims, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  signif = foreach (s = 1:n_sims, .combine = rbind, .options.snow=opts
  ) %dopar% {
    sim_dat_t_anova = function(groups,
                               n,
                               means,
                               sds){
      if(length(groups) > 1 | !all.equal(groups, as.integer(groups)) | groups <= 0) stop("'groups' must be a single positive integer.")
      if(length(n) != 1 & length(n) != groups) stop("'n' must be either a single positive integer or a vector of positive integers
                                                with the same length as the number of groups specified.")
      if(length(means) != groups) stop("Means must be specified for each group.")
      if(length(sds) != 1 & length(sds) != groups) stop("'sds' must be either a single standard deviation value or
                                                    a vector of values specifying the sd for each group")
      if(!all(sds > 0)) stop("All standard deviation values must be positive.")

      if(length(n) == 1) n = rep(n, groups)
      if(length(sds) == 1) sds = rep(sds, groups)

      sim_dat = data.frame(group = c(), DV = c())

      for(g in 1:groups){
        sim_dat = rbind(sim_dat,
                        data.frame(group = rep(g, n[g]),
                                   DV = rnorm(n[g], means[g], sds[g])))
      }

      return(sim_dat)
    }

    fake = sim_dat_t_anova(groups, n, means, sds)   #generate a fake dataset
    pval = ifelse(groups == 1,
                  t.test(fake$DV, alternative = alternative, mu = mu, conf.level = 1-alpha)$p.val,
                  ifelse(groups == 2,
                         t.test(DV ~ group, data = fake, paired = paired, alternative = alternative, mu = mu, conf.level = 1-alpha)$p.val,
                         ifelse(anova_type == 1,
                                summary(aov(DV ~ group, data=fake))[[1]]$`Pr(>F)`[1],
                                car::Anova(aov(DV ~ group, data=fake), type = anova_type)$`Pr(>F)`[1])))
    for_return = pval < alpha
    return(for_return)
  }
  close(pb)
  parallel::stopCluster(cl)
  return(mean(signif))

}
