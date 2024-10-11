#' Simulated power tests for a single Categorical IV and a Continuous DV
#'
#' @description
#' This function will generate data for one- and two-sample *t*-tests, *z*-test, and one-way ANOVA.
#' As these tests assume a normal distribution from the population, all simulated samples are drawn from such.
#'
#'
#' @param groups Positive Integer Scalar. The number of groups to perform the test with.
#' @param n Positive Integer Scalar or Vector. The number of observations per group. If a scalar is provided, all groups will have that number of observations. If a vector is provided, the there must be a number of integers equal to `groups`; these will be the numbers of observations in a given group.
#' @param means Numeric Vector. The theoretical means for each group.
#' @param sds Positive Numeric Integer or Vector. The theoretical standard deviation(s) for the group(s).
#'
#'
#' @export sim_dat_t_anova



sim_dat_t_anova = function(groups,
                           n,
                           means,
                           sds){
  if(length(groups) > 1 & typeof(groups) == "character") {
    group_names = groups
    groups = length(groups)
  } else group_names = NULL

  if(length(groups) > 1 | !all.equal(groups, as.integer(groups)) | groups <= 0) stop("'groups' must be a single positive integer.")
  if(length(n) != 1 & length(n) != groups) stop("'n' must be either a single positive integer or a vector of positive integers
                                                with the same length as the number of groups specified.")
  if(length(means) != groups) stop("Number of means provided does not match the number of groups.")
  if(length(sds) != 1 & length(sds) != groups) stop("'sds' must be either a single standard deviation value or
                                                    a vector of values specifying the sd for each group")
  if(!all(sds > 0)) stop("All standard deviation values must be positive.")

  if(length(n) == 1) n = rep(n, groups)
  if(length(sds) == 1) sds = rep(sds, groups)

  sim_dat = data.frame(group = c(), DV = c())

  if(is.null(group_names)){
    for(g in 1:groups){
      sim_dat = rbind(sim_dat,
                      data.frame(group = rep(g, n[g]),
                                 DV = rnorm(n[g], means[g], sds[g])))
    }
  } else {
    for(g in 1:groups){
      sim_dat = rbind(sim_dat,
                      data.frame(group = rep(group_names[g], n[g]),
                                 DV = rnorm(n[g], means[g], sds[g])))
    }
  }

  return(sim_dat)
}
