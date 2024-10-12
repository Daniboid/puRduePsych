#' Calculate power for *t*-test or One-Way Anova
#'
#' @description
#' Calculates power for a given means, sd, sample size(s), and alpha-level.
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
#' @param show_plot Logical (Boolean). Should a plot be output as well? Defaults to `FALSE`
#' @param effect_size Logical (Boolean). Should the standardized effect size (Cohen's *d* or *f2*) be returned? Defaults to `FALSE`
#'
#'
#' @import utils
#' @import rlang
#' @import stats
#' @import ggplot2
#' @import ggpubr
#' @export pow_t_anova
#'


pow_t_anova = function(groups,
                       n,
                       means,
                       sds,
                       alpha = .05,
                       alternative = NULL,
                       eq_var = NULL,
                       mu = NULL,
                       paired = F,
                       anova_type = NULL,
                       show_plot = F,
                       effect_size = F) {

  if(length(groups) > 1 | !all.equal(groups, as.integer(groups)) | groups <= 0) stop("'groups' must be a single positive integer.")
  if(length(n) != 1 & length(n) != groups) stop("'n' must be either a single positive integer or a vector of positive integers
                                                with the same length as the number of groups specified.")
  if(length(means) != groups) stop("Number of means provided does not match the number of groups.")
  if(length(sds) != 1 & length(sds) != groups) stop("'sds' must be either a single standard deviation value or
                                                    a vector of values specifying the sd for each group")
  if(!all(sds > 0)) stop("All standard deviation values must be positive.")
  if(groups == 2 & paired & length(n) > 1 & !all(n==n[1]) ) stop("Paired samples *t*-tests need equal group sizes.")

  requireNamespace("car")

  if(groups == 1 | (paired & groups == 2)) { # Is it a one-sample test?
    # notify about ignoring
    if(!is.null(anova_type)) rlang::warn("`anova_type` provided for a one-sample test; this argument will be ignored...")
    if(paired & groups == 1) rlang::warn("`paired` provided for a one sample test; this argument will be ignored...")
    if(!is.null(eq_var)) rlang::warn("`eq_var` provided for a one sample test; this argument will be ignored...")

    # set defaults
    if (is.null(mu)) mu = 0
    if (is.null(alternative)) alternative = "two.sided"

    # find critical values
    crit_vals = ifelse(c(alternative, alternative) == "two.sided",
                       qt(c(alpha/2, 1-(alpha/2)), df = n-1),
                       ifelse(alternative == 'less',
                              qt(alpha, df = n-1),
                              qt(1-alpha, df = n-1)))
    # Cohen's d
    d = ifelse(paired & length(sds) == 1,
               (means[1]-means[2])/sds,
               ifelse(paired,
                      stop('Cannot currently compute power for paired samples with different SDs...'),
                      (means - mu)/sds))

    #NCP
    ncp = d*sqrt(n)

    # Power
    pow = ifelse(alternative == "two.sided",
                 pt(min(crit_vals), df = n-1, ncp = ncp) + pt(max(crit_vals), n-1, ncp = ncp, lower.tail = F),
                 ifelse(alternative == "less",
                        pt(crit_vals, df = n-1, ncp = ncp),
                        pt(crit_vals, df = n-1, ncp = ncp, lower.tail = F)))

    if (show_plot){
      t = seq(-4,+4,by=.1)
      plot_df = data.frame(t,
                           Null = dt(t, df=n-1),
                           Alternative = dt(t, df = n-1, ncp = ncp)) |>
        tidyr::pivot_longer(2:3, names_to = "Distribution", values_to = "Density")
      plt = ggplot2::ggplot(plot_df, ggplot2::aes(x = t, y = Density, color = Distribution)) +
        ggplot2::geom_line(linewidth = 1.5) +
        ggplot2::geom_vline(xintercept=crit_vals, linewidth = 1.5, color = "red", linetype = 2) +
        ggpubr::theme_pubr() +
        ggplot2::scale_color_manual(values = c("#cfb991", "#000000"))


      if (alternative == "two.sided") {
        plt = plt +
         ggplot2::geom_ribbon(
            data = plot_df[plot_df$Distribution == "Alternative" &
                             plot_df$t < min(crit_vals) ,],
            mapping = ggplot2::aes(ymin = 0,
                          ymax = Density),
            fill = "#6f727b", alpha = .5) +
         ggplot2::geom_ribbon(
            data = plot_df[plot_df$Distribution == "Alternative" &
                             plot_df$t > max(crit_vals) ,],
            mapping = ggplot2::aes(ymin = 0,
                          ymax = Density),
            fill = "#6f727b", alpha = .5)
      } else if (alternative == "less") {
        plt = plt +
         ggplot2::geom_ribbon(
            data = plot_df[plot_df$Distribution == "Alternative" &
                             plot_df$t < min(crit_vals) ,],
            mapping = ggplot2::aes(ymin = 0,
                          ymax = Density),
            fill = "#6f727b", alpha = .5)
      } else {
        plt = plt +
         ggplot2::geom_ribbon(
            data = plot_df[plot_df$Distribution == "Alternative" &
                             plot_df$t > max(crit_vals) ,],
            mapping = ggplot2::aes(ymin = 0,
                          ymax = Density),
            fill = "#6f727b", alpha = .5)
      }
    }

  } else if (groups == 2){ # Is it a two-sample test?
    # notify about ignoring
    if(!is.null(anova_type)) rlang::warn("`anova_type` provided for a two-sample test; this argument will be ignored...")
    if(!is.null(mu)) rlang::warn("`mu` provided for a two-sample test; this argument will be ignored...")

    # set defaults
    if(is.null(alternative)) alternative = "two.sided"
    if(is.null(eq_var)) eq_var = F


    if (eq_var & !paired) {
      if(length(n)==1) df = n*2-2 else df = sum(n)-2


      # find critical value(s)
      crit_vals = ifelse(c(alternative, alternative) == "two.sided",
                         qt(c(alpha/2, 1-(alpha/2)), df = df),
                         ifelse(alternative == 'less',
                                qt(alpha, df = df),
                                qt(1-alpha, df = df)))
      # Cohen's d
      d = ifelse(length(sds)== 1,
                 (means[1] - means[2])/sds,
                 ifelse(length(n) == 1,
                        (means[1] - means[2]) / sqrt((sds[1]^2+sds[2]^2)/2),
                        (means[1] - means[2]) / sqrt(((n[1]-1)*sds[1]^2+(n[2]-1)*sds[2]^2)/(sum(n)-2))))

      #NCP
      if(length(n)==1) ncp = d*sqrt(n/2) else ncp = d*(1/sqrt(1/n[1]+1/n[2]))

      # Power
      pow = ifelse(alternative == "two.sided",
                   pt(min(crit_vals), df = df, ncp = ncp) + pt(max(crit_vals), df, ncp = ncp, lower.tail = F),
                   ifelse(alternative == "less",
                          pt(crit_vals, df = df, ncp = ncp),
                          pt(crit_vals, df = df, ncp = ncp, lower.tail = F)))

      if (show_plot){
        t = seq(-4,+4,by=.1)
        plot_df = data.frame(t,
                             Null = dt(t, df=df),
                             Alternative = dt(t, df = df, ncp = ncp)) |>
          tidyr::pivot_longer(2:3, names_to = "Distribution", values_to = "Density")
        plt = ggplot2::ggplot(plot_df, ggplot2::aes(x = t, y = Density, color = Distribution)) +
          ggplot2::geom_line(linewidth = 1.5) +
          ggplot2::geom_vline(xintercept=crit_vals, linewidth = 1.5, color = "red", linetype = 2) +
          ggpubr::theme_pubr() +
          ggplot2::scale_color_manual(values = c("#cfb991", "#000000"))

        pwr::pwr.t2n.test(n1 = 25, n2 = 30, d = .2)

        if (alternative == "two.sided") {
          plt = plt +
           ggplot2::geom_ribbon(
              data = plot_df[plot_df$Distribution == "Alternative" &
                               plot_df$t < min(crit_vals) ,],
              mapping = ggplot2::aes(ymin = 0,
                            ymax = Density),
              fill = "#6f727b", alpha = .5) +
           ggplot2::geom_ribbon(
              data = plot_df[plot_df$Distribution == "Alternative" &
                               plot_df$t > max(crit_vals) ,],
              mapping = ggplot2::aes(ymin = 0,
                            ymax = Density),
              fill = "#6f727b", alpha = .5)
        } else if (alternative == "less") {
          plt = plt +
           ggplot2::geom_ribbon(
              data = plot_df[plot_df$Distribution == "Alternative" &
                               plot_df$t < min(crit_vals) ,],
              mapping = ggplot2::aes(ymin = 0,
                            ymax = Density),
              fill = "#6f727b", alpha = .5)
        } else {
          plt = plt +
           ggplot2::geom_ribbon(
              data = plot_df[plot_df$Distribution == "Alternative" &
                               plot_df$t > max(crit_vals) ,],
              mapping = ggplot2::aes(ymin = 0,
                            ymax = Density),
              fill = "#6f727b", alpha = .5)
        }
      }
    } else if (!eq_var & !paired) {
      # compute DF
      df = ifelse(length(sds) == 2 & length(n) == 2,
                  (sds[1]^2/n[1] + sds[2]^2/n[2])^2 / ((sds[1]^2/n[1])^2/(n[1]-1) + (sds[2]^2/n[2])^2/(n[2]-1)),
                  ifelse(length(sds) == 2 & length(n) == 1,
                         (sds[1]^2/n + sds[2]^2/n)^2 / ((sds[1]^2/n)^2/(n-1) + (sds[2]^2/n)^2/(n-1)),
                         ifelse(length(sds) == 1 & length(n) == 2,
                                (sds^2/n[1] + sds^2/n[2])^2 / ((sds^2/n[1])^2/(n[1]-1) + (sds^2/n[2])^2/(n[2]-1)),
                                n*2-2)))

      # find critical value(s)
      crit_vals = ifelse(c(alternative, alternative) == "two.sided",
                         qt(c(alpha/2, 1-(alpha/2)), df = df),
                         ifelse(alternative == 'less',
                                qt(alpha, df = df),
                                qt(1-alpha, df = df)))
      # Cohen's d
      d = ifelse(length(sds)== 1,
                 (means[1] - means[2])/sds,
                 ifelse(length(n) == 1,
                        (means[1] - means[2]) / sqrt((sds[1]^2+sds[2]^2)/2),
                        (means[1] - means[2]) / sqrt(((n[1]-1)*sds[1]^2+(n[2]-1)*sds[2]^2)/(sum(n)-2))))

      #NCP
      if(length(n)==1) ncp = d*sqrt(n/2) else ncp = d*(1/sqrt(1/n[1]+1/n[2]))

      # Power
      pow = ifelse(alternative == "two.sided",
                   pt(min(crit_vals), df = df, ncp = ncp) + pt(max(crit_vals), df, ncp = ncp, lower.tail = F),
                   ifelse(alternative == "less",
                          pt(crit_vals, df = df, ncp = ncp),
                          pt(crit_vals, df = df, ncp = ncp, lower.tail = F)))

      if (show_plot){
        t = seq(-4,+4,by=.1)
        plot_df = data.frame(t,
                             Null = dt(t, df=df),
                             Alternative = dt(t, df = df, ncp = ncp)) |>
          tidyr::pivot_longer(2:3, names_to = "Distribution", values_to = "Density")
        plt = ggplot2::ggplot(plot_df, ggplot2::aes(x = t, y = Density, color = Distribution)) +
          ggplot2::geom_line(linewidth = 1.5) +
          ggplot2::geom_vline(xintercept=crit_vals, linewidth = 1.5, color = "red", linetype = 2) +
          ggpubr::theme_pubr() +
          ggplot2::scale_color_manual(values = c("#cfb991", "#000000"))


        if (alternative == "two.sided") {
          plt = plt +
           ggplot2::geom_ribbon(
              data = plot_df[plot_df$Distribution == "Alternative" &
                               plot_df$t < min(crit_vals) ,],
              mapping = ggplot2::aes(ymin = 0,
                            ymax = Density),
              fill = "#6f727b", alpha = .5) +
           ggplot2::geom_ribbon(
              data = plot_df[plot_df$Distribution == "Alternative" &
                               plot_df$t > max(crit_vals) ,],
              mapping = ggplot2::aes(ymin = 0,
                            ymax = Density),
              fill = "#6f727b", alpha = .5)
        } else if (alternative == "less") {
          plt = plt +
           ggplot2::geom_ribbon(
              data = plot_df[plot_df$Distribution == "Alternative" &
                               plot_df$t < min(crit_vals) ,],
              mapping = ggplot2::aes(ymin = 0,
                            ymax = Density),
              fill = "#6f727b", alpha = .5)
        } else {
          plt = plt +
           ggplot2::geom_ribbon(
              data = plot_df[plot_df$Distribution == "Alternative" &
                               plot_df$t > max(crit_vals) ,],
              mapping = ggplot2::aes(ymin = 0,
                            ymax = Density),
              fill = "#6f727b", alpha = .5)
        }
      }
    }

  } else { # Is it a one-way ANOVA?
    # notify about ignoring
    if(!is.null(mu)) rlang::warn("`mu` provided for an ANOVA; this argument will be ignored...")
    if(!is.null(alternative)) rlang::warn("`alternative` provided for a test with 3 or more groups. One-Way ANOVA is always right-tailed; this argument will be ignored...")
    if(paired) rlang::warn("`paired` provided for a test with 3 or more groups. This function cannot currently handle repeated
                              measures ANOVA; this argument will be ignored...")



    # calculate DF
    df1 = groups - 1
    df2 = ifelse(length(n) == 1,
                 (n-1) * groups,
                 sum(n) - groups)
    # set defaults
    if(is.null(anova_type)) anova_type = 3

    # Calculate eta-squared
    if (length(n) == 1) SS_a = n*sum((means-mean(means))^2) else SS_a = sum(n*(means-mean(unlist(foreach::foreach(m = means, n = n) %do% {rep(m, n)})))^2)
    if(length(sds)==1) SS_e = sds^2*(n-1)*groups else SS_e = sum(sds^2*(n-1))
    eta_sq = SS_a/(SS_a+SS_e)
    f2 = eta_sq/(1-eta_sq)
    print(f2)

    # Cohen's f
    if(length(n)==1) ncp = groups * n * eta_sq/(1-eta_sq) else ncp = sum(n)*eta_sq/(1-eta_sq)

    # Determine critical value
    crit_val = qf(1-alpha, df1, df2)

    pow = pf(crit_val, df1, df2, ncp, lower.tail = F)

    if (show_plot){
      `F` = seq(0,25,by=.1)
      plot_df = data.frame(`F`,
                           Null = df(`F`, df1, df2),
                           Alternative = df(`F`, df1, df2, ncp = ncp)) |>
        tidyr::pivot_longer(2:3, names_to = "Distribution", values_to = "Density")
      plt = ggplot2::ggplot(plot_df, ggplot2::aes(x = `F`, y = Density, color = Distribution)) +
        ggplot2::geom_line(linewidth = 1.5) +
        ggplot2::geom_vline(ggplot2::aes(xintercept=crit_val), linewidth = 1.5, color = "red", linetype = 2) +
        ggpubr::theme_pubr() +
        ggplot2::geom_ribbon(data = plot_df[plot_df$Distribution == "Alternative" &
                                              plot_df$`F` > crit_val,],
                             mapping = ggplot2::aes(ymin = 0, ymax = Density),
                             fill = "#6f727b", alpha = .5) +
        ggplot2::scale_color_manual(values = c("#cfb991", "#000000"))
      }
  }

  return_list = list(power = pow)
  if(show_plot) return_list$plot = plt
  if(effect_size) return$effect_size = ifelse(groups < 3, d, f2)

  return(return_list)
}
