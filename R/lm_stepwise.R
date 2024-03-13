#' A Function for Stepwise Regression
#'
#' @param y Either: Character or an object created by the `lm()` function. If this is argument is a character string it should specify the outcome varaible for the model. If it is a linear model object, it should be the full model with the outcome regressed on all predictors incuding those at each step.
#' @param steps List of Charactor Vectors. This list should specify the predictos in the model at each step.
#' At the highest level the list should be specified using the `list()` method, but each vector should be specified using the `c()` method.
#' For example, one version of the argument could be `steps = list(step1=c("pred1", "pred2"), step2=c("pred")`. If step names are not specified, the steps will be numbered in the order that they are specified.
#' @param data Data.frame. The data from which the model(s) should be calculated. This argument is necessary if `y` is a character string.
#'


lm_stepwise = function(y,
                       steps,
                       data = NULL) {
  warning("STEPWISE REGRESSIONS IN THE puRduePsych PACKAGE ARE NOT YET FUNCTIONAL...")

  # Check for things that will cause errors
  if(is.null(y) | is.null(steps)) stop("Both y and the steps need to be specified.")
  if(typeof(steps) != "list") stop("`steps` must be a list of vectors containing variable names.")
  if(length(steps) <= 1) stop("`steps` must contain more than 1 vector of variable names.")

  if(typeof(y) == "character"){
    if(is.null(data)) stop("Data is required when y is specifying an outcome variable.")
    if(!y %in% colnames(data)) stop("The outcome varaible is not in the data set.")

    mod = lm(as.formula(paste(y, "~", paste(unlist(steps), sep=" + "))),
           data)
  } else if(typeof(y) != "list" | is.null(y$model)) {
    stop("The `y` argument must either be a character string specifying the outcome or the full linear model from which the steps will be calculated.")
  } else {
    mod = y
    y = as.character(y$call$formula)[2]
  }

  if(!all(unlist(steps) %in% colnames(mod$model))) stop("Not all predictors are in the regression model.")

  if(is.null(names(steps))) names(steps) = rep("", length(steps))

  # Get the output...
  step_output = data.frame(
    step = c(),
    R_sq = c(),
    R_sq_adj = c(),
    mod_F = c(),
    mod_df1 = c(),
    mod_df2 = c(),
    mod_sig = c(),
    R_sq_change = c(),
    change_F = c(),
    change_df1 = c(),
    change_sig = c()
  )
  prev_R_sq = 0

  coefs_output = list()

  for(s in 1:length(steps)){
    tmp_reg = lm(eval(parse(text=paste(y, "~", paste(c(unlist(steps[1:s])), collapse =" + ")))),
                 mod$model)
    tmp_reg_summ = summary(tmp_reg)

    step = ifelse(names(steps)[s] != "",
                  names(steps)[s],
                  paste("Step", s))

    cat(paste("\n", step, "\n\tAdding", paste(unlist(steps[s]), collapse = " & "),"to the model..\n\n"))
    print(tmp_reg_summ$coefficients)

    R_sq = tmp_reg_summ$r.squared
    R_sq_adj = tmp_reg_summ$adj.r.squared
    mod_F = tmp_reg_summ$fstatistic["value"]
    mod_df1 = tmp_reg_summ$fstatistic["numdf"]
    mod_df2 = tmp_reg_summ$fstatistic["dendf"]
    mod_sig = pf(mod_F, mod_df1, mod_df2, lower.tail = F)
    R_sq_change = R_sq - prev_R_sq
    prev_R_sq = R_sq
    change_df1 = length(steps[[s]])
    change_F = (R_sq_change/(change_df1)) / ((1-R_sq)/mod_df2)
    change_sig = pf(change_F, change_df1, mod_df2, lower.tail = F)

    # add to output

    step_output = rbind(step_output,
                        data.frame(
                          step,
                          R_sq,
                          R_sq_adj,
                          mod_F,
                          mod_df1,
                          mod_df2,
                          mod_sig,
                          R_sq_change,
                          change_F,
                          change_df1,
                          change_sig
                          )
    )

    tmp_summ = lm_summarize(tmp_reg)
    print(tmp_summ)
    # coefs_output[[s]] = tmp_summ
    # names(coefs_output)[s] = step
  }


  rownames(step_output) = 1:nrow(step_output)
  return(list(step_output, coefs_output))
}
