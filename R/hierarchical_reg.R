#' A Function for Step-wise & Hierarchical Regression
#'
#' @param y Either: Character or an object created by the `lm()` function. If this is argument is a character string it should specify the outcome variable for the model. If it is a linear model object, it should be the full model with the outcome regressed on all predictors including those at each step.
#' @param chunks List of Character Vectors. This list should specify the predictors in the model that should be chunked together.
#' At the highest level the list should be specified using the `list()` method, but each vector should be specified using the `c()` method.
#' For example, one version of the argument could be `chunks = list(step1=c("pred1", "pred2"), step2=c("pred3"))`. If step names are not specified, the chunks will be numbered in the order that they are entered into the model.
#' If `chunks` are not specified, the individual predictors will be treated as the items to be included at each step.
#' @param data Data.frame. The data from which the model(s) should be calculated. This argument is necessary if `y` is a character string.
#' @param stepwise Logical. If False (default), the `chunks` will be entered into the model in the order that they are listed. If True, the `chunks` will be entered into the model based on the amount of variance they explain.
#' @param verbose Logical. If `True` the coefficients of the model will be output to the console for each chunk bring processed while the function runs. This will not be saved, as it is already part of the `lm` object and cam be accessed from there. Defaults to `False`.
#' @param steps_verbose Logical. If `True` the order that the chunks were entered into the model will be output to the console while the function tries to identify the optimal order. If `NULL` (default) this will will take the value of `verbose`.
#' @param step_iters Numeric. Only used if `stepwise == True`. The number of iterations of step-wise comparisons to do before ending the procedure comparing variances explained. Defaults to 50.
#'
#' @returns A list of data.frames. The first is the information about the different steps/chunks and their R-squared Changes. The second is the list of coefficient summaries at each step.
#'
#' @export hierarchical_reg
#'


hierarchical_reg = function(y,
                            chunks = NULL,
                            data = NULL,
                            stepwise = F,
                            verbose = F,
                            steps_verbose = NULL,
                            step_iters = 50) {
  # Check for things that will cause errors
  if(!is.null(chunks) & typeof(chunks) != "list") stop("`chunks` must be a list of vectors containing variable names.")
  if(!is.null(chunks) & length(chunks) <= 1) stop("`chunks` must contain more than 1 vector of variable names.")

  if(typeof(y) == "character"){
    if(is.null(data)) stop("Data is required when y is specifying an outcome variable.")
    if(!y %in% colnames(data)) stop("The outcome varaible is not in the data set.")

    mod = stats::lm(stats::as.formula(paste(y, "~", paste(unlist(chunks), sep=" + "))),
           data)
  } else if(typeof(y) != "list" | is.null(y$model)) {
    stop("The `y` argument must either be a character string specifying the outcome or the full linear model from which the chunks will be calculated.")
  } else {
    mod = y
    y = as.character(mod$terms)[2]
    if(is.null(chunks)) {
      chunks = as.list(unlist(strsplit(as.character(mod$terms)[3], " \\+ ")))
      names(chunks) = unlist(chunks)
    }
  }

  if(!all(unlist(unname(chunks)) %in% colnames(mod$model))) stop("Not all predictors are in the regression model.")

  if(is.null(names(chunks))) names(chunks) = rep("", length(chunks))

  if(is.null(steps_verbose)) steps_verbose = verbose

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

  for(s in 1:length(chunks)){
    step = ifelse(names(chunks)[s] != "",
                  names(chunks)[s],
                  paste("Step", s))
    if(verbose) cat(paste("\n", step, "\n\tAdding", paste(unlist(chunks[s]), collapse = " & "),"to the model..\n\n"))

    tmp_reg = stats::lm(eval(parse(text=paste(y, "~", paste(c(unlist(chunks[1:s])), collapse =" + ")))),
                 data=mod$model)
    tmp_reg_summ = summary(tmp_reg)
    # print(tmp_reg_summ$coefficients)

    R_sq = tmp_reg_summ$r.squared
    R_sq_adj = tmp_reg_summ$adj.r.squared
    mod_F = tmp_reg_summ$fstatistic["value"]
    mod_df1 = tmp_reg_summ$fstatistic["numdf"]
    mod_df2 = tmp_reg_summ$fstatistic["dendf"]
    mod_sig = stats::pf(mod_F, mod_df1, mod_df2, lower.tail = F)
    R_sq_change = R_sq - prev_R_sq
    prev_R_sq = R_sq
    change_df1 = length(chunks[s])
    change_F = (R_sq_change/(change_df1)) / ((1-R_sq)/mod_df2)
    change_sig = stats::pf(change_F, change_df1, mod_df2, lower.tail = F)

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

    #tmp_reg$model = mod$model

    tmp_summ = lm_summarize(tmp_reg, warn=F)
    if(verbose) print(tmp_summ)
    coefs_output[[s]] = tmp_summ
    names(coefs_output)[s] = step
  }

  ## For stepwise

  if(stepwise){
    old_chunks = chunks
    new_chunks = chunks[order(step_output$R_sq_change, decreasing=T)]

    iter_count=0

    while(!identical(old_chunks, new_chunks)){
      if (steps_verbose) cat(paste("Stepwise Old Chunk Order:", paste(names(old_chunks), collapse=", "),
                                   "\n\t\t     New:", paste(names(new_chunks), collapse=", "), "\n\n"))

      tmp_output = hierarchical_reg(mod, new_chunks, verbose = F, stepwise = F)
      step_output = tmp_output[[1]]
      coefs_output = tmp_output[[2]]
      old_chunks = new_chunks
      new_chunks = old_chunks[order(step_output$R_sq_change, decreasing=T)]

      iter_count = iter_count+1
      if(iter_count >= step_iters) {
        old_chunks = new_chunks
        warning(paste("The order of the stepwise components did not converge on a single order.",
                      "\n\tMax iteration count (", step_iters, ")  reached.",
                      "\n\tUse `steps_verbose=True` to see what steps were evaluated in what order, or",
                      "\n\tincrease `step_iters` to increase the max iteration count.", sep = ""),
                immediate. = T)
      }
    }
  }


  rownames(step_output) = 1:nrow(step_output)
  return(list(step_output, coefs_output))
}
