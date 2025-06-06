#' A Function for Step-wise & Hierarchical Regression
#'
#' @param y Either: Character or an object created by the `lm()` function. If this is argument is a character string it should specify the outcome variable for the model. If it is a linear model object, it should be the full model with the outcome regressed on all predictors including those at each step.
#' @param chunks List of Character Vectors. This list should specify the predictors in the model that should be chunked together.
#' At the highest level the list should be specified using the `list()` method, but each vector should be specified using the `c()` method.
#' For example, one version of the argument could be `chunks = list(step1=c("pred1", "pred2"), step2=c("pred3"))`. If step names are not specified, the chunks will be numbered in the order that they are entered into the model.
#' If `chunks` are not specified, the individual predictors will be treated as the items to be included at each step.
#' @param data Data.frame. The data from which the model(s) should be calculated. This argument is necessary if `y` is a character string.
#' @param stepwise Logical. If `False` (default), the `chunks` will be entered into the model in the order that they are listed. If True, the `chunks` will be entered into the model based on the amount of variance they explain.
#' @param simultaneous Logical. If `True` (default) the value of the chunks will be evaluated in a (pseudo) simultaneous manner. The list of data.frames detailing the individual variables' descriptions will only be fore the overall regression.
#' Additionally, the summary of the steps will detail the semi-partial $R^2$ for each `chunk`, rather than the $\\Delta R^2$ at each step.
#' @param verbose Logical. If `True` the coefficients of the model will be output to the console for each chunk bring processed while the function runs. This will not be saved, as it is already part of the `lm` object and cam be accessed from there. Defaults to `False`.
#' @param steps_verbose Logical. If `True` the order that the chunks were entered into the model will be output to the console while the function tries to identify the optimal order. If `NULL` (default) this will will take the value of `verbose`.
#' @param simult_verbose Logical. If `True` the function will output which chunk's semipartial is being calculated to the console while running. If `NULL` (default) this will will take the value of `verbose`.
#' @param warn Logical. If `True` (default) warnings will be output.
#'
#' @returns A list of data.frames. The first is the information about the different steps/chunks and their R-squared Changes. The second is the list of coefficient summaries at each step.
#'
#' @export hierarchical_reg
#'


hierarchical_reg = function(y,
                            chunks = NULL,
                            data = NULL,
                            stepwise = F,
                            simultaneous = F,
                            verbose = F,
                            steps_verbose = NULL,
                            simult_verbose = NULL,
                            warn = T) {
  # Check for things that will cause errors
  if(!is.null(chunks) & typeof(chunks) != "list") stop("`chunks` must be a list of vectors containing variable names.")
  if(!is.null(chunks) & length(chunks) <= 1) stop("`chunks` must contain more than 1 vector of variable names.")
  if(stepwise & simultaneous) stop("You cannot (really, just shouldn't) do simultaneous and stepwise regressions at the same time... so this function doesn't allow it.")

  if(typeof(y) == "character"){
    if(warn) warning("Implementation of 'y' as an outcome variable has not been fully tested. Please, let Dani know if you encounter any errors.",
            immediate. = T)
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
  if(is.null(simult_verbose)) simult_verbose = verbose

  ## For stepwise

  if(stepwise){
    fin_chunks = list()

    for(c in 1:(length(chunks)-1)){
      if(steps_verbose) cat(paste0("Checking:\n\t",
                                   paste(names(chunks)[!names(chunks) %in% names(fin_chunks)], collapse = ", "),
                                   "\n"))
      tmp_hier = hierarchical_reg(mod,
                                  chunks = chunks[!names(chunks) %in% names(fin_chunks)],
                                  simultaneous = T,
                                  verbose = F,
                                  warn=F)
      fin_chunks = append(fin_chunks, chunks[names(chunks) == tmp_hier$steps$step[tmp_hier$steps$R_sq_semi == max(tmp_hier$steps$R_sq_semi)]])
      if(steps_verbose) cat(paste0("... ", names(fin_chunks)[length(fin_chunks)],
                                   " identified as the highest R^2 (", max(tmp_hier$steps$R_sq_semi), ") at this step.\n",
                                   "... Current chunk order: ", paste(names(fin_chunks), collapse = ", "),
                                   "\n\n"))
    }
    chunks = append(fin_chunks, chunks[!names(chunks) %in% names(fin_chunks)])


    if(steps_verbose) cat(paste0("Final Chunks order is:\t", paste(names(chunks), collapse = ", "),
                                 "\n\n"))
    if(verbose) cat("Finished identifying stepwise set order. Starting overall hierarchical regression...\n\n")
  }

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
  prev_df1 = 0

  coefs_output = list()

  for(s in 1:length(chunks)){
    step = ifelse(names(chunks)[s] != "",
                  names(chunks)[s],
                  paste("Step", s))
    if(verbose) cat(paste("\n", step, "\n\tAdding", paste(unlist(chunks[s]), collapse = " & "),"to the model..\n"))

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
    change_df1 = mod_df1 - prev_df1
    prev_df1 = mod_df1
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
    tmp_summ = lm_summarize(tmp_reg, warn=warn, verbose=verbose)
    if(verbose) print(tmp_summ)
    coefs_output[[s]] = tmp_summ
    names(coefs_output)[s] = step
  }


  # For Simultaneous

  if (simultaneous){
    step_output = data.frame(
      step      = c(),
      R_sq_semi = c(),
      F_semi    = c(),
      df1_semi  = c(),
      df2_semi  = c(),
      sig       = c()
    )
    for(c in 1:length(chunks)){
      if(simult_verbose) cat(paste0("Evaluating semipartial for the set: ",
                                    names(chunks)[c]), "\n\n") # list(budy_chunk=unlist(unname(chunks[-c])), chunks[c]))
      tmp_chunks = list(budy_chunk=unlist(unname(chunks[-c])), chunks[c])
      tmp_highr  = hierarchical_reg(mod, tmp_chunks, warn=warn)$steps

      step_output = rbind(step_output,
                          data.frame(
                            step      = names(chunks)[c],
                            R_sq_semi = tmp_highr$R_sq_change[2],
                            F_semi    = tmp_highr$change_F[2],
                            df1_semi  = tmp_highr$change_df1[2],
                            df2_semi  = tmp_highr$mod_df2[2],
                            sig       = tmp_highr$change_sig[2]
                          ))
    }

    coefs_output = coefs_output[length(coefs_output)]
  }

  if(nrow(step_output>=1)) rownames(step_output) = 1:nrow(step_output)
  return(list(steps=step_output, coefs=coefs_output))
}
