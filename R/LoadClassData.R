#'
#' Load the Class Data for Examples
#'
#' @name LoadClassData
#'
#' @description
#' Load the class data to the environment for use.
#'

load("./data/ClassData.RData")

# # Dani Larranaga - 1/19/2024
# #
# # This script is designed to import and properly label/scale the data for
# # Dr. Don Lynams' Regression course at Purdue University.
#
# # To use this script, add it to the folder where your ".Rproj" file for the course is
# # and then in any R-script or R-markdown where you need this data you can include
# # the code: `source("LoadClassData.R")`. This should pop-up a browser window,
# # allowing you to select the file "psy631_data.csv" in whatever folder you
# # have it saved. Alternatively, you can alter the `file.choose()` part of line
# # 17 to a (relative or absolute) path on your device.
#
# # Load in the Data
#
# # 1) get the file path
# filePath = "../data/psy631_data.csv"
#
# # 2) read file
# df_PSY631Data = read.csv(filePath, header=T)
#
#
# # Fix Variables
#
# # Nominal variables
# df_PSY631Data$gender = factor(df_PSY631Data$gender,
#                               labels = c("male", "female"))
# df_PSY631Data$parent90 = factor(df_PSY631Data$parent90,
#                                 levels = c(1,2,3),
#                                 labels = c("Male+Female Caretakers",
#                                            "Female Caretaker Only",
#                                            "Male Caretaker Only"))
#
# # Ordinal Variables
# df_PSY631Data$court94 = ordered(df_PSY631Data$court94,
#                                 labels = c("no", "yes"))
# df_PSY631Data$jail94 = ordered(df_PSY631Data$jail94,
#                                 labels = c("no", "yes"))
# df_PSY631Data$impgrp = ordered(df_PSY631Data$impgrp,
#                                labels = c("Low Impulsivity",
#                                           "High Impulsivity"))
# df_PSY631Data$cenhod90 = ordered(df_PSY631Data$cenhod90,
#                                  labels = c("High SES",
#                                             "Middle SES",
#                                             "Low SES"))
# df_PSY631Data$cd90 = ordered(df_PSY631Data$cd90,
#                              labels = c("No CD",
#                                         "Conduct Disorder"))
# df_PSY631Data$adhd90 = ordered(df_PSY631Data$adhd90,
#                                labels = c("No CD",
#                                           "Conduct Disorder"))
