#' ####################################################
#'   
#'   2stgTGIOS
#'   Loading the required R libraries
#'   
#'   Q1-2022
#'   Francois Mercier 
#'   
#' ####################################################


if (!require("remotes")) {
  install.packages("remotes")
}
#' remotes::install_github("genentech/jmpost")
#' install.packages("jmpost")

#' General purpose
#' =================
library(tidyverse)
library(tidyselect)

#' Data import/export
#' =================
library(haven)

#' Survival
#' =================
library(survival)
library(survminer)

#' Joint models
#' =================
library(jmpost)
