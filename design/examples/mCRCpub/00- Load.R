#' ####################################################
#'
#'   2stgTGIOS
#'   Loading the required R libraries
#'
#'   Q1-2022
#'   Francois Mercier
#'
#' ####################################################


#' if (!require("remotes")) {
#'   install.packages("remotes")
#' }
#' remotes::install_github("genentech/jmpost")
#' install.packages("ghibli")

#' General purpose
#' =================
library(tidyverse)
library(tidyselect)
library(ghibli)

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
