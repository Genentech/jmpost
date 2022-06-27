#' JMdata class
#' @param data_sld Longitudinal data
#' @param data_os Overall survival data
#' @param data Modified data and prepared for the mcmc run
#' @param vars List with the names of columns important for the preparation of data for the stan object
#' @param shared_treatment Character, Identifies the treatment group
#' @param censoring_threshold Numeric, defines the threshold for the accuracy of the sld measurements
#' @export jm_data


jm_data <- setClass(
    "JMdata",
    representation(
        data_sld = "data.frame", # the sld data frame for the longitudinal dat a
        data_os = "data.frame",  # the overall survival data frame for the os model
        data = "list", # to be filled with the modified data list later
        vars = "list", # map of the variables
        shared_treatment = "character", # which is the treatment group
        censoring_threshold = "numeric"
    )
)

#' JMdata variable mapping
#' @param longitudinal Longitudinal observations
#' @param os_user_id Patients ID for overall survival data
#' @param overall_survival_death Event indicator for survival data
#' @param long_user_id Patients ID for longitudinal data
#' @param time_survival Time of event for survival data
#' @param os_study_id Study indicator
#' @param os_arm Arm indicator
#' @param treatment Treatment indicator
#' @export
vars <- function(longitudinal = "AVAL",
                 os_user_id = "USUBJID",
                 overall_survival_death = "DEATH",
                 long_user_id = "USUBJID",
                 time_survival = "AYR",
                 os_study_id = "STUDYID",
                 os_arm = "ARM",
                 treatment = "TRT01P") {
  as.list(environment())
}



