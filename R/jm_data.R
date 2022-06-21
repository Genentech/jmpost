#' @export

jm_data <- setClass(
    "JMdata",
    representation(
        data_sld = "data.frame", # the sld data frame for the longitudinal dat a
        data_os = "data.frame",  # the overall survival data frame for the os model
        data = "list", # to be filled with the modified data list later
        vars = "list", # map of the variables
        shared_treatement = "character" # which is the treatment group
    )
)

vars <- function(longitudinal = "AVAL",
                 ID = "USUBJID",
                 overall_survival_death = "DEATH",
                 long_user_id = "USUBJID",
                 AYR = "AYR",
                 ID_INDEX = "USUBJID_INDEX",
                 os_study_id = "STUDYID",
                 os_arm = "ARM",
                 treatment = "TRT01P") {
  as.list(environment())
}



