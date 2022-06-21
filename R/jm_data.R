#' @export

jm_data <- setClass(
    "JMdata",
    representation(
        data_sld = "data.frame",
        data_os = "data.frame",
        data = "list",
        vars = "list",
        shared_treatement = "character"
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



