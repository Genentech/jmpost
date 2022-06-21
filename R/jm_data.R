#' @export

vars <- function(longitudinal = "AVAL",
                 ID = "USUBJID",
                 overall_survival_death = "DEATH",
                 long_user_id = "USUBJID",
                 AYR = "AYR",
                 ID_INDEX = "USUBJID_INDEX",
                 os_study_id = "STUDYID",
                 os_arm = "ARM") {
  as.list(environment())
}



