

.DataSurvival <- setClass(
    Class = "DataSurvival",
    representation = list(
        data = "data.frame",
        formula = "formula",
        subject = "character",
        arm = "character",
        study = "character"
    )
)

#' @export
DataSurvival <- function(data, formula, subject, arm , study) {
    .DataSurvival(
        data = data,
        formula = formula,
        subject = subject,
        arm = arm,
        study = study
    )
}


    # as.character(os_frm[[2]][[1]])
    # os_time <- as.character(os_frm[[2]][[2]])
    # os_event <- as.character(os_frm[[2]][[3]])
