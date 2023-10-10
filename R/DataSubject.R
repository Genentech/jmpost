
#TODO docs
.DataSubject <- setClass(
    Class = "DataSubject",
    representation = list(
        data = "data.frame",
        subject = "character",
        arm = "character",
        study = "character"
    )
)
#TODO docs
#' @export
DataSubject <- function(data, subject, arm, study) {
    vars <- c(subject, arm, study)
    vars_frm_chr <- paste0("~ ", paste(vars, collapse = " + "))
    .DataSubject(
        data = remove_missing_rows(data, as.formula(vars_frm_chr)),
        subject = subject,
        arm = arm,
        study = study
    )
}

setValidity(
    "DataSubject",
    method = function(object) {
        if (length(object@subject) != 1) {
            return("`subject` must be a length 1 character")
        }
        if (length(object@arm) != 1) {
            return("`arm` must be a length 1 character")
        }
        if (length(object@study) != 1) {
            return("`study` must be a length 1 character")
        }
        if (nrow(object@data) == 0) {
            return("`data` should not have 0 rows")
        }
        sbj <- object@data[[object@subject]]
        if (!(is(sbj, "character") | is(sbj, "factor"))) {
            return("`data[[subject]]` should be of type character or factor")
        }
    }
)

#TODO docs
extractVariableNames.DataSubject <- function(object) {
    list(
        subject = object@subject,
        arm = object@arm,
        study = object@study
    )
}

#TODO docs
#' @family as_stan_list
as_stan_list.DataSubject <- function(x) {
    df <- as.data.frame(x)
    vars <- extractVariableNames(x)
    list(
        Nind = nrow(df),
        pt_to_ind = stats::setNames(
            seq_len(nlevels(df[[vars$subject]])),
            levels(df[[vars$subject]])
        ),
        n_studies = length(unique(df[[vars$study]])),
        n_arms = length(unique(df[[vars$arm]])),
        pt_study_index = as.numeric(df[[vars$study]]),
        pt_arm_index = as.numeric(df[[vars$arm]])
    )
}

# TODO - docs
as.data.frame.DataSubject <- function(x, ...) {
    x@data
}

# TODO - docs
suit_up.DataSubject <- function(object, ...) {
    data <- as.data.frame(object)
    vars <- extractVariableNames(object)
    assert_that(
        vars$subject %in% names(data),
        vars$arm %in% names(data),
        vars$study %in% names(data)
    )
    data[[vars$subject]] <- factor(data[[vars$subject]])
    data[[vars$arm]] <- factor(data[[vars$arm]])
    data[[vars$study]] <- factor(data[[vars$study]])
    DataSubject(
        data = data,
        subject = object@subject,
        arm = object@arm,
        study = object@study
    )
}
