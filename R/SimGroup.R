
#' Define Simulation Group
#'
#' Specifies a simulation group to be used by [`SimJointData()`].
#'
#' @param n (`numeric`)\cr number of subjects in the group.
#' @param arm (`character`)\cr treatment arm.
#' @param study (`character`)\cr study name.
#'
#' @slot n (`numeric`)\cr See arguments.
#' @slot arm (`character`)\cr See arguments.
#' @slot study (`character`)\cr See arguments.
#'
#' @examples
#' SimGroup(n = 50, arm = "Arm-A", study = "Study-1")
#' @name SimGroup-class
#' @exportClass SimGroup
.SimGroup <- setClass(
    "SimGroup",
    slots = c(
        n = "numeric",
        arm = "character",
        study = "character"
    )
)

#' @export
#' @rdname SimGroup-class
SimGroup <- function(n, arm, study) {
    .SimGroup(
        n = n,
        arm = arm,
        study = study
    )
}


setValidity(
    "SimGroup",
    function(object) {
        if (length(object@n) != 1) {
            return("`n` must be a length 1 integer")
        }
        if (length(object@arm) != 1) {
            return("`arm` must be a length 1 string")
        }
        if (length(object@study) != 1) {
            return("`study` must be a length 1 string")
        }
        if (any(object@n < 1) | any(object@n %% 1 != 0)) {
            return("`n` must be positive integer")
        }
        return(TRUE)
    }
)
