#' @include DataSurvival.R
#' @include DataLongitudinal.R
NULL

#' @rdname DataJoint
.DataJoint <- setClass(
  Class = "DataJoint",
  representation = list(
    survival = "DataSurvival",
    longitudinal = "DataLongitudinal"
  )
)


#' `DataJoint`
#'
#' The [`DataJoint`] class handles combining data from a \linkS4class{DataSurvival} object and a
#' \linkS4class{DataLongitudinal} object
#'
#' @param survival A \linkS4class{DataSurvival} object created by [DataSurvival()]
#' @param longitudinal A \linkS4class{DataLongitudinal} object created by [DataLongitudinal()]
#'
#' @details
#'
#' ## Coercion
#'
#' - `as.list(x)`, `as(x, "list")`: Coerces x into a list of data components required
#' for fitting a [JointModel()].
#' See the vignette (TODO) for more details
#'
#' @export
#' @aliases as.list,DataJoint-method
DataJoint <- function(survival, longitudinal) {
  .DataJoint(
    survival = survival,
    longitudinal = longitudinal
  )
}


setValidity(
  Class = "DataJoint",
  method = function(object) {
    lm <- as.data.frame(object@longitudinal)
    lvars <- extractVariableNames(object@longitudinal)
    os <- as.data.frame(object@survival)
    ovars <- extractVariableNames(object@survival)

    if (!all(as.character(lm[[lvars$pt]]) %in% as.character(os[[ovars$pt]]))) {
      return("There are subjects in the longitudinal data that do not exist in the survival data")
    }
    if (!all(as.character(os[[ovars$pt]]) %in% as.character(lm[[lvars$pt]]))) {
      return("There are subjects in the survival data that do not exist in the longitudinal data")
    }
  }
)

#' @export
setMethod(
  "as.list",
  signature = "DataJoint",
  definition = function(x) {
    append(
      as.list(x@survival),
      as.list(x@longitudinal)
    )
  }
)


#' @export
setAs(
  from = "DataJoint",
  to = "list",
  def = function(from) as.list(from)
)




### TODO - The following parameters were only used in the generated quantities which are
###        currently excluded
#     Death = osd_final$DEATH,   # Only used in generated quantities
#     arm_to_study_index = c(1L, 2L, 3L, 3L, 3L, 3L),
#     # The patients in each of the four different treatment arms.
#     n_index_per_arm = c(
#         sum(osd_final$ARM == "IMV210_A"),
#         sum(osd_final$ARM == "IMV211_A"),
#         sum(osd_final$ARM == "MOR_A"),
#         sum(osd_final$ARM == "MOR_AT"),
#         sum(osd_final$ARM == "MOR_ASG"),
#         sum(osd_final$ARM == "MOR_AEV")
#     ),
#     index_per_arm = c(
#         which(osd_final$ARM == "IMV210_A"),
#         which(osd_final$ARM == "IMV211_A"),
#         which(osd_final$ARM == "MOR_A"),
#         which(osd_final$ARM == "MOR_AT"),
#         which(osd_final$ARM == "MOR_ASG"),
#         which(osd_final$ARM == "MOR_AEV")
#     ),
