
#' Quantity Grid Specification
#'
#' @param subjects (`character` or `NULL`)\cr vector of subjects to extract quantities for.
#' If `NULL` will default to all subjects within the dataset.
#'
#' @param times (`numeric` or `NULL`)\cr vector of time points to extract quantities at.
#' If `NULL` will default to 201 evenly spaced timepoints between 0 and either the max
#' observation time (for [`LongitudinalQuantities`]) or max event time (for [`SurvivalQuantities`]).
#'
#' @param groups (`list`)\cr named list of subjects to extract quantities for. See Group Specification.
#'
#' @param spec (`list`)\cr named list of subjects to extract quantities for. The names of each
#' element should be the required subjects with the element itself being a numeric vector of timepoints
#' to generate the quantity at.
#'
#' @param length.out (`numeric`)\cr number of evenly spaced timepoints to generate quantities at.
#'
#' @param newdata (`data.frame`) \cr new data to generate quantities for. Must contain the same columns
#' and factor levels of the original data used in the [`DataSurvival`] object.
#'
#' @param params (`list`)\cr named list of parameters to fix the longitudinal model parameters at when
#' predicting survival quantities. See [`getPredictionNames()`] for the required parameters.
#'
#' @description
#' These functions are used to specify which subjects and timepoints should be generated
#' when calculating quantities via [`SurvivalQuantities`] and [`LongitudinalQuantities`].
#'
#' @details
#'
#' - `GridFixed()` is used to specify a fixed set of timepoints to generate quantities at for
#' all the specified subjects.
#'
#' - `GridGrouped()` is similar to `GridFixed()` but allows for groupwise averaging
#' (see Group Specification).
#'
#' - `GridObserved()` generates quantities at the observed longitudinal timepoints for each
#' subject.
#'
#' - `GridManual()` allows for individual timepoint specification for each subject.
#'
#' - `GridEven()` generates quantities for each subject at N evenly spaced timepoints
#' between each subjects first and last longitudinal observations.
#'
#' - `GridEvent()` generates one quantity for each subject at their event/censor time
#' as indicated by the `time` variable in the survival dataset.
#'
#' - `GridPopulation()` generates longitudinal model quantities based on the population parameters at the
#' specified time points. Generates 1 set of quantities for each distinct combination of `arm`
#' and `study` within the [`DataSubject`] object provided to the [`JointModel`].
#'
#' - `GridPrediction()` generates survival quantities based on any user-defined values at the
#' specified time points. This is useful for generating quantities for a new dataset
#' on specific longitudinal model parameters. See [`getPredictionNames()`] to determine which
#' longitudinal model parameters need to be defined for a given longitudinal model.
#'
#' @section Group Specification:
#' For `GridGrouped()`, `groups` must be a named list of character vectors. Each element of the list
#' must be a character vector of the subjects that will form the group where the element name
#' is the corresponding name of the group. For example if the goal was to create two groups
#' named `Group-1` and `Group-2` which are composed of the subjects `sub-1`, `sub-2` and
#' `sub-3`, `sub-4` respectively then this would be specified as:
#' ```
#' GridGrouped(
#'     groups = list(
#'         "Group-1" = c("sub-1", "sub-2"),
#'         "Group-2" = c("sub-3", "sub-4")
#'     )
#' )
#' ```
#' @seealso [`SurvivalQuantities`], [`LongitudinalQuantities`]
#' @name Grid-Functions
NULL






#' Grid Developer Notes
#'
#' @description
#' Developer details for implementing / extending `Grid` objects for defining
#' generated quantities for [`SurvivalQuantities`] and [`LongitudinalQuantities`].
#'
#' @slot subjects (`character` or `NULL`)\cr vector of subjects to extract quantities for.
#' If `NULL` will default to all subjects within the dataset.
#' @slot times (`numeric` or `NULL`)\cr vector of time points to extract quantities at.
#' If `NULL` will default to 201 evenly spaced timepoints between 0 and either the max
#' @slot groups (`list`)\cr named list of subjects to extract quantities for. See details.
#'
#' @details
#' All grid classes must inherit from the abstract `Grid` class.
#' All grid classes must provide `as.QuantityGenerator(object, data)` and
#' `as.QuantityCollapser(object, data)` methods where `data` is a [`DataJoint`] object.
#' These methods must return a `QuantityGenerator` and `QuantityCollapser` object respectively.
#' The `QuantityGenerator` object specifies unique subject/timepoint combinations that samples
#' should be generated at.
#' The `QuantityCollapser` object specifies how to combine these generated samples
#' to form the desired quantities.
#' As an example say we want to generate grouped samples for the groups `Group-1` and `Group-2`
#' which consist of the subjects `sub-1`, `sub-2` and `sub-3`, `sub-4` respectively at two time points
#' `10` and `20`. We can achieve this as follows:
#' ```
#' QuantityGenerator(
#'     times = c(10, 10, 10, 10, 20, 20, 20, 20),
#'     subjects = c("sub-1" "sub-2", "sub-3", "sub-4", "sub-1" "sub-2", "sub-3", "sub-4")
#' )
#' QuantityCollapser(
#'     times = c(10, 20, 10 , 20),
#'     groups = c("Group-1", "Group-1", "Group-2", "Group-2"),
#'     indexes = list(c(1, 2), c(5, 6), c(3, 4), c(7, 8))
#' )
#' ```
#' For population based quantities use the `arms` and `studies` arguments of `QuantityGenerator`
#' instead of `subjects`.
#'
#' @inheritSection Grid-Functions Group Specification
#'
#' @keywords internal
#' @seealso `Quant-Dev`
#' @name Grid-Dev
NULL



#' Quantity Developer Notes
#'
#' @description
#' Developer details for `QuantityX` objects/methods. This page just outlines the arguments
#' and slots of these objects/methods. For the full implementation details please see [Grid-Dev]
#'
#' @slot times (`numeric`)\cr See Arguments for details.
#' @slot subjects (`character`)\cr See Arguments for details.
#' @slot groups (`character`)\cr See Arguments for details.
#' @slot indexes (`list`)\cr See Arguments for details.
#'
#' @param times (`numeric`)\cr vector of time points to extract quantities at.
#' @param subjects (`character`)\cr vector of subjects to extract quantities for.
#' @param groups (`character`)\cr vector of labels to apply to the generated quantities.
#' @param indexes (`list`)\cr list of indexes that specify which observations from a
#' `QuantityGenerator` should be combined to form the desired quantities.
#'
#' @param object (`Grid`)\cr object to convert to a `QuantityGenerator` or `QuantityCollapser`.
#' @param data (`DataJoint`)\cr Survival and Longitudinal Data.
#' @param ... Not currently used.
#'
#' @details
#' The `as.QuantityGenerator` must return a `QuantityGenerator` object.
#' The `as.QuantityCollapser` must return a `QuantityCollapser` object.
#' @keywords internal
#' @name Quant-Dev
NULL


#' @rdname Grid-Dev
.Grid <- setClass("Grid")


#' @rdname Quant-Dev
.QuantityGenerator <- setClass(
    "QuantityGenerator"
)
#' `QuantityGenerator` -> `list`
#' @description
#' Converts a `QuantityGenerator` object to a list containing the required input data for a stan
#' model.
#' @param object (`QuantityGenerator`)\cr object to convert to a list.
#' @param data (`DataJoint`)\cr Survival and Longitudinal Data.
#' @param ... Not currently used.
#' @keywords internal
as_stan_list.QuantityGenerator <- function(object, data, ...) {
    stop("as_stan_list.QuantityGenerator not implemented")
}



#' @rdname Quant-Dev
.QuantityCollapser <- setClass(
    "QuantityCollapser",
    slots = c(
        "times" = "numeric",
        "groups" = "character",
        "indexes" = "list"
    )
)
#' @rdname Quant-Dev
QuantityCollapser <- function(times, groups, indexes) {
    .QuantityCollapser(
        times = times,
        groups = groups,
        indexes = indexes
    )
}

setValidity(
    "QuantityCollapser",
    function(object) {
        if (
            length(object@times) != length(object@groups) ||
                length(object@times) != length(object@indexes)
        ) {
            return("Length of `times`, `groups`, and `indexes` must be equal")
        }
    }
)

#' @export
length.QuantityCollapser <- function(x) {
    length(x@indexes)
}


#' Expand and Validate Subjects
#'
#' @param subjects (`character`)\cr vector of subjects that should exist in `data`
#' @param data (`DataJoint`)\cr Survival and Longitudinal Data.
#'
#' @description
#' If `subjects` is `NULL` this will return a named list of all subjects in `data`.
#' Else it will return `subjects` as a named list ensuring that all subjects exist in `data`.
#'
#' @keywords internal
subjects_to_list <- function(subjects = NULL, data) {
    data_list <- as.list(data)
    subjects_exp <- if (is.null(subjects)) {
        subs <- as.list(names(data_list$subject_to_index))
        names(subs) <- names(data_list$subject_to_index)
        subs
    } else {
        subs <- as.list(subjects)
        names(subs) <- subjects
        subs
    }
    subjects_exp_vec <- unlist(subjects_exp, use.names = FALSE)
    assert_that(
        identical(subjects_exp_vec, unique(subjects_exp_vec)),
        msg = "All subject names must be unique"
    )
    assert_that(
        all(subjects_exp_vec %in% names(data_list$subject_to_index)),
        msg = "Not all subjects exist within the data object"
    )
    subjects_exp
}
