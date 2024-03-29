# "missing" = no argument provided
# "NULL" = explicit NULL
setClassUnion("empty", c("missing", "NULL"))

# merge ----

#' `merge`
#'
#' Merge two `StanModule` or `ParameterList` objects.
#'
#' @param x first module.
#' @param y second module.
#' @param ... additional arguments.
#'
#' @export
# Needs to be S4 for multiple dispatch !
setGeneric(
    name = "merge",
    def = function(x, y, ...) standardGeneric("merge")
)


# show ----

#' Printing of Different Classes
#'
#' These methods print objects of different classes.
#'
#' @name show
#' @aliases show
#'
#' @param object what to print.
#'
#' @export
NULL


# write_stan ----

#' `write_stan`
#'
#' Write the Stan code for a Stan module.
#'
#' @param object the module.
#' @param file_path (`string`)\cr output file.
#'
#' @export
write_stan <- function(object, file_path) {
    UseMethod("write_stan")
}

# compileStanModel ----

#' `compileStanModel`
#'
#' Compile the Stan module.
#'
#' @param object the module.
#'
#' @export
compileStanModel <- function(object) {
    UseMethod("compileStanModel")
}


# sampleStanModel ----

#' `sampleStanModel`
#'
#' Sample from a Stan Module.
#'
#' @param object the module.
#' @param ... additional arguments.
#'
#' @export
sampleStanModel <- function(object, ...) {
    UseMethod("sampleStanModel")
}


# as.StanModule ----

#' `as.StanModule`
#'
#' Converts an object into a [`StanModule`].
#'
#' @param object what to convert.
#' @param ... additional options.
#' @family as.StanModule
#' @keywords internal
as.StanModule <- function(object, ...) {
    UseMethod("as.StanModule")
}



#' `getParameters`
#'
#' Extract any modelling parameters as a [`ParameterList`] object
#' from a model.
#'
#' @param object where to obtain the parameters from.
#'
#' @keywords internal
getParameters <- function(object) {
    UseMethod("getParameters")
}


# extractVariableNames ----

#' Extract Mapping to Standardised Variable Names
#'
#' @description
#' Extract a `list` that maps the variable names in a user-defined
#' `data.frame` to standardised values.
#'
#' @param object the data object.
#' @family extractVariableNames
#' @keywords internal
extractVariableNames <- function(object) {
    UseMethod("extractVariableNames")
}



# initialValues ----

#' `initialValues`
#'
#' Obtain the `list` of initial values to be passed to the Stan sampler.
#'
#' @param object where to get the initial values from.
#' @param n_chains the number of initial values to generate. See details.
#' @param ... Not currently used.
#'
#' @details
#' There are multiple ways of specifying initial values to Stan, see the `init` argument
#' in [cmdstanr::model-method-sample] for full details. Within this package we supply
#' initial values via a list of lists where each inner list contains the initial values
#' for a single chain. As such the `n_chains` argument specifies the number of inner lists
#' to generate.
#'
#' See the Vignette for further details of how to specify initial values.
#'
#' @export
initialValues <- function(object, ...) {
    UseMethod("initialValues")
}


# size ----

#' `size`
#'
#' Obtain the `list` of parameter sizes.
#'
#' @param object where to get the parameter sizes from.
#'
#' @keywords internal
size <- function(object) {
    UseMethod("size")
}


# generateQuantities ----

#' `generateQuantities`
#'
#' Obtain the generated quantities from a Stan Model.
#'
#' @param object object to obtain generated quantities from
#' @param ... additional options.
#'
#' @export
generateQuantities <- function(object, ...) {
    UseMethod("generateQuantities")
}


#' Prepare Data Object
#'
#' @param object (`DataSubject` or `DataLongitudinal` or `DataSurvival`) \cr data object to "harmonise"
#' @param subject_var (`character`) \cr the name of the variable containing the subject identifier.
#' @param subject_ord (`character`) \cr the expected levels (in order) of the subject identifier.
#' @param ... not used.
#'
#' @details
#' This utility function prepares the datasets in the data objects in order to ensure they
#' are consistent and compatible with each other.
#'
#' In particular it ensures that the `subject` variable, as specified by `DataSubject`,
#' is available in `DataLongitudinal` and `DataSurvival` and that all levels are present
#' in all 3 data objects.
#'
#' It also sorts the datasets to ensure that indexes are consistent e.g. index 1 for
#' `DataSubject@data` corresponds to the same subject as index 1 for `DataSurvival@data`.
#' For `DataLongitudinal` the data is additionally sorted by time and outcome value.
#'
#' @seealso [`DataJoint`], [`DataSurvival`], [`DataSubject`], [`DataLongitudinal`]
#'
#' @keywords internal
#' @return Returns the original object but with the data standardised (see details)
harmonise <- function(object, ...) {
    UseMethod("harmonise")
}


#' @rdname harmonise
harmonise.default <- function(object, ...) {
    NULL
}


#' `as_stan_list`
#'
#' @description
#' Extracts a list of data elements from an object to be used as input
#' to a Stan Model
#'
#' @param object to be converted.
#' @param ... additional options.
#'
#' @family as_stan_list
#' @export
as_stan_list <- function(object, ...) {
    UseMethod("as_stan_list")
}

#' @rdname as_stan_list
#' @export
as_stan_list.default <- function(object, ...) {
    NULL
}


#' `as_print_string`
#'
#' @description
#' Returns the character representation of an object which is suitable
#' for printing to the console
#'
#' @param object to be converted to string.
#' @param ... additional options.
#'
#' @family as_print_string
#' @keywords internal
as_print_string <- function(object, ...) {
    UseMethod("as_print_string")
}

#' Show an Object
#'
#' Prints an object to the console.
#'
#' @param object Object to be printed
#'
#' @name show-object
NULL



#' `brierScore`
#'
#' @description
#' Returns the Brier Score for a given model
#'
#' @param object to calculate Brier Score for.
#' @param ... additional options.
#'
#' @family brierScore
#' @export
brierScore <- function(object, ...) {
    UseMethod("brierScore")
}






#' Standard Link Methods
#'
#' @param object ([`StanModel`]) \cr A [`StanModel`] object.
#' @param ... Not used.
#'
#' @description
#' These generic functions enable [`LongitudinalModel`] objects to provide
#' their own implementations for the most common link functions.
#'
#' @details
#' Each of these methods should return a [`StanModule`] argument that implements
#' the models corresponding version of that link type.
#' For `enableLink` this is called once for a model regardless of how many links
#' are used and its purpose is to provide the stan code to initialise any
#' link specific objects (to avoid clashes with each individual link function declaring
#' the same required stan objects).
#'
#' For further details on how to use these methods please see
#' \code{vignette("extending-jmpost", package = "jmpost")}.
#'
#' @name standard-link-methods
NULL


#' @describeIn standard-link-methods hook to include any common link code to be shared across all
#' link functions
#' @export
enableLink <- function(object, ...) {
    UseMethod("enableLink")
}

#' @describeIn standard-link-methods Time to growth link
#' @export
linkTTG <- function(object, ...) {
    UseMethod("linkTTG")
}

#' @describeIn standard-link-methods Derivative of the SLD over time link
#' @export
linkDSLD <- function(object, ...) {
    UseMethod("linkDSLD")
}

#' @describeIn standard-link-methods Current SLD link
#' @export
linkIdentity <- function(object, ...) {
    UseMethod("linkIdentity")
}


#' Generate Simulated Observations
#'
#' @param object (`SimLongitudinal` or `SimSurvival`) \cr object to generate observations from.
#' @param times_df (`data.frame`) \cr the times at which to generate observations. See details.
#'
#' @details
#' The `times_df` argument should be a `data.frame` as created by `sampleSubjects` but
#' replicated for each time point at which observations are to be generated. That is if you want
#' to generate observations for times `c(0, 1, 2, 3)` then `times_df` should be created as:
#' ```
#' subject_dat <- sampleSubjects(object, ...)
#' times_df <- tidyr::expand_grid(
#'     subject_dat,
#'     time = c(0, 1, 2, 3)
#' )
#' ```
#'
#' @export
sampleObservations <- function(object, times_df) {
    UseMethod("sampleObservations")
}


#' Generate Simulated Subjects
#'
#' @param object (`SimLongitudinal` or `SimSurvival`) \cr object to generate subjects from.
#' @param subjects_df (`data.frame`) \cr the subjects to generate observations for. See details.
#'
#' @details
#' The `subjects_df` argument should be a `data.frame` with 1 row per desired subject to create
#' with the following columns:
#' - `study` (`factor`) the study identifier.
#' - `arm` (`factor`) the treatment arm identifier.
#' - `pt` (`character`) the subject identifier.
#'
#' This method takes care of generating all the individual subject data required for the
#' [`sampleObservations`] method to generate the observations.
#' @export
sampleSubjects <- function(object, subjects_df) {
    UseMethod("sampleSubjects")
}


#' Generate time windows for evaluating a hazard function
#'
#' @param object (`SurvivalModel`) \cr object to generate time windows for.
#' @param ... Not used.
#'
hazardWindows <- function(object, ...) {
    UseMethod("hazardWindows")
}


#' Coerce to `CmdStanMCMC`
#'
#' @param object to be converted
#' @param ... additional options
#'
#' @description
#' Coerces an object to a [`cmdstanr::CmdStanMCMC`] object
#'
#' @export
as.CmdStanMCMC <- function(object, ...) {
    UseMethod("as.CmdStanMCMC")
}
