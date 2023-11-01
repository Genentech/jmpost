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



# addLink ----

#' `addLink`
#'
#' Add a link to a longitudinal model.
#'
#' @param x the longitudinal model.
#' @param y the link to be added.
#' @param ... additional arguments.
#'
#' @export
# Needs to be S4 for multiple dispatch !
setGeneric(
    name = "addLink",
    def = function(x, y, ...) standardGeneric("addLink")
)



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


# getParameters ----

#' `getParameters`
#'
#' Obtain the parameters from a [`StanModel`].
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
#'
#' @keywords internal
initialValues <- function(object) {
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
