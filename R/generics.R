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
setGeneric(
    name = "merge",
    def = function(x, y, ...) standardGeneric("merge")
)

# as.character ----

#' Character Conversion of Different Classes
#'
#' These methods convert objects of different classes to `character`
#' representation.
#'
#' @name as.character
#' @aliases as.character
#'
#' @param x what to convert.
#'
#' @exportMethod as.character
NULL

# as.data.frame ----

#' Data Frame Conversion of Different Classes
#'
#' These methods convert objects of different classes to `data.frame`
#' representation.
#'
#' @name as.data.frame
#' @aliases as.data.frame
#'
#' @param x what to convert.
#' @param row.names not used.
#' @param optional not used.
#' @param ... not used.
#'
#' @export
NULL

# as.list ----

#' List Conversion of Different Classes
#'
#' These methods convert objects of different classes to `list`
#' representation.
#'
#' @name as.list
#' @aliases as.list
#'
#' @param x what to convert.
#' @param ... not used.
#'
#' @export
NULL



# autoplot ----

#' Plotting Methods for Different Classes
#'
#' These plot methods visualize various objects.
#'
#' @name autoplot
#'
#' @param object what to plot.
#' @param ... other arguments passed to plotting methods.
#'
#' @family `autoplot`
#'
#' @export autoplot
NULL



# summary ----

#' Object Summaries
#'
#' NOTE: This man page is for the `summary` S4 generic function defined within
#' jmpost. See [base::summary()] for the default method.
#' @name summary
#' @inheritParams base::summary
#' @family `summary`
#' @export summary
setGeneric("summary", base::summary, signature = c("object"))


# subset ----

#' Subsetting Vectors, Matrices and Data Frames
#'
#' NOTE: This man page is for the `subset` S4 generic function defined within
#' jmpost. See [base::subset()] for the default method.
#' @name subset
#' @inheritParams base::subset
#' @family `subset`
#' @export subset
setGeneric("subset", subset, signature = c("x"))



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

# names ----

#' Names of Different Classes
#'
#' These methods return the names of objects of different classes.
#'
#' @name names
#' @aliases names
#'
#' @param x where to get the names from.
#'
#' @exportMethod names
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
setGeneric(
    name = "write_stan",
    def = function(object, file_path) standardGeneric("write_stan")
)

# compileStanModel ----

#' `compileStanModel`
#'
#' Compile the Stan module.
#'
#' @param object the module.
#'
#' @export
setGeneric(
    name = "compileStanModel",
    def = function(object) standardGeneric("compileStanModel")
)

# sampleStanModel ----

#' `sampleStanModel`
#'
#' Sample from a Stan Module.
#'
#' @param object the module.
#' @param ... additional arguments.
#'
#' @export
setGeneric(
    name = "sampleStanModel",
    def = function(object, ...) standardGeneric("sampleStanModel")
)

# as.StanModule ----

#' `as.StanModule`
#'
#' Convert a [`Link`] or [`ParameterList`] into a [`StanModule`].
#'
#' @param object what to convert.
#'
#' @keywords internal
setGeneric(
    name = "as.StanModule",
    def = function(object) standardGeneric("as.StanModule")
)

# getParameters ----

#' `getParameters`
#'
#' Obtain the parameters from a [`StanModel`].
#'
#' @param object where to obtain the parameters from.
#'
#' @keywords internal
setGeneric(
    name = "getParameters",
    def = function(object) standardGeneric("getParameters")
)

# extractVariableNames ----

#' `extractVariableNames`
#'
#' Extract the `list` of variable names mapping to key variables
#' from a [`DataLongitudinal`] or [`DataSurvival`] object.
#'
#' @param object the data object.
#'
#' @keywords internal
setGeneric(
    name = "extractVariableNames",
    def = function(object) standardGeneric("extractVariableNames")
)

# initialValues ----

#' `initialValues`
#'
#' Obtain the `list` of initial values to be passed to the Stan sampler.
#'
#' @param object where to get the initial values from.
#'
#' @keywords internal
setGeneric(
    name = "initialValues",
    def = function(object) standardGeneric("initialValues")
)

# size ----

#' `size`
#'
#' Obtain the `list` of parameter sizes.
#'
#' @param object where to get the parameter sizes from.
#'
#' @keywords internal
setGeneric(
    name = "size",
    def = function(object) standardGeneric("size")
)

# longitudinal ----

#' `longitudinal`
#'
#' Obtain the longitudinal fit samples from [`JointModelSamples`].
#'
#' @param object samples to extract the longitudinal fits from.
#' @param ... additional options.
#'
#' @export
setGeneric(
    name = "longitudinal",
    def = function(object, ...) standardGeneric("longitudinal")
)


# generateQuantities ----

#' `generateQuantities`
#'
#' Obtain the generated quantities from a Stan Model.
#'
#' @param object object to obtain generated quantities from
#' @param ... additional options.
#'
#' @export
setGeneric(
    name = "generateQuantities",
    def = function(object, ...) standardGeneric("generateQuantities")
)



#' `extractSurvivalQuantities`
#'
#' Extracts quantity samples from a stan model
#'
#' @param object object to obtain quantities from
#' @param ... additional options.
#'
#' @family `extractSurvivalQuantities`
#' @export
setGeneric(
    name = "extractSurvivalQuantities",
    def = function(object, ...) standardGeneric("extractSurvivalQuantities")
)
