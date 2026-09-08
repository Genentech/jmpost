# "missing" = no argument provided
# "NULL" = explicit NULL
setClassUnion("empty", c("missing", "NULL"))
setClassUnion("numeric_or_NULL", c("numeric", "NULL"))
setClassUnion("character_or_NULL", c("character", "NULL"))
setClassUnion("data.frame_or_NULL", c("data.frame", "NULL"))

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
#'
#' @examples
#' show(prior_normal(0, 1))
NULL


# write_stan ----

#' `write_stan`
#'
#' Write the Stan code for a Stan module.
#'
#' @param object the module.
#' @typed destination: "`character` or  `connection`"
#'   Where to write stan code to.
#' @param ... Additional arguments
#'
#' @export
#'
#' @returns Invisibly returns `NULL` after writing the Stan program.
#'
#' @examples
#' path <- tempfile(fileext = ".stan")
#' write_stan(JointModel(LongitudinalGSF()), path)
#' unlink(path)
write_stan <- function(object, destination, ...) {
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
#'
#' @returns A compiled `cmdstanr::CmdStanModel` object.
#'
#' @examples
#' \dontrun{
#' compileStanModel(JointModel(LongitudinalGSF()))
#' }
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
#'
#' @returns A `JointModelSamples` object.
#'
#' @examples
#' \dontrun{
#' sampleStanModel(JointModel(LongitudinalGSF()), data = joint_data)
#' }
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
#'
#' @returns A `StanModule` object.
as.StanModule <- function(object, ...) {
    UseMethod("as.StanModule")
}


#' `getParameters`
#'
#' Extract any modelling parameters as a [`ParameterList`] object
#' from a model.
#'
#' @param object where to obtain the parameters from.
#' @param ... additional options.
#'
#' @export
getParameters <- function(object, ...) {
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
#'
#' @returns A named list mapping source variables to standard names.
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
#'
#' @returns Initial values for the object, usually as a numeric vector or list.
#'
#' @examples
#' initialValues(prior_normal(0, 1))
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
#'
#' @returns A numeric value or named list describing parameter sizes.
size <- function(object) {
    UseMethod("size")
}


# auxiliaryInitialValues ----

#' `auxiliaryInitialValues`
#'
#' Obtain initial values for any additional Stan parameters introduced by an object.
#'
#' @param object where to get the auxiliary initial values from.
#' @param ... additional options.
#'
#' @details
#' Some objects, such as shrinkage priors, declare additional Stan parameters
#' beyond the main model parameter. This helper returns named initial values for
#' those additional parameters.
#'
#' @keywords internal
#'
#' @returns A named `list` of initial values for auxiliary parameters.
auxiliaryInitialValues <- function(object, ...) {
    UseMethod("auxiliaryInitialValues")
}


# auxiliarySize ----

#' `auxiliarySize`
#'
#' Obtain sizes for any additional Stan parameters introduced by an object.
#'
#' @param object where to get the auxiliary parameter sizes from.
#' @param ... additional options.
#'
#' @details
#' Some objects, such as shrinkage priors, declare additional Stan parameters
#' beyond the main model parameter. This helper returns named sizes for those
#' additional parameters so their initial values can be expanded consistently.
#'
#' @keywords internal
#'
#' @returns A named `list` of auxiliary parameter sizes.
auxiliarySize <- function(object, ...) {
    UseMethod("auxiliarySize")
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
#'
#' @returns A `cmdstanr::CmdStanGQ` object containing generated quantities.
#'
#' @examples
#' \dontrun{
#' generateQuantities(fit, generator, "survival")
#' }
generateQuantities <- function(object, ...) {
    UseMethod("generateQuantities")
}


#' Population Generated-Quantities Stan Data
#'
#' Obtain the model-specific Stan declarations and data needed for longitudinal
#' population generated quantities.
#'
#' This generic first dispatches on the quantity generator.  Its
#' [`QuantityGeneratorPopulation`] method then dispatches on `model`, allowing
#' longitudinal-model authors to provide a
#' `gq_population_stan_data.<model-class>()` method alongside their model.
#'
#' @param object A quantity generator.
#' @param model A longitudinal model.
#' @param data A [`DataJoint`] object, or `NULL` when only declarations are
#'   required.
#' @param ... Additional options.
#' @export
#'
#' @returns A list with `declarations`, a character scalar of Stan data-block
#' declarations, and `data`, a named list of Stan data values.
gq_population_stan_data <- function(object, model, data = NULL, ...) {
    UseMethod("gq_population_stan_data")
}

#' @rdname gq_population_stan_data
#' @export
gq_population_stan_data.default <- function(
    object,
    model,
    data = NULL,
    ...
) {
    list(declarations = "", data = list())
}


#' Prepare Data Object
#'
#' @typed object: "`DataSubject` or `DataLongitudinal` or `DataSurvival`"
#'   data object to "harmonise"
#' @typed subject_var: character
#'   the name of the variable containing the subject identifier.
#' @typed subject_ord: character
#'   the expected levels (in order) of the subject identifier.
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
#'
#' @returns A named `list` suitable for use as Stan data.
#'
#' @examples
#' as_stan_list(Parameter(prior_normal(0, 1), "beta"))
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
#'
#' @returns A character vector suitable for printing.
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
#'
#' @returns A numeric Brier score or a `data.frame` of scores over time.
#'
#' @examples
#' \dontrun{
#' brierScore(survival_quantities)
#' }
brierScore <- function(object, ...) {
    UseMethod("brierScore")
}


#' Generate Simulated Observations
#'
#' @typed object: "`SimLongitudinal` or `SimSurvival`"
#'   object to generate observations from.
#' @typed times_df: data.frame
#'   the times at which to generate observations. See details.
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
#'   )
#' ```
#'
#' @export
#'
#' @returns A `data.frame` containing simulated observations.
#'
#' @examples
#' sim <- SimLongitudinalRandomSlope(slope_mu = 0.01, slope_sigma = 0.5)
#' subjects <- data.frame(study = factor("S"), arm = factor("A"), subject = "1")
#' subject_parameters <- sampleSubjects(sim, subjects)
#' sampleObservations(sim, transform(subject_parameters, time = 0))
sampleObservations <- function(object, times_df) {
    UseMethod("sampleObservations")
}


#' Generate Simulated Subjects
#'
#' @typed object: "`SimLongitudinal` or `SimSurvival`"
#'   object to generate subjects from.
#' @typed subjects_df: data.frame
#'   the subjects to generate observations for. See details.
#'
#' @details
#' The `subjects_df` argument should be a `data.frame` with 1 row per desired subject to create
#' with the following columns:
#' - `study` (`factor`) the study identifier.
#' - `arm` (`factor`) the treatment arm identifier.
#' - `subject` (`character`) the subject identifier.
#'
#' This method takes care of generating all the individual subject data required for the
#' [`sampleObservations`] method to generate the observations.
#' @export
#'
#' @returns A `data.frame` containing simulated subject-level parameters.
#'
#' @examples
#' subjects <- data.frame(study = factor("S"), arm = factor("A"), subject = "1")
#' sim <- SimLongitudinalRandomSlope(slope_mu = 0.01, slope_sigma = 0.5)
#' sampleSubjects(sim, subjects)
sampleSubjects <- function(object, subjects_df) {
    UseMethod("sampleSubjects")
}


#' Generate time windows for evaluating a hazard function
#'
#' @typed object: SurvivalModel
#'   object to generate time windows for.
#' @param ... Not used.
#'
#'
#' @returns A `data.frame` describing the hazard-evaluation intervals.
hazardWindows <- function(object, ...) {
    UseMethod("hazardWindows")
}

#' @rdname Quant-Dev
#' @export
#'
#' @returns A `QuantityGenerator` or `QuantityCollapser` object, according to the function called.
#'
#' @examples
#' \dontrun{
#' grid <- GridFixed(times = c(0, 10))
#' as.QuantityGenerator(grid, joint_data)
#' as.QuantityCollapser(grid, joint_data)
#' }
as.QuantityGenerator <- function(object, ...) {
    UseMethod("as.QuantityGenerator")
}

#' @rdname Quant-Dev
#' @export
as.QuantityCollapser <- function(object, ...) {
    UseMethod("as.QuantityCollapser")
}


#' Coalesce Time
#'
#' @typed object: Grid
#'   object to coalesce time for.
#' @typed times: numeric
#'   the times to coalesce to.
#' @param ... Not used
#'
#' Method used to replace NULL times on grid objects (if appropriate)
#'
#' @keywords internal
#'
#' @returns A `Grid` object with missing time points replaced.
coalesceGridTime <- function(object, times, ...) {
    UseMethod("coalesceGridTime")
}
#' @export
#'
#' @returns The input object, unchanged.
coalesceGridTime.default <- function(object, times, ...) {
    object
}


#' Resolve a Promise
#'
#' @typed object: ANY
#'   an object to resolve.
#' @typed ...: ANY
#'   additional arguments.
#'
#' If `object` is not a promise will just return itself else will resolve the promise
#' and return the promised object.
#'
#' @export
#'
#' @returns The input object, or the value obtained by resolving a promise.
#'
#' @examples
#' resolvePromise(1)
#' resolvePromise(linkDSLD(), LongitudinalGSF())
resolvePromise <- function(object, ...) {
    UseMethod("resolvePromise")
}

#' @rdname resolvePromise
#' @export
resolvePromise.default <- function(object, ...) {
    object
}

#' Enable Link Generic
#'
#' @typed object: LongitudinalModel
#'   to enable link for.
#' @param ... Not used.
#'
#' Optional hook method that is called on a [`LongitudinalModel`] only if a link method
#' is provided to [`JointModel`]. This can be used to allow the model to include any
#' optional stan code that is only required if there are links present.
#'
#' @return [`LongitudinalModel`] object
#'
#' @export
#'
#' @examples
#' enableLink(LongitudinalGSF())
enableLink <- function(object, ...) {
    UseMethod("enableLink")
}
#' @export
#'
#' @returns The longitudinal model with its link-related Stan code enabled.
enableLink.default <- function(object, ...) {
    object
}


#' Enable Generated Quantities Generic
#'
#' @typed object: StanModel
#'   to enable generated quantities for.
#' @param ... Not used.
#'
#' Optional hook method that is called on a [`StanModel`] if attempting to use
#' either [`LongitudinalQuantities`] or [`SurvivalQuantities`]
#'
#' @return [`StanModule`] object
#'
#' @export
#'
#' @examples
#' enableGQ(LongitudinalGSF())
enableGQ <- function(object, ...) {
    UseMethod("enableGQ")
}
#' @export
#'
#' @returns A `StanModule` object containing the generated-quantities code.
enableGQ.default <- function(object, ...) {
    StanModule()
}


#' Get Prediction Names
#'
#' Utility function that returns the names of the required parameters for predicting
#' survival quantities with [`GridPrediction`].
#'
#' @typed object: LongitudinalModel
#'   A longitudinal model object
#' @param ... Not used.
#' @export
#'
#' @returns A character vector of required prediction parameter names, or `NULL`.
#'
#' @examples
#' getPredictionNames(LongitudinalGSF())
getPredictionNames <- function(object, ...) {
    UseMethod("getPredictionNames")
}

#' @rdname getPredictionNames
#' @export
getPredictionNames.default <- function(object, ...) {
    NULL
}


#' Get Random Effects Names
#'
#' Utility function that returns the names of the random effects parameters.
#' The main use for this is to allow the [`LongitudinalRandomEffects`] function
#' to know which parameters it needs to extract and to what common names
#' it should map the parameters to.
#'
#' @typed object: LongitudinalModel
#'   A longitudinal model object
#' @param ... Not used.
#' @export
#'
#' @returns A named character vector of random-effect parameter names, or `NULL`.
#'
#' @examples
#' getRandomEffectsNames(LongitudinalGSF())
getRandomEffectsNames <- function(object, ...) {
    UseMethod("getRandomEffectsNames")
}

#' @rdname getRandomEffectsNames
#' @export
getRandomEffectsNames.default <- function(object, ...) {
    NULL
}


#' Required Longitudinal Covariates
#'
#' Return the subject-level covariates required to generate longitudinal
#' population quantities for a model.
#'
#' @typed object: LongitudinalModel
#'   A longitudinal model object.
#' @param ... Not used.
#' @export
#'
#' @returns A character vector of covariate names.
required_longitudinal_covs <- function(object, ...) {
    UseMethod("required_longitudinal_covs")
}

#' @rdname required_longitudinal_covs
#' @export
required_longitudinal_covs.default <- function(object, ...) {
    character()
}


#' Required Longitudinal Simulation Covariates
#'
#' Return the subject-level covariates required to simulate a longitudinal
#' model from posterior draws. Unlike [required_longitudinal_covs()], this
#' includes covariates used only by variability predictors.
#'
#' @typed object: LongitudinalModel
#'   A longitudinal model object.
#' @param ... Not used.
#' @export
#'
#' @returns A character vector of covariate names.
required_simulation_covariates <- function(object, ...) {
    UseMethod("required_simulation_covariates")
}

#' @rdname required_simulation_covariates
#' @export
required_simulation_covariates.default <- function(object, ...) {
    character()
}


#' As Formula
#'
#' Utility wrapper function to convert an object to a formula.
#' @typed x: ANY
#'   object to convert to a formula.
#' @param ... Not used.
#' @export
#'
#' @returns A `formula` object.
#'
#' @examples
#' as_formula("response ~ time")
as_formula <- function(x, ...) {
    UseMethod("as_formula")
}

#' @importFrom stats as.formula
#' @export
#'
#' @returns A `formula` object.
as_formula.default <- function(x, ...) {
    as.formula(x, ...)
}


#' Set Constraints
#'
#' Applies constraints to a prior distribution to ensure any sampled numbers
#' from the distribution fall within the constraints
#'
#' @typed object: Prior
#'   a prior distribution to apply constraints to
#' @typed lower: numeric
#'   lower constraint boundary
#' @typed upper: numeric
#'   upper constraint boundary
#'
#' @export
#'
#' @returns A `Prior` object with the requested bounds.
#'
#' @examples
#' set_limits(prior_normal(0, 1), lower = 0)
set_limits <- function(object, lower = -Inf, upper = Inf) {
    UseMethod("set_limits")
}


#' Save Object to File
#'
#' @typed object: ANY
#'   object to save.
#' @typed file: character
#'   file to save object to.
#' @typed ...: ANY
#'   additional arguments.
#'
#' @family saveObject
#' @export
#'
#' @returns Invisibly returns `NULL` after saving the object.
#'
#' @examples
#' \dontrun{
#' saveObject(fit, "joint-model-fit.rds")
#' }
saveObject <- function(object, file, ...) {
    UseMethod("saveObject")
}


#' Extract Covariate Names
#'
#' @typed object: ANY
#'   the object to extract covariate names from.
#' @param ... additional arguments added by methods.
#'
#' @family covariates
#' @export
#'
#' @returns A character vector containing the covariate names.
#'
#' @examples
#' surv_data <- DataSurvival(os_data, Surv(os_time, os_event) ~ age + sex)
#' covariates(surv_data)
covariates <- function(object, ...) {
    UseMethod("covariates")
}

#' Extract Covariate Shrinkage Factors from the Posterior Samples
#'
#' @typed object: ANY
#'   the object to extract shrinkage factors from.
#' @param ... additional arguments added by methods.
#'
#' @family shrinkage
#' @export
#'
#' @returns A named numeric vector or matrix of shrinkage factors.
#'
#' @examples
#' \dontrun{
#' shrinkage(fit)
#' }
shrinkage <- function(object, ...) {
    UseMethod("shrinkage")
}
