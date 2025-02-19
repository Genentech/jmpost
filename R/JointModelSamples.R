#' @include JointModel.R
#' @include SurvivalQuantities.R
NULL

setOldClass("CmdStanMCMC")

# JointModelSamples-class ----

#' `JointModelSamples`
#'
#' Contains samples from a [`JointModel`].
#'
#' @slot model ([`JointModel`])\cr the model that the samples were drawn from.
#' @slot data ([`DataJoint`])\cr the data that the model was fitted on.
#' @slot results ([`cmdstanr::CmdStanMCMC`])\cr the STAN samples.
#'
#' @aliases JointModelSamples
#' @export
.JointModelSamples <- setClass(
    "JointModelSamples",
    slots = c(
        model = "JointModel",
        data = "DataJoint",
        results = "CmdStanMCMC"
    )
)


#' @rdname generateQuantities
#' @param generator (`QuantityGenerator`)\cr object that specifies which subjects and time points
#' to calculate the quantities at
#' @param type (`character`)\cr type of quantities to be generated, must be either "survival" or
#' "longitudinal".
#' @export
generateQuantities.JointModelSamples <- function(object, generator, type, ...) {

    data <- as_stan_list(object@data) |>
        append(as_stan_list(object@model@parameters)) |>
        append(as_stan_list(generator, data = object@data, model = object@model))

    stanobj <- as.StanModule(object, generator = generator, type = type)
    model <- compileStanModel(stanobj)

    devnull <- utils::capture.output(
        results <- model$generate_quantities(
            data = data,
            fitted_params = object@results
        )
    )
    return(results)
}


#' `JointModelSamples` -> `StanModule`
#'
#' Converts a `JointModelSamples` object into a `StanModule` object ensuring
#' that the resulting `StanModule` object is able to generate post sampling
#' quantities.
#'
#' @inheritParams generateQuantities
#' @export
as.StanModule.JointModelSamples <- function(object, generator, type, ...) {
    assert_that(
        is(generator, "QuantityGenerator"),
        length(type) == 1,
        type %in% c("survival", "longitudinal")
    )

    quant_stanobj <- read_stan("base/quantities.stan") |>
        decorated_render(
            include_gq_longitudinal_idv = (type == "longitudinal") & is(generator, "QuantityGeneratorSubject"),
            include_gq_longitudinal_pop = (type == "longitudinal") & is(generator, "QuantityGeneratorPopulation"),
            include_gq_survival_idv = (type == "survival") & is(generator, "QuantityGeneratorSubject"),
            include_gq_survival_pred = (type == "survival") & is(generator, "QuantityGeneratorPrediction")
        ) |>
        StanModule()

    stanobj <- Reduce(
        merge,
        list(
            as.StanModule(object@model),
            enableGQ(object@model),
            quant_stanobj
        )
    )
    stanobj
}



#' `JointModelSamples` -> Printable `Character`
#'
#' Converts [`JointModelSamples`] object into a printable string.
#' @param object ([`JointModelSamples`])\cr samples as drawn from a [`JointModel`].
#' @family JointModelSamples
#' @param indent (`numeric`)\cr how much white space to prefix the print string with.
#' @keywords internal
#' @export
as_print_string.JointModelSamples <- function(object, indent = 1, ...) {
    sizes <- vapply(
        cmdstanr::as.CmdStanMCMC(object)$metadata()[["stan_variable_sizes"]],
        \(x) {
            if (length(x) == 1 && x == 1) return("")
            paste0("[", paste(x, collapse = ", "), "]")
        },
        character(1)
    )
    variable_string <- paste0(
        "        ",
        cmdstanr::as.CmdStanMCMC(object)$metadata()[["stan_variables"]],
        sizes
    )
    template <- c(
        "JointModelSamples Object with:",
        "",
        "    # of samples per chain = %d",
        "    # of chains            = %d",
        "",
        "    Variables:",
        variable_string[order(variable_string)]
    )
    pad <- rep(" ", indent) |> paste(collapse = "")
    template_padded <- paste(pad, template)
    sprintf(
        paste(template_padded, collapse = "\n"),
        cmdstanr::as.CmdStanMCMC(object)$metadata()$iter_sampling,
        cmdstanr::as.CmdStanMCMC(object)$num_chains()
    )
}

#' @rdname show-object
#' @export
setMethod(
    f = "show",
    signature = "JointModelSamples",
    definition = function(object) {
        string <- as_print_string(object)
        cat("\n", string, "\n\n")
    }
)


#' @rdname as.CmdStanMCMC
as.CmdStanMCMC.JointModelSamples <- function(object, ...) {
    return(object@results)
}


#' Save a `JointModelSamples` object to a file.
#'
#' This function is just a wrapper around `saveRDS` that saves the object to a file
#' ensuring that all of the Stan samples are correctly stored. Note that as
#' CmdStanR objects store their samples as a csv file the samples may be lost
#' if you call `saveRDS` directly on the object.
#'
#' @param object ([`JointModelSamples`])\cr the object to save.
#' @param file (`character`)\cr the file to save the object to.
#' @param ... (`ANY`)\cr additional arguments to [`saveRDS`].
#'
#' @family saveObject
#'
#' @export
saveObject.JointModelSamples <- function(object, file, ...) {
    object@results$draws()
    try(object@results$sampler_diagnostics(), silent = TRUE)
    try(object@results$init(), silent = TRUE)
    try(object@results$profiles(), silent = TRUE)
    saveRDS(object, file, ...)
}
