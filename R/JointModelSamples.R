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
#' @param generator ([`QuantityGenerator`])\cr object that specifies which subjects and time points
#' to calculate the quantities at
#' @param type (`character`)\cr type of quantities to be generated, must be either "survival" or
#' "longitudinal".
#' @export
generateQuantities.JointModelSamples <- function(object, generator, type, ...) {

    data <- append(
        as_stan_list(object@data),
        as_stan_list(object@model@parameters)
    )

    patients <- generator@subjects
    times <- generator@times

    assert_that(
        length(type) == 1,
        type %in% c("survival", "longitudinal"),
        length(patients) == length(times),
        all(patients %in% names(data$subject_to_index))
    )

    if (type == "survival") {
        data[["gq_long_flag"]] <- 0
        data[["gq_surv_flag"]] <- 1
    } else {
        data[["gq_long_flag"]] <- 1
        data[["gq_surv_flag"]] <- 0
    }

    data[["gq_n_quant"]] <- length(patients)
    data[["gq_pt_index"]] <- data$subject_to_index[as.character(patients)]
    data[["gq_times"]] <- times

    stanObject <- object@model@stan
    stanObject_data <- merge(
        stanObject,
        StanModule("base/generated_quantities_data.stan")
    )

    model <- compileStanModel(stanObject_data)
    devnull <- utils::capture.output(
        results <- model$generate_quantities(
            data = data,
            fitted_params = object@results
        )
    )
    return(results)
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
        as.CmdStanMCMC(object)$metadata()[["stan_variable_sizes"]],
        \(x) {
            if (length(x) == 1 && x == 1) return("")
            paste0("[", paste(x, collapse = ", "), "]")
        },
        character(1)
    )
    variable_string <- paste0(
        "        ",
        as.CmdStanMCMC(object)$metadata()[["stan_variables"]],
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
        as.CmdStanMCMC(object)$metadata()$iter_sampling,
        as.CmdStanMCMC(object)$metadata()$num_chains
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
#' @export
as.CmdStanMCMC.JointModelSamples <- function(object, ...) {
    return(object@results)
}
