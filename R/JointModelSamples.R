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
        type %in% c("survival", "longitudinal")
    )

    # If `arms` have been provided assume that we are generating population
    # quantities and not individual subject quantities
    if (length(generator@arms)) {

        data[["gq_n_quant"]] <- length(generator@arms)
        data[["gq_long_pop_arm_index"]] <- generator@arms
        data[["gq_long_pop_study_index"]] <- generator@studies
        assert_that(
            length(generator@arms) == length(generator@studies),
            length(generator@arms) == length(times)
        )
    } else {
        data[["gq_pt_index"]] <- data$subject_to_index[as.character(patients)]
        data[["gq_n_quant"]] <- length(patients)

        # dummy pop indexes in order for stan code to actualy compile. In this setting
        # this matrix isn't actually used so doesn't matter what these values are
        # but don't want to have to burden individual longitudinal models with the
        # conditional logic to check if they are generating population quantities or not
        data[["gq_long_pop_arm_index"]] <- rep(1, length(patients))
        data[["gq_long_pop_study_index"]] <- rep(1, length(patients))
        assert_that(
            length(patients) == length(times),
            all(patients %in% names(data$subject_to_index))
        )
    }

    data[["gq_times"]] <- times

    quant_stanobj <- read_stan("base/quantities.stan") |>
        decorated_render(
            include_gq_survival_idv = (type == "survival"),
            include_gq_longitudinal_idv = (type == "longitudinal") & !length(generator@arms),
            include_gq_longitudinal_pop = (type == "longitudinal") & length(generator@arms)
        ) |>
        StanModule()

    stanobj <- merge(
        as.StanModule(object@model, include_gq = TRUE),
        quant_stanobj
    )

    model <- compileStanModel(stanobj)
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
