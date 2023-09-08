

#' NULL Documentation page to house re-usable elements across SurvivalSamples methods/objects
#'
#' @param object (`SurvivalSamples`) \cr A [SurvivalSamples][SurvivalSamples-class]
#' object created by [SurvivalSamples()]
#'
#' @param patients (`character` or `list` or `NULL`)\cr which patients to calculate the desired
#' quantities for.
#' See "Patient Specification" for more details.
#'
#' @param type (`character`)\cr The quantity to be generated.
#' Must be one of `surv`, `haz`, `loghaz`, `cumhaz`.
#'
#' @param time_grid (`numeric`)\cr a vector of time points to calculate the desired quantity at.
#'
#' @name SurvivalSamples-Joint
#'
#' @section Patient Specification:
#' If `patients` is a character vector then quantities / summary statistics
#' will only be calculated for those specific patients
#'
#' If `patients` is a list then any elements with more than 1 patient ID will be grouped together
#' and their quantities / summary statistics (as selected by `type`)
#' will be calculated by taking the point-wise average. For example:
#' `patients = list("g1" = c("pt1", "pt2"), "g2" = c("pt3", "pt4"))` would result
#' in 2 groups being created whose values are the pointwise average
#' of `c("pt1", "pt2")` and `c("pt3", "pt4")` respectively.
#'
#' If `patients=NULL` then all patients from original dataset will be selected
#' @keywords internal
NULL


# SurvivalSamples-class ----

#' `SurvivalSamples`
#'
#' This class is an extension of [JointModelSamples][JointModelSamples-class] so that we
#' can define specific survival postprocessing methods for it.
#'
#' @family SurvivalSamples
.SurvivalSamples <- setClass(
    "SurvivalSamples",
    contains = "JointModelSamples"
)

#' SurvivalSamples Constructor Function
#'
#' Creates a [SurvivalSamples][SurvivalSamples-class] object from a
#' [JointModelSamples][JointModelSamples-class] object
#'
#' @param object (`JointModelSamples`) \cr A [JointModelSamples][JointModelSamples-class] object
#' @family SurvivalSamples
#' @export
SurvivalSamples <- function(object) {
    .SurvivalSamples(object)
}


# TODO - test function
#' Predict
#'
#' @inheritParams SurvivalSamples-Joint
#' @inheritSection SurvivalSamples-Joint Patient Specification
#'
#' @description
#' This method returns a `data.frame` of key quantities (survival / log-hazard / etc)
#' for selected patients at a given set of time points.
#'
#' @family SurvivalSamples
setMethod(
    f = "predict",
    signature = "SurvivalSamples",
    definition = function(
        object,
        patients = NULL,
        time_grid = NULL,
        type = c("surv", "haz", "loghaz", "cumhaz")
    ) {
        type <- match.arg(type)

        data <- as.list(object@data)
        patients <- decompose_patients(patients, names(data$pt_to_ind))

        time_grid <- expand_time_grid(time_grid, max(data[["Times"]]))

        gq <- generateQuantities(
            object,
            patients = patients$patients_vec,
            time_grid_lm = numeric(0),
            time_grid_sm = time_grid
        )

        quantities <- extract_survival_quantities(gq, type)

        quantities_summarised <- lapply(
            patients$patients_index,
            summarise_by_group,
            time_index = seq_along(time_grid),
            quantities = quantities
        )

        for (i in seq_along(quantities_summarised)) {
            assert_that(nrow(quantities_summarised[[i]]) == length(time_grid))
            quantities_summarised[[i]][["time"]] <- time_grid
            quantities_summarised[[i]][["group"]] <- names(patients$patients_list)[[i]]
            quantities_summarised[[i]][["type"]] <- type
        }
        Reduce(rbind, quantities_summarised)
    }
)


#' Summarise Quantities By Group
#'
#' This function takes a [posterior::draws_matrix()] (matrix of cmdstanr sample draws) and calculates
#' summary statistics (median / lower ci / upper ci) for selected columns.
#' A key feature is that it allows for columns to be aggregated together (see details).
#'
#' @param subject_index (`numeric`)\cr Which subject indices to extract from `quantities`.
#' See details.
#'
#' @param time_index (`numeric`)\cr Which time point indices to extract from `quantities`.
#' See details.
#'
#' @param quantities ([`posterior::draws_matrix`])\cr A matrix of sample draws.
#' See details.
#'
#' @details
#' It is assumed that `quantities` consists of the cartesian product
#' of subject indices and time indices. That is, if the matrix contains 4 subjects and 3 time
#' points then it should have 12 columns.
#' It is also assumed that each column of `quantities` are named as:
#' ```
#' "quantity[x,y]"
#' ```
#' Where
#' - `x` is the subject index
#' - `y` is the time point index
#'
#' The resulting `data.frame` that is created will have 1 row per value of `time_index` where
#' each row represents the summary statistics for that time point.
#'
#' Note that if multiple values are provided for `subject_index` then the pointwise average
#' will be calculated for each time point by taking the mean across the specified subjects
#' at that time point.
#'
#' @return A data frame containing 1 row per `time_index` (in order) with the following columns:
#' - `median` - The median value of the samples in `quantities`
#' - `lower` - The lower `95%` CI value of the samples in `quantities`
#' - `upper` - The upper `95%` CI value of the samples in `quantities`
#'
#' @keywords internal
summarise_by_group <- function(subject_index, time_index, quantities) {
    assert_that(
        is.numeric(subject_index),
        is.numeric(time_index),
        length(time_index) == length(unique(time_index)),
        inherits(quantities, "draws_matrix")
    )
    stacked_quantities <- array(dim = c(
        nrow(quantities),
        length(time_index),
        length(subject_index)
    ))
    for (ind in seq_along(subject_index)) {
        quantity_index <- sprintf(
            "quantity[%i,%i]",
            subject_index[ind],
            time_index
        )
        stacked_quantities[, , ind] <- quantities[, quantity_index]
    }
    averaged_quantities <- apply(
        stacked_quantities,
        c(1, 2),
        mean,
        simplify = TRUE
    )
    samples_median_ci(averaged_quantities)
}



#' Extract Survival Quantities
#'
#' Utility function to extract generated quantities from a [cmdstanr::CmdStanGQ] object.
#' Multiple quantities are generated by default so this is a convenience function to extract
#' the desired ones and return them them as a user friendly [posterior::draws_matrix] object
#'
#' @param gq (`CmdStanGQ`) \cr A [cmdstanr::CmdStanGQ] object created by [generateQuantities]
#' @inheritParams SurvivalSamples-Joint
#' @keywords internal
extract_survival_quantities <- function(gq, type = c("surv", "haz", "loghaz", "cumhaz")) {
    type <- match.arg(type)
    assert_that(
        inherits(gq, "CmdStanGQ")
    )
    meta <- switch(type,
        surv = list("log_surv_fit_at_time_grid", exp),
        cumhaz = list("log_surv_fit_at_time_grid", \(x) -x),
        haz = list("log_haz_fit_at_time_grid", exp),
        loghaz = list("log_haz_fit_at_time_grid", identity)
    )
    result <- gq$draws(meta[[1]], format = "draws_matrix")
    result_transformed <- meta[[2]](result)
    cnames <- colnames(result_transformed)
    colnames(result_transformed) <- gsub(meta[[1]], "quantity", cnames)
    result_transformed
}


# SurvivalSamples-autoplot ----

# TODO - UNIT TESTS
#' Automatic Plotting for SurvivalSamples
#'
#' @inheritParams SurvivalSamples-Joint
#' @inheritSection SurvivalSamples-Joint Patient Specification
#' @param add_km (`logical`) \cr If `TRUE` Kaplan-Meier curves will be added to the plot for
#' each group/patient as defined by `patients`
#' @param add_ci (`logical`) \cr If `TRUE` 95% CI will be added to the plot for
#' each group/patient as defined by `patients`
#' @param add_wrap (`logical`) \cr If `TRUE` will apply a [ggplot2::facet_wrap()] to the plot
#' by each group/patient as defined by `patients`
#' @param ... other arguments passed to plotting methods.
#'
#' @family autoplot
#' @family SurvivalSamples
#'
setMethod(
    f = "autoplot",
    signature = c(object = "SurvivalSamples"),
    function(object,
             patients,
             time_grid = NULL,
             type = c("surv", "haz", "loghaz", "cumhaz"),
             add_km = FALSE,
             add_ci = TRUE,
             add_wrap = TRUE,
             ...) {
        assert_that(is.flag(add_km))
        kmdf <- if (add_km) subset(object@data, patients) else NULL
        type <- match.arg(type)
        all_fit_df <- predict(object, patients, time_grid, type)
        label <- switch(type,
            "surv" = expression(S(t)),
            "cumhaz" = expression(H(t)),
            "haz" = expression(h(t)),
            "loghaz" = expression(log(h(t)))
        )
        survival_plot(
            data = all_fit_df,
            add_ci = add_ci,
            add_wrap = add_wrap,
            kmdf = kmdf,
            y_label = label
        )
    }
)



#' Survival Plot
#'
#' Internal plotting function to create survival plots with KM curve overlays
#' This function predominately exists to extract core logic into its own function
#' to enable easier unit testing.
#'
#' @param data (`data.frame`)\cr A `data.frame` of summary statistics for a survival
#' curve to be plotted. See details.
#' @param add_ci (`logical`)\cr Should confidence intervals be added? Default = `TRUE`.
#' @param add_wrap (`logical`)\cr Should the plots be wrapped by `data$group`? Default = `TRUE`.
#' @param kmdf (`data.frame` or `NULL`)\cr A `data.frame` of event times and status used to plot
#' overlaying KM curves. If `NULL` no KM curve will be plotted. See details.
#' @param y_label (`character` or `expression`) \cr Label to display on the y-axis.
#' Default = `expression(S(t))`
#' @param x_label (`character` or `expression`) \cr Label to display on the x-axis.
#'
#' @details
#'
#' ## `data`
#' Should contain the following columns:
#' - `time` - Time point
#' - `group` - The group in which the observation belongs to
#' - `median` - The median value for the summary statistic
#' - `upper` - The upper 95% CI for the summary statistic
#' - `lower` - The lower 95% CI for the summary statistic
#'
#' ## `kmdf`
#' Should contain the following columns:
#' - `time` - The time at which an event occurred
#' - `event` - 1/0 status indicator for the event
#' - `group` - Which group the event belongs to, should correspond to values in `data$group`
#' @keywords internal
survival_plot <- function(
    data,
    add_ci = TRUE,
    add_wrap = TRUE,
    kmdf = NULL,
    y_label = expression(S(t)),
    x_label = expression(t)
) {
    assert_that(
        is.flag(add_ci),
        is.flag(add_wrap),
        is.expression(y_label) || is.character(y_label),
        is.expression(x_label) || is.character(x_label),
        is.null(kmdf) | is.data.frame(kmdf)
    )

    p <- ggplot() +
        xlab(x_label) +
        ylab(y_label) +
        theme_bw()

    if (add_wrap) {
        p <- p + facet_wrap(~group)
        aes_ci <- aes(x = .data$time, ymin = .data$lower, ymax = .data$upper)
        aes_line <- aes(x = .data$time, y = .data$median)
        aes_km <- aes(time = .data$time, status = .data$event)
    } else {
        aes_ci <- aes(
            x = .data$time,
            ymin = .data$lower,
            ymax = .data$upper,
            fill = .data$group,
            group = .data$group
        )
        aes_line <- aes(
            x = .data$time,
            y = .data$median,
            colour = .data$group,
            group = .data$group
        )
        aes_km <- aes(
            time = .data$time,
            status = .data$event,
            group = .data$group,
            colour = .data$group
        )
    }
    p <- p + geom_line(aes_line, data = data)
    if (add_ci) {
        p <- p + geom_ribbon(aes_ci, data = data, alpha = 0.3)
    }
    if (!is.null(kmdf)) {
        p <- p +
            ggplot2.utils::geom_km(aes_km, data = kmdf) +
            ggplot2.utils::geom_km_ticks(aes_km, data = kmdf)
    }
    p
}
