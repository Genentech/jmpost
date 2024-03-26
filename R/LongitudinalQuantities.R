
#' @include DataJoint.R
#' @include Quantities.R
NULL


#' Re-used documentation for `LongitudinalQuantities`
#'
#' @param x ([`LongitudinalQuantities`]) \cr longitudinal quantities.
#' @param object ([`LongitudinalQuantities`]) \cr longitudinal quantities.
#' @param ... not used.
#'
#' @keywords internal
#' @name LongitudinalQuantities-Shared
NULL


# TODO - Update docs
#' `LongitudinalQuantities` Object & Constructor Function
#'
#' Constructor function to generate a `LongitudinalQuantities` object.
#'
#' @details
#' Note that unlike [`SurvivalQuantities`], [`LongitudinalQuantities`] does not support
#' group aggregation.
#'
#' @slot quantities (`Quantities`)\cr The sampled quantities. Should contain 1 element per
#' element of `group`
#'
#'
#' @family LongitudinalQuantities
#' @name LongitudinalQuantities-class
#' @export LongitudinalQuantities
.LongitudinalQuantities <- setClass(
    "LongitudinalQuantities",
    slots = c(
        "quantities" = "Quantities",
        "data" = "DataJoint"
    )
)

#' @param object ([`JointModelSamples`]) \cr samples as drawn from a Joint Model.
#'
#' @param groups (`character`, `NULL`)\cr which patients to calculate the desired
#' quantities for.
#'
#' @param time_grid (`numeric` or `NULL`)\cr a vector of time points to calculate the desired
#' quantity at. If `NULL` will be set to `seq(0, max_longitudinal_time, length = 201)`.
#' @rdname LongitudinalQuantities-class
LongitudinalQuantities <- function(
    object,
    grid
) {
    assert_class(object, "JointModelSamples")
    assert_class(grid, "Grid")

    gq <- generateQuantities(
        object,
        generator = as.QuantityGenerator(grid, object@data),
        type = "longitudinal"
    )

    quantities_raw <- extract_quantities(gq, type = "lm_identity")

    collapser <- as.QuantityCollapser(grid, object@data)
    quantities <- collapse_quantities(quantities_raw, collapser)

    .LongitudinalQuantities(
        quantities = Quantities(
            quantities,
            groups = collapser@groups,
            times = collapser@times
        ),
        data = object@data
    )
}



#' `as.data.frame`
#'
#' @param x ([`LongitudinalQuantities`]) \cr longitudinal quantities.
#' @param ... not used.
#' @family LongitudinalQuantities
#' @export
as.data.frame.LongitudinalQuantities <- function(x, ...) {
    as.data.frame(x@quantities)
}



#' summary
#'
#' @description
#' This method returns a `data.frame` of the longitudinal quantities.
#'
#' @param conf.level (`numeric`) \cr confidence level of the interval.
#' @inheritParams LongitudinalQuantities-Shared
#'
#' @family LongitudinalQuantities
#' @family summary
#' @export
summary.LongitudinalQuantities <- function(
    object,
    conf.level = 0.95,
    ...
) {
    summary(object@quantities, conf.level = conf.level)
}


#' Longitudinal Plot
#'
#' Internal plotting function to create longitudinal plots
#' This function predominately exists to extract core logic into its own function
#' to enable easier unit testing.
#'
#' @param data (`data.frame`)\cr summary statistics for longitudinal
#' value estimates. See details.
#' @param data_obs (`data.frame`)\cr real observed values to be
#' overlaid for reference.  See details.
#' @param add_ci (`logical`)\cr Should confidence intervals be added? Default = `TRUE`.
#' @details
#'
#' ## `data`
#' Should contain the following columns:
#' - `time` (`numeric`) \cr time point for the summary statistic.
#' - `group` (`character`) \cr the group in which the observation belongs to.
#' - `median` (`numeric`) \cr the median value for the summary statistic.
#' - `upper` (`numeric`) \cr the upper 95% CI for the summary statistic.
#' - `lower` (`numeric`) \cr the lower 95% CI for the summary statistic.
#'
#' ## `data_obs`
#' Should contain the following columns:
#' - `time` (`numeric`) \cr the time at which the observed value occurred.
#' - `Yob` (`numeric`) \cr the real observed value.
#' - `group` (`character`) \cr which group the event belongs to, should correspond to
#' values in `data$group`.
#' @keywords internal
longitudinal_plot <- function(
    data,
    data_obs = NULL,
    add_ci = FALSE
) {
    p <- ggplot() +
        geom_line(aes(x = .data$time, y = .data$median), data = data) +
        xlab(expression(t)) +
        ylab(expression(y)) +
        facet_wrap(~group) +
        theme_bw()

    if (add_ci) {
        p <- p + geom_ribbon(
            aes(x = .data$time, ymin = .data$lower, ymax = .data$upper),
            data = data,
            alpha = 0.3
        )
    }

    if (!is.null(data_obs)) {
        p <- p + geom_point(aes(x = .data$time, y = .data$Yob), data = data_obs)
    }
    return(p)
}


#' Automatic Plotting for `LongitudinalQuantities`
#'
#' @param conf.level (`numeric`) \cr confidence level of the interval. If values of `FALSE`,
#' `NULL` or `0` are provided then confidence regions will not be added to the plot.
#' @inheritParams LongitudinalQuantities-Shared
#'
#' @family LongitudinalQuantities
#' @family autoplot
#' @export
autoplot.LongitudinalQuantities <- function(object, conf.level = 0.95, ...) {
    include_ci <- !is.null(conf.level) && is.numeric(conf.level) && conf.level > 0
    # If CI aren't needed supply a default 0.95 to summary function as it needs
    # a value to be specified to work.
    conf.level <- if (include_ci) conf.level else 0.95
    data_sum <- summary(object, conf.level = conf.level)
    data_obs <- extract_observed_values(object@data)
    assert_that(
        "group" %in% names(data_sum),
        "subject" %in% names(data_obs)
    )
    data_obs$group <- data_obs$subject
    data_obs <- data_obs[data_obs$group %in% data_sum$group, ]
    longitudinal_plot(
        data = data_sum,
        data_obs = data_obs,
        add_ci = include_ci
    )
}



#' @rdname show-object
#' @export
setMethod(
    f = "show",
    signature = "LongitudinalQuantities",
    definition = function(object) {
        template <- c(
            "LongitudinalQuantities Object:",
            "    # of samples    = %d",
            "    # of quantities = %d"
        )
        string <- sprintf(
            paste(template, collapse = "\n"),
            nrow(object@quantities),
            ncol(object@quantities)
        )
        cat("\n", string, "\n\n")
    }
)
