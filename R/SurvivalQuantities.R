
#' @include DataJoint.R
#' @include Quantities.R
NULL



#' Re-used documentation for `SurvivalQuantities`
#'
#' @param object ([`SurvivalQuantities`]) \cr survival quantities.
#' @param x ([`SurvivalQuantities`]) \cr survival quantities.
#' @param ... not used.
#'
#' @keywords internal
#' @name SurvivalQuantities-Shared
NULL



#' `SurvivalQuantities` Object & Constructor Function
#'
#' Constructor function to generate a `SurvivalQuantities` object.
#'
#' @slot quantities (`Quantities`)\cr The sampled quantities. Should contain 1 element per
#' element of `group`
#' @slot groups (`list`)\cr See argument section for details
#' @slot type (`character`)\cr See See argument section for details
#' @slot time_grid (`numeric`)\cr See argument section for details
#' @slot data ([`DataJoint`])\cr The data that the Joint Model was fitted to to produce
#' the samples/quantities
#'
#' @section Group Specification:
#' If `groups` is a character vector of subject IDs then the survival quantities will
#' only be calculated for those specific subjects.
#'
#' If `groups` is a list then any elements with more than 1 subject ID will be grouped together
#' and their quantities will be calculated by taking a point-wise average.
#' For example: `groups = list("g1" = c("sub1", "sub2"), "g2" = c("sub3", "sub4"))` would result
#' in 2 groups being created whose values are the pointwise average
#' of `c("sub1", "sub2")` and `c("sub3", "sub4")` respectively.
#'
#' If `groups=NULL` then all subjects from original dataset will be selected
#'
#' @family SurvivalQuantities
#' @name SurvivalQuantities-class
#' @export SurvivalQuantities
.SurvivalQuantities <- setClass(
    Class = "SurvivalQuantities",
    slots = c(
        "quantities" = "Quantities",
        "grid" = "Grid",
        "type" = "character",
        "data" = "DataJoint"
    )
)

#' @param object ([`JointModelSamples`]) \cr samples as drawn from a Joint Model.
#'
#' @param grid (`Grid`) \cr object that specifies which subjects and time points to calculate the
#' quantities for. See [Grid-Functions].
#'
#' @param type (`character`)\cr quantity to be generated.
#' Must be one of `surv`, `haz`, `loghaz`, `cumhaz`.
#'
#' @rdname SurvivalQuantities-class
SurvivalQuantities <- function(
    object,
    grid,
    type = c("surv", "haz", "loghaz", "cumhaz")
) {
    type <- match.arg(type)
    assert_class(object, "JointModelSamples")
    assert_class(grid, "Grid")
    assert_that(
        !is(grid, "GridPopulation"),
        msg = "GridPopulation objects are not supported for `SurvivalQuantities`"
    )

    time_grid <- seq(
        from = 0,
        to = max(as.list(object@data)[["event_times"]]),
        length = 201
    )

    grid <- coalesceGridTime(grid, time_grid)

    generator <- as.QuantityGenerator(grid, data = object@data)

    assert_that(
        all(generator@times >= 0),
        msg = "Time points must be greater than or equal to 0"
    )

    gq <- generateQuantities(
        object,
        generator = generator,
        type = "survival"
    )

    quantities_raw <- extract_quantities(gq, type)
    collapser <- as.QuantityCollapser(grid, object@data)
    quantities <- collapse_quantities(quantities_raw, collapser)

    .SurvivalQuantities(
        quantities = Quantities(
            quantities,
            groups = collapser@groups,
            times = collapser@times
        ),
        grid = grid,
        data = object@data,
        type = type
    )
}



#' `as.data.frame`
#'
#' @param x ([`SurvivalQuantities`]) \cr longitudinal quantities.
#' @param ... not used.
#' @family SurvivalQuantities
#' @export
as.data.frame.SurvivalQuantities <- function(x, ...) {
    as.data.frame(x@quantities)
}



#' summary
#'
#' @description
#' This method returns a `data.frame` of the longitudinal quantities.
#'
#' @param conf.level (`numeric`) \cr confidence level of the interval.
#' @inheritParams SurvivalQuantities-Shared
#'
#' @family SurvivalQuantities
#' @family summary
#' @export
summary.SurvivalQuantities <- function(
    object,
    conf.level = 0.95,
    ...
) {
    summary(object@quantities, conf.level = conf.level)
}


#' Automatic Plotting for `SurvivalQuantities``
#'
#' @inheritParams SurvivalQuantities-Shared
#' @param add_km (`logical`) \cr if `TRUE` Kaplan-Meier curves will be added to the plot for
#' each group/subject.
#' @param add_wrap (`logical`) \cr if `TRUE` will apply a [ggplot2::facet_wrap()] to the plot
#' by each group/subject.
#' @param conf.level (`numeric`) \cr confidence level of the interval. If values of `FALSE`,
#' `NULL` or `0` are provided then confidence regions will not be added to the plot
#' @param ... not used.
#'
#' @family SurvivalQuantities
#' @family autoplot
#' @export
autoplot.SurvivalQuantities <- function(
    object,
    conf.level = 0.95,
    add_km = FALSE,
    add_wrap = TRUE,
    ...
) {
    include_ci <- !is.null(conf.level) && is.numeric(conf.level) && conf.level > 0
    # If CI aren't needed supply a default 0.95 to summary function as it needs
    # a value to be specified to work
    conf.level <- if (include_ci) conf.level else 0.95
    assert_that(
        is.flag(add_km),
        length(conf.level) == 1,
        conf.level < 1,
        is.flag(add_wrap)
    )
    kmdf <- if (add_km) {
        subset(
            object@data,
            as.list(object@grid, data = object@data)
        )
    } else {
        NULL
    }
    all_fit_df <- summary(object, conf.level = conf.level)
    label <- switch(
        object@type,
        "surv" = expression(S(t)),
        "cumhaz" = expression(H(t)),
        "haz" = expression(h(t)),
        "loghaz" = expression(log(h(t)))
    )
    survival_plot(
        data = all_fit_df,
        add_ci = include_ci,
        add_wrap = add_wrap,
        kmdf = kmdf,
        y_label = label
    )
}




#' Survival Plot
#'
#' Internal plotting function to create survival plots with KM curve overlays
#' This function predominately exists to extract core logic into its own function
#' to enable easier unit testing.
#'
#' @param data (`data.frame`)\cr summary statistics for a survival
#' curve to be plotted. See details.
#' @param add_ci (`logical`)\cr should confidence intervals be added? Default = `TRUE`.
#' @param add_wrap (`logical`)\cr should the plots be wrapped by `data$group`? Default = `TRUE`.
#' @param kmdf (`data.frame` or `NULL`)\cr event times and status used to plot
#' overlaying KM curves. If `NULL` no KM curve will be plotted. See details.
#' @param y_label (`character` or `expression`) \cr label to display on the y-axis.
#' Default = `expression(S(t))`.
#' @param x_label (`character` or `expression`) \cr label to display on the x-axis.
#'
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
#' ## `kmdf`
#' Should contain the following columns:
#' - `time` (`numeric`) \cr the time at which an event occurred.
#' - `event` (`numeric`) \cr 1/0 status indicator for the event.
#' - `group` (`character`) \cr which group the event belongs to, should correspond to values in `data$group`.
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


#' @rdname show-object
#' @export
setMethod(
    f = "show",
    signature = "SurvivalQuantities",
    definition = function(object) {
        template <- c(
            "SurvivalQuantities Object:",
            "    # of samples    = %d",
            "    # of quantities = %d",
            "    Type            = %s"
        )
        string <- sprintf(
            paste(template, collapse = "\n"),
            nrow(object@quantities),
            ncol(object@quantities),
            object@type
        )
        cat("\n", string, "\n\n")
    }
)



#' `brierScore`
#'
#' @description
#' Derives the Brier Scores (using Inverse Probability of Censoring Weighting)
#' for the Survival estimates as detailed in \insertCite{blanche2015}{jmpost}.
#'
#' @inheritParams SurvivalQuantities-Shared
#' @inheritParams Brier-Score-Shared
#'
#' @family brierScore
#' @family SurvivalQuantities
#' @references
#' \insertAllCited{}
#' @export
brierScore.SurvivalQuantities <- function(
    object,
    maintain_cen_order = TRUE,
    event_offset = TRUE,
    ...
) {
    assert_that(
        object@type == "surv",
        msg = paste(
            "Brier Score can only be calculated when the survival quantities were",
            "generated with `type = 'surv'`",
            collapse = " "
        )
    )
    assert_that(
        is(object@grid, "GridFixed"),
        msg = paste(
            "Brier Score can only be calculated when the survival quantities were",
            "generated with `grid = GridFixed()`",
            collapse = " "
        )
    )

    sdat <- summary(object)
    times <- unique(as.QuantityGenerator(object@grid, object@data)@times)
    times <- times[order(times)]
    assert_that(
        nrow(sdat) == length(times) * length(unique(sdat$group))
    )

    subject_col <- extractVariableNames(object@data@subject)$subject
    time_col <- extractVariableNames(object@data@survival)$time
    event_col <- extractVariableNames(object@data@survival)$event
    groups <- as.character(object@data@survival@data[[subject_col]])
    orig_times <- object@data@survival@data[[time_col]]
    events <- as.numeric(object@data@survival@data[[event_col]])

    pred_mat <- matrix(
        ncol = length(times),
        nrow = length(unique(sdat$group))
    )
    for (i in seq_along(times)) {
        pred_mat[, i] <- sdat[sdat["time"] == times[i], "median"]
        assert_that(
            all(groups == sdat[sdat["time"] == times[i], "group"])
        )
    }
    brier_score(
        t = times,
        times = orig_times,
        events = events,
        pred_mat = 1 - pred_mat,
        maintain_cen_order = maintain_cen_order,
        event_offset = event_offset
    )
}
