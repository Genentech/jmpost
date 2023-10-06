
#' @include DataJoint.R
#' @include Quantities.R
NULL


#' `SurvivalQuantities` Object & Constructor Function
#'
#' Constructor function to generate a `SurvivalQuantities` object.
#'
#' @param object ([`JointModelSamples`]) \cr samples as drawn from a Joint Model.
#'
#' @param groups (`character` or `list` or `NULL`)\cr which patients to calculate the desired
#' quantities for.
#' See "Group Specification" for more details.
#'
#' @param type (`character`)\cr quantity to be generated.
#' Must be one of `surv`, `haz`, `loghaz`, `cumhaz`.
#'
#' @param time_grid (`numeric` or `NULL`)\cr vector of time points to calculate the desired
#' quantity at. If `NULL` will be set to `seq(0, max_survival_time, length = 201)`.
#'
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
#' For example: `groups = list("g1" = c("pt1", "pt2"), "g2" = c("pt3", "pt4"))` would result
#' in 2 groups being created whose values are the pointwise average
#' of `c("pt1", "pt2")` and `c("pt3", "pt4")` respectively.
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
        "groups" = "list",
        "type" = "character",
        "time_grid" = "numeric",
        "data" = "DataJoint"
    )
)

#' @rdname SurvivalQuantities-class
SurvivalQuantities <- function(
    object,
    groups = NULL,
    time_grid = NULL,
    type = c("surv", "haz", "loghaz", "cumhaz")
) {
    type <- match.arg(type)

    data <- as.list(object@data)
    patients <- decompose_patients(groups, names(data$pt_to_ind))

    time_grid <- expand_time_grid(time_grid, max(data[["Times"]]))

    gq <- generateQuantities(
        object,
        patients = patients$unique_values,
        time_grid_lm = numeric(0),
        time_grid_sm = time_grid
    )

    quantities_raw <- extract_quantities(gq, type)

    quantities <- lapply(
        patients$indexes,
        average_samples_by_index,
        time_index = seq_along(time_grid),
        quantities = quantities_raw
    )

    .SurvivalQuantities(
        quantities = Quantities(quantities),
        groups = patients$groups,
        type = type,
        time_grid = time_grid,
        data = object@data
    )
}

setValidity(
    Class = "SurvivalQuantities",
    method = function(object) {
        if (length(object@quantities) != length(object@groups)) {
            return("`quantities` and `groups` should be the same length")
        }
        if (length(object@groups) != length(names(object@groups))) {
            return("`groups` must be a named list")
        }
        if (length(object@type) != 1) {
            return("`type` should be a length 1 character")
        }
        if (!object@type %in% c("surv", "loghaz", "cumhaz", "haz")) {
            return("`type` must be one of 'surv', 'loghaz', 'cumhaz', 'haz'")
        }
        if (length(object@time_grid) == 0) {
            return("`time_grid` cannot be length 0")
        }
        if (ncol(object@quantities) != length(object@time_grid)) {
            return("`quantities` must have #columns = `length(time_grid)`")
        }
        for (group in object@groups) {
            if (!(is.character(group) && length(group > 0))) {
                return("Each element of `groups` must be a length >= 1 character vector")
            }
        }
    }
)


#' summary
#'
#'
#' @description
#' This method returns a `data.frame` of key quantities (survival / log-hazard / etc)
#'
#' @param object ([`SurvivalQuantities`]) \cr survival quantities.
#' @param conf.level (`numeric`) \cr confidence level of the interval.
#' @param ... not used.
#'
#' @family SurvivalQuantities
#' @family summary
#' @export
summary.SurvivalQuantities <- function(
    object,
    conf.level = 0.95,
    ...
) {
    summary(
        object@quantities,
        time_grid = object@time_grid,
        groups = object@groups,
        type = object@type,
        conf.level = conf.level
    )
}



#' `SurvivalQuantities` -> `data.frame`
#'
#' @param x ([`SurvivalQuantities`]) \cr survival quantities.
#' @param ... not used.
#' @family SurvivalQuantities
#' @export
as.data.frame.SurvivalQuantities <- function(x, ...) {
    as.data.frame(
        x@quantities,
        time_grid = x@time_grid,
        groups = x@groups,
        type = x@type
    )
}


#' Automatic Plotting for LongitudinalQuantities
#'
#' @param object ([`SurvivalQuantities`]) \cr survival quantities.
#' @param add_km (`logical`) \cr if `TRUE` Kaplan-Meier curves will be added to the plot for
#' each group/patient.
#' @param add_wrap (`logical`) \cr if `TRUE` will apply a [ggplot2::facet_wrap()] to the plot
#' by each group/patient.
#' @param conf.level (`numeric`) \cr confidence level of the interval. If values of `FALSE`,
#' `NULL` or `0` are provided then confidence regions will not be added to the plot
#' @param ... not used.
#'
#' @family SurvivalQuantities
#' @family autoplot
#' @export
autoplot.SurvivalQuantities <- function(object,
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
    kmdf <- if (add_km) subset(object@data, object@groups) else NULL
    all_fit_df <- summary(object, conf.level = conf.level)
    label <- switch(object@type,
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
