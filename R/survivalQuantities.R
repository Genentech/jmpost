
#' @include DataJoint.R
NULL

#' NULL Documentation page to house re-usable elements across SurvivalQuantities methods/objects
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
#' @name SurvivalQuantities-Shared
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



#' SurvivalQuantities
#'
#' Constructor function to generate a `SurvivalQuantities` object.
#'
#' @param quantities (`JointModelSamples`) \cr A [JointModelSamples](JointModelSamples-class) object
#' @param groups See slot section for details
#' @param type See slot section for details
#' @param time_grid See slot section for details
#' @param data See slot section for details
#'
#' @slot quantities See argument section for details
#' @slot groups See argument section for details
#' @slot type See See argument section for details
#' @slot time_grid See argument section for details
#' @slot data See argument section for details
#'
#' @family SurvivalQuantities
#' @seealso [JointModelSamples][JointModelSamples-class]
#' @name SurvivalQuantities-class
#' @export SurvivalQuantities
.SurvivalQuantities <- setClass(
    Class = "SurvivalQuantities",
    slots = c(
        "quantities" = "list",
        "groups" = "list",
        "type" = "character",
        "time_grid" = "numeric",
        "data" = "DataJoint"
    )
)
#' @rdname SurvivalQuantities-class
SurvivalQuantities <- function(quantities, groups, type, time_grid, data) {
    .SurvivalQuantities(
        quantities = quantities,
        groups = groups,
        type = type,
        time_grid = time_grid,
        data = data
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
        for (element in object@quantities) {
            if (!inherits(element, "matrix")) {
                return("each element of `quantities` must be a matrix")
            }
            if (ncol(element) != length(object@time_grid)) {
                return("each element of `quantities` must have #columns = `length(time_grid)`")
            }
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
#' for selected patients at a given set of time points.
#'
#' @family SurvivalQuantities
#' @family summary
setMethod(
    f = "summary",
    signature = "SurvivalQuantities",
    definition = function(
        object,
        conf.level = 0.95
    ) {

        quantities_summarised <- lapply(
            object@quantities,
            samples_median_ci,
            level = conf.level
        )

        for (i in seq_along(quantities_summarised)) {
            assert_that(nrow(quantities_summarised[[i]]) == length(object@time_grid))
            quantities_summarised[[i]][["time"]] <- object@time_grid
            quantities_summarised[[i]][["group"]] <- names(object@groups)[[i]]
            quantities_summarised[[i]][["type"]] <- object@type
        }
        Reduce(rbind, quantities_summarised)
    }
)


#' `as.data.frame`
#'
#' @param x ([`SurvivalQuantities`]) \cr Survival Quantities
#' @param ... Not used
#' @family as.data.frame
#' @family SurvivalQuantities
setMethod(
    f = "as.data.frame",
    signature = "SurvivalQuantities",
    definition = function(x, ...) {

        quantities_df <- lapply(
            x@quantities,
            \(element) {
                n <- nrow(element)
                values <- as.vector(element)
                times <- rep(x@time_grid, each = n)
                type <- rep(x@type, each = length(x@time_grid) * n)
                assert_that(
                    length(values) == length(times),
                    length(times) == length(type)
                )
                data.frame(
                    values = values,
                    time = times,
                    type = type,
                    stringsAsFactors = FALSE
                )
            }
        )
        for (i in seq_along(quantities_df)) {
            quantities_df[[i]]["group"] <- names(x@groups)[[i]]
        }
        res <- Reduce(rbind, quantities_df)
        res[, c("values", "time", "group", "type")]
    }
)



# SurvivalSamples-autoplot ----

#' Automatic Plotting for SurvivalSamples
#'
#' @param object ([`SurvivalQuantities`]) \cr Survival Quantities
#' @param add_km (`logical`) \cr If `TRUE` Kaplan-Meier curves will be added to the plot for
#' each group/patient as defined by `patients`
#' @param add_ci (`logical`) \cr If `TRUE` 95% CI will be added to the plot for
#' each group/patient as defined by `patients`
#' @param add_wrap (`logical`) \cr If `TRUE` will apply a [ggplot2::facet_wrap()] to the plot
#' by each group/patient as defined by `patients`
#' @param ... other arguments passed to plotting methods.
#'
#' @family autoplot
#' @family SurvivalQuantities
#'
setMethod(
    f = "autoplot",
    signature = c(object = "SurvivalQuantities"),
    function(object,
             add_km = FALSE,
             add_ci = TRUE,
             add_wrap = TRUE,
             ...) {
        assert_that(is.flag(add_km))
        kmdf <- if (add_km) subset(object@data, object@groups) else NULL
        all_fit_df <- summary(object)
        label <- switch(object@type,
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
