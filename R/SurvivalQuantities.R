
#' @include DataJoint.R
NULL


#' `SurvivalQuantities` Object & Constructor Function
#'
#' Constructor function to generate a `SurvivalQuantities` object.
#'
#' @param object ([`JointModelSamples`]) \cr Samples as drawn from a Joint Model
#'
#' @param groups (`character` or `list` or `NULL`)\cr which patients to calculate the desired
#' quantities for.
#' See "Group Specification" for more details.
#'
#' @param type (`character`)\cr The quantity to be generated.
#' Must be one of `surv`, `haz`, `loghaz`, `cumhaz`.
#'
#' @param time_grid (`numeric` or `NULL`)\cr a vector of time points to calculate the desired
#' quantity at. If `NULL` will be set to `seq(0, max_survival_time, length = 201)`
#'
#'
#' @slot quantities (`list`)\cr Contains 1 named element per element of `groups`.
#' Each element is a matrix of averaged samples for the corresponding patient
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
        "quantities" = "list",
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

    quantities_raw <- extract_survival_quantities(gq, type)

    quantities <- lapply(
        patients$indexes,
        average_samples_by_index,
        time_index = seq_along(time_grid),
        quantities = quantities_raw
    )

    .SurvivalQuantities(
        quantities = quantities,
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
#'
#' @param object ([`SurvivalQuantities`]) \cr Survival Quantities.
#' @param conf.level (`numeric`) \cr confidence level of the interval.
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
#' @inheritParams as.data.frame
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
#' each group/patient
#' @param add_ci (`logical`) \cr If `TRUE` 95% CI will be added to the plot for
#' each group/patient
#' @param add_wrap (`logical`) \cr If `TRUE` will apply a [ggplot2::facet_wrap()] to the plot
#' by each group/patient
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




#' Extract and Average Quantities By Group Index
#'
#' This function takes a [posterior::draws_matrix()] (matrix of cmdstanr sample draws) and extracts
#' the specified columns and aggregates them by calculating the pointwise average.
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
#' This function returns a `matrix` with 1 row per sample and 1 column per `time_index`.
#'
#' Note that if multiple values are provided for `subject_index` then the pointwise average
#' will be calculated for each time point by taking the mean across the specified subjects
#' at each time point.
#'
#' @keywords internal
average_samples_by_index <- function(subject_index, time_index, quantities) {
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
    apply(
        stacked_quantities,
        c(1, 2),
        mean,
        simplify = TRUE
    )
}



#' Extract Survival Quantities
#'
#' Utility function to extract generated quantities from a [cmdstanr::CmdStanGQ] object.
#' Multiple quantities are generated by default so this is a convenience function to extract
#' the desired ones and return them them as a user friendly [posterior::draws_matrix] object
#'
#' @param gq (`CmdStanGQ`) \cr A [cmdstanr::CmdStanGQ] object created by [generateQuantities]
#' @inheritParams SurvivalQuantities-class
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