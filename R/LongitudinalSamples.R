# LongitudinalSamples-class ----

#' `LongitudinalSamples`
#'
#' This class is an extension of `list`so that we
#' can define specific longitudinal postprocessing methods for it.
#'
#' @aliases LongitudinalSamples
#' @exportClass LongitudinalSamples
.LongitudinalSamples <- setClass(
    "LongitudinalSamples",
    contains = "list"
)

# LongitudinalSamples-[ ----

#' @rdname LongitudinalSamples-class
#'
#' @typed x: LongitudinalSamples
#'   the samples object to subset.
#' @typed i: vector
#'   the index vector.
#' @param j not used.
#' @param drop not used.
#' @param ... not used.
#'
#' @returns The subsetted `LongitudinalSamples` object.
#' @export
setMethod(
    f = "[",
    signature = "LongitudinalSamples",
    definition = function(x, i, ...) {
        # Note that we cannot use `callNextMethod()` here because `list` is S3.
        x@.Data <- x@.Data[i]
        x
    }
)

# autoplot-LongitudinalSamples ----

#' @rdname autoplot
setMethod(
    f = "autoplot",
    signature = c(object = "LongitudinalSamples"),
    function(object, ...) {
        all_fit_dfs <- lapply(object, "[[", i = "summary")
        all_fit_dfs_with_pt_id <- Map(cbind, all_fit_dfs, pt_id = names(object))
        all_fit_df <- do.call(rbind, all_fit_dfs_with_pt_id)

        obs_dfs <- lapply(object, "[[", i = "observed")
        obs_dfs_with_pt_id <- Map(cbind, obs_dfs, pt_id = names(object))
        all_obs_df <- do.call(rbind, obs_dfs_with_pt_id)

        ggplot() +
            geom_line(aes(x = .data$time, y = .data$median), data = all_fit_df) +
            geom_ribbon(aes(x = .data$time, ymin = .data$lower, ymax = .data$upper), data = all_fit_df, alpha = 0.3) +
            geom_point(aes(x = .data$t, y = .data$y), data = all_obs_df) +
            xlab(expression(t)) +
            ylab(expression(y)) +
            facet_wrap(~ pt_id)
    }
)
