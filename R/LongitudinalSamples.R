#' Longitudinal Samples Storage
#'
#' This class is an extension of a standard `list` so that we
#' can define custom methods for it.
#'
#' @name LongitudinalSamples
#' @export
.LongitudinalSamples <- setClass(
    "LongitudinalSamples",
    contains = "list"
)

#' @rdname LongitudinalSamples
#'
#' @param x (`LongitudinalSamples`)\cr the samples object to subset.
#' @param i (`vector`)\cr the index vector.
#'
#' @return The subsetted `LongitudinalSamples` object.
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

# autoplot ----

#' @rdname autoplot
#' @export
#' @importFrom ggplot2 autoplot
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
            geom_line(aes(x = time, y = median), data = all_fit_df) +
            geom_ribbon(aes(x = time, ymin = lower, ymax = upper), data = all_fit_df, alpha = 0.3) +
            geom_point(aes(x = t, y = y), data = all_obs_df) +
            xlab(expression(t)) +
            ylab(expression(y)) +
            facet_grid(~ pt_id)
    }
)
