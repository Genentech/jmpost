# SurvivalSamples-class ----

#' `SurvivalSamples`
#'
#' This class is an extension of `list` so that we
#' can define specific survival postprocessing methods for it.
#'
#' @aliases SurvivalSamples
#' @exportClass SurvivalSamples
.SurvivalSamples <- setClass(
    "SurvivalSamples",
    contains = "list"
)

# SurvivalSamples-[ ----

#' @rdname SurvivalSamples-class
#'
#' @typed x: SurvivalSamples
#'   the samples object to subset.
#' @typed i: vector
#'   the index vector.
#' @param j not used.
#' @param drop not used.
#' @param ... not used.
#'
#' @returns The subsetted `SurvivalSamples` object.
#' @export
setMethod(
    f = "[",
    signature = "SurvivalSamples",
    definition = function(x, i, ...) {
        # Note that we cannot use `callNextMethod()` here because `list` is S3.
        x@.Data <- x@.Data[i]
        x
    }
)

# SurvivalSamples-aggregate ----

#' @rdname aggregate
#' @typed groups: list
#'   defining into which groups to aggregate
#'   individual samples, where the names are the new group labels and
#'   the character vectors are the old individual sample labels.
setMethod(
    f = "aggregate",
    signature = c(x = "SurvivalSamples"),
    definition = function(x, groups, ...) {
        assert_that(
            is.list(groups),
            !is.null(names(groups)),
            length(x) > 0
        )
        x_names <- names(x)
        x <- as(x, "list")
        names(x) <- x_names
        time_grid <- x[[1]]$summary$time
        results <- list()
        for (this_group in names(groups)) {
            this_result <- list()
            this_ids <- groups[[this_group]]
            # Samples.
            this_ids_samples <- Map("[[", x[this_ids], i = "samples")
            this_ids_samples <- lapply(this_ids_samples, "/", length(this_ids))
            this_result$samples <- Reduce(f = "+", x = this_ids_samples)
            # Summary.
            surv_fit <- samples_median_ci(this_result$samples)
            this_result$summary <- cbind(time = time_grid, surv_fit)
            # Observations.
            this_ids_obs <- Map("[[", x[this_ids], i = "observed")
            this_result$observed <- do.call(rbind, this_ids_obs)
            # Save all.
            results[[this_group]] <- this_result
        }
        .SurvivalSamples(results)
    }
)

# SurvivalSamples-autoplot ----

#' @rdname autoplot
#' @typed add_km: flag
#'   whether to add the Kaplan-Meier plot of the
#'   survival data to the plots.
setMethod(
    f = "autoplot",
    signature = c(object = "SurvivalSamples"),
    function(object, add_km = TRUE, ...) {
        assert_that(is.flag(add_km))

        all_fit_dfs <- lapply(object, "[[", i = "summary")
        all_fit_dfs_with_id <- Map(cbind, all_fit_dfs, id = names(object))
        all_fit_df <- do.call(rbind, all_fit_dfs_with_id)

        obs_dfs <- lapply(object, "[[", i = "observed")
        obs_dfs_with_id <- Map(cbind, obs_dfs, id = names(object))
        all_obs_df <- do.call(rbind, obs_dfs_with_id)
        # To avoid issues with logical status in the Kaplan-Meier layer.
        all_obs_df$death_num <- as.numeric(all_obs_df$death)

        p <- ggplot() +
            geom_line(aes(x = .data$time, y = .data$median), data = all_fit_df) +
            geom_ribbon(aes(x = .data$time, ymin = .data$lower, ymax = .data$upper), data = all_fit_df, alpha = 0.3) +
            xlab(expression(t)) +
            ylab(expression(S(t))) +
            facet_wrap(~ id)
        if (add_km) {
            p <- p +
                ggplot2.utils::geom_km(aes(time = .data$t, status = .data$death_num), data = all_obs_df) +
                ggplot2.utils::geom_km_ticks(aes(time = .data$t, status = .data$death_num), data = all_obs_df)
        }
        p
    }
)
