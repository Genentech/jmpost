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
#' @param x (`SurvivalSamples`)\cr the samples object to subset.
#' @param i (`vector`)\cr the index vector.
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
#' @param groups (`list`)\cr defining into which groups to aggregate
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
