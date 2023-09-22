



.SurvivalQuantities <- setClass(
    Class = "SurvivalQuantities",
    slots = c(
        "quantities" = "list",
        "group_names" = "character",
        "type" = "character",
        "time_grid" = "numeric"
    )
)
SurvivalQuantities <- function(
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
        patients = patients$unique_values,
        time_grid_lm = numeric(0),
        time_grid_sm = time_grid
    )

    quantities_raw <- extract_survival_quantities(gq, type)

    quantities <- lapply(
        patients$indexes,
        summarise_by_group,
        time_index = seq_along(time_grid),
        quantities = quantities_raw
    )

    .SurvivalQuantities(
        quantities = quantities,
        group_names = group_names,
        type = type,
        time_grid = time_grid
    )
}


setValidity(
    Class = "SurvivalQuantities",
    method = function(object) {
        if (length(object@quantities) != length(object@group_names)) {
            return("`quantities` and `group_names` should be the same length")
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
            quantities_summarised[[i]][["group"]] <- object@group_names[[i]]
            quantities_summarised[[i]][["type"]] <- object@type
        }
        Reduce(rbind, quantities_summarised)
    }
)


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
            quantities_df[[i]]["group"] <- x@group_names[[i]]
        }
        res <- Reduce(rbind, quantities_df)
        res[, c("values", "time", "group", "type")]
    }
)

