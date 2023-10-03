# LongitudinalSamples-class ----

# TODO - Documentation
.LongitudinalQuantities <- setClass(
    "LongitudinalQuantities",
    slots = c(
        "quantities" = "list",
        "quantities_predicted" = "list",
        "groups" = "list",
        "time_grid" = "numeric"
    )
)
LongitudinalQuantities <- function(
    object,
    groups = NULL,
    time_grid = NULL
) {
    assert_that(inhertits(object, "JointModelSamples"))
    assert_that(is.character(groups))
    data <- as.list(object@data)
    patients <- decompose_patients(groups, names(data$pt_to_ind))
    time_grid <- expand_time_grid(time_grid, max(data[["Tobs"]]))

    gq <- generateQuantities(
        object,
        patients = patients$unique_values,
        time_grid_lm = time_grid,
        time_grid_sm = numeric(0)
    )
browser()
    # extract_survival_quantities
    quantities_raw <- extract_longitudinal_quantities(gq)

    # TODO - Think this can be deleted
    quantities <- lapply(
        patients$indexes,
        average_samples_by_index,
        time_index = seq_along(time_grid),
        quantities = quantities_raw
    )

    .LongitudinalQuantities(
        quantities = quantities,
        quantities_predicted = object@data,
        groups = patients$groups,
        time_grid = time_grid
    )
}


summarise_ypred <- function(object) {
    assert_that(inhertits(object, "JointModelSamples"))
    data <- as.list(object@data)
    y_fit_samples <- object@results$draws("Ypred", format = "draws_matrix")
    x <- data.frame(
        subject = names(data$pt_to_ind)[data$ind_index],
        time = data$Tobs,
        samples_median_ci(y_fit_samples)
    )
    row.names(x) <- NULL
    x
}



# TODO - merge this with survival version it is just an identity mapping
extract_longitudinal_quantities <- function(gq) {
    assert_that(
        inherits(gq, "CmdStanGQ")
    )
    result <- gq$draws("y_fit_at_time_grid", format = "draws_matrix")
    cnames <- colnames(result)
    colnames(result) <- gsub("y_fit_at_time_grid", "quantity", cnames)
    result
}

# y_fit_at_grid_samples <- gq$draws(format = "draws_matrix")

# data frame of ypred values




    # results <- list()
    # for (this_pt_ind in seq_along(patients)) {
    #     this_pt <- patients[this_pt_ind]
    #     this_result <- list()
    #     this_y_fit_names <- sprintf(
    #         "y_fit_at_time_grid[%i,%i]",
    #         this_pt_ind,
    #         seq_along(time_grid)
    #     )
    #     this_result$samples <- y_fit_at_grid_samples[, this_y_fit_names, drop = FALSE]
    #     this_result$summary <- data.frame(
    #         time = time_grid,
    #         samples_median_ci(this_result$samples)
    #     )
    #     for_this_pt <- which(data$ind_index == data$pt_to_ind[this_pt])
    #     this_fit <- samples_median_ci(y_fit_samples[, for_this_pt, drop = FALSE])
    #     this_result$observed <- data.frame(
    #         t = data$Tobs[for_this_pt],
    #         y = data$Yobs[for_this_pt],
    #         this_fit
    #     )
    #     results[[this_pt]] <- this_result
    # }



as.data.frame.LongitudinalQuantities <- function(...) {}




summary.LongitudinalQuantities <- function(
    object,
    conf.level = 0.95,
    ...
) {
    #TODO 
}




longitudinal_plot <- function(...) {}

autoplot.LongitudinalSamples <- function(object, ...) {
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


# Manually specify flow + features
adsl <- dm |>
    mutate(VAR1 = derive_var1(inp1, inp2)) |>
    mutate(VAR2 = derive_var2(inp1, inp3))

# wrapper + Specific feature flag
make_adsl(add_total_column = TRUE)

# Wrapper + study flag (logic in wrapper)
make_adsl(is_study_x = TRUE)

# Logic in wrapper from environment variables or something
make_adsl()
