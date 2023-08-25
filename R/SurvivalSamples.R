






.SurvivalSamples <- setClass(
    "SurvivalSamples",
    contains = "JointModelSamples"
)

setMethod(
    f = "survival",
    signature = "JointModelSamples",
    definition = function(object) {
        .SurvivalSamples(object)
    }
)

setMethod(
    f = "predict",
    signature = "SurvivalSamples",
    definition = function(
        object,
        patients = NULL,
        time_grid = NULL,
        type = c("surv", "haz", "loghaz", "cumhaz")
    ) {
        type <- match.arg(type)

        if (is.character(patients)) {
            patients <- expand_patients(patients, names(object@data$pt_to_ind))
            names(patients) <- patients
            patients <- as.list(patients)
        }

        patients <- lapply(
            patients,
            expand_patients,
            all_pts = names(object@data$pt_to_ind)
        )

        assert_that(
            is.list(patients),
            length(unique(names(patients))) == length(patients),
            all(vapply(patients, is.character, logical(1)))
        )

        patients_vec <- unique(unlist(patients))
        patients_lookup <- setNames(seq_along(patients_vec), patients_vec)
        patients_index <- lapply(
            patients,
            \(x) patients_lookup[x]
        )

        data <- object@data
        time_grid <- expand_time_grid(time_grid, max(data[["Times"]]))

        gq <- generateQuantities(
            object,
            patients = patients_vec,
            time_grid_lm = numeric(0),
            time_grid_sm = time_grid
        )

        quantities <- extract_quantities(gq, type)

        quantities_summarised <- lapply(
            patients_index,
            summarise_by_group,
            time_grid = time_grid,
            quantities = quantities
        )

        for (i in seq_along(quantities_summarised)) {
            quantities_summarised[[i]][["group"]] <- names(patients)[[i]]
            quantities_summarised[[i]][["type"]] <- type
        }
        Reduce(rbind, quantities_summarised)
    }
)


summarise_by_group <- function(indexes, time_grid, quantities) {
    stacked_quantities <- array(dim = c(
        nrow(quantities),
        length(time_grid),
        length(indexes)
    ))
    for (i in seq_along(indexes)) {
        index <- indexes[i]
        index_quant_names <- sprintf(
            "quantity[%i,%i]",
            index,
            seq_along(time_grid)
        )
        stacked_quantities[, , i] <- quantities[, index_quant_names]
    }
    averaged_quantities <- apply(
        stacked_quantities,
        c(1, 2),
        mean,
        simplify = TRUE
    )
    data.frame(
        time = time_grid,
        samples_median_ci(averaged_quantities)
    )
}

extract_quantities <- function(gq, type = c("surv", "haz", "loghaz", "cumhaz")) {
    meta <- switch(
        type,
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


# SurvivalSamples-autoplot ----

#' @rdname autoplot
#' @param add_km (`flag`)\cr whether to add the Kaplan-Meier plot of the
#'   survival data to the plots.
setMethod(
    f = "autoplot",
    signature = c(object = "SurvivalSamples"),
    function(
        object,
        patients,
        time_grid = NULL,
        type = c("surv", "haz", "loghaz", "cumhaz"),
        add_km = TRUE,
        add_ci = TRUE,
        add_wrap = TRUE,
        ...
    ) {
        assert_that(is.flag(add_km))
        type <- match.arg(type)
        all_fit_df <- predict(object, patients, time_grid, type)

        label <- switch(type,
            "surv" = expression(S(t)),
            "cumhaz" = expression(H(t)),
            "haz" = expression(h(t)),
            "loghaz" = expression(log(h(t)))
        )

        p <- ggplot() +
            xlab(expression(t)) +
            theme_bw() +
            ylab(label)

        if (add_wrap) {
            p <- p + facet_wrap(~group)
            aes_ci <- aes(x = .data$time, ymin = .data$lower, ymax = .data$upper)
            aes_line <- aes(x = .data$time, y = .data$median)
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
        }

        p <- p + geom_line(aes_line, data = all_fit_df)

        if (add_ci) {
            p <- p + geom_ribbon(aes_ci, data = all_fit_df, alpha = 0.3)
        }
        # if (add_km) {
        #     p <- p +
        #         ggplot2.utils::geom_km(aes(time = .data$t, status = .data$death_num), data = all_obs_df) +
        #         ggplot2.utils::geom_km_ticks(aes(time = .data$t, status = .data$death_num), data = all_obs_df)
        # }
        p
    }
)
