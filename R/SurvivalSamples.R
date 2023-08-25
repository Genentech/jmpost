





# TODO - Document function
# TODO - test function
.SurvivalSamples <- setClass(
    "SurvivalSamples",
    contains = "JointModelSamples"
)

# TODO - Document function
# TODO - test function
setMethod(
    f = "survival",
    signature = "JointModelSamples",
    definition = function(object) {
        .SurvivalSamples(object)
    }
)

# TODO - Document function
# TODO - test function
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

        data <- as.list(object@data)
        patients <- decompose_patients(patients, names(data$pt_to_ind))

        time_grid <- expand_time_grid(time_grid, max(data[["Times"]]))

        gq <- generateQuantities(
            object,
            patients = patients$patients_vec,
            time_grid_lm = numeric(0),
            time_grid_sm = time_grid
        )

        quantities <- extract_survival_quantities(gq, type)

        quantities_summarised <- lapply(
            patients$patients_index,
            summarise_by_group,
            time_grid = time_grid,
            quantities = quantities
        )

        for (i in seq_along(quantities_summarised)) {
            quantities_summarised[[i]][["group"]] <- names(patients$patients_list)[[i]]
            quantities_summarised[[i]][["type"]] <- type
        }
        Reduce(rbind, quantities_summarised)
    }
)



# TODO - document function
# TODO - Test function
decompose_patients <- function(patients, all_pts) {
    if (is.character(patients)) {
        patients <- expand_patients(patients, all_pts)
        names(patients) <- patients
        patients <- as.list(patients)
    }
    patients <- lapply(
        patients,
        expand_patients,
        all_pts = all_pts
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
    list(
        patients_list = patients,
        patients_vec = patients_vec,
        patients_index = patients_index
    )
}


# TODO - Document function
# TODO - test function
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

# TODO - Document function
# TODO - test function
extract_survival_quantities <- function(gq, type = c("surv", "haz", "loghaz", "cumhaz")) {
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
# TODO - Document function
# TODO - test function
setMethod(
    f = "autoplot",
    signature = c(object = "SurvivalSamples"),
    function(
        object,
        patients,
        time_grid = NULL,
        type = c("surv", "haz", "loghaz", "cumhaz"),
        add_km = FALSE,
        add_ci = TRUE,
        add_wrap = TRUE,
        ...
    ) {
        assert_that(is.flag(add_km))
        kmdf <- if (add_km) subset(object@data, patients) else NULL
        type <- match.arg(type)
        all_fit_df <- predict(object, patients, time_grid, type)
        survival_plot(all_fit_df, type, add_ci, add_wrap, kmdf)
    }
)

# TODO - Document function
# TODO - test function
survival_plot <- function(
    dat,
    type = c("surv", "haz", "loghaz", "cumhaz"),
    add_ci = TRUE,
    add_wrap = TRUE,
    kmdf = NULL
) {
    type <- match.arg(type)
    assert_that(
        is.flag(add_ci),
        is.flag(add_wrap)
    )

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
    p <- p + geom_line(aes_line, data = dat)
    if (add_ci) {
        p <- p + geom_ribbon(aes_ci, data = dat, alpha = 0.3)
    }
    if (!is.null(kmdf)) {
        assert_that(
            type == "surv",
            msg = "KM curves are only supported for `type='surv'`"
        )
        p <- p +
            ggplot2.utils::geom_km(aes_km, data = kmdf) +
            ggplot2.utils::geom_km_ticks(aes_km, data = kmdf)
    }
    p
}
