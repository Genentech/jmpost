

test_that("DataSurvival being rendered to list is as expected for simple inputs", {

    x <- data.frame(
        vpt = c("b", "a", "c", "d", "e"),
        vtime = c(10, 20, 30, 25, 15),
        vevent = c(1, 1, 0, 1, 0),
        vcov1 = c("A", "A", "B", "B", "A"),
        vcov2 = rnorm(5)
    )

    df <- DataSurvival(
        data = x,
        formula = Surv(vtime, vevent) ~ vcov1 * vcov2
    )

    res <- as_stan_list(df)
    covmat <- model.matrix(~ vcov1 * vcov2, data = x)
    covmat <- covmat[, -1]
    rownames(covmat) <- NULL

    expect_equal(
        c(
            "n_subject_event", "subject_event_index", "event_times", "p_os_cov_design",
            "os_cov_design", "n_nodes", "nodes", "weights"
        ),
        names(res)
    )
    expect_equal(res$n_subject_event, 3)
    expect_equal(res$p_os_cov_design, 3)
    expect_equal(res$os_cov_design, covmat)
    expect_equal(res$subject_event_index, c(1, 2, 4))
    expect_equal(res$event_times, c(10, 20, 30, 25, 15))


    ## Dropped rows works as expected

    x <- data.frame(
        vpt = c("b", "a", "c", "d", "e"),
        vtime = c(10, 20, 30, 25, 15),
        vevent = c(1, 1, 0, 1, 0),
        vcov1 = c("A", "A", "B", NA, "A"),
        vcov2 = rnorm(5)
    )
    expect_message(
        obj <- DataSurvival(
            data = x,
            formula = Surv(vtime, vevent) ~ vcov1 * vcov2
        ),
        "1 observation(s) were removed"
    )
    expect_equal(
        as.data.frame(obj),
        x |> dplyr::filter(!is.na(vcov1))
    )
    res <- as_stan_list(obj)
    covmat <- model.matrix(~ vcov1 * vcov2, data = x)
    covmat <- covmat[, -1]
    rownames(covmat) <- NULL

    expect_equal(
        c(
            "n_subject_event", "subject_event_index", "event_times", "p_os_cov_design",
            "os_cov_design", "n_nodes", "nodes", "weights"
        ),
        names(res)
    )
    expect_equal(res$n_subject_event, 2)
    expect_equal(res$p_os_cov_design, 3)
    expect_equal(res$os_cov_design, covmat)
    expect_equal(res$subject_event_index, c(1, 2))
    expect_equal(res$event_times, c(10, 20, 30, 15))
})


test_that("DataSurvival print method works as expected", {

    expect_snapshot({
        x <- data.frame(
            vpt = c("b", "a", "c", "d", "e"),
            vtime = c(10, 20, 30, 25, 15),
            vevent = c(1, 1, 0, 1, 0),
            vcov1 = c("A", "A", "B", "B", "A"),
            vcov2 = rnorm(5),
            vcov3 = rnorm(5)
        )

        df <- DataSurvival(
            data = x,
            formula = Surv(vtime, vevent) ~ vcov1 * vcov2 + vcov1:vcov2 + vcov1^2 + vcov2^2
        )
        print(df)
    })

})


test_that("mirror_design_matrix() works as expected", {
    set.seed(3102)
    N <- 50
    beta_trt <- c("A" = 0, "B" = 3)
    beta_sex <- c("M" = 0, "F" = 0.8)
    sample_cat <- function(lvls, n) {
        factor(sample(lvls, size = N, replace = TRUE), levels = lvls)
    }
    dat <- dplyr::tibble(
        trt = sample_cat(c("A", "B"), N),
        sex = sample_cat(c("M", "F"), N),
        covar1 = rnorm(N),
        covar2 = rnorm(N),
        lp =  0.6 * covar1 + -0.4 * covar2 + beta_trt[trt] + beta_sex[sex],
        lambda = 1 / 200 * exp(lp),
        time_real = flexsurv::rweibullPH(N, 0.95, lambda),
        cnsr = rexp(N, 1 / 300),
        event = ifelse(cnsr < time_real, 0, 1),
        time = ifelse(cnsr < time_real, cnsr, time_real)
    )

    x1 <- DataSurvival(
        dat,
        Surv(time, event) ~ trt * sex + covar1 * covar2 + covar1 * sex
    )
    x2 <- DataSurvival(
        dat,
        survival::Surv(time, event) ~ trt * sex + covar1 * covar2 + covar1 * sex
    )
    expect_equal(x1, x2)

    new_data <- dat |>
        slice(5, 10) |>
        mutate(trt = "B", sex = c("F", "M"))

    new_design <- mirror_design_matrix(x2, new_data)
    old_design <- as_stan_list(x2)$os_cov_design

    expect_equal(
        names(new_design),
        names(old_design)
    )
    expect_equal(
        attributes(new_design)$dimnames,
        attributes(old_design)$dimnames
    )

    expected <- matrix(
        c(
            1, 1,                                # trtB
            1, 0,                                # sexF
            new_data$covar1,                     # covar1
            new_data$covar2,                     # covar2
            1, 0,                                # trtB:sexF
            new_data$covar1 * new_data$covar2,   # covar1:covar2
            new_data$covar1 * c(1, 0)            # covar1:sex
        ),
        ncol = 7,
        nrow = 2,
        byrow = FALSE
    )

    dimnames(new_design) <- NULL
    expect_equal(
        new_design,
        expected
    )

    ###########
    #
    # Error handling
    #

    # New factor level
    new_data <- dat |>
        slice(5, 10) |>
        mutate(trt = "C", sex = c("F", "M"))

    expect_error(
        mirror_design_matrix(x, new_data),
        regex = "trt has new level C"
    )

    # Missing variable
    new_data <- dat |>
        slice(5, 10) |>
        mutate(trt = "B", sex = c("F", "M")) |>
        select(-covar1)

    expect_error(
        mirror_design_matrix(x, new_data),
        regex = "'covar1' not found"
    )

    # Change of data type
    new_data <- dat |>
        slice(5, 10) |>
        mutate(trt = "B", sex = c("F", "M")) |>
        mutate(covar1 = c("A"))

    expect_error(
        mirror_design_matrix(x, new_data),
        regex = "'covar1' was fitted with type \"numeric\" but type \"character\""
    )
})
