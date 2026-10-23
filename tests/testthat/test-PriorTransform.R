test_that("prior transformations require known kinds and positive scales", {
    expect_s4_class(prior_transform_exp(), "PriorTransform")
    expect_s4_class(prior_transform_square(), "PriorTransform")
    for (scale in list(0, -1, Inf, NA_real_, numeric(), c(1, 2))) {
        expect_error(prior_transform_exp(scale), "finite positive number")
    }
    expect_error(prior_transform_exp(exp))
    expect_error(.PriorTransform(kind = "custom", scale = 1), "Unknown")
})


test_that("transformation inverses and bounds use the intended branch", {
    x <- c(-2, 0, 3)
    for (scale in c(1, 2)) {
        transform <- prior_transform_exp(scale)
        expect_equal(
            .prior_transform_inverse(
                transform, .prior_transform_forward(transform, x)
            ),
            x
        )
        expect_equal(.prior_transform_domain(transform), c(-Inf, Inf))
        expect_equal(
            .prior_transform_forward(transform, c(-Inf, Inf)), c(0, Inf)
        )
        expect_equal(
            .prior_transform_inverse(transform, c(0, Inf)), c(-Inf, Inf)
        )
        expect_error(.prior_transform_inverse(transform, -1), "non-negative")
    }

    transform <- prior_transform_square()
    expect_equal(.prior_transform_domain(transform), c(0, Inf))
    expect_equal(
        .prior_transform_forward(transform, c(0, 2, Inf)), c(0, 4, Inf)
    )
    expect_equal(
        .prior_transform_inverse(transform, c(0, 4, Inf)), c(0, 2, Inf)
    )
    expect_error(.prior_transform_forward(transform, -2), "non-negative")
    expect_error(.prior_transform_inverse(transform, -4), "non-negative")
})


test_that("Stan expressions include the complete change-of-variable Jacobian", {
    cases <- list(
        list(prior_transform_exp(), "exp(x)", "x"),
        list(prior_transform_exp(2), "exp(2 * (x))", "log(2) + 2 * (x)"),
        list(prior_transform_square(), "square(x)", "log(2) + log(x)")
    )
    for (case in cases) {
        code <- .prior_transform_stan(case[[1]], "x")
        expect_equal(code, list(value = case[[2]], log_jacobian = case[[3]]))

        # Compare the emitted log Jacobian with an independent numerical
        # derivative of the transformation on its valid domain.
        x <- 0.7
        h <- 1e-6
        derivative <- (
            .prior_transform_forward(case[[1]], x + h) -
                .prior_transform_forward(case[[1]], x - h)
        ) / (2 * h)
        log_jacobian <- eval(parse(text = code$log_jacobian))
        expect_equal(log_jacobian, log(abs(derivative)), tolerance = 1e-8)
    }
})


test_that("scalar helper and wrapper transformations are equivalent", {
    helpers <- list(
        \(transform) prior_normal(60, 60, transform),
        \(transform) prior_std_normal(transform),
        \(transform) prior_cauchy(0, 1, transform),
        \(transform) prior_gamma(2, 1, transform),
        \(transform) prior_lognormal(0, 1, transform),
        \(transform) prior_beta(2, 3, transform),
        \(transform) prior_uniform(-1, 2, transform),
        \(transform) prior_student_t(4, 0, 1, transform),
        \(transform) prior_logistic(0, 1, transform),
        \(transform) prior_loglogistic(2, 3, transform),
        \(transform) prior_invgamma(1.5, 0.3, transform)
    )
    for (helper in helpers) {
        transforms <- list(prior_transform_exp(), prior_transform_square())
        for (transform in transforms) {
            direct <- helper(transform)
            wrapped <- prior_transform(helper(NULL), transform)
            expect_s4_class(direct, "Prior")
            expect_equal(
                as.StanModule(direct, name = "x"),
                as.StanModule(wrapped, name = "x")
            )
            expect_equal(
                as_stan_list(direct, name = "x"),
                as_stan_list(helper(NULL), name = "x")
            )
            expect_true(is.finite(initialValues(direct)))
        }
    }
    for (dist in list(
        prior_flat(), prior_const(1), prior_init_only(prior_normal(0, 1)),
        prior_horseshoe(), prior_normal_vector(c(0, 1), c(1, 2)),
        prior_normal(1, 1, transform = prior_transform_exp())
    )) {
        expect_error(
            prior_transform(dist, prior_transform_exp()),
            "supported, untransformed scalar"
        )
    }
    expect_error(prior_transform(1, prior_transform_exp()), "Prior object")
    expect_error(prior_normal(0, 1, transform = exp), "package-defined")
})


test_that("density support and truncation are mapped between scales", {
    x <- prior_normal(60, 60, transform = prior_transform_exp())
    expect_equal(x@limits, c(-Inf, Inf))
    code <- as.StanModule(x, name = "x")@model
    expect_match(code, "exp(x) ~ normal(prior_mu_x, prior_sigma_x) T[0, ];",
        fixed = TRUE
    )
    expect_match(code, "target += x;", fixed = TRUE)
    expect_match(as.character(x), "on exp(x)", fixed = TRUE)

    x <- prior_invgamma(1.5, 0.3, transform = prior_transform_exp(2))
    code <- as.StanModule(x, name = "x")@model
    expect_match(code, "exp(2 * (x)) ~ inv_gamma(", fixed = TRUE)
    expect_match(code, "target += log(2) + 2 * (x);", fixed = TRUE)
    expect_false(grepl("T[", code, fixed = TRUE))

    x <- set_limits(
        prior_normal(100, 100, transform = prior_transform_square()),
        lower = 1e-10
    )
    expect_equal(x@limits, c(1e-10, Inf))
    code <- as.StanModule(Parameter(x, "sigma"))
    expect_match(code@parameters, "real<lower=1e-10> sigma;", fixed = TRUE)
    model <- paste(code@model, collapse = "\n")
    expect_match(model, "T[1e-20, ];", fixed = TRUE)
    expect_match(model, "target += log(2) + log(sigma);", fixed = TRUE)

    x <- prior_transform(
        set_limits(prior_normal(0, 1), lower = 1, upper = 4),
        prior_transform_square()
    )
    expect_equal(x@limits, c(1, 2))
    x <- set_limits(x, lower = 1.5, upper = 10)
    expect_equal(x@limits, c(1.5, 2))
    expect_match(
        as.StanModule(x, name = "x")@model, "T[2.25, 4];", fixed = TRUE
    )

    x <- prior_beta(2, 3, transform = prior_transform_exp())
    expect_equal(x@limits, c(-Inf, 0))
    expect_equal(set_limits(x, upper = 2)@limits, c(-Inf, 0))
    expect_equal(
        prior_uniform(-1, 4, transform = prior_transform_square())@limits,
        c(0, 2)
    )
    expect_error(
        prior_uniform(-2, -1, transform = prior_transform_exp()),
        "does not overlap"
    )
    expect_error(set_limits(x, lower = 1), "Lower limit")
    # A Cauchy density requires its positive-range truncation normaliser too.
    expect_match(
        as.StanModule(
            prior_cauchy(0, 1, transform = prior_transform_exp()), name = "x"
        )@model,
        "T[0, ];", fixed = TRUE
    )
})


test_that("initialization inverse-transforms valid draws before shrinkage", {
    withr::local_options(list(jmpost.prior_shrinkage = 0.5))
    with_mocked_bindings(
        {
            x <- prior_normal(60, 60, transform = prior_transform_exp())
            expect_equal(initialValues(x), (log(60) + 4) / 2)
            expect_true(all(is.finite(x@sample(200))))
        },
        local_rnorm = \(n, ...) rep(c(-1, 0, exp(4)), length.out = n)
    )
    with_mocked_bindings(
        {
            x <- prior_normal(100, 100, transform = prior_transform_square())
            expect_equal(initialValues(x), (10 + 6) / 2)
            x <- set_limits(x, lower = 5, upper = 7)
            expect_equal(initialValues(x), 6)
            # Initialization-only and flat wrappers use the model-scale draws.
            expect_equal(initialValues(prior_init_only(x)), 6)
            expect_equal(initialValues(prior_flat(init = x)), 6)
        },
        local_rnorm = \(n, ...) rep(36, n)
    )
    for (prior in list(
        prior_std_normal(transform = prior_transform_exp()),
        prior_normal(-1, 2, transform = prior_transform_square()),
        prior_invgamma(0.5, 1, transform = prior_transform_exp(2))
    )) {
        expect_true(is.finite(prior@centre))
        expect_true(is.finite(initialValues(prior)))
    }
    with_mocked_bindings(
        expect_error(
            initialValues(prior_std_normal(transform = prior_transform_exp())),
            "Unable to generate initial values"
        ),
        local_rnorm = \(n, ...) rep(-1, n)
    )
})


test_that("transformed priors render scalar and vector Stan models", {
    priors <- list(
        prior_normal(60, 60, transform = prior_transform_exp()),
        prior_invgamma(1.5, 0.3, transform = prior_transform_exp(2)),
        prior_normal(100, 100, transform = prior_transform_square()),
        prior_std_normal(transform = prior_transform_exp()),
        prior_beta(2, 3, transform = prior_transform_exp())
    )
    for (prior in priors) {
        code <- as.StanModule(Parameter(prior, "theta"))
        expect_stan_syntax(code)
        code <- merge(
            StanModule("data {\n    int<lower=0> n;\n}"),
            as.StanModule(Parameter(prior, "theta", size = "n"))
        )
        model <- paste(code@model, collapse = "\n")
        expect_match(model, "if (n > 0)", fixed = TRUE)
        expect_match(model, "theta[prior_i]", fixed = TRUE)
        expect_stan_syntax(code)
    }
})


test_that("the PRIME prior specification renders and initializes", {
    model <- JointModel(longitudinal = LongitudinalSteinFojoCov(
        mu_b_intercept_prior = prior_normal(
            60, 60, transform = prior_transform_exp()
        ),
        mu_s_intercept_prior = prior_normal(
            0.50091, 0.50091, transform = prior_transform_exp()
        ),
        mu_g_intercept_prior = prior_normal(
            0.3, 0.3, transform = prior_transform_exp()
        ),
        omega_b_intercept_prior = prior_invgamma(
            1.5, 0.3, transform = prior_transform_exp(2)
        ),
        omega_s_intercept_prior = prior_invgamma(
            1.5, 0.3, transform = prior_transform_exp(2)
        ),
        omega_g_intercept_prior = prior_invgamma(
            1.5, 0.3, transform = prior_transform_exp(2)
        ),
        mu_b_coefficients_prior = prior_flat(),
        mu_s_coefficients_prior = prior_flat(),
        mu_g_coefficients_prior = prior_flat(),
        sigma = prior_normal(100, 100, transform = prior_transform_square()),
        centred_baseline = TRUE,
        scaled_variance = FALSE
    ))
    expect_stan_syntax(as.StanModule(model))
    initial <- initialValues(model@longitudinal@parameters, n_chains = 2)
    expect_length(initial, 2)
    expect_true(all(is.finite(unlist(initial))))
})


test_that("compiled transformed densities match independent calculations", {
    skip_if_not(is_full_test())
    parameters <- ParameterList(
        Parameter(
            prior_normal(2, 1, transform = prior_transform_exp()), "intercept"
        ),
        Parameter(
            prior_invgamma(1.5, 0.3, transform = prior_transform_exp(2)),
            "log_omega"
        ),
        Parameter(
            set_limits(
                prior_normal(2, 2, transform = prior_transform_square()),
                lower = 0.2, upper = 3
            ),
            "sigma", size = 2
        ),
        Parameter(
            prior_normal(0, 1, transform = prior_transform_exp()),
            "empty", size = "n"
        )
    )
    module <- merge(
        StanModule("data {\n    int<lower=0> n;\n}"),
        as.StanModule(parameters)
    )
    # Model methods need the compiled source, not only a cached executable.
    model <- cmdstanr::cmdstan_model(
        cmdstanr::write_stan_file(as.character(module)), quiet = TRUE
    )
    baseline <- list(
        intercept = log(2), log_omega = log(0.2),
        sigma = c(1, 1.5), empty = numeric()
    )
    suppressMessages(capture.output(
        fit <- model$sample(
            data = c(list(n = 0L), as_stan_list(parameters)),
            init = list(baseline), fixed_param = TRUE,
            iter_warmup = 0, iter_sampling = 1, chains = 1,
            seed = 12345, refresh = 0
        )
    ))
    expected <- function(values) {
        variance <- exp(2 * values$log_omega)
        dnorm(exp(values$intercept), 2, 1, log = TRUE) +
            values$intercept +
            dgamma(1 / variance, 1.5, rate = 0.3, log = TRUE) -
            2 * log(variance) + log(2) + 2 * values$log_omega +
            sum(dnorm(values$sigma^2, 2, 2, log = TRUE) +
                log(2) + log(values$sigma))
    }
    log_density <- function(values) {
        fit$log_prob(fit$unconstrain_variables(values), jacobian = FALSE)
    }
    # Compare differences because Stan may drop constant density terms.
    for (values in list(
        list(intercept = -0.5, log_omega = -1, sigma = c(0.4, 2.5)),
        list(intercept = 1, log_omega = 0.3, sigma = c(2, 0.7))
    )) {
        values$empty <- numeric()
        expect_equal(
            log_density(values) - log_density(baseline),
            expected(values) - expected(baseline),
            tolerance = 1e-8
        )
    }
})
