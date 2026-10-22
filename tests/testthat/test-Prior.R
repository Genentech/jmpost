test_that("Priors work as expected", {
    x <- prior_normal(4, 10)
    with_mocked_bindings(
        expect_equal(
            initialValues(x),
            4 * 0.5
        ),
        local_rnorm = \(n, ...) rep(0, n)
    )
    expect_equal(
        as.StanModule(x, name = "bob"),
        StanModule(test_path("models", "Prior_1.stan"))
    )
    expect_equal(
        as_stan_list(x, name = "bob"),
        list(prior_mu_bob = 4, prior_sigma_bob = 10)
    )

    x <- prior_lognormal(log(4), 2)
    with_mocked_bindings(
        expect_equal(
            initialValues(x),
            exp(log(4) + 2) * 0.5
        ),
        local_rlnorm = \(n, ...) rep(0, n)
    )
    expect_equal(
        as.StanModule(x, name = "tim"),
        StanModule(test_path("models", "Prior_2.stan"))
    )
    expect_equal(
        as_stan_list(x, name = "tim"),
        list(prior_mu_tim = log(4), prior_sigma_tim = 2)
    )

    tom <- prior_logistic(1, 2)
    dave <- prior_loglogistic(3, 4)
    jim <- prior_invgamma(5, 6)
    ben <- prior_student_t(7, 8, 9)
    kim <- prior_uniform(10, 11)

    header <- StanModule(
        "parameters {
    real tom;
    real dave;
    real jim;
    real ben;
    real kim;
}"
    )
    tom_sm <- as.StanModule(tom, name = "tom")
    dave_sm <- as.StanModule(dave, name = "dave")
    jim_sm <- as.StanModule(jim, name = "jim")
    ben_sm <- as.StanModule(ben, name = "ben")
    kim_sm <- as.StanModule(kim, name = "kim")

    full_sm <- list(header, tom_sm, dave_sm, jim_sm, ben_sm, kim_sm) |>
        Reduce(f = merge)
    expect_equal(
        full_sm,
        StanModule(test_path("models", "Prior_3.stan"))
    )

    ## Check that the model syntax is correct (e.g. that we have
    ## correctly specified the stan prior distribution function names)
    model_obj <- cmdstanr::cmdstan_model(
        test_path("models", "Prior_3.stan"),
        compile = FALSE
    )
    expect_true(model_obj$check_syntax(quiet = TRUE))
})


test_that("Invalid prior parameters are rejected", {
    expect_error(
        prior_normal(0, -1),
        regexp = "Invalid.*`sigma`"
    )

    expect_error(
        prior_beta(-3, 1),
        regexp = "Invalid.*`a`"
    )

    expect_error(
        prior_beta(5, -1),
        regexp = "Invalid.*`b`"
    )

    expect_error(
        prior_gamma(5, -1),
        regexp = "Invalid.*`beta`"
    )

    expect_error(
        prior_lognormal(5, -1),
        regexp = "Invalid.*`sigma`"
    )

    expect_error(
        prior_logistic(5, -1),
        regexp = "Invalid.*`sigma`"
    )

    expect_error(
        prior_loglogistic(5, -1),
        regexp = "Invalid.*`beta`"
    )

    expect_error(
        prior_loglogistic(-1, 6),
        regexp = "Invalid.*`alpha`"
    )

    expect_error(
        prior_student_t(-1, 6, 2),
        regexp = "Invalid.*`nu`"
    )
    expect_error(
        prior_student_t(1, 6, -2),
        regexp = "Invalid.*`sigma`"
    )

    expect_error(
        prior_invgamma(alpha = -1, beta = 2),
        regexp = "Invalid.*`alpha`"
    )
    expect_error(
        prior_invgamma(alpha = 1, beta = -2),
        regexp = "Invalid.*`beta`"
    )

    expect_error(
        prior_uniform(10, 9),
        regexp = "`alpha`` must be less than `beta`"
    )

    # Ensure that validation doesn't wrongly reject priors with no user specified parameters
    expect_s4_class(prior_init_only(prior_normal(3, 1)), "Prior")
    expect_s4_class(prior_std_normal(), "Prior")
    expect_s4_class(prior_const(1), "Prior")
})

test_that("prior_const() works as expected", {
    x <- prior_const(1)

    expect_equal(initialValues(x), 1)
    expect_equal(as.character(set_limits(x, lower = 0)), "const(value = 1)")
    expect_equal(
        as_stan_list(x, name = "sm_exp_lambda"),
        list(prior_const_sm_exp_lambda = 1)
    )
    expect_equal(
        as.StanModule(x, name = "sm_exp_lambda")@data,
        "    real prior_const_sm_exp_lambda;"
    )
    expect_equal(as.StanModule(x, name = "sm_exp_lambda")@model, "")
    expect_equal(
        as.StanModule(x, name = "bob")@data,
        "    real prior_const_bob;"
    )

    pars <- ParameterList(
        Parameter(name = "fixed", prior = x, size = 1),
        Parameter(name = "sampled", prior = prior_normal(0, 1), size = 1)
    )
    expect_equal(names(initialValues(pars, n_chains = 1)[[1]]), "sampled")
    expect_equal(
        as_stan_list(pars),
        list(
            prior_const_fixed = 1,
            prior_mu_sampled = 0,
            prior_sigma_sampled = 1
        )
    )
})

test_that("prior_const_vector() works as expected", {
    x <- prior_const_vector(c(1, 2, 3))

    expect_equal(
        as.character(x),
        "const(values = [1, 2, 3])"
    )
    expect_equal(initialValues(x), c(1, 2, 3))
    expect_equal(
        as_stan_list(x, name = "bob"),
        list(prior_const_bob = c(1, 2, 3))
    )
    expect_equal(
        as.StanModule(x, name = "bob", size = 3)@data,
        "    vector[3] prior_const_bob;"
    )
    expect_equal(as.StanModule(x, name = "bob", size = 3)@model, "")

    expect_error(
        prior_const_vector(numeric()),
        "`values` must be a non-empty numeric vector",
        fixed = TRUE
    )
    expect_error(
        prior_const_vector("not numeric"),
        "`values` must be a non-empty numeric vector",
        fixed = TRUE
    )
})

test_that("show() works for Prior objects", {
    expect_snapshot(print(prior_cauchy(0, 0.8)))
    expect_snapshot(print(prior_const(1)))
    expect_snapshot(print(prior_normal(0, 0.8)))
    expect_snapshot(print(prior_std_normal()))
    expect_snapshot(print(prior_beta(5, 1)))
    expect_snapshot(print(prior_gamma(2.56, 12)))
    expect_snapshot(print(prior_init_only(prior_normal(1, 4))))
    expect_snapshot(print(prior_uniform(8, 10)))
    expect_snapshot(print(prior_student_t(3, 10, 4)))
    expect_snapshot(print(prior_logistic(sigma = 2, 10)))
    expect_snapshot(print(prior_loglogistic(1, 2)))
    expect_snapshot(print(prior_invgamma(alpha = 1, beta = 2)))
})


test_that("jmpost.prior_shrinkage works as expected", {
    x <- prior_normal(1, 2)
    with_mocked_bindings(
        {
            options("jmpost.prior_shrinkage" = 0.5)
            expect_equal(
                initialValues(x),
                1 * 0.5 + 4 * 0.5
            )

            options("jmpost.prior_shrinkage" = 0.9)
            expect_equal(
                initialValues(x),
                1 * 0.9 + 4 * 0.1
            )

            options("jmpost.prior_shrinkage" = 0.1)
            expect_equal(
                initialValues(x),
                1 * 0.1 + 4 * 0.9
            )

            ## Reset Shrinkage factor
            options("jmpost.prior_shrinkage" = 0.5)
        },
        local_rnorm = \(n, ...) rep(4, n)
    )
})


test_that("Limits work as expected", {
    x <- prior_normal(0, 1)
    x <- set_limits(x, lower = 0, upper = 1)
    ivs <- replicate(
        n = 100,
        initialValues(x)
    )
    expect_true(all(ivs > 0))
    expect_true(all(ivs < 1))

    expect_equal(
        as.StanModule(x, name = "bob")@model,
        "    bob ~ normal(prior_mu_bob, prior_sigma_bob) T[0, 1];"
    )

    x <- prior_cauchy(-200, 150)
    x <- set_limits(x, lower = 0)
    ivs <- replicate(
        n = 100,
        initialValues(x)
    )
    expect_true(all(ivs > 0))
    expect_equal(
        as.StanModule(x, name = "tim")@model,
        "    tim ~ cauchy(prior_mu_tim, prior_sigma_tim);"
    )

    ## Put an impossible constraint on the distribution
    x <- prior_lognormal(0, 1)
    x <- set_limits(x, upper = 0)
    expect_error(initialValues(x), regex = "Unable to generate")
    expect_equal(
        as.StanModule(x, name = "phil")@model,
        "    phil ~ lognormal(prior_mu_phil, prior_sigma_phil) T[, 0];"
    )
})


test_that("redundant positive-support prior truncation is omitted", {
    positive_priors <- list(
        cauchy = prior_cauchy(0, 1),
        gamma = prior_gamma(2, 1),
        invgamma = prior_invgamma(2, 1),
        loglogistic = prior_loglogistic(2, 3),
        lognormal = prior_lognormal(0, 1)
    )

    for (prior in positive_priors) {
        x <- set_limits(prior, lower = 0)
        expect_false(grepl("T\\[0, \\]", as.character(x)))
        expect_false(grepl(
            "T\\[0, \\]",
            as.StanModule(x, name = "theta")@model
        ))

        x <- set_limits(prior, lower = 1e-8)
        expect_true(grepl("T\\[1e-08, \\]", as.character(x)))
        expect_true(grepl(
            "T\\[1e-08, \\]",
            as.StanModule(x, name = "theta")@model
        ))

        x <- set_limits(prior, lower = 0, upper = 1)
        expect_true(grepl("T\\[0, 1\\]", as.character(x)))
        expect_true(grepl(
            "T\\[0, 1\\]",
            as.StanModule(x, name = "theta")@model
        ))
    }

    x <- set_limits(prior_normal(0, 1), lower = 0)
    expect_true(grepl("T\\[0, \\]", as.character(x)))
    expect_true(grepl("T\\[0, \\]", as.StanModule(x, name = "theta")@model))
})


test_that("median(Prior) works as expected", {
    set.seed(2410)

    # Unrestricted
    p1 <- prior_normal(-200, 400)
    expect_equal(
        median(p1),
        -200,
        tolerance = 0.15
    )

    # Constrained
    p2 <- set_limits(p1, lower = 0)

    actual <- rnorm(6000, -200, 400) * 0.5 + -200 * 0.5
    actual_red <- actual[actual >= 0]

    expect_equal(
        median(p2),
        median(actual_red),
        tolerance = 0.15
    )
})


test_that("Parameters in priors must be length 1 #422", {
    expect_error(
        prior_normal(c(1, 2), 1),
        "Parameter `mu`"
    )

    expect_error(
        prior_normal(1, c(1, 2)),
        "Parameter `sigma`"
    )

    expect_error(
        prior_normal(c(1, 2), c(1, 2)),
        "Parameter `mu`"
    )

    expect_error(
        prior_gamma(c(1, 2), 2),
        "Parameter `alpha`"
    )

    expect_error(
        prior_horseshoe(df = -1),
        regexp = "Invalid.*`df`"
    )

    expect_error(
        prior_horseshoe(scale_global = -1),
        regexp = "Invalid.*`scale_global`"
    )
})

test_that("prior_normal_vector works as expected", {
    x <- prior_normal_vector(c(1, 2, 3), c(4, 5, 6))
    x_char <- as.character(x)

    expect_equal(
        x_char,
        "normal(mus = [1, 2, 3], sigmas = [4, 5, 6])"
    )

    x_inits <- initialValues(x)
    expect_numeric(x_inits, len = 3)

    x_stan_module <- as.StanModule(x, name = "bob")
    expect_equal(
        x_stan_module,
        StanModule(test_path("models", "Prior_4.stan"))
    )
    expect_equal(
        as_stan_list(x, name = "bob"),
        list(
            prior_mus_bob = c(1, 2, 3),
            prior_sigmas_bob = c(4, 5, 6),
            prior_dim_mus_bob = 3,
            prior_dim_sigmas_bob = 3
        )
    )
})

test_that("prior_horseshoe works as expected", {
    x <- prior_horseshoe(
        df = 1,
        df_global = 2,
        df_slab = 3,
        scale_global = 0.4,
        scale_slab = 2
    )

    expect_equal(
        as.character(x),
        paste0(
            "horseshoe(df = 1, df_global = 2, df_slab = 3, ",
            "scale_global = 0.4, scale_slab = 2)"
        )
    )

    x_inits <- initialValues(x)
    expect_numeric(x_inits, len = 1)

    auxiliary_inits <- auxiliaryInitialValues(x, name = "beta", size = 3)
    expect_equal(
        names(auxiliary_inits),
        c("prior_local_beta", "prior_global_beta", "prior_slab_beta")
    )
    expect_numeric(auxiliary_inits$prior_local_beta, len = 3, lower = 0)
    expect_numeric(auxiliary_inits$prior_global_beta, len = 1, lower = 0)
    expect_numeric(auxiliary_inits$prior_slab_beta, len = 1, lower = 0)

    expect_equal(
        auxiliarySize(x, name = "beta", size = "p"),
        list(
            prior_local_beta = "p",
            prior_global_beta = 1,
            prior_slab_beta = 1
        )
    )

    x_stan_module <- as.StanModule(x, name = "beta", size = "p")
    expect_equal(
        x_stan_module@data,
        c(
            "    real<lower=0> prior_df_beta;",
            "    real<lower=0> prior_df_global_beta;",
            "    real<lower=0> prior_df_slab_beta;",
            "    real<lower=0> prior_scale_global_beta;",
            "    real<lower=0> prior_scale_slab_beta;"
        )
    )
    expect_equal(
        x_stan_module@parameters,
        c(
            "    vector<lower=0>[p] prior_local_beta;",
            "    real<lower=0> prior_global_beta;",
            "    real<lower=0> prior_slab_beta;"
        )
    )
    expect_true(
        any(grepl(
            "vector<lower=0>\\[p\\] prior_scales_beta",
            x_stan_module@transformed_parameters
        ))
    )
    expect_true(
        any(grepl(
            "vector<lower=0, upper=1>\\[p\\] prior_shrinkage_factors_beta",
            x_stan_module@generated_quantities
        ))
    )
    expect_true(
        any(grepl(
            "shrinkage_horseshoe(prior_local_beta, prior_global_beta, prior_c2_beta)",
            x_stan_module@generated_quantities,
            fixed = TRUE
        ))
    )
    expect_true(
        any(grepl(
            "beta ~ normal(rep_vector(0, p), prior_scales_beta);",
            x_stan_module@model,
            fixed = TRUE
        ))
    )
    expect_equal(
        as_stan_list(x, name = "beta"),
        list(
            prior_df_beta = 1,
            prior_df_global_beta = 2,
            prior_df_slab_beta = 3,
            prior_scale_global_beta = 0.4,
            prior_scale_slab_beta = 2
        )
    )

    # Check the model syntax.
    header <- merge(
        StanModule("base/functions.stan"),
        StanModule(
            "data {
    int<lower=1> p;
}
parameters {
    vector[p] beta;
}"
        )
    )
    stan_file <- cmdstanr::write_stan_file(
        as.character(merge(header, x_stan_module)),
        dir = tempdir()
    )
    model_obj <- cmdstanr::cmdstan_model(stan_file, compile = FALSE)
    expect_true(model_obj$check_syntax(quiet = TRUE))
})

test_that("default auxiliary prior methods return empty lists", {
    x <- prior_normal(0, 1)

    expect_equal(
        auxiliaryInitialValues(x, name = "beta", size = 3),
        list()
    )
    expect_equal(
        auxiliarySize(x, name = "beta", size = 3),
        list()
    )
})

test_that("horseshoe computational scales must be positive", {
    for (scale in list(0, -1, -Inf, NA_real_, "small")) {
        expect_error(prior_horseshoe(scale_noncentred = scale))
    }
    expect_s4_class(prior_horseshoe(scale_noncentred = Inf), "Prior")
    expect_s4_class(prior_horseshoe(scale_noncentred = 0.1), "Prior")
})

test_that("horseshoe sampling coordinates have a standard-normal density", {
    skip_if_not(is_full_test())

    # Check the compiled density, including Stan's affine Jacobian. Merely
    # checking the rendered declaration would miss a mismatched scale or an
    # incorrect dependence between raw coefficients and the scale parameters.
    parameter <- Parameter(name = "beta", prior = prior_horseshoe(), size = 3)
    module <- merge(
        StanModule("base/functions.stan"),
        as.StanModule(parameter)
    )
    model <- compileStanModel(module)
    z <- c(-0.7, 0.2, 1.1)

    for (tau in c(0.01, 0.8)) {
        lambda <- c(0.1, 1, 5)
        c2 <- 4 * 1.3
        scales <- tau * sqrt(c2 * lambda^2 / (c2 + tau^2 * lambda^2))
        init <- list(
            prior_local_beta = lambda,
            prior_global_beta = tau,
            prior_slab_beta = 1.3,
            beta = scales * z
        )
        fit <- model$sample(
            data = as_stan_list(parameter),
            init = list(init),
            fixed_param = TRUE,
            iter_warmup = 0,
            iter_sampling = 1,
            chains = 1,
            seed = 12345,
            refresh = 0
        )
        unconstrained <- fit$unconstrain_variables(init)
        expect_equal(tail(unconstrained, 3), z, ignore_attr = TRUE)

        at_zero <- unconstrained
        at_zero[6:8] <- 0
        expect_equal(
            fit$log_prob(unconstrained) - fit$log_prob(at_zero),
            -sum(z^2) / 2
        )
        gradient <- fit$grad_log_prob(unconstrained)
        expect_equal(tail(gradient, 3), -z, ignore_attr = TRUE)
        expect_equal(
            head(gradient, 5),
            head(fit$grad_log_prob(at_zero), 5)
        )
    }
})

test_that("scale_noncentred preserves the horseshoe prior including Jacobians", {
    skip_if_not(is_full_test())
    # CmdStanR's model methods need the C++ source from a fresh compilation,
    # rather than an executable reused from the package cache.
    withr::local_options(list(jmpost.cache_dir = withr::local_tempdir()))

    computational_scales <- c(centred = NA, full = Inf, small = 0.01, cap = 0.1)
    fits <- list()
    for (name in names(computational_scales)) {
        scale <- computational_scales[[name]]
        prior <- prior_horseshoe(
            scale_noncentred = if (is.na(scale)) Inf else scale
        )
        if (is.na(scale)) {
            # Independent reference: the original centred horseshoe, with an
            # ordinary beta declaration and no affine transform.
            prior@repr_multiplier <- \(name, size) ""
        }
        parameter <- Parameter(
            name = "beta", size = 3, prior = prior
        )
        model <- compileStanModel(merge(
            StanModule("base/functions.stan"), as.StanModule(parameter)
        ))
        fits[[name]] <- model$sample(
            data = as_stan_list(parameter),
            init = list(list(
                prior_local_beta = c(0.1, 1, 5), prior_global_beta = 0.2,
                prior_slab_beta = 1.3, beta = c(0.01, -0.08, 0.4)
            )),
            chains = 1, iter_warmup = 0, iter_sampling = 1,
            fixed_param = TRUE, seed = 12345, refresh = 0
        )
    }
    for (tau in c(0.005, 0.2, 2)) {
        values <- list(
            prior_local_beta = c(0.1, 1, 5), prior_global_beta = tau,
            prior_slab_beta = 1.3, beta = c(0.01, -0.08, 0.4)
        )
        scales <- tau * sqrt(5.2 * values$prior_local_beta^2 /
            (5.2 + tau^2 * values$prior_local_beta^2))
        reference <- fits$centred
        reference_u <- reference$unconstrain_variables(values)
        reference_density <- reference$log_prob(reference_u, jacobian = FALSE)
        reference_target <- reference$log_prob(reference_u, jacobian = TRUE)

        for (name in names(computational_scales)[-1]) {
            scale <- computational_scales[[name]]
            multiplier <- scales / sqrt(1 + (scales / scale)^2)
            fit <- fits[[name]]
            unconstrained <- fit$unconstrain_variables(values)
            context <- paste("scale_noncentred =", scale, "; tau =", tau)

            # All parameterizations must give the same joint prior density at
            # the same physical coefficients AND auxiliary shrinkage scales.
            expect_equal(
                fit$log_prob(unconstrained, jacobian = FALSE),
                reference_density, info = context
            )
            # Sampling uses the density in raw coordinates. Its difference
            # from the centred target must be precisely log |d beta / d raw|.
            # The auxiliary log-transform Jacobians cancel in this comparison.
            # Thus p(raw, scales) d raw = p(beta, scales) d beta, so integrating
            # out the scales also leaves the marginal horseshoe prior intact.
            expect_equal(
                fit$log_prob(unconstrained, jacobian = TRUE) -
                    reference_target,
                sum(log(multiplier)), info = context
            )
            expect_equal(
                tail(unconstrained, 3), values$beta / multiplier,
                ignore_attr = TRUE, info = context
            )
            expect_equal(
                tail(fit$grad_log_prob(unconstrained), 3),
                -values$beta * multiplier / scales^2,
                ignore_attr = TRUE, info = context
            )
        }
    }
})
