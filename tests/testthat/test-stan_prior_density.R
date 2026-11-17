test_that("Stan density argument splitting handles empty and scalar lists", {
    for (code in c("", " ", "\n\t")) {
        expect_identical(stan_density_arguments(code), character())
    }
    expect_identical(stan_density_arguments("  sigma  "), "sigma")
    expect_identical(
        stan_density_arguments(" mu,\n sigma , -2.5, 1e-3, mu "),
        c("mu", "sigma", "-2.5", "1e-3", "mu")
    )
})


test_that("Stan density arguments retain nested calls and indexing", {
    expect_identical(
        stan_density_arguments(paste(
            "rep_vector(0, K),",
            "scales_horseshoe(local, global, square(scale) * slab)"
        )),
        c(
            "rep_vector(0, K)",
            "scales_horseshoe(local, global, square(scale) * slab)"
        )
    )
    expect_identical(
        stan_density_arguments("mu[index[1, 2], 3], sigma[, cols[2, 3]]"),
        c("mu[index[1, 2], 3]", "sigma[, cols[2, 3]]")
    )
    expect_identical(
        stan_density_arguments("(a + f(b[1, 2], c)), sqrt(1 + square(sigma))"),
        c("(a + f(b[1, 2], c))", "sqrt(1 + square(sigma))")
    )
    expect_identical(
        stan_density_arguments(" [1, 2, 3]',\n mu +  2 * sigma "),
        c("[1, 2, 3]'", "mu +  2 * sigma")
    )
})


test_that("prior conversion handles empty fragments and line comments", {
    expect_identical(stan_prior_density(""), "")
    expect_equal(trimws(stan_prior_density(" \n\t")), "")
    expect_equal(
        trimws(stan_prior_density(
            "// ignored ~ normal(0, 1); target += 100;"
        )),
        ""
    )
    expect_equal(
        stringr::str_squish(stan_prior_density(paste(
            "// first prior",
            "x ~ normal(0, 1); // y ~ normal(3, 4);",
            "z ~ std_normal(); // target += 100;",
            sep = "\n"
        ))),
        "lprior += normal_lpdf(x | 0, 1); lprior += std_normal_lpdf(z);"
    )
})


test_that("prior conversion retains transformed density expressions", {
    cases <- list(
        c("x ~ normal(mu, sigma);", "lprior += normal_lpdf(x | mu, sigma);"),
        c("x ~ std_normal();", "lprior += std_normal_lpdf(x);"),
        c(
            "exp(2 * x) ~ inv_gamma(alpha, beta);",
            "lprior += inv_gamma_lpdf(exp(2 * x) | alpha, beta);"
        ),
        c(
            "x[indices] ~ normal(\n mu[indices],\n sqrt(square(sigma) + 1));",
            paste(
                "lprior += normal_lpdf(x[indices] | mu[indices],",
                "sqrt(square(sigma) + 1));"
            )
        )
    )
    for (case in cases) {
        expect_equal(stringr::str_squish(stan_prior_density(case[1])), case[2])
    }
})


test_that("prior conversion retains explicit densities and Jacobians", {
    code <- paste(
        "x ~ lognormal(mu, sigma);",
        "target += log(2) + x;",
        "target\n  += sum(log(scales));",
        "target += normal_lpdf(y | 0, 1);",
        sep = "\n"
    )
    expect_equal(
        stringr::str_squish(stan_prior_density(code)),
        paste(
            "lprior += lognormal_lpdf(x | mu, sigma);",
            "lprior += log(2) + x;",
            "lprior += sum(log(scales));",
            "lprior += normal_lpdf(y | 0, 1);"
        )
    )
})


test_that("prior conversion preserves local declarations and braced loops", {
    code <- paste(
        "if (K > 0) {",
        "    for (i in 1:K) {",
        "        real value = exp(x[i]);",
        "        value ~ normal(mu, sigma);",
        "        target += x[i];",
        "    }",
        "}",
        sep = "\n"
    )
    expect_equal(
        stringr::str_squish(stan_prior_density(code)),
        paste(
            "if (K > 0) { for (i in 1:K) { real value = exp(x[i]);",
            "lprior += normal_lpdf(value | mu, sigma); lprior += x[i]; } }"
        )
    )
})


test_that("prior truncation uses the correct normalising probability", {
    cases <- list(
        c(
            "x ~ normal(mu, sigma) T[0, ];",
            paste0(
                "normal_lccdf(0 | prior_density_element(mu, prior_j), ",
                "prior_density_element(sigma, prior_j))"
            )
        ),
        c(
            "x ~ normal(mu, sigma) T[ , 2];",
            paste0(
                "normal_lcdf(2 | prior_density_element(mu, prior_j), ",
                "prior_density_element(sigma, prior_j))"
            )
        ),
        c(
            "x ~ normal(mu, sigma) T[-1, 3];",
            paste0(
                "log_diff_exp(normal_lcdf(3 | ",
                "prior_density_element(mu, prior_j), ",
                "prior_density_element(sigma, prior_j)), ",
                "normal_lcdf(-1 | prior_density_element(mu, prior_j), ",
                "prior_density_element(sigma, prior_j)))"
            )
        )
    )
    for (case in cases) {
        expect_equal(
            stringr::str_squish(stan_prior_density(case[1])),
            paste(
                "lprior += normal_lpdf(x | mu, sigma);",
                "if (prior_density_size(x) > 0) {",
                "for (prior_j in 1:prior_density_size(x)) {",
                "lprior -=",
                paste0(case[2], ";"),
                "} }"
            )
        )
    }
    expect_equal(
        stringr::str_squish(stan_prior_density("x~std_normal()T[0,];")),
        paste(
            "lprior += std_normal_lpdf(x);",
            "if (prior_density_size(x) > 0) {",
            "for (prior_j in 1:prior_density_size(x)) {",
            "lprior -= std_normal_lccdf(0); } }"
        )
    )
})


test_that("truncated priors retain nested vector distribution arguments", {
    code <- paste(
        "beta ~ normal(rep_vector(0, K),",
        "scales_horseshoe(local, global, c2)) T[-1, 1];"
    )
    converted <- stringr::str_squish(stan_prior_density(code))
    expect_match(
        converted,
        paste(
            "lprior += normal_lpdf(beta | rep_vector(0, K),",
            "scales_horseshoe(local, global, c2));"
        ),
        fixed = TRUE
    )
    for (bound in c(-1, 1)) {
        expect_match(
            converted,
            paste0(
                "normal_lcdf(",
                bound,
                " | ",
                "prior_density_element(rep_vector(0, K), prior_j), ",
                "prior_density_element(",
                "scales_horseshoe(local, global, c2), prior_j))"
            ),
            fixed = TRUE
        )
    }
    expect_false(grepl("target|~|T\\[", converted))
})


test_that("multiple truncated priors keep their own arguments and bounds", {
    code <- paste(
        "x ~ normal(0, scale_x) T[0, ];",
        "y ~ gamma(shape, rate) T[, 5];",
        "z ~ std_normal();",
        sep = "\n"
    )
    converted <- stringr::str_squish(stan_prior_density(code))
    expect_match(
        converted,
        paste0(
            "normal_lccdf(0 | prior_density_element(0, prior_j), ",
            "prior_density_element(scale_x, prior_j))"
        ),
        fixed = TRUE
    )
    expect_match(
        converted,
        paste0(
            "gamma_lcdf(5 | prior_density_element(shape, prior_j), ",
            "prior_density_element(rate, prior_j))"
        ),
        fixed = TRUE
    )
    expect_match(converted, "lprior += std_normal_lpdf(z);", fixed = TRUE)
    expect_equal(stringr::str_count(converted, stringr::fixed("lprior +=")), 3L)
    expect_equal(stringr::str_count(converted, stringr::fixed("lprior -=")), 2L)
})
