# Usage:
# Rscript benchmark.R prepare <repository> <main|affine> <scratch-directory>
# Rscript benchmark.R run <scratch-directory> <results-directory>
# Keep generated Stan, executables and posterior draws in scratch only.
args <- commandArgs(trailingOnly = TRUE)
mode <- args[[1]]

if (mode == "prepare") {
    repo <- args[[2]]
    variant <- args[[3]]
    scratch <- args[[4]]
    # read_stan() checks cwd/inst before package resources.
    setwd(repo)
    pkgload::load_all(repo, helpers = FALSE, quiet = TRUE)
    dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
    n <- 60L
    mu <- c(bsld = log(60), ks = log(0.45), kg = log(0.2))
    omega <- c(bsld = 0.2, ks = 0.25, kg = 0.2)
    set.seed(6143)
    latent <- sapply(names(mu), function(effect) {
        exp(rnorm(n, mu[[effect]], omega[[effect]]))
    })
    subjects <- data.frame(
        subject = sprintf("S%03d", seq_len(n)), arm = "A", study = "X"
    )
    for (scenario in c("sparse", "dense")) {
        times <- if (scenario == "sparse") c(0, 0.5, 1, 2, 3) else {
            c(0, 0.25, 0.5, 1, 1.5, 2, 2.5, 3)
        }
        subject_index <- rep(seq_len(n), each = length(times))
        time <- rep(times, n)
        mean_sld <- latent[subject_index, "bsld"] * (
            exp(-latent[subject_index, "ks"] * time) +
                exp(latent[subject_index, "kg"] * time) - 1
        )
        set.seed(7143)
        longitudinal <- data.frame(
            subject = subjects$subject[subject_index], time = time,
            sld = mean_sld + rnorm(length(time), 0, 2)
        )
        data <- DataJoint(
            subject = DataSubject(subjects, "subject", "arm", "study"),
            longitudinal = DataLongitudinal(longitudinal, sld ~ time)
        )
        for (centred in c(FALSE, TRUE)) {
            tag <- paste(variant, centred, sep = "-")
            model <- JointModel(LongitudinalSteinFojo(
                mu_bsld = prior_normal(mu[["bsld"]], 0.35),
                mu_ks = prior_normal(mu[["ks"]], 0.35),
                mu_kg = prior_normal(mu[["kg"]], 0.35),
                omega_bsld = prior_lognormal(log(omega[["bsld"]]), 0.35),
                omega_ks = prior_lognormal(log(omega[["ks"]]), 0.35),
                omega_kg = prior_lognormal(log(omega[["kg"]]), 0.35),
                sigma = prior_lognormal(log(2), 0.35),
                centred = centred
            ))
            writeLines(as.character(model), file.path(scratch, paste0(tag, ".stan")))
            stan_data <- c(as_stan_list(data, model), as_stan_list(model@parameters))
            cmdstanr::write_stan_json(stan_data, file.path(
                scratch, paste0(tag, "-", scenario, "-data.json")
            ))
            # Match natural-scale starting points across implementations.
            set.seed(8143)
            inits <- lapply(seq_len(4), function(chain) {
                init <- list(lm_sf_sigma = 2 * exp(rnorm(1, 0, 0.03)))
                for (effect in names(mu)) {
                    location <- mu[[effect]] + rnorm(1, 0, 0.02)
                    scale <- omega[[effect]] * exp(rnorm(1, 0, 0.03))
                    z <- rnorm(n, 0, 0.05)
                    log_psi <- location + scale * z
                    init[[paste0("lm_sf_mu_", effect)]] <- as.array(location)
                    init[[paste0("lm_sf_omega_", effect)]] <- as.array(scale)
                    if (variant == "affine") {
                        init[[paste0("lm_sf_log_psi_", effect)]] <- log_psi
                    } else if (centred) {
                        init[[paste0("lm_sf_psi_", effect)]] <- exp(log_psi)
                    } else {
                        init[[paste0("lm_sf_eta_tilde_", effect)]] <- z
                    }
                }
                init
            })
            saveRDS(inits, file.path(scratch, paste0(tag, "-init.rds")))
        }
    }
    quit(status = 0)
}

stopifnot(mode == "run")
scratch <- args[[2]]
destination <- args[[3]]
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
for (scenario in c("sparse", "dense")) {
    for (centred in c(FALSE, TRUE)) {
        paired_data <- lapply(c("main", "affine"), function(variant) {
            jsonlite::read_json(file.path(scratch, paste0(
                variant, "-", centred, "-", scenario, "-data.json"
            )))
        })
        stopifnot(identical(paired_data[[1]], paired_data[[2]]))
    }
}
compiled <- list()
compilation <- list()
for (variant in c("main", "affine")) {
    for (centred in c(FALSE, TRUE)) {
        tag <- paste(variant, centred, sep = "-")
        elapsed <- system.time({
            compiled[[tag]] <- cmdstanr::cmdstan_model(
                file.path(scratch, paste0(tag, ".stan")), quiet = TRUE
            )
        })[["elapsed"]]
        compilation[[tag]] <- data.frame(
            variant = variant, centred = centred, compile_seconds = elapsed
        )
    }
}
write.csv(do.call(rbind, compilation), file.path(destination, "compilation.csv"),
    row.names = FALSE)

results <- list()
parameters <- list()
for (scenario in c("sparse", "dense")) {
    for (centred in c(FALSE, TRUE)) {
        for (seed in c(9321L, 9322L)) {
            # Reverse pair order for the second seed to reduce order effects.
            variants <- if (seed == 9321L) c("main", "affine") else {
                c("affine", "main")
            }
            for (variant in variants) {
                tag <- paste(variant, centred, sep = "-")
                key <- paste(tag, scenario, seed, sep = "-")
                cat("BENCHMARK", key, "\n")
                elapsed <- system.time({
                    fit <- compiled[[tag]]$sample(
                        data = file.path(scratch, paste0(tag, "-", scenario,
                            "-data.json")),
                        init = readRDS(file.path(scratch, paste0(tag, "-init.rds"))),
                        chains = 4, parallel_chains = 4,
                        iter_warmup = 1000, iter_sampling = 1000,
                        adapt_delta = 0.9, max_treedepth = 12,
                        seed = seed, refresh = 0, sig_figs = 12,
                        output_dir = scratch
                    )
                })[["elapsed"]]
                variables <- c("lm_sf_mu_bsld", "lm_sf_mu_ks", "lm_sf_mu_kg",
                    "lm_sf_omega_bsld", "lm_sf_omega_ks", "lm_sf_omega_kg",
                    "lm_sf_sigma", "lm_sf_psi_bsld", "lm_sf_psi_ks", "lm_sf_psi_kg")
                summary <- fit$summary(variables = variables,
                    mean = mean, sd = sd, mcse_mean = posterior::mcse_mean,
                    rhat = posterior::rhat, ess_bulk = posterior::ess_bulk,
                    ess_tail = posterior::ess_tail)
                timing <- fit$time()$chains
                diagnostics <- posterior::as_draws_matrix(fit$sampler_diagnostics())
                energy <- posterior::as_draws_array(
                    fit$sampler_diagnostics()
                )[, , "energy__"]
                ebfmi <- apply(energy, 2, function(x) {
                    mean(diff(x)^2) / mean((x - mean(x))^2)
                })
                hyper <- !grepl("psi_", summary$variable, fixed = TRUE)
                sampling_seconds <- max(timing$sampling)
                results[[key]] <- data.frame(
                    variant = variant, scenario = scenario, centred = centred,
                    seed = seed, wall_seconds = elapsed,
                    warmup_seconds = max(timing$warmup),
                    sampling_seconds = sampling_seconds,
                    chain_sampling_seconds = sum(timing$sampling),
                    median_ess_bulk = median(summary$ess_bulk),
                    min_ess_bulk = min(summary$ess_bulk),
                    median_hyper_ess_bulk = median(summary$ess_bulk[hyper]),
                    median_ess_per_second = median(summary$ess_bulk) / sampling_seconds,
                    hyper_ess_per_second = median(summary$ess_bulk[hyper]) / sampling_seconds,
                    max_rhat = max(summary$rhat),
                    min_ebfmi = min(ebfmi),
                    divergences = sum(diagnostics[, "divergent__"]),
                    treedepth_hits = sum(diagnostics[, "treedepth__"] >= 12),
                    total_leapfrogs = sum(diagnostics[, "n_leapfrog__"])
                )
                parameters[[key]] <- cbind(
                    variant = variant, scenario = scenario, centred = centred,
                    seed = seed, summary
                )
                write.csv(do.call(rbind, results), file.path(destination, "runs.csv"),
                    row.names = FALSE)
                write.csv(do.call(rbind, parameters), file.path(destination,
                    "parameters.csv"), row.names = FALSE)
            }
        }
    }
}
capture.output(sessionInfo(), file = file.path(destination, "session-info.txt"))
