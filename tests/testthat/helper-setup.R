mcmc_results <- readRDS(test_path("fixtures", "mcmc_results.rds"))

# If running in CICD set cache directory to a local directory so that it can be
# cached for future runs
is_github_actions <- function() {
    Sys.getenv("CI") == "true"
}

if (is_github_actions()) {
    MODEL_DIR <- file.path(Sys.getenv("HOME"), ".jmpost")
} else {
    MODEL_DIR <- tempdir()
}
