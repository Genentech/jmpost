mcmc_results <- readRDS(test_path("fixtures", "mcmc_results.rds"))

CACHE_DIR <- if (Sys.getenv("JMPOST_CACHE_DIR") == "") {
    tempdir()
} else {
    Sys.getenv("JMPOST_CACHE_DIR")
}
