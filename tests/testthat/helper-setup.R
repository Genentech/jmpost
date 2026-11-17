CACHE_DIR <- if (Sys.getenv("JMPOST_CACHE_DIR") == "") {
    tempdir()
} else {
    Sys.getenv("JMPOST_CACHE_DIR")
}


is_full_test <- function() {
    toupper(Sys.getenv("JMPOST_FULL_TEST")) == "TRUE"
}

activate_full_test <- function() {
    Sys.setenv(JMPOST_FULL_TEST = "TRUE")
}

disable_full_test <- function() {
    Sys.setenv(JMPOST_FULL_TEST = "FALSE")
}

is_graph_snapshot_enabled <- function() {
    toupper(Sys.getenv("JMPOST_GRAPH_SNAPSHOT")) == "TRUE"
}


# Runs stans syntax parser and will throw an error if stan detects an
# issue that would prevent compilation
expect_stan_syntax <- function(code) {
    stan_file <- cmdstanr::write_stan_file(as.character(code))
    if ("check_syntax_stan_file" %in% getNamespaceExports("cmdstanr")) {
        expect_true(cmdstanr::check_syntax_stan_file(stan_file, quiet = TRUE))
    } else {
        model_obj <- cmdstanr::cmdstan_model(stan_file, compile = FALSE)
        expect_true(model_obj$check_syntax(quiet = TRUE))
    }
}


# Load multiple `inst` stan files including the base functions file (as many of them
# depend on this)
load_with_base_stan <- function(...) {
    sm <- Reduce(
        merge,
        lapply(list(...), StanModule)
    )
    merge(
        sm,
        StanModule("base/functions.stan")
    )
}


run_quietly <- function(expr) {
    suppressMessages(capture.output((x <- expr)))
    return(x)
}
