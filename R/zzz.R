
.onLoad <- function(libname, pkgname) {

    cache_dir <- Sys.getenv("JMPOST_CACHE_DIR")

    if (cache_dir == "" || is.null(cache_dir)) {
        cache_dir <- tempfile()
    }

    current_opts <- names(options())
    jmpost_opts <- list(
        jmpost.cache.dir = cache_dir
    )
    for (opt in names(jmpost_opts)) {
        if (!opt %in% current_opts) {
            options(jmpost_opts[opt])
        }
    }


}


# This only exists to silence the false positive R CMD CHECK warning about
# importing but not using the posterior package. posterior is a dependency
# of rcmdstan that we use a lot implicitly. Also we link to their documentation
# pages in ours
.never_run <- function() {
    posterior::as_draws()
}
