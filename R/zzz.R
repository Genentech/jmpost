
.onLoad <- function(libname, pkgname) {
    set_options()
}


# This only exists to silence the false positive R CMD CHECK warning about
# importing but not using the posterior package. posterior is a dependency
# of rcmdstan that we use a lot implicitly. Also we link to their documentation
# pages in ours
.never_run <- function() {
    posterior::as_draws()
}
