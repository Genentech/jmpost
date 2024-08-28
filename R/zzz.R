
.onAttach <- function(libname, pkgname) {
    if (!is_cmdstanr_available()) {
        packageStartupMessage(
            "jmpost uses cmdstanr for compiling and sampling from models, but it does not seem to be installed.\n",
            "To install:\n",
            "install.packages(\"cmdstanr\", repos = c(\"https://stan-dev.r-universe.dev/\", getOption(\"repos\")))"
        )
    } else if (is.null(cmdstanr::cmdstan_version(error_on_NA = FALSE))) {
        possible_paths <- unique(c(
            cmdstanr::cmdstan_default_install_path(),
            Sys.getenv("CMDSTAN"),
            Sys.getenv("CMDSTAN_PATH"),
            "/root/.cmdstan",
            "~/.cmdstan"
        ))
        possible_paths <- possible_paths[dir.exists(possible_paths)]

        if (length(possible_paths)) {
            for (try_path in possible_paths) {
                new_path <- tryCatch( 
                    suppressMessages(cmdstanr::set_cmdstan_path(try_path)),
                    warning = function(w) NULL,
                    error = function(e) NULL
                )
            }
            if (!is.null(new_path)) {
                packageStartupMessage("CmdStan path set to: ", new_path)
            }
        } else {
            packageStartupMessage("jmpost could not identify CmdStan path. Please use cmdstanr::set_cmdstan_path()")
        }
    }
    return(invisible(NULL))
}

.onLoad <- function(...) {
    set_options()
    s3_register("cmdstanr::as.CmdStanMCMC", "JointModelSamples")
}

# This only exists to silence the false positive R CMD CHECK warning about
# importing but not using the posterior package. posterior is a dependency
# of rcmdstan that we use a lot implicitly. Also we link to their documentation
# pages in ours
.never_run <- function() {
    posterior::as_draws()
}
