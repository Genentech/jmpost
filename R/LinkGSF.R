#' @include Link.R
NULL

# LinkGSF-class ----

#' `LinkGSF`
#'
#' This class extends the general [`Link`] class for the [`LongitudinalGSF`]
#' model.
#'
#' @exportClass LinkGSF
.LinkGSF <- setClass(
    Class = "LinkGSF",
    contains = "Link"
)

# LinkGSF-constructors ----

#' @rdname LinkGSF-class
#'
#' @param components (`list`)\cr which link components should be included.
#'
#' @export
LinkGSF <- function(
        components = list(
            link_gsf_dsld(),
            link_gsf_ttg()
        )
) {

    items <- lapply(
        components,
        function(x) {
            list(
                parameter = x@parameter_name,
                contribution_function = x@contribution_fname
            )
        }
    )

    rendered_link <- jinjar::render(
        .x = paste0(read_stan("lm-gsf/link.stan"), collapse = "\n"),
        items = items
    )

    parameters <- ParameterList()
    stan_components <- StanModule()
    for (item in components) {
        parameters <- merge(parameters, item@parameter)
        stan_components <- merge(stan_components, item@stan)
    }

    stan_full <- merge(
        stan_components,
        StanModule(rendered_link)
    )

    x <- Link(
        stan = stan_full,
        parameters = parameters
    )

    .LinkGSF(x)
}

# link_gsf_abstract-class ----

#' `link_gsf_abstract`
#'
#' Class to specify the contents for [`LinkGSF`].
#'
#' @slot stan (`StanModule`)\cr the Stan code.
#' @slot parameter (`ParameterList`)\cr the parameter specification.
#' @slot parameter_name (`character`)\cr the name of the parameter.
#' @slot contribution_fname (`character`)\cr the function name of the contribution.
#'
#' @exportClass link_gsf_abstract
.link_gsf_abstract <- setClass(
    Class = "link_gsf_abstract",
    slots = list(
        "stan" = "StanModule",
        "parameter" = "ParameterList",
        "parameter_name" = "character",
        "contribution_fname" = "character"
    )
)

# link_gsf_abstract-constructors ----

#' @rdname link_gsf_abstract-class
#'
#' @inheritParams stanmodule_arguments
#' @param parameter_name (`character`)\cr the name of the parameter.
#' @param contribution_fname (`character`)\cr the function name of the contribution.
#'
#' @note Only the `functions` part of `stan` will be used.
#'
#' @export
link_gsf_abstract <- function(stan,
                              parameter,
                              parameter_name,
                              contribution_fname) {
    .link_gsf_abstract(
        parameter = parameter,
        parameter_name = parameter_name,
        contribution_fname = contribution_fname,
        stan = StanModule(
            paste0(
                "functions {\n",
                as.list(stan)[["functions"]],
                "\n}"
            )
        )
    )
}

# link_gsf_ttg-class ----

#' `link_gsf_ttg`
#'
#' This class extends the general [`link_gsf_abstract`] for the time-to-growth
#' (`ttg`) link contribution.
#'
#' @exportClass link_gsf_ttg
.link_gsf_ttg <- setClass(
    Class = "link_gsf_ttg",
    contains = "link_gsf_abstract"
)

# link_gsf_ttg-constructors ----

#' @rdname link_gsf_ttg-class
#'
#' @param gamma (`Prior`)\cr prior for the link coefficient `gamma`.
#'
#' @export
link_gsf_ttg <- function(
        gamma = prior_normal(0, 5, init = 0)
) {
    link_gsf_abstract(
        stan = StanModule("lm-gsf/link_ttg.stan"),
        parameter = ParameterList(Parameter(name = "lm_gsf_gamma", prior = gamma, size = 1)),
        parameter_name = "lm_gsf_gamma",
        contribution_fname = "link_ttg_contribution"
    )
}

# link_gsf_dsld-class ----

#' `link_gsf_dsld`
#'
#' This class extends the general [`link_gsf_abstract`] for the derivative of the
#' sum of longest diameters (`dsld`) link contribution.
#'
#' @exportClass link_gsf_dsld
.link_gsf_dsld <- setClass(
    Class = "link_gsf_dsld",
    contains = "link_gsf_abstract"
)


# link_gsf_dsld-constructors ----

#' @rdname link_gsf_dsld-class
#'
#' @param beta (`Prior`)\cr prior for the link coefficient `beta`.
#'
#' @export
link_gsf_dsld <- function(
        beta = Parameter(prior_normal(0, 5, init = 0))
) {
    link_gsf_abstract(
        stan = StanModule("lm-gsf/link_dsld.stan"),
        parameter = ParameterList(Parameter(name = "lm_gsf_beta", prior = beta, size = 1)),
        parameter_name = "lm_gsf_beta",
        contribution_fname = "link_dsld_contribution"
    )
}
