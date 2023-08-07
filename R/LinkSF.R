#' @include Link.R
NULL

# LinkSF-class ----

#' `LinkSF`
#'
#' This class extends the general [`Link`][Link-class] class for
#' the [`LongitudinalSF`][LongitudinalSF-class]
#' model.
#'
#' @exportClass LinkSF
.LinkSF <- setClass(
    Class = "LinkSF",
    contains = "Link"
)

# LinkSF-constructors ----

#' @rdname LinkSF-class
#'
#' @param ... (`link_sf_abstract`)\cr which link components should be included. If no arguments
#' are provided then this will be set to [link_sf_dsld()] and [link_sf_ttg()]
#'
#' @export
LinkSF <- function(...) {

    components <- list(...)
    if (!length(components)) {
        components <- list(
            link_sf_dsld(),
            link_sf_ttg()
        )
    }

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
        .x = paste0(read_stan("lm-sf/link.stan"), collapse = "\n"),
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

    .LinkSF(x)
}

# link_sf_abstract-class ----

#' `link_sf_abstract`
#'
#' Class to specify the contents for [`LinkSF`].
#'
#' @slot stan (`StanModule`)\cr the Stan code.
#' @slot parameter (`ParameterList`)\cr the parameter specification.
#' @slot parameter_name (`character`)\cr the name of the parameter.
#' @slot contribution_fname (`character`)\cr the function name of the contribution.
#'
#' @exportClass link_sf_abstract
#' @keywords internal
.link_sf_abstract <- setClass(
    Class = "link_sf_abstract",
    slots = list(
        "stan" = "StanModule",
        "parameter" = "ParameterList",
        "parameter_name" = "character",
        "contribution_fname" = "character"
    )
)

# link_sf_abstract-constructors ----

#' @rdname link_sf_abstract-class
#'
#' @inheritParams stanmodel_arguments
#' @param parameter_name (`character`)\cr the name of the parameter.
#' @param contribution_fname (`character`)\cr the function name of the contribution.
#'
#' @note Only the `functions` part of `stan` will be used.
#'
#' @export
link_sf_abstract <- function(stan,
                              parameter,
                              parameter_name,
                              contribution_fname) {
    .link_sf_abstract(
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

# link_sf_ttg-class ----

#' `link_sf_ttg`
#'
#' This class extends the general [`link_sf_abstract`] for the time-to-growth
#' (`ttg`) link contribution.
#'
#' @exportClass link_sf_ttg
.link_sf_ttg <- setClass(
    Class = "link_sf_ttg",
    contains = "link_sf_abstract"
)

# link_sf_ttg-constructors ----

#' @rdname link_sf_ttg-class
#'
#' @param gamma (`Prior`)\cr prior for the link coefficient `gamma`.
#'
#' @export
link_sf_ttg <- function(
        gamma = prior_normal(0, 5)
) {
    .link_sf_ttg(
        stan = StanModule("lm-sf/link_ttg.stan"),
        parameter = ParameterList(Parameter(name = "lm_sf_gamma", prior = gamma, size = 1)),
        parameter_name = "lm_sf_gamma",
        contribution_fname = "link_ttg_contribution"
    )
}

# link_sf_dsld-class ----

#' `link_sf_dsld`
#'
#' This class extends the general [`link_sf_abstract`] for the derivative of the
#' sum of longest diameters (`dsld`) link contribution.
#'
#' @exportClass link_sf_dsld
.link_sf_dsld <- setClass(
    Class = "link_sf_dsld",
    contains = "link_sf_abstract"
)


# link_sf_dsld-constructors ----

#' @rdname link_sf_dsld-class
#'
#' @param beta (`Prior`)\cr prior for the link coefficient `beta`.
#'
#' @export
link_sf_dsld <- function(
        beta = prior_normal(0, 5)
) {
    .link_sf_dsld(
        stan = StanModule("lm-sf/link_dsld.stan"),
        parameter = ParameterList(Parameter(name = "lm_sf_beta", prior = beta, size = 1)),
        parameter_name = "lm_sf_beta",
        contribution_fname = "link_dsld_contribution"
    )
}
