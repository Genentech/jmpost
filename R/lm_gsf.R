

.LongitudinalGSF <- setClass(
    Class = "LongitudinalGSF",
    contains = "LongitudinalModel"
)


#' @export
LongitudinalGSF <- function (
    mu_bsld = Parameter(prior_lognormal(log(55), 5), init = log(55)),
    mu_ks = Parameter(prior_lognormal(0, 0.5), init = 0),
    mu_kg = Parameter(prior_lognormal(-0.36, 1), init = -0.36),
    mu_phi = Parameter(prior_beta(2, 8), init = 0.2),
    omega_bsld = Parameter(prior_lognormal(0, 1), init = 0),
    omega_ks = Parameter(prior_lognormal(0, 1), init = 0),
    omega_kg = Parameter(prior_lognormal(0, 1), init = 0),
    omega_phi = Parameter(prior_lognormal(0, 1), init = 0),
    sigma = Parameter(prior_lognormal(-1.6, 0.8), init = -1.6),
    tilde_bsld = Parameter(prior_normal(0, 5), init = 0),
    tilde_ks = Parameter(prior_normal(0, 5), init = 0),
    tilde_kg = Parameter(prior_normal(0, 5), init = 0),
    tilde_phi = Parameter(prior_normal(0, 5), init = 0)
) {
    x <- LongitudinalModel(
        stan = StanModule( x = "lm-gsf/model.stan"),
        parameters = ParameterList(
            lm_gsf_mu_bsld = mu_bsld,
            lm_gsf_mu_ks = mu_ks,
            lm_gsf_mu_kg = mu_kg,
            lm_gsf_mu_phi = mu_phi,
            lm_gsf_omega_bsld = omega_bsld,
            lm_gsf_omega_ks = omega_ks,
            lm_gsf_omega_kg = omega_kg,
            lm_gsf_omega_phi = omega_phi,
            lm_gsf_sigma = sigma,
            lm_gsf_eta_tilde_bsld = tilde_bsld,
            lm_gsf_eta_tilde_ks = tilde_ks,
            lm_gsf_eta_tilde_kg = tilde_kg,
            lm_gsf_eta_tilde_phi = tilde_phi
        )
    )
    .LongitudinalGSF(x)
}


.LinkGSF <- setClass(
    Class = "LinkGSF",
    contains = "Link"
)


#' @export
LinkGSF <- function(
    components = list(
        link_gsf_dsld(),
        link_gsf_ttg()
    )
) {

    # TODO - Allow for single LinkAbstract (right now it must be a list
    #        of LinkAbstracts, even if just 1)
    # TODO - Check all elements of a list input are LinkAbstract

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
        StanModule(rendered_link),
        stan_components
    )

    x <- Link(
        stan = stan_full,
        parameters = parameters
    )

    .LinkGSF(x)
}


# TODO - Priors
.link_gsf_abstract <- setClass(
    Class = "link_gsf_abstract",
    slots = list(
        "stan" = "StanModule",
        "parameter" = "ParameterList",
        "parameter_name" = "character",
        "contribution_fname" = "character"
    )
)
#' @export
link_gsf_abstract <- function(stan, parameter, parameter_name, contribution_fname) {
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


.link_gsf_ttg <- setClass(
    Class = "link_gsf_ttg",
    contains = "link_gsf_abstract"
)
#' @export
link_gsf_ttg <- function(
    gamma = Parameter(prior_normal(0, 5), init = 0)
) {
    link_gsf_abstract(
        stan = StanModule("lm-gsf/link_ttg.stan"),
        parameter = ParameterList(lm_gsf_gamma = gamma),
        parameter_name = "lm_gsf_gamma",
        contribution_fname = "link_ttg_contribution"
    )
}


.link_gsf_dsld <- setClass(
    Class = "link_gsf_dsld",
    contains = "link_gsf_abstract"
)
#' @export
link_gsf_dsld <- function(
    beta = Parameter(prior_normal(0, 5), init = 0)
) {
    link_gsf_abstract(
        stan = StanModule("lm-gsf/link_dsld.stan"),
        parameter = ParameterList(lm_gsf_beta = beta),
        parameter_name = "lm_gsf_beta",
        contribution_fname = "link_dsld_contribution"
    )
}




