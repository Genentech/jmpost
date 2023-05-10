#' @include LongitudinalModel.R
#' @include StanModule.R
#' @include generics.R
#' @include ParameterList.R
#' @include Parameter.R
#' @include Link.R
NULL

.LongitudinalGSF <- setClass(
  Class = "LongitudinalGSF",
  contains = "LongitudinalModel"
)


#' @export
LongitudinalGSF <- function(
    mu_bsld = prior_lognormal(log(55), 5, init = 55),
    mu_ks = prior_lognormal(0, 0.5, init = 0),
    mu_kg = prior_lognormal(-0.36, 1, init = -0.36),
    mu_phi = prior_beta(2, 8, init = 0.2),
    omega_bsld = prior_lognormal(0, 1, init = 0),
    omega_ks = prior_lognormal(0, 1, init = 0),
    omega_kg = prior_lognormal(0, 1, init = 0),
    omega_phi = prior_lognormal(0, 1, init = 0),
    sigma = prior_lognormal(-1.6, 0.8, init = -1.6),
    tilde_bsld = prior_normal(0, 5, init = 0),
    tilde_ks = prior_normal(0, 5, init = 0),
    tilde_kg = prior_normal(0, 5, init = 0),
    tilde_phi = prior_normal(0, 5, init = 0)) {
  x <- LongitudinalModel(
    stan = merge(
      StanModule("lm-gsf/model.stan"),
      StanModule("lm-gsf/functions.stan")
    ),
    parameters = ParameterList(
      Parameter(name = "lm_gsf_mu_bsld", prior = mu_bsld, size = "n_studies"),
      Parameter(name = "lm_gsf_mu_ks", prior = mu_ks, size = "n_arms"),
      Parameter(name = "lm_gsf_mu_kg", prior = mu_kg, size = "n_arms"),
      Parameter(name = "lm_gsf_mu_phi", prior = mu_phi, size = "n_arms"),
      Parameter(name = "lm_gsf_omega_bsld", prior = omega_bsld, size = 1),
      Parameter(name = "lm_gsf_omega_ks", prior = omega_ks, size = 1),
      Parameter(name = "lm_gsf_omega_kg", prior = omega_kg, size = 1),
      Parameter(name = "lm_gsf_omega_phi", prior = omega_phi, size = 1),
      Parameter(name = "lm_gsf_sigma", prior = sigma, size = 1),
      Parameter(name = "lm_gsf_eta_tilde_bsld", prior = tilde_bsld, size = "Nind"),
      Parameter(name = "lm_gsf_eta_tilde_ks", prior = tilde_ks, size = "Nind"),
      Parameter(name = "lm_gsf_eta_tilde_kg", prior = tilde_kg, size = "Nind"),
      Parameter(name = "lm_gsf_eta_tilde_phi", prior = tilde_phi, size = "Nind")
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
    )) {
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
    gamma = prior_normal(0, 5, init = 0)) {
  link_gsf_abstract(
    stan = StanModule("lm-gsf/link_ttg.stan"),
    parameter = ParameterList(Parameter(name = "lm_gsf_gamma", prior = gamma, size = 1)),
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
    beta = Parameter(prior_normal(0, 5), init = 0)) {
  link_gsf_abstract(
    stan = StanModule("lm-gsf/link_dsld.stan"),
    parameter = ParameterList(Parameter(name = "lm_gsf_beta", prior = beta, size = 1)),
    parameter_name = "lm_gsf_beta",
    contribution_fname = "link_dsld_contribution"
  )
}
