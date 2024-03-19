
<!-- markdownlint-disable-file -->
<!-- README.md needs to be generated from README.Rmd. Please edit that file -->

# jmpost <a href="https://genentech.github.io/jmpost/"><img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Code
Coverage](https://raw.githubusercontent.com/genentech/jmpost/_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/genentech/jmpost/_xml_coverage_reports/data/main/coverage.xml)
<!-- badges: end -->  

The goal of the `jmpost` package is to fit joint models involving:

1.  a parametric time-to-event sub-model,
2.  a nonlinear (or linear) mixed-effect sub-model, describing
    individual time profiles (*i.e.* trajectories) for a continuous
    marker,
3.  a link function (*a.k.a.* association term).

More specifically, the model implemented in this package utilizes a
modelling framework described previously **\[1-3\]** to link overall
survival to tumour size data in oncology clinical trials.

**\[1\]** [Tardivon *et al.* Association between tumour size kinetics
and survival in patients with urothelial carcinoma treated with
atezolizumab: Implications for patient follow-up. *Clin Pharm Ther*,
2019](https://doi.org/10.1002/cpt.1450).  
**\[2\]** [Kerioui *et al.* Bayesian inference using Hamiltonian
Monte-Carlo algorithm for nonlinear joint modelling in the context of
cancer immunotherapy. *Stat in Med*,
2020](https://doi.org/10.1002/sim.8756).  
**\[3\]** [Kerioui *et al.* Modelling the association between biomarkers
and clinical outcome: An introduction to nonlinear joint models. *Br J
Clin Pharm*, 2022](https://doi.org/10.1111/bcp.15200).

The models are implemented in [STAN](https://mc-stan.org/), and the
package provides a flexible user interface. Please reach out to us via
issues or email (see the `DESCRIPTION` file) if you have comments or
questions or would like to get involved in the ongoing development,
thank you!

## Installation

**GitHub**

You can install the current development version from GitHub with:

``` r
if (!require("remotes")) {
    install.packages("remotes")
}
remotes::install_github("genentech/jmpost")
```

Please note that this package requires
[`cmdstanr`](https://mc-stan.org/cmdstanr/).

**CRAN**

This package has not been published to CRAN yet.

## Getting Started

See also the [model
fitting](https://genentech.github.io/jmpost/main/articles/model_fitting.html)
vignette for more details. Here we present a very basic example here.

First we simulate a data set. In practice you want to follow a similar
structure of the input data and use `DataJoint()` to bring it into the
right format.

``` r
library(jmpost)
#> Registered S3 methods overwritten by 'ggpp':
#>   method                  from   
#>   heightDetails.titleGrob ggplot2
#>   widthDetails.titleGrob  ggplot2
set.seed(321)
sim_data <- SimJointData(
    design = list(
        SimGroup(50, "Arm-A", "Study-X"),
        SimGroup(50, "Arm-B", "Study-X")
    ),
    longitudinal = SimLongitudinalRandomSlope(
        times = c(1, 50, 100, 150, 200, 250, 300),
    ),
    survival = SimSurvivalWeibullPH(
        lambda = 1 / 300,
        gamma = 0.97
    )
)
#> INFO: 1 patients did not die before max(times)

joint_data <- DataJoint(
    subject = DataSubject(
        data = sim_data@survival,
        subject = "pt",
        arm = "arm",
        study = "study"
    ),
    survival = DataSurvival(
        data = sim_data@survival,
        formula = Surv(time, event) ~ cov_cat + cov_cont
    ),
    longitudinal = DataLongitudinal(
        data = sim_data@longitudinal,
        formula = sld ~ time,
        threshold = 5
    )
)
```

Then we specify the joint model, here we use a Generalized Stein-Fojo
model for the longitudinal part, and a Weibull proportional hazards
model for the survival part. The longitudinal model impacts the hazard
via a term for the derivative and another term for the time-to-growth.

``` r
joint_model <- JointModel(
    longitudinal = LongitudinalGSF(),
    survival = SurvivalWeibullPH(),
    link = Link(
        link_dsld(),
        link_ttg()
    )
)
```

Finally we can sample the parameters via MCMC from the underlying Stan
model. Note that in a real application you will choose more warm up and
sampling iterations.

``` r
mcmc_results <- sampleStanModel(
    joint_model,
    data = joint_data,
    iter_sampling = 100,
    iter_warmup = 100,
    chains = 1,
    parallel_chains = 1
)
```

## Citing `jmpost`

To cite `jmpost` please see
[here](https://genentech.github.io/jmpost/main/authors.html#citation).
