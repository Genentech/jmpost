
`jmpost` is currently under development and not yet testable.

The goal of the `jmpost` package is to fit joint models involving:
1. a parametric time-to-event sub-model,
2. a nonlinear (or linear) mixed-effect sub-model, describing individual time profiles (_i.e._ trajectories) for a continuous marker,
3. a link function (_a.k.a._ association term).
More specifically, the model implemented in this package utilizes a modeling framework described previously **[1-3]** to link overall survival to tumor size data in oncology clinical trials.

**[1]** [Tardivon _et al._ Association between tumor size kinetics and survival in patients with urothelial carcinoma treated with atezolizumab: Implications for patient follow-up. _Clin Pharm Ther_, 2019](https://doi.org/10.1002/cpt.1450).  
**[2]** [Kerioui _et al._ Bayesian inference using Hamiltonian Monte-Carlo algorithm for nonlinear joint modeling in the context of cancer immunotherapy. _Stat in Med_, 2020](https://doi.org/10.1002/sim.8756).  
**[3]** [Kerioui _et al._ Modeling the association between biomarkers and clinical outcome: An introduction to nonlinear joint models. _Br J Clin Pharm_, 2022](https://doi.org/10.1111/bcp.15200).

The models are implemented in [STAN](https://mc-stan.org/), and the package provides a flexible user interface.
Please reach out to us via issues or email (see the `DESCRIPTION` file) if you have comments or questions, thank you!
