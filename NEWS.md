
# jmpost (development version)

- `LongitudinalModel` objects now have a new slot `scaled_variance` which stores the used variance option, and this is part of the `print` output now thereby transparently communicating to the user the choice of the multiplicative or additive error model.
- Changed default longitudinal model option `scaled_variance` to `FALSE`, corresponding to an additive error model (both for the simulation as well as for the inference functions). Also added this option to the random effects model for consistency with the other models.
- Included new `populationHR()` function to calculate population effects from a `JointModelSample` object, by marginalising over the patient-level random effects (#447).
- Included new `LongitudinalRandomEffects()` function which can be used to extract the patient-level random effects parameter samples from a `JointModelSample` object (#423).
- Introduced the `saveObject()` method for `JointModelSample` objects in order to serialise them to disk (#431).
- Added support for truncated prior distributions e.g. you can now apply a normal prior to a strictly positive parameter and jmpost will take care of adjusting the density accordingly (#429).
- Included new Gamma distribution survival model (#411).
- Reworked LOO calculations to apply to each individual submodel and disabled LOO calculations for the overall joint model (#402).
- Added support for additive variance (#403).
- Added support for independent variances per study/arm (#389).
- Miscellaneous bug fixes.

# jmpost 0.0.1

- Initial Release
