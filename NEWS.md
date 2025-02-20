
# jmpost (development version)

- Introduced the `saveObject()` method for `JointModelSample` objects in order to serialise them to disk (#431).
- Added support for truncated prior distributions e.g. you can now apply a normal prior to a strictly positive parameter and jmpost will take care of adjusting the density accordingly (#429).
- Included new Gamma distribution survival model (#411).
- Reworked LOO calculations to apply to each individual submodel and disabled LOO calculations for the overall joint model (#402).
- Added support for additive variance (#403).
- Added support for independent variances per study/arm (#389).
- Miscellaneous bug fixes.

# jmpost 0.0.1

- Initial Release
