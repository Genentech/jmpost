# Calculate Population Hazard Ratios

Calculates hazard ratios marginalised over subject specific random
effects using the approach proposed by (van Oudenhoven et al. 2020) .

## Usage

``` r
populationHR(
  object,
  hr_formula = object@data@survival@formula,
  baseline = ~bs(time, df = 10),
  quantiles = c(0.025, 0.975)
)
```

## Arguments

- object:

  (`JointModelSamples`) samples as drawn from a Joint Model.

- hr_formula:

  (`formula`) defines the terms to include in the hazard ratio
  calculation. By default this uses the right side of the formula used
  in the survival model. Set to `NULL` not include any terms

- baseline:

  (`formula`) terms to model baseline hazard using variable `time`.
  Default is a B-spline from splines: `~bs(time, df = 10)`

- quantiles:

  (`numeric`) vector of two values in (0, 1) for calculating quantiles
  from log hazard ratio distributions.

## Value

A list containing a summary of parameter distributions as a `data.frame`
and a matrix containing the parameter estimates for each sample.

## References

van Oudenhoven FM, Swinkels SHN, Ibrahim JG, Rizopoulos D (2020). “A
marginal estimate for the overall treatment effect on a survival outcome
within the joint modeling framework.” *Statistics in Medicine*,
**39**(28), 4120-4132.
[doi:10.1002/sim.8713](https://doi.org/10.1002/sim.8713) ,
https://onlinelibrary.wiley.com/doi/pdf/10.1002/sim.8713,
<https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.8713>.
