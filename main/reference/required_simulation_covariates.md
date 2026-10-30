# Required Longitudinal Simulation Covariates

Return the subject-level covariates required to simulate a longitudinal
model from posterior draws. Unlike
[`required_longitudinal_covs()`](https://genentech.github.io/jmpost/reference/required_longitudinal_covs.md),
this includes covariates used only by variability predictors.

## Usage

``` r
required_simulation_covariates(object, ...)

# Default S3 method
required_simulation_covariates(object, ...)

# S3 method for class 'LongitudinalClaretBrunoCov'
required_simulation_covariates(object, ...)

# S3 method for class 'LongitudinalGSFCov'
required_simulation_covariates(object, ...)

# S3 method for class 'LongitudinalRandomSlopeCov'
required_simulation_covariates(object, ...)

# S3 method for class 'LongitudinalSteinFojoCov'
required_simulation_covariates(object, ...)
```

## Arguments

- object:

  (`LongitudinalModel`) A longitudinal model object.

- ...:

  Not used.

## Value

A character vector of covariate names.
