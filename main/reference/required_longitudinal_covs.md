# Required Longitudinal Covariates

Return the subject-level covariates required to generate longitudinal
population quantities for a model.

## Usage

``` r
required_longitudinal_covs(object, ...)

# Default S3 method
required_longitudinal_covs(object, ...)

# S3 method for class 'LongitudinalClaretBrunoCov'
required_longitudinal_covs(object, ...)

# S3 method for class 'LongitudinalGSFCov'
required_longitudinal_covs(object, ...)

# S3 method for class 'LongitudinalRandomSlopeCov'
required_longitudinal_covs(object, ...)

# S3 method for class 'LongitudinalSteinFojoCov'
required_longitudinal_covs(object, ...)
```

## Arguments

- object:

  (`LongitudinalModel`) A longitudinal model object.

- ...:

  Not used.

## Value

A character vector of covariate names.
