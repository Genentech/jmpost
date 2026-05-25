# Get Random Effects Names

Utility function that returns the names of the random effects
parameters. The main use for this is to allow the
[`LongitudinalRandomEffects`](https://genentech.github.io/jmpost/reference/LongitudinalRandomEffects.md)
function to know which parameters it needs to extract and to what common
names it should map the parameters to.

## Usage

``` r
getRandomEffectsNames(object, ...)

# Default S3 method
getRandomEffectsNames(object, ...)

# S3 method for class 'JointModel'
getRandomEffectsNames(object, ...)

# S3 method for class 'LongitudinalGSF'
getRandomEffectsNames(object, ...)

# S3 method for class 'LongitudinalGSF'
getRandomEffectsNames(object, ...)

# S3 method for class 'LongitudinalRandomSlope'
getRandomEffectsNames(object, ...)

# S3 method for class 'LongitudinalSteinFojo'
getRandomEffectsNames(object, ...)
```

## Arguments

- object:

  (`LongitudinalModel`) A longitudinal model object

- ...:

  Not used.
