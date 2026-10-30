# Extract Covariate Shrinkage Factors from the Posterior Samples

Extract Covariate Shrinkage Factors from the Posterior Samples

## Usage

``` r
shrinkage(object, ...)
```

## Arguments

- object:

  (`ANY`) the object to extract shrinkage factors from.

- ...:

  additional arguments added by methods.

## Value

For
[`JointModelSamples`](https://genentech.github.io/jmpost/reference/JointModelSamples-class.md),
a posterior draws object containing the sampled prior shrinkage factors;
see
[`shrinkage.JointModelSamples()`](https://genentech.github.io/jmpost/reference/shrinkage.JointModelSamples.md).

## See also

Other shrinkage:
[`shrinkage.JointModelSamples()`](https://genentech.github.io/jmpost/reference/shrinkage.JointModelSamples.md)

## Examples

``` r
if (FALSE) { # \dontrun{
shrinkage(fit)
} # }
```
