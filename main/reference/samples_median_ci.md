# Obtain Median and Credible Intervals from MCMC samples

Obtain Median and Credible Intervals from MCMC samples

## Usage

``` r
samples_median_ci(samples, level = 0.95)
```

## Arguments

- samples:

  (`matrix`) with samples in rows and parameters in columns.

- level:

  (`number`) credibility level to use for the credible intervals.

## Value

A `data.frame` with columns `median`, `lower` and `upper`.
