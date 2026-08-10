# Generate Simulated Observations

Generate Simulated Observations

## Usage

``` r
sampleObservations(object, times_df)

# S3 method for class 'SimLongitudinalClaretBruno'
sampleObservations(object, times_df)

# S3 method for class 'SimLongitudinalGSF'
sampleObservations(object, times_df)

# S3 method for class 'SimLongitudinalRandomSlope'
sampleObservations(object, times_df)

# S3 method for class 'SimLongitudinalSteinFojo'
sampleObservations(object, times_df)

# S3 method for class 'SimSurvival'
sampleObservations(object, times_df)
```

## Arguments

- object:

  (`SimLongitudinal` or `SimSurvival`) object to generate observations
  from.

- times_df:

  (`data.frame`) the times at which to generate observations. See
  details.

## Value

A `data.frame` containing simulated observations.

## Details

The `times_df` argument should be a `data.frame` as created by
`sampleSubjects` but replicated for each time point at which
observations are to be generated. That is if you want to generate
observations for times `c(0, 1, 2, 3)` then `times_df` should be created
as:

    subject_dat <- sampleSubjects(object, ...)
    times_df <- tidyr::expand_grid(
        subject_dat,
        time = c(0, 1, 2, 3)
      )

## Examples

``` r
sim <- SimLongitudinalRandomSlope(slope_mu = 0.01, slope_sigma = 0.5)
subjects <- data.frame(study = factor("S"), arm = factor("A"), subject = "1")
subject_parameters <- sampleSubjects(sim, subjects)
sampleObservations(sim, transform(subject_parameters, time = 0))
#>   study arm subject intercept  slope_ind time sld_mu sld_sd      sld
#> 1     S   A       1        50 -0.2273247    0     50      2 51.10869
#>   log_haz_link
#> 1            0
```
