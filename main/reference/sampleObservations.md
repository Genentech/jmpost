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
