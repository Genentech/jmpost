# Generate Simulated Subjects

Generate Simulated Subjects

## Usage

``` r
sampleSubjects(object, subjects_df)

# S3 method for class 'SimLongitudinalClaretBruno'
sampleSubjects(object, subjects_df)

# S3 method for class 'SimLongitudinalGSF'
sampleSubjects(object, subjects_df)

# S3 method for class 'SimLongitudinalRandomSlope'
sampleSubjects(object, subjects_df)

# S3 method for class 'SimLongitudinalSteinFojo'
sampleSubjects(object, subjects_df)

# S3 method for class 'SimSurvival'
sampleSubjects(object, subjects_df)
```

## Arguments

- object:

  (`SimLongitudinal` or `SimSurvival`) object to generate subjects from.

- subjects_df:

  (`data.frame`) the subjects to generate observations for. See details.

## Value

A `data.frame` containing simulated subject-level parameters.

## Details

The `subjects_df` argument should be a `data.frame` with 1 row per
desired subject to create with the following columns:

- `study` (`factor`) the study identifier.

- `arm` (`factor`) the treatment arm identifier.

- `subject` (`character`) the subject identifier.

This method takes care of generating all the individual subject data
required for the
[`sampleObservations`](https://genentech.github.io/jmpost/reference/sampleObservations.md)
method to generate the observations.

## Examples

``` r
subjects <- data.frame(study = factor("S"), arm = factor("A"), subject = "1")
sim <- SimLongitudinalRandomSlope(slope_mu = 0.01, slope_sigma = 0.5)
sampleSubjects(sim, subjects)
#>   study arm subject intercept slope_ind
#> 1     S   A       1        50  1.093206
```
