# Cut Study Data

Cut Study Data

## Usage

``` r
cut_data(object, cut_time)
```

## Arguments

- object:

  A
  [SimJointData](https://genentech.github.io/jmpost/reference/SimJointData-class.md)
  object

- cut_time:

  (`numeric`) A vector of cut off times, either length 1 for all
  patients or `nrow(object@survival)` for a time per patient.

## Details

All observations after this time are remove. Survival is censored at
this time and any longitudinal values are removed.

## Examples

``` r
data <- SimJointData(
  survival = SimSurvivalExponential(lambda = 1/10),
  longitudinal = SimLongitudinalSteinFojo()
  )
data <- cut_data(data, 5)
data@survival
#> # A tibble: 100 × 7
#>    subject     study   arm    time cov_cont cov_cat event
#>    <chr>       <fct>   <fct> <dbl>    <dbl> <fct>   <dbl>
#>  1 subject_001 Study-1 Arm-A     5  -0.318  B           0
#>  2 subject_002 Study-1 Arm-A     5   2.03   C           0
#>  3 subject_003 Study-1 Arm-A     3  -0.813  C           1
#>  4 subject_004 Study-1 Arm-A     5  -1.10   B           0
#>  5 subject_005 Study-1 Arm-A     5  -2.38   B           0
#>  6 subject_006 Study-1 Arm-A     5  -0.897  A           0
#>  7 subject_007 Study-1 Arm-A     5  -0.0498 A           0
#>  8 subject_008 Study-1 Arm-A     5  -0.500  C           0
#>  9 subject_009 Study-1 Arm-A     5  -0.800  C           0
#> 10 subject_010 Study-1 Arm-A     5  -0.721  C           0
#> # ℹ 90 more rows
# Now max time is 5
max(data@survival$time)
#> [1] 5
```
