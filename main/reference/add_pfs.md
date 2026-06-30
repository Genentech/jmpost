# Add PFS events at Tumour Progression to Data

Adds new columns `pfs_time` and `pfs_event` based on observed changes to
SLD.

## Usage

``` r
add_pfs(
  object,
  relative_threshold = 1.2,
  absolute_threshold = 5,
  from_time = 0,
  observed_after = FALSE
)
```

## Arguments

- object:

  (`SimJointData`) where the PFS events should be added.

- relative_threshold:

  (`number`) a multiplicative threshold for the change in SLD compared
  to the `min(SLD)`. Default is 1.2 meaning a 20% increase.

- absolute_threshold:

  (`number`) an absolute threshold for the change in SLD compared to the
  minimum. Default is 5.

- from_time:

  (`number`) Ignore observations before this time for determining SLD
  minimum.

- observed_after:

  (`logical`) If `FALSE` set longitudinal observations after the
  progression time to `observed = FALSE`

## Details

Both thresholds must be met for a progression to be declared.

## Examples

``` r
data <- SimJointData(
  survival = SimSurvivalExponential(lambda = 1/10),
  longitudinal = SimLongitudinalSteinFojo()
  )
data <- add_pfs(data)
data@survival # now has pfs_time and pfs_event columns
#> # A tibble: 100 × 9
#>    subject     study   arm    time cov_cont cov_cat event pfs_time pfs_event
#>    <chr>       <fct>   <fct> <dbl>    <dbl> <fct>   <dbl>    <dbl>     <dbl>
#>  1 subject_001 Study-1 Arm-A     1 -1.40    C           1     1            1
#>  2 subject_002 Study-1 Arm-A     8  0.255   A           1     8            1
#>  3 subject_003 Study-1 Arm-A     9 -2.44    A           1     9            1
#>  4 subject_004 Study-1 Arm-A    15 -0.00557 C           1    15            1
#>  5 subject_005 Study-1 Arm-A     5  0.622   C           1     5            1
#>  6 subject_006 Study-1 Arm-A     1  1.15    A           1     1            1
#>  7 subject_007 Study-1 Arm-A    12 -1.82    B           1    12            1
#>  8 subject_008 Study-1 Arm-A     2 -0.247   C           1     2            1
#>  9 subject_009 Study-1 Arm-A     6 -0.244   A           1     1.51         1
#> 10 subject_010 Study-1 Arm-A     3 -0.283   A           1     3            1
#> # ℹ 90 more rows
```
