# Simulating Joint Longitudinal and Time-to-Event Data

Simulating Joint Longitudinal and Time-to-Event Data

## Usage

``` r
SimJointData(
  design = list(SimGroup(n = 50, study = "Study-1", arm = "Arm-A"), SimGroup(n = 50,
    study = "Study-1", arm = "Arm-B")),
  longitudinal,
  survival,
  .silent = FALSE
)
```

## Arguments

- design:

  (`list`) a list of
  [`SimGroup`](https://genentech.github.io/jmpost/reference/SimGroup-class.md)
  objects. See details.

- longitudinal:

  (`SimLongitudinal`) object specifying how to simulate the longitudinal
  data

- survival:

  (`SimSurvival`) object specifying how to simulate the survival data

- .silent:

  (`flag`) whether to suppress info messages

## Value

A `SimJointData` object.

## Details

The `design` argument is used to specify how many distinct groups should
be simulated including key information such as the number of subjects
within the group as well as which treatment arm and study the group
belongs to. The `design` argument should be a list of
[`SimGroup`](https://genentech.github.io/jmpost/reference/SimGroup-class.md)
objects e.g.

    design = list(
        SimGroup(n = 50, study = "Study-1", arm = "Arm-A"),
        SimGroup(n = 50, study = "Study-1", arm = "Arm-B")
      )

## Slots

- `longitudinal`:

  (`data.frame`)\
  the simulated longitudinal data.

- `survival`:

  (`data.frame`)\
  the simulated survival data.

## Examples

``` r
set.seed(1)
SimJointData(
  design = list(SimGroup(3, "A", "Study 1")),
  longitudinal = SimLongitudinalRandomSlope(
    times = c(0, 10),
    slope_mu = 0.01,
    slope_sigma = 0.5
  ),
  survival = SimSurvivalExponential(lambda = 1 / 300)
)
#> 
#> A SimJointData Object
#> 
```
