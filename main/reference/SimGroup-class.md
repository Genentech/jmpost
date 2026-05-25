# Define Simulation Group

Specifies a simulation group to be used by
[`SimJointData()`](https://genentech.github.io/jmpost/reference/SimJointData-class.md).

## Usage

``` r
SimGroup(n, arm, study)
```

## Arguments

- n:

  (`numeric`) number of subjects in the group.

- arm:

  (`character`) treatment arm.

- study:

  (`character`) study name.

## Slots

- `n`:

  (`numeric`)\
  See arguments.

- `arm`:

  (`character`)\
  See arguments.

- `study`:

  (`character`)\
  See arguments.

## Examples

``` r
SimGroup(n = 50, arm = "Arm-A", study = "Study-1")
#> An object of class "SimGroup"
#> Slot "n":
#> [1] 50
#> 
#> Slot "arm":
#> [1] "Arm-A"
#> 
#> Slot "study":
#> [1] "Study-1"
#> 
```
