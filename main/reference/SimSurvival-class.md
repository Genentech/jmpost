# Abstract Simulation Class for Survival Data

Abstract Simulation Class for Survival Data

## Usage

``` r
SimSurvival(
  time_max = 2000,
  time_step = 1,
  lambda_censor = 1/3000,
  beta_cont = 0.2,
  beta_cat = c(A = 0, B = -0.4, C = 0.2),
  loghazard,
  name = "SimSurvival"
)
```

## Arguments

- time_max:

  (`number`) the maximum time to simulate to.

- time_step:

  (`number`) the time interval between evaluating the log-hazard
  function.

- lambda_censor:

  (`number`) the censoring rate.

- beta_cont:

  (`number`) the continuous covariate coefficient.

- beta_cat:

  (`numeric`) the categorical covariate coefficients.

- loghazard:

  (`function`) the log hazard function.

- name:

  (`character`) the name of the object.

## Slots

- `time_max`:

  (`numeric`)\
  See arguments.

- `time_step`:

  (`numeric`)\
  See arguments.

- `lambda_censor`:

  (`numeric`)\
  See arguments.

- `beta_cont`:

  (`numeric`)\
  See arguments.

- `beta_cat`:

  (`numeric`)\
  See arguments.

- `loghazard`:

  (`function`)\
  See arguments.

- `name`:

  (`character`)\
  See arguments.

- `beta_os_cov`:

  (`numeric`) Additional regression coefficients for survival models.

## Hazard Evaluation

Event times are simulated by sampling a cumulative hazard limit from a
\\U(0, 1)\\ distribution for each subject and then counting how much
hazard they've been exposed to by evaluating the log-hazard function at
a set interval. The `time_max` argument sets the upper bound for the
number of time points to evaluate the log-hazard function at with
subjects who have not had an event being censored at `time_max`. The
`time_step` argument sets the interval at which to evaluate the
log-hazard function. Setting smaller values for `time_step` will
increase the precision of the simulation at the cost of increased
computation time. Likewise, setting large values for `time_max` will
minimize the number of censored subjects at the cost of increased
computation time.

## See also

Other SimSurvival:
[`SimSurvivalExponential()`](https://genentech.github.io/jmpost/reference/SimSurvivalExponential.md),
[`SimSurvivalGamma()`](https://genentech.github.io/jmpost/reference/SimSurvivalGamma.md),
[`SimSurvivalLogLogistic()`](https://genentech.github.io/jmpost/reference/SimSurvivalLogLogistic.md),
[`SimSurvivalWeibullPH()`](https://genentech.github.io/jmpost/reference/SimSurvivalWeibullPH.md)
