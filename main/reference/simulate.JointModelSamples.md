# Simulate Patients from Posterior Predictive Distribution

Simulate Patients from Posterior Predictive Distribution

## Usage

``` r
# S3 method for class 'JointModelSamples'
simulate(
  object,
  nsim = NULL,
  seed = NULL,
  newdata = NULL,
  times = c(-2, 0, 10, 50, 100),
  jitter_var = c(0, 0),
  time_max = 2000,
  time_step = 1,
  lambda_censor = 1/3000,
  scaled_variance = FALSE,
  ...
)
```

## Arguments

- object:

  (`JointModelSamples`) the object containing the posterior samples.

- nsim:

  ignored.

- seed:

  ignored.

- newdata:

  (`data.frame`) A data frame containing data in the same format as the
  `object@data@survival@data`. Importantly, it should contain the same
  covariates and factor levels as the variables used in the survival
  formula `object@data@survival@formula` and the same columns used for
  `study`, `id`, and `arm`.

- times:

  (`numeric`) times to simulate SLD for all patients.

- jitter_var:

  (`numeric`) variances to add noise to the observed SLD `times`. The
  first value is for any times less than 0 and the second for any times
  after 0. All positive (negative) times will remain positive
  (negative). Jitter values are generated from a normal distribution
  with mean 0 and the given variances.

- time_max:

  (`number`) the maximum time to simulate to.

- time_step:

  (`number`) the time interval between evaluating the log-hazard
  function.

- lambda_censor:

  (`number`) the censoring rate, as the parameter of an exponential
  distribution.

- scaled_variance:

  (`flag`) Should variance be scaled by the expected value. Must be set
  the same as was used for model fitting.

- ...:

  unused Unused.

## Details

Simulates a set of patients based on the covariates of those used in the
model fit or from `newdata`, which must contain the same column names
and factor levels.
