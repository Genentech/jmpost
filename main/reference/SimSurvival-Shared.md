# `SimSurvival` Function Arguments

The documentation lists all the conventional arguments for
[`SimSurvival`](https://genentech.github.io/jmpost/reference/SimSurvival-class.md)
constructors.

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

- ...:

  Not Used.

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
