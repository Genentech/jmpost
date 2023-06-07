
# Initial Values

You can specify initial values in one of three ways:

1. Use the default. By default all initial values are set to the mean of the prior distribution. That is if you use a `prior_gamma(1, 3)` the initial value will be set to `1/3`. 

2. Manually specify in the prior function. That is you can set initial values via `prior_normal(0, 1, init = 20)`. Note that if the parameter represents multiple parameters (e.g. 1 per arm) then single values are replicated for each of the parameters. You can specify different initival values by instead specifying a vector e.g. `prior_normal(0, 1, init = c(10, 20))`. Care is needed to ensure the correct dimensionality. 

3. Provide a list of initial values to the sampling function. Instead of relying on `jmpost` to generate the initial values for you, you can instead specify a list of values directly to the sampling function e.g.

```R
initial_values <- list("mu" = c(10, 15), "sigma" = 0.4)
samples <- mp <- sampleStanModel(
    JointModel(...),
    data = DataJoint(...),
    init = initial_values
)
```

However ensuring you have set initial values for all parameters and ensuring you have the parameter names specified correctly can be tricky. To assist with this it is recommend to use the `jmpost` generated initial values as a starting point e.g.

```R
jm <- JointModel(...)
initial_values <- initialValues(jm)
initial_values$mu <- c(10, 15)
samples <- mp <- sampleStanModel(
    jm,
    data = DataJoint(...),
    init = initial_values
)
```










