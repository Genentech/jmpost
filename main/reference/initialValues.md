# `initialValues`

Obtain the `list` of initial values to be passed to the Stan sampler.

## Usage

``` r
initialValues(object, ...)

# S3 method for class 'StanModel'
initialValues(object, n_chains, ...)

# S3 method for class 'LinkComponent'
initialValues(object, n_chains, ...)

# S3 method for class 'Link'
initialValues(object, ...)

# S3 method for class 'JointModel'
initialValues(object, n_chains, ...)
```

## Arguments

- object:

  where to get the initial values from.

- ...:

  Not currently used.

- n_chains:

  the number of initial values to generate. See details.

## Details

There are multiple ways of specifying initial values to Stan, see the
`init` argument in
[cmdstanr::model-method-sample](https://mc-stan.org/cmdstanr/reference/model-method-sample.html)
for full details. Within this package we supply initial values via a
list of lists where each inner list contains the initial values for a
single chain. As such the `n_chains` argument specifies the number of
inner lists to generate.

See the Vignette for further details of how to specify initial values.

## See also

Other LinkComponent:
[`LinkComponent-class`](https://genentech.github.io/jmpost/reference/LinkComponent-class.md),
[`as.StanModule.LinkComponent()`](https://genentech.github.io/jmpost/reference/as.StanModule.LinkComponent.md),
[`as.list.LinkComponent()`](https://genentech.github.io/jmpost/reference/as.list.LinkComponent.md),
[`getParameters()`](https://genentech.github.io/jmpost/reference/getParameters.md)
