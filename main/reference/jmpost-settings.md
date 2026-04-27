# jmpost settings

Define settings that modify the behaviour of the `jmpost` package

Each of the following are the name of options that can be set via:

    options(<option_name> = <value>)

### `jmpost.prior_shrinkage`

Default = `0.5`

By default all initial values are drawn as random sample from the
respective prior distribution with a shrinkage factor towards the mean.
That is:

    initial_value = prior_mean * prior_shrinkage + (1 - prior_shrinkage) * prior_sample

This setting controls the shrinkage factor. A value of 0 means no
shrinkage (i.e. pure random draw) whilst a value of 1 means the initial
value is just the mean.

### `jmpost.cache_dir`

Default = [`tempfile()`](https://rdrr.io/r/base/tempfile.html)

Directory to store compiled stan models in. If not set, a temporary
directory is used for the given R session. Can also be set via the
environment variable `JMPOST_CACHE_DIR`.

### `jmpost.gauss_quad_n`

Default = 15

In most cases the survival function of the joint model does not have a
closed form and as such it is calculated by integrating the hazard
function. `jmpost` estimates this via Gaussian Quadrature, in particular
it uses
[`statmod::gauss.quad`](https://rdrr.io/pkg/statmod/man/gauss.quad.html)
with `kind = "legendre"` to create the nodes and weights.

This option specifies the `n` argument in the call to
[`statmod::gauss.quad`](https://rdrr.io/pkg/statmod/man/gauss.quad.html).
In general higher values of `n` lead to better accuracy of the
approximation but at the cost of increased computational time.

## Usage

``` r
set_options()
```

## Examples

``` r
if (FALSE) { # \dontrun{
options(jmpost.prior_shrinkage = 0.5)
} # }
```
