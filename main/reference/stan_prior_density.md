# Render Stan Prior Density Accumulation

Converts the continuous prior statements used by `jmpost` into explicit
log-density evaluations for the generated quantities block.

## Usage

``` r
stan_prior_density(code)
```

## Arguments

- code:

  (`string`) Stan model-block contents containing only prior
  contributions. The caller must remove likelihood contributions before
  calling this function.

## Value

A length-one character vector containing Stan statements that accumulate
the joint log prior in `lprior`.

## Details

Sampling statements of the form `x ~ distribution(arguments);` become
`lprior += distribution_lpdf(x | arguments);`. Distributions with no
arguments, such as `std_normal()`, omit the vertical bar. Explicit
`target +=` statements are redirected to `lprior`, retaining adjustments
such as transformed-prior Jacobians. Line comments starting with `//`
are removed; surrounding braced control flow and local declarations are
retained.

The model block already evaluates these priors for sampling.
Re-evaluating them in generated quantities deliberately adds work once
per saved draw to keep the extra logging calculation out of HMC leapfrog
and gradient evaluations. The original sampling statements also let Stan
omit terms that are constant with respect to the parameters. Computing
`lprior` in transformed parameters and adding it to `target` would avoid
the repeated prior evaluation when saving draws, but would evaluate the
full logged density at every leapfrog step. Likelihood outputs reuse the
existing likelihood evaluations and only add aggregation.

For truncation with scalar bounds (`T[lower, upper]`, including either
one-sided form), the output subtracts the log normalising probability
for each element of `x`. Lower-only truncation uses the log
complementary CDF, upper-only truncation uses the log CDF, and two-sided
truncation uses `log_diff_exp()` of the two log CDFs. Distribution
arguments are accessed elementwise, broadcasting scalars and length-one
vectors. The loop is guarded so that an empty vector contributes zero.
Parameter-dependent normalisation, such as that of a bounded horseshoe
prior, is therefore retained.

This helper handles the well-formed continuous prior syntax emitted by
the package templates, rather than arbitrary Stan programs. Sampling
statements must end in semicolons; control-flow bodies must be braced.
Block comments, discrete distributions and brace-delimited array
literals are unsupported. The caller must declare and initialise
`lprior` and include the `prior_density_size()` and
`prior_density_element()` overloads from `base/functions.stan` when
truncation is present. The original model code is not modified by this
conversion.

## See also

[`stan_density_arguments()`](https://genentech.github.io/jmpost/reference/stan_density_arguments.md)
