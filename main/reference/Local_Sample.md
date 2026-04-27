# Stub functions for sampling from distributions

These functions only exist so that they can be mocked during unit tests
in order to provide deterministic values. In most cases these are just
straight forward pass throughs for the underlying distributions.

## Usage

``` r
local_rnorm(...)

local_rcauchy(...)

local_rgamma(...)

local_rlnorm(...)

local_rbeta(...)

local_runif(...)

local_rt(n, nu, mu, sigma)

local_rlogis(...)

local_rloglogis(n, alpha, beta)

local_rinvgamma(n, alpha, beta)
```

## Arguments

- ...:

  Pass any additional arguments to the underlying distribution.

- nu:

  (`number`)\
  Parameter for underlying distribution.

- mu:

  (`number`)\
  Parameter for underlying distribution.

- sigma:

  (`number`)\
  Parameter for underlying distribution.

- alpha:

  (`number`)\
  Parameter for underlying distribution.

- beta:

  (`number`)\
  Parameter for underlying distribution.

## Details

### Log-Logistic

There is no log-logistic sampling function within base R so it was
implemented in terms of sampling from the CDF distribution. Using the
Stan parameterisation the CDF is defined as: \$\$ u = F(x) =
\frac{1}{1 + (x/ \alpha)^{-\beta}} \$\$ The inverse of this function is:
\$\$ x = ((u / (1 - u))^(1 / beta)) \* alpha \$\$

Thus we can sample u from a \\Uni(0, 1)\\ distribution and then derive x
from this.

### Inverse-Gamma

The inverse Gamma distribution is defined as 1/Gamma thus we calculate
this simply by sampling sampling from the Gamma distribution and then
taking the reciprocal.

### Student-t

R's sampling functions only produce the standard Student-t distribution
so in order to match Stan's implementation we multiply by the scale
parameter and add the location parameter. See this [Stack
Overflow](https://stats.stackexchange.com/a/623611) post for details
