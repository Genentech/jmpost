# `Link`

Simple container class to enable the use of multiple link components in
a joint model. Note that the constructor of this object is idempotent
e.g. `Link(Link(x)) == Link(x)`

## Usage

``` r
Link(...)
```

## Arguments

- ...:

  ([`LinkComponent`](https://genentech.github.io/jmpost/reference/LinkComponent-class.md)
  or
  [`PromiseLinkComponent`](https://genentech.github.io/jmpost/reference/PromiseLinkComponent-class.md))\
  an arbitrary number of link components.

## Slots

- `components`:

  (`list`)\
  a list of
  [`LinkComponent`](https://genentech.github.io/jmpost/reference/LinkComponent-class.md)
  or
  [`PromiseLinkComponent`](https://genentech.github.io/jmpost/reference/PromiseLinkComponent-class.md)
  objects.

- `resolved`:

  (`logical`)\
  indicates if all the `components` have been resolved.

## See also

Other Link:
[`as.StanModule.Link()`](https://genentech.github.io/jmpost/reference/as.StanModule.Link.md),
[`as.list.Link()`](https://genentech.github.io/jmpost/reference/as.list.Link.md),
[`length.Link()`](https://genentech.github.io/jmpost/reference/length.Link.md)

## Examples

``` r
Link(
    linkDSLD(),
    linkTTG()
)
#> 
#> Link with the following components/parameters:
#>     link_dsld ~ normal(mu = 0, sigma = 2)
#>     link_ttg ~ normal(mu = 0, sigma = 2)
```
