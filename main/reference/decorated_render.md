# `decorated_render`

Simple wrapper around
[`jinjar::render()`](https://davidchall.github.io/jinjar/reference/render.html)
that provides some additional default variables about the system (avoids
each call to jinjar having to specify them)

## Usage

``` r
decorated_render(...)
```

## Arguments

- ...:

  Arguments passed onto
  [`jinjar::render()`](https://davidchall.github.io/jinjar/reference/render.html)

## Value

See
[`jinjar::render()`](https://davidchall.github.io/jinjar/reference/render.html)
