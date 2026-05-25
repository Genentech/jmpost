# `StanModule` -\> `character`

Converts a
[`StanModule`](https://genentech.github.io/jmpost/reference/StanModule-class.md)
object into a valid Stan program file where each line of the returned
`character` vector represents a line of the program

## Usage

``` r
# S3 method for class 'StanModule'
as.character(x, ...)
```

## Arguments

- x:

  (`StanModule`) A stan program

- ...:

  Not Used.

## See also

Other StanModule:
[`StanModule-class`](https://genentech.github.io/jmpost/reference/StanModule-class.md),
[`as.list.StanModule()`](https://genentech.github.io/jmpost/reference/as.list.StanModule.md),
[`as_print_string.StanModule()`](https://genentech.github.io/jmpost/reference/as_print_string.StanModule.md)
