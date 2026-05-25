# `StanModule` Object and Constructor Function

`StanModule` Object and Constructor Function

## Usage

``` r
StanModule(x = "", ...)
```

## Arguments

- x:

  (`string`) file path to a Stan program or a character vector of Stan
  code to be parsed.

- ...:

  additional arguments passed to the constructor.

## Slots

- `functions`:

  (`character`)\
  the `functions` block.

- `data`:

  (`character`)\
  the `data` block.

- `transformed_data`:

  (`character`)\
  the `transformed_data` block.

- `parameters`:

  (`character`)\
  the `parameters` block.

- `transformed_parameters`:

  (`character`)\
  the `transformed_parameters` block.

- `model`:

  (`character`)\
  the `model` block.

- `generated_quantities`:

  (`character`)\
  the `generated_quantities` block.

## See also

Other StanModule:
[`as.character.StanModule()`](https://genentech.github.io/jmpost/reference/as.character.StanModule.md),
[`as.list.StanModule()`](https://genentech.github.io/jmpost/reference/as.list.StanModule.md),
[`as_print_string.StanModule()`](https://genentech.github.io/jmpost/reference/as_print_string.StanModule.md)
