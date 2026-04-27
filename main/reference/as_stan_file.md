# Merging Code Blocks into Stan Code Character Vector

Merging Code Blocks into Stan Code Character Vector

## Usage

``` r
as_stan_file(
  functions = "",
  data = "",
  transformed_data = "",
  parameters = "",
  transformed_parameters = "",
  model = "",
  generated_quantities = "",
  stan_blocks = STAN_BLOCKS
)
```

## Arguments

- functions:

  (`character`)\
  code block.

- data:

  (`character`)\
  code block.

- transformed_data:

  (`character`)\
  code block.

- parameters:

  (`character`)\
  code block.

- transformed_parameters:

  (`character`)\
  code block.

- model:

  (`character`)\
  code block.

- generated_quantities:

  (`character`)\
  code block.

- stan_blocks:

  (`list`)\
  reference list of stan blocks.

## Value

Character vector of the complete Stan code.
