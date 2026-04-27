# Add Missing Stan Blocks

Add Missing Stan Blocks

## Usage

``` r
add_missing_stan_blocks(x, stan_blocks = STAN_BLOCKS)
```

## Arguments

- x:

  (`list`)\
  list of Stan code blocks

- stan_blocks:

  (`list`)\
  reference list of stan blocks.

## Value

Amended list `x` such that all blocks in the global variable
`STAN_BLOCKS` are contained.
