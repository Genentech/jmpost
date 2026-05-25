# Create Grouped Quantities

This function takes a matrix of quantity samples and aggregates them by
calculating the pointwise average.

## Usage

``` r
collapse_quantities(quantities_raw, collapser)
```

## Arguments

- quantities_raw:

  (`matrix`) of samples with 1 row per sample and 1 column per distinct
  quantity.

- collapser:

  (`QuantityCollapser`) specifies which columns to combine together.

## Details

This function essentially implements the group wise average by
collapsing multiple columns together based on the specification provided
by the `QuantityCollapser` object. The
[Grid-Dev](https://genentech.github.io/jmpost/reference/Grid-Dev.md)
page provides an example of what this function implements
