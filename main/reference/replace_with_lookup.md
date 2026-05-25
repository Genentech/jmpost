# Replace Character Size by Looked Up Numbers

Replace Character Size by Looked Up Numbers

## Usage

``` r
replace_with_lookup(sizes, data)
```

## Arguments

- sizes:

  (`list`) may include character elements that correspond to names in
  the data list.

- data:

  (`list`) data containing numeric values.

## Value

A list of sizes with character elements in `sizes` replaced by their
corresponding numeric values in `data`.

## Details

An attribute `array` for each returned list element indicates whether
the parameter needs to be handled as an array. This is the case when the
size is larger than 1, or when the size was looked up in the `data`,
because in that case it is flexible and hence is handled as an array in
the Stan code.

## Note

Each element in the final list of sizes must be a single number.
