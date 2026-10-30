# Read a SAS Dataset from a Zip Archive

Read a SAS Dataset from a Zip Archive

## Usage

``` r
read_sas7bdat_from_zip(zip_file, file)
```

## Arguments

- zip_file:

  (`string`)\
  path to the zip archive.

- file:

  (`string`)\
  name of the `.sas7bdat` file within the archive.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
as returned by
[`haven::read_sas()`](https://haven.tidyverse.org/reference/read_sas.html).

## Examples

``` r
zip_file <- system.file("extdata", "prime_subset.zip", package = "jmpost")
adsl <- read_sas7bdat_from_zip(zip_file, "adsl_pds2019.sas7bdat")
```
