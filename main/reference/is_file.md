# Is String a Valid File?

A utility function to check if a string is a valid file or not. Used to
help address short comings of
[`file.exists()`](https://rdrr.io/r/base/files.html) that will return
`TRUE` for a directory as well as a file.

## Usage

``` r
is_file(filename = NULL)
```

## Arguments

- filename:

  (`string`) file name.
