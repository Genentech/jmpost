# Save a `JointModelSamples` object to a file.

This function is just a wrapper around `saveRDS` that saves the object
to a file ensuring that all of the Stan samples are correctly stored.
Note that as `cmdstanr` objects store their samples as a csv file the
samples may be lost if you call `saveRDS` directly on the object.

## Usage

``` r
# S3 method for class 'JointModelSamples'
saveObject(object, file, ...)
```

## Arguments

- object:

  (`JointModelSamples`) the object to save.

- file:

  (`character`) the file to save the object to.

- ...:

  (`ANY`) additional arguments to
  [`saveRDS`](https://rdrr.io/r/base/readRDS.html).

## See also

Other saveObject:
[`saveObject()`](https://genentech.github.io/jmpost/reference/saveObject.md)
