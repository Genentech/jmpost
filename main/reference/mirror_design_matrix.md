# Build design matrix for prediction data

This function takes a `DataSurvival` object and a `data.frame` object
and generates a design matrix for the `data.frame` that has the
identical structure to the design matrix of the `DataSurvival` object.

This is used for predicting new data using a model that was trained on a
different original data source

## Usage

``` r
mirror_design_matrix(olddata, newdata)
```

## Arguments

- olddata:

  (`DataSurvival`) The original data to be used as a template for the
  new data

- newdata:

  (`data.frame`) The new data to be used to generate the design matrix
