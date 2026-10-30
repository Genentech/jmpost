# Copy a Connection to a Temporary File

Copy a Connection to a Temporary File

## Usage

``` r
connection_to_file(connection)
```

## Arguments

- connection:

  (`connection`)\
  a connection, opened or not. If it is not open it is opened in binary
  mode and closed again afterwards.

## Value

The path to a temporary file containing the connection's bytes.
