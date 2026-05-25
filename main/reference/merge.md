# `merge`

Merge two `StanModule` or `ParameterList` objects.

## Usage

``` r
merge(x, y, ...)

# S4 method for class 'StanModule,StanModule'
merge(x, y, stan_blocks = STAN_BLOCKS, ...)

# S4 method for class 'ParameterList,ParameterList'
merge(x, y)

# S4 method for class 'StanModel,NULL'
merge(x, y, ...)

# S4 method for class 'NULL,StanModel'
merge(x, y, ...)

# S4 method for class 'StanModule,NULL'
merge(x, y, ...)

# S4 method for class 'NULL,StanModule'
merge(x, y, ...)

# S4 method for class 'ParameterList,NULL'
merge(x, y, ...)

# S4 method for class 'NULL,ParameterList'
merge(x, y, ...)

# S4 method for class 'NULL,NULL'
merge(x, y, ...)
```

## Arguments

- x:

  first module.

- y:

  second module.

- ...:

  additional arguments.

- stan_blocks:

  (`list`) reference list of stan blocks.
