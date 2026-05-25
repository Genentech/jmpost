# Parameter Getter Functions

Getter functions for the slots of a
[`Parameter`](https://genentech.github.io/jmpost/reference/Parameter-class.md)
object

## Usage

``` r
# S3 method for class 'Parameter'
names(x)

# S3 method for class 'Parameter'
initialValues(object, ...)

# S3 method for class 'Parameter'
size(object)
```

## Arguments

- x:

  (`Paramater`) A model parameter

- object:

  (`Paramater`) A model parameter

- ...:

  Not used.

## Functions

- `names(Parameter)`: The parameter's name

- `initialValues(Parameter)`: The parameter's initial values

- `size(Parameter)`: The parameter's dimensionality

## See also

Other Parameter:
[`Parameter-class`](https://genentech.github.io/jmpost/reference/Parameter-class.md),
[`as.StanModule.Parameter()`](https://genentech.github.io/jmpost/reference/as.StanModule.Parameter.md),
[`as.character.Parameter()`](https://genentech.github.io/jmpost/reference/as.character.Parameter.md),
[`as_stan_list.Parameter()`](https://genentech.github.io/jmpost/reference/as_stan_list.Parameter.md)
