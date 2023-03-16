





## Hazard Link contribution from longmodel

```
matrix link_contribution(matrix time, matrix pars_lm)
```

- must return same dimensions as time
- where time is 1 row per subject (Nind) and 1 column per required timepoint (nodes from the guasian quadrature) (i.e. if multiple timepoints need to be evaluated for each subject it will still be 1 row but with multiple columns, if only 1 timepoint needs to be evaluated then there will only be 1 column)
- pars_lm is a matrix with 1 row per subject and as many columns as you need defined in transformed_parameters()


Examples for time

1   4  12  15    <-  subject 1
1   4  12  15    <-  subject 2
1   4  12  15    <-  subject 3
1   4  12  15
1   4  12  15


## Specifying hazard distributions

```
log_h0(matrix time, row_vector pars_os)
```

- must return same dimensions as time
- where time is 1 row per subject and x columns per timepoints for the same subject
- pars_os is a row_vector which you define in the transformed_parameters() block













