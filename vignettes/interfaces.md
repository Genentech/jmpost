





## Hazard Link contribution from longmodel

```
matrix link_contribution(matrix time, matrix pars_lm)
```

- must return same dimensions as time
- where time is 1 row per subject and x columns per timepoints for the same subject
- pars_lm is a matrix with 1 row per subject and as many columns as you need defined in transformed_parameters()




## Specifying hazard distributions

```
log_h0(matrix time, row_vector pars_os)
```

- must return same dimensions as time
- where time is 1 row per subject and x columns per timepoints for the same subject
- pars_os is a row_vector which you define in the transformed_parameters() block













