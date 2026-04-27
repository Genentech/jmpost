# Simulating Data

``` r

library(jmpost)
#> Registered S3 methods overwritten by 'ggpp':
#>   method                  from   
#>   heightDetails.titleGrob ggplot2
#>   widthDetails.titleGrob  ggplot2
#> CmdStan path set to: /root/.cmdstan/cmdstan-2.38.0
```

The `jmpost` package includes data simulation functionality for the
included joint models. The data simulation is based on specifying the
longitudinal model and the survival model including link parameters.

## Example

``` r

set.seed(129)
sim_data <- SimJointData(
    design = list(
        SimGroup(50, "Arm-A", "Study-X"),
        SimGroup(50, "Arm-B", "Study-X")
    ),
    longitudinal = SimLongitudinalSteinFojo(
        times = c(-2, 1, 30, 90, 150, 210),
        mu_g = log(c(0.005, 0.005)),
        mu_s = log(c(0.06, 0.007)),
        omega_g = 0.3,
        link_dsld = 0.1,
    ),
    survival = SimSurvivalWeibullPH(
        lambda = 1 / 300,
        gamma = 0.97,
        time_max = 2000,
        time_step = 1,
        lambda_cen = 1 / 9000,
        beta_cat = c(
            "A" = 0,
            "B" = -0.1,
            "C" = 0.5
        ),
        beta_cont = 0.3
    )
)
```

This object has `survival` and `longitudinal` components.

``` r

head(sim_data@survival)
#> # A tibble: 6 × 7
#>   subject     study   arm    time cov_cont cov_cat event
#>   <chr>       <fct>   <fct> <dbl>    <dbl> <fct>   <dbl>
#> 1 subject_001 Study-X Arm-A    42   -1.12  B           1
#> 2 subject_002 Study-X Arm-A    20   -0.990 C           1
#> 3 subject_003 Study-X Arm-A   832   -1.37  C           1
#> 4 subject_004 Study-X Arm-A   112   -1.36  C           1
#> 5 subject_005 Study-X Arm-A    13    2.00  B           1
#> 6 subject_006 Study-X Arm-A   135    0.696 B           1
head(sim_data@longitudinal)
#> # A tibble: 6 × 6
#>   subject     arm   study    time   sld observed
#>   <chr>       <fct> <fct>   <dbl> <dbl> <lgl>   
#> 1 subject_001 Arm-A Study-X    -2  64.0 TRUE    
#> 2 subject_001 Arm-A Study-X     1  60.2 TRUE    
#> 3 subject_001 Arm-A Study-X    30  26.9 TRUE    
#> 4 subject_001 Arm-A Study-X    90  59.7 FALSE   
#> 5 subject_001 Arm-A Study-X   150 124.  FALSE   
#> 6 subject_001 Arm-A Study-X   210 226.  FALSE
```

We can see the trajectory of the tumour size.

``` r

library(ggplot2)
ggplot(
    sim_data@longitudinal,
    aes(x = time, y = sld, group = subject)
) +
    geom_line(alpha = 0.1) +
    geom_point() +
    facet_wrap(~arm)
```

![](simulating-data_files/figure-html/unnamed-chunk-3-1.png)

We can also visualise the Kaplan-Meier survival curves.

``` r

library(survival)
plot(
    survfit(Surv(time, event) ~ arm, data = sim_data@survival),
    col = 1:2,
    lwd = 2,
    main = "Overall Survival"
)
legend("topright", col = 1:2, lwd = 2, legend = c("Arm-A", "Arm-B"))
```

![](simulating-data_files/figure-html/unnamed-chunk-4-1.png)

## Administrative Cut Off

We may wish to have a fixed maximum study duration and remove any
simulated values after that time. This can be specified as a single
fixed value or as a vector if we additionally wish to consider enrolment
time.

``` r

sim_data_end <- cut_data(sim_data, 700)
plot(
    survfit(Surv(time, event) ~ arm, data = sim_data_end@survival),
    col = 1:2,
    lwd = 2,
    main = "Overall Survival (truncated at time=700)"
)
```

![](simulating-data_files/figure-html/unnamed-chunk-5-1.png)

## Progression

We might also be interested in time to progression defined as an
increase in tumour size over a threshold. The threshold is based on the
minimum observed SLD up to the given time and the relative and absolute
growth. For example we might require the SLD to increase at least 20%
and at least 5mm. We don’t include any observations before time 0 in the
calculation of the minimum and we don’t observe any SLD values after
progression.

``` r

sim_data_pd <- add_pfs(
    sim_data_end,
    relative_threshold = 1.2,
    absolute_threshold = 5,
    from_time = 0,
    observed_after = FALSE
)
```

Let’s look at the plots again up to progression.

``` r

ggplot(
    sim_data_pd@longitudinal |> dplyr::filter(observed), # here we only include observed values
    aes(x = time, y = sld, group = subject)
) +
    geom_line(alpha = 0.1) +
    geom_point() +
    facet_wrap(~arm)
```

![](simulating-data_files/figure-html/unnamed-chunk-7-1.png)

``` r

plot(
    survfit(Surv(pfs_time, pfs_event) ~ arm, data = sim_data_pd@survival),
    col = 1:2,
    lwd = 2,
    main = "Progression Free Survival"
)
legend("topright", col = 1:2, lwd = 2, legend = c("Arm-A", "Arm-B"))
```

![](simulating-data_files/figure-html/unnamed-chunk-8-1.png)

## Model Fitting from Simulated Data

As already descried in the quick start vignette, we can use this data to
fit a joint model.

``` r

os_data <- sim_data_end@survival
long_data <- sim_data_end@longitudinal
joint_data <- DataJoint(
    subject = DataSubject(
        data = os_data,
        subject = "subject",
        arm = "arm",
        study = "study"
    ),
    survival = DataSurvival(
        data = os_data,
        formula = Surv(time, event) ~ cov_cat + cov_cont
    ),
    longitudinal = DataLongitudinal(
        data = long_data,
        formula = sld ~ time,
        threshold = 5
    )
)


sf_model <- JointModel(
    longitudinal = LongitudinalSteinFojo(
        mu_bsld = prior_normal(log(60), 0.2),
        mu_ks = prior_normal(-3, 0.4),
        mu_kg = prior_normal(log(0.005), 0.3),
        centred = TRUE
    ),
    survival = SurvivalWeibullPH(),
    link = linkDSLD()
)
```

``` r

set.seed(202671)
mcmc_results <- sampleStanModel(
    sf_model,
    data = joint_data,
    iter_sampling = 1000,
    iter_warmup = 500,
    chains = 4,
    parallel_chains = 4,
    step_size = 0.01
)
```

``` r

knitr::kable(
    mcmc_results@results$summary(
        c("lm_sf_mu_bsld", "lm_sf_mu_ks", "lm_sf_mu_kg", "lm_sf_omega_bsld", "lm_sf_omega_ks", "lm_sf_omega_ks")
    ),
    digits = 3
)
```

| variable | mean | median | sd | mad | q5 | q95 | rhat | ess_bulk | ess_tail |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| lm_sf_mu_bsld\[1\] | 4.079 | 4.079 | 0.021 | 0.021 | 4.045 | 4.114 | 1.000 | 5987.428 | 2918.645 |
| lm_sf_mu_ks\[1\] | -2.869 | -2.869 | 0.027 | 0.026 | -2.913 | -2.825 | 1.002 | 5410.170 | 2800.191 |
| lm_sf_mu_ks\[2\] | -4.967 | -4.968 | 0.029 | 0.029 | -5.015 | -4.921 | 1.001 | 5287.283 | 3114.357 |
| lm_sf_mu_kg\[1\] | -5.372 | -5.372 | 0.044 | 0.043 | -5.442 | -5.301 | 1.000 | 5527.698 | 2841.944 |
| lm_sf_mu_kg\[2\] | -5.339 | -5.339 | 0.041 | 0.039 | -5.406 | -5.270 | 1.001 | 5421.597 | 2809.312 |
| lm_sf_omega_bsld\[1\] | 0.207 | 0.206 | 0.014 | 0.014 | 0.185 | 0.232 | 1.000 | 5964.946 | 2854.316 |
| lm_sf_omega_ks\[1\] | 0.188 | 0.186 | 0.019 | 0.019 | 0.159 | 0.222 | 1.001 | 5040.261 | 2584.869 |
| lm_sf_omega_ks\[2\] | 0.194 | 0.192 | 0.022 | 0.021 | 0.162 | 0.233 | 1.002 | 4079.301 | 2510.376 |

## Simulating from a Fitted Model

The package also have the functionality to generate new data based on
the fitted model. We use the \[simulate.JointModelSamples\] function.

``` r

?simulate.JointModelSamples
set.seed(198802)
new_model_data <- simulate(
    mcmc_results,
    times = c(-2, 1, 30, 60, 90, 120, 150, 180),
    jitter_var = c(0, 2)
)
```

For this simulation we set

- the times we wish to have longitudinal observations
- variance for jitter around those observation times
- time_max and time_step for numerical integration
- lambda parameter for exponential censoring times
- scaled_variance which should be set to the same as the fitted model.
  This is described more in the statistical specifications vignette.

Now we can inspect the new simulated data.

``` r

head(new_model_data@longitudinal)
#>       subject   arm   study       time      sld observed
#> 1 subject_001 Arm-A Study-X  -2.000000 52.87169     TRUE
#> 2 subject_001 Arm-A Study-X   1.092538 52.32004     TRUE
#> 3 subject_001 Arm-A Study-X  31.020617 19.78334     TRUE
#> 4 subject_001 Arm-A Study-X  60.795321 14.85165     TRUE
#> 5 subject_001 Arm-A Study-X  91.002436 18.72383     TRUE
#> 6 subject_001 Arm-A Study-X 120.535991 25.86408     TRUE
head(new_model_data@survival)
#> # A tibble: 6 × 7
#>   subject     study   arm    time event cov_cat cov_cont
#>   <fct>       <fct>   <fct> <dbl> <dbl> <fct>      <dbl>
#> 1 subject_001 Study-X Arm-A   443     1 B         -1.12 
#> 2 subject_002 Study-X Arm-A   240     1 C         -0.990
#> 3 subject_003 Study-X Arm-A   233     1 C         -1.37 
#> 4 subject_004 Study-X Arm-A   420     1 C         -1.36 
#> 5 subject_005 Study-X Arm-A    18     1 B          2.00 
#> 6 subject_006 Study-X Arm-A   135     1 B          0.696
```

``` r

ggplot(
    dplyr::bind_rows(
        "model sim" = new_model_data@longitudinal,
        "original data" = sim_data@longitudinal,
        .id = "sim"
    ),
    aes(x = time, y = sld, group = subject)
) +
    geom_line(alpha = 0.1) +
    geom_point() +
    facet_wrap(~ arm + sim) +
    coord_cartesian(ylim = c(0, 400)) +
    ggtitle("Original Data versus Model Simulated Data")
```

![](simulating-data_files/figure-html/unnamed-chunk-14-1.png)

``` r

plot(
    survfit(Surv(time, event) ~ arm, data = new_model_data@survival),
    col = 1:2,
    lwd = 2,
    main = "Overall Survival (simulated from model)"
)
lines(
    survfit(Surv(time, event) ~ arm, data = sim_data@survival),
    col = 1:2,
    lwd = 2,
    lty = 2
)
legend("topright",
    col = c(1, 2, 1, 2), lwd = 2, lty = c(1, 1, 2, 2),
    legend = c("Arm-A from model", "Arm-B from model", "Arm-A original", "Arm-B original")
)
```

![](simulating-data_files/figure-html/unnamed-chunk-15-1.png)
