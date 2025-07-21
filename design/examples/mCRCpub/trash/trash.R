<!--

    ## Comparing {flexsurv}, {survstan} and {jmpost}

    ### Data preparation

    ### Flexsurv implementation

    The details of the exponential model implementation in {flexsurv} are
provided in the reference article
([DOI](https://www.jstatsoft.org/article/view/v070i08)). The exponential
distribution has a single parameter and only supports a hazard that is
constant over time. The hazard is equal to the rate parameter.

```{r, eval=FALSE, echo=TRUE}
flexsurv.exp<-flexsurv::flexsurvreg(Surv(recyrs, censrec)~group, data=bc, dist="exp")
flexsurv.exp
```

The same model in {survstan} would be implemented as follow:

    ```{r, eval=FALSE, echo=TRUE}
survstan.exp<-survstan::phreg(Surv(recyrs, censrec)~group, data=bc, dist="exponential")
summary(survstan.exp)

```


As shown in the below table, these various model calls provide
consistent outputs. More details on the {jmpost} outputs are provided in
the separate vignette ABC123.

```{r, file="expon-ph-benchmark.R", echo=FALSE}
library(knitr)
library(kableExtra)

# Create table
altogether %>%
    kable(escape = F) %>%
    kable_styling()
```

Both `jmpost` and `survstan` implement these analysis in Stan, a
probabilistic programming language supporting Bayesian approach.

-->
