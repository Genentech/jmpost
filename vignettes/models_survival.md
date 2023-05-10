# Overal Survival Model Specification

## General Overal Survival Model

$$
h_i(t) =
    h_{0}(t | \theta) exp\left(
    \beta X_i +
    C(t \mid \phi_i)
\right)
$$

where:

* $l_{0}(t)$ is the baseline log-hazard function at time $t$ conditioned on parameters $\theta$
* $X_i$ is the matrix of covariates for subject $i$
* $C(t \mid \phi_i)$ is the link contribution function at time $t$ given parameters $\phi_i$ from a corresponding longditudinal

## Weibull Distribution

$$
h_{0}(t \mid \lambda, \gamma) =  \lambda \gamma t^{(\gamma - 1)}
$$

## Log-Logistic Distribution

$$
\begin{aligned}
t &\sim \text{Log-Logistic}(\lambda, p)
\\
\\
f_0(t \mid \lambda, p) &= \frac{
    \lambda p (\lambda t)^{p-1}
} {
    (1 + (\lambda t)^p)^2
}
\\
\\
h_0(t \mid \lambda, p) &= \frac{
    \lambda p (\lambda t) ^{p -1}
}{
    1 + (\lambda t) ^p
}
\\
\\
S_0(t \mid \lambda, p) &= \frac{
    1
}{
    1 + (\lambda t)^p
}
\end{aligned}
$$

Where:

* $f_(t)$ is the probability density function
* $h_0(t)$ is the hazard distribution function
* $S_0(t)$ is the survival distribution function
