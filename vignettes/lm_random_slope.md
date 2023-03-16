
# Longitudinal Model Specifications



## Random Slope Model

$$
y_{ij} = \mu_0 + s_i t_{ij} + \epsilon_{ij}
$$

where:

- $y_{ij}$ is the tumour size for subject $i$ at timepoint $j$
- $\mu_0$ is the intercept
- $s_i$ is the random slope for subject $i$ with $s_i \sim N(\mu_{sk}, \sigma_s)$
- $\mu_{sk}$ is the mean for the random slope per treatment arm $k$
- $\sigma_s$ is the variance term for these slopes
- $\epsilon_{ij}$ is the random error and that $\epsilon_{ij} \sim N(0, \sigma)$

### Log-Hazard Contribution

$$
C_i(t \mid \tau,  s_i) = \tau s_i
$$

Where:

- $t$ is time
- $s_i$ is the subjects random slope coeficient
- $\tau$ is a global scaling coeficient

That is to say a subjects hazard is constant where that constant is a multiple of their random slope

