
# Longitudinal Model Specifications



## Random Slope Model

$$
y_{ij} = \mu_0 + s_i t_{ij} + \epsilon_{ij}
$$

where:

- $y_{ij}$ is the tumour size for subject $i$ at timepoint $j$
- $\mu_0$ is the intercept
- $s_i$ is the slope for subject $i$ and that $s_i \sim N(\mu_s, \sigma_s)$
- $\mu_s$ is the mean for all subjects slopes
- $\sigma_s$ is the variance term for these slopes
- $\epsilon_{ij}$ is the random error and that $\epsilon_{ij} \sim N(0, \sigma)$

### Link Contribution

$$
C(t \mid \tau,  s_i) = \tau s_i
$$

Where:

- $s_i$ is the subjects random slope coeficient
- $\tau$ is a global scaling coeficient