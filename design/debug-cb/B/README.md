




## Model Specification

Simple by-patient Random effects model. We have no study or arm level hierarchical effects. There is no censoring and we only account for strictly positive time.


$$
y_{ij} \sim \mathcal{N} \left( \mu_{ij}, \mu_{ij}^2 \sigma^2 \right) \\
\\
\\
\mu_{ij} =
b_i \cdot \exp \left( g_i t_{ij} - \frac{p_i}{c_i} \left( 1 - e^{-c_i t_{ij}} \right) \right) 
$$

- $i$ is the patient index
- $j$ is the time index

### Priors

$$
b_i \sim \text{LogNormal} \left( log(\mu_b) , \sigma_b \right) \\
g_i \sim \text{LogNormal} \left( log(\mu_g) , \sigma_g \right) \\
p_i \sim \text{LogNormal} \left( log(\mu_p) , \sigma_p \right) \\
c_i \sim \text{LogNormal} \left( log(\mu_c) , \sigma_c \right)
\\
\mu_b \sim \text{LogNormal} \left( \right) \\
\mu_g \sim \text{LogNormal} \left( \right) \\
\mu_p \sim \text{LogNormal} \left( \right) \\
\mu_c \sim \text{LogNormal} \left( \right) \\
\\
\sigma_b \sim \text{LogNormal} \left( \right) \\
\sigma_g \sim \text{LogNormal} \left( \right) \\
\sigma_p \sim \text{LogNormal} \left( \right) \\
\sigma_c \sim \text{LogNormal} \left( \right) \\
\\
\sigma \sim \text{LogNormal} \left( \right) \\
$$





