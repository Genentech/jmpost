




## Model Specification

Fixed effects only model, that is we have no patient level random effects. We have no study or arm level hierarchical effects. There is no censoring and we only account for strictly positive time.


$$
\begin{align*}
y_i &\sim \mathcal{N} \left( \mu_i, \mu_i^2 \sigma^2 \right) \\
\\
\mu_i =
b \cdot &\exp \left( g t_{i} - \frac{p}{c} \left( 1 - e^{-c t_{i}} \right) \right) 
\end{align*}
$$

- $i$ is the observation index

### Priors

$$
\begin{align*}
b &\sim \text{LogNormal} \left( \right) \\
g &\sim \text{LogNormal} \left( \right) \\
p &\sim \text{LogNormal} \left( \right) \\
c &\sim \text{LogNormal} \left( \right) \\
\sigma &\sim \text{LogNormal} \left( \right)
\end{align*}
$$





