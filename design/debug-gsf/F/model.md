
## Model Specification



$$
\begin{align*}
y_{ij}  &\sim N(SLD_{ij}, \sigma \cdot SLD_{ij}) \\ \\
SLD_{ij} &= b_i \Big [ (\phi_i e^{-s_it_{j}}) + (1-\phi_i) \cdot e^{g_it_j} \Big] \\ \\
b_i &\sim \text{LogNormal}(\mu_b,\ \sigma_b) \\
s_i &\sim \text{LogNormal}(\mu_s,\ \sigma_s) \\
g_i &\sim \text{LogNormal}(\mu_g,\ \sigma_g) \\
\phi_i &\sim \text{Beta}(a_\phi,\ b_\phi) \\
\end{align*}
$$

Where:
- $i$ is the subject index
- $j$ is the time index
- $y_{ij}$ is the observed SLD value
- $SLD_{ij}$ is the expected SLD value
- $b_i$ is the baseline parameter 
- $s_i$ is the shrinkage parameter
- $g_i$ is the growth parameter¦
- $\phi_i$ is the proportion of responding cells



Priors:
$$
\begin{align*}

\mu_b &\sim \text{LogNormal}(\log(60),\ 0.4) \\
\mu_s &\sim \text{LogNormal}(\log(0.6),\ 0.4) \\
\mu_g &\sim \text{LogNormal}(\log(0.25),\ 0.4) \\
\\
\sigma_b &\sim \text{LogNormal}(\log(0.4),\ 0.3) \\
\sigma_s &\sim \text{LogNormal}(\log(0.2),\ 0.3) \\
\sigma_g &\sim \text{LogNormal}(\log(0.2),\ 0.3) \\
\\

a_\phi &\sim \text{LogNormal}(\log(2),\ 0.3) \\
b_\phi &\sim \text{LogNormal}(\log(2),\ 0.3) \\
\\

\sigma &\sim \text{LogNormal}(\log(0.05),\ 0.3)

\end{align*}
$$









