
## Model Specification



$$
\begin{align*}
y_{ij}  &\sim N(SLD_{ij}, \sigma) \\ \\


SLD_{ij} &= b_i \Big [ 
     e^{-s_it_{j}} + 
     e^{g_it_j} - 
     1
\Big] \\ \\


b_i =& \exp(LB_i) \\
s_i =& \exp(KS_i) \\
g_i =& \exp(LG_i) \\
\\

LB_i &\sim N(\mu_b, \sigma_b) \\
LS_i &\sim N(\mu_s, \sigma_s) \\
LG_i &\sim N(\mu_g, \sigma_g) \\

\end{align*}
$$

Where:
- $i$ is the subject index
- $j$ is the time index
- $y_{ij}$ is the observed SLD value
- $SLD_{ij}$ is the expected SLD value
- $b_i$ is the baseline parameter 
- $s_i$ is the shrinkage parameter
- $g_i$ is the growth parameter
- $\phi_i$ is the proportion of responding cells



Priors:
$$
\begin{align*}
\mu_b &\sim \text{LogNormal}\big(\log(60),\ 0.5\big) \\
\mu_s &\sim \text{LogNormal}\big(\log(0.6),\ 0.3\big) \\
\mu_g &\sim \text{LogNormal}\big(\log(0.2),\ 0.3\big) \\
 \\
\sigma_{b} & \sim N(0, 1) \\
\sigma_{s} & \sim N(0, 0.3) \\
\sigma_{g} & \sim N(0, 0.3) \\
\\
\sigma &\sim \text{LogNormal}\big(\log(0.3),\ 0.2\big)
\end{align*}
$$









