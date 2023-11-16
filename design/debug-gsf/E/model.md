
## Model Specification



$$
\begin{align*}
y_{ij}  &\sim N(SLD_{ij}, \sigma \cdot SLD_{ij}) \\ \\
SLD_{ij} &= b_i \Big [ (\phi_i e^{-s_it_{j}}) + (1-\phi_i) \cdot e^{g_it_j} \Big] \\ \\
b_i =& \exp(\log(\mu_b) + \eta_{bi})\\ 
s_i =& \exp(\log(\mu_s) + \eta_{si})\\ 
g_i =& \exp(\log(\mu_s) + \eta_{gi}) \\ 
\phi_i =& \text{logit}^{-1}(\text{logit}(\mu_\phi) + \eta_{\phi i}) \\
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
\mu_\phi &\sim \text{Beta}(3, 3) \\ \\
 
\eta_{bi} & \sim N(0, 1) \\
\eta_{si} & \sim N(0, 0.3) \\
\eta_{gi} & \sim N(0, 0.3) \\
\eta_{\phi i} & \sim N(0, 2) \\\\
\sigma &\sim \text{LogNormal}\big(\log(0.3),\ 0.2\big)
\end{align*}
$$









