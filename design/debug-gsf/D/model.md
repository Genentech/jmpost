
## Model Specification



$$
\begin{align*}
y_{ij}  &\sim N(SLD_{ij}, \sigma \cdot SLD_{ij}) \\ \\
SLD_{ij} &= b_i \Big [ (\phi_i e^{-s_it_{j}}) + (1-\phi_i) \cdot e^{g_it_j} \Big] \\ \\
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
\phi_i &\sim \text{Beta}(3, 3) \\
s_i &\sim \text{LogNormal}\big(\log(0.4),\ 0.5\big) \\
g_i &\sim \text{LogNormal}\big(\log(0.4),\ 0.5\big) \\
b_i &\sim \text{LogNormal}\big(\log(60),\ 1\big) 
\end{align*}
$$

That is to say that $\phi_i$, $s_i$, $b_i$ and $g_i$ are being modeled as independent parameters per each subject.








