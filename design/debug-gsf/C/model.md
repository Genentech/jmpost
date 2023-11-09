
## Model Specification



$$
y_{ij}  \sim N(SLD_{ij}, \sigma \cdot SLD_{ij})
$$

$$
SLD_{ij} = b_i \Big [ \frac{1}{2}e^{-st_{j}} + \frac{1}{2}e^{gt_j} \Big]
$$

Where:
- $i$ is the subject index
- $j$ is the time index
- $y_{ij}$ is the observed SLD value
- $SLD_{ij}$ is the expected SLD value
- $b_i$ is the baseline parameter (this is a known parameter and not estimated by the model)
- $s$ is the shrinkage parameter
- $g$ is the growth parameter


Priors:
$$
\begin{align*}
s &\sim \text{LogNormal}\big(\log(0.4),\ 0.5\big) \\
g &\sim \text{LogNormal}\big(\log(0.4),\ 0.5\big) \\
\end{align*}
$$








