# Implements the Stein-Fojo Model 


## Reference

- DOI: 10.1634/theoncologist.2008-0016;

## Longitudinal Model

$$
y_{ij} \sim \mathcal{N}(SLD_{ij}, SLD_{ij}^2 \sigma^2)
$$

Where:

* $y_{ij}$ is the observed tumour mesasurements
* $SLD_{ij}$ is the expected sum of longest diameter for subject $i$ at time point $j$


### Expected SLD

$$
SLD_{ij} =b_{i}
\left[
    e^{-s_{i}t_{ij}}+
    e^ {g_{i}t_{ij}} - 1
\right]
$$

Where: 

* $i$ is the subject index
* $j$ is the visit index
* $t_{ij}$ is the time since first treatment for subject $i$ at visit $j$
* $SLD_{ij}$ is the observed SLD measurement for subject $i$ at visit $j$
* $b_i$ is the Baseline SLD measurement
* $s_i$ is the kinetics shrinkage parameter
* $g_i$ is the kinetics tumour growth parameter


### Expected SLD Parameters

$$
\begin{aligned}
s_i &= \exp\left(
    \ln(m_{s l_i}) + \eta_{s i} * \omega_{s}
\right) 
\\
g_i &= \exp\left(
    \ln(m_{g l_i}) + \eta_{g i} * \omega_{g}
\right) 
\\
b_i &= \exp\left(
    \ln(m_{b l_i}) + \eta_{b i} * \omega_{b}
\right)
\end{aligned}
$$


Where:

* $i$ is the subject index
* $l_i$ is the group/treatment index for subject $i$
* $m_{xl_i}$ is the mean for parameter $x$ in group $l_i$
* $\eta_{xi}$ is a random effects offset on parameter $x$ for subject $i$
* $\omega_{x}$ is the variance for the random effects on parameter $x$


### Expected SLD Hyper-parameters


$$
\begin{aligned}
m_{s l_i} &\sim \text{Lognormal}(\mu_s, \sigma_{s}) \\
m_{g l_i} &\sim \text{Lognormal}(\mu_g, \sigma_{g}) \\
\end{aligned}
$$

Where:

* $l_i$ is the group/treatment index for subject $i$
* $\mu_x$ is the mean of the parameter distribution
* $\sigma_x$ is the variance of the parameter distribution


### Default Priors

#TODO - Double check this is accurate

$$
\begin{aligned}
\eta_{bi} &\sim \mathcal{N}(0,5) \\
\eta_{si} &\sim \mathcal{N}(0,5) \\
\eta_{gi} &\sim \mathcal{N}(0,5) \\
\\
\omega_{b} &\sim \text{Lognormal}(0,1);\\
\omega_{s} &\sim \text{Lognormal}(0,1);\\
\omega_{g} &\sim \text{Lognormal}(0,1);\\
\\
\mu_{bi} &\sim \text{Lognormal}(55,5); \\
\\
\mu_s &\sim \text{Lognormal}(1,0.5); \\
\mu_g &\sim \text{Lognormal}(-0.36,1); \\ 
\\
\sigma_s &\sim \text{Lognormal}(0,0.5); \\
\sigma_g &\sim \text{Lognormal}(0,0.5); \\
\end{aligned}
$$


## Link Contribution

$$
C(t \mid b_i, s_i, g_i) = \sum_{k=1}^p \tau_k G_k(t \mid b_i, s_i, g_i)
$$

Where:

* $b_i$ is the Baseline SLD measurement
* $s_i$ is the kinetics shrinkage parameter
* $g_i$ is the kinetics tumour growth parameter
* $G_k(.)$ are arbitrary functions of the SLD parameters. Several available functions provided by this package are listed below
* $\tau_k$ is the scaling coeficient for $G_k(.)$




### Derivative of the SLD Trajectory (dsld-link)

    
$$
G(t \mid b_i, s_i, g_i, ) = b_i 
\left[
    g_i  e^{g_i  t} - 
    s_i  e^{-s_i  t}
\right]
$$

Where:

* $b_i$ is the Baseline SLD measurement
* $s_i$ is the SLD kinetics shrinkage parameter
* $g_i$ is the SLD kinetics tumour growth parameter



### Time to Growth (ttg-link)


$$
G(t \mid b_i, s_i, g_i) = \frac{
    \text{log}(s_i) - \text{log}(g_i)
}{
    s_i + g_i
}
$$

Where:

* $G_i$ is the time to growth for subject $i$
* $b_i$ is the Baseline SLD measurement
* $s_i$ is the kinetics shrinkage parameter
* $g_i$ is the kinetics tumour growth parameter



