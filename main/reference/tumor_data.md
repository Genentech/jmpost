# OAK Study Tumor Growth Data

Longitudinal tumor size measurements from the OAK clinical trial, a
Phase III study comparing atezolizumab (MPDL3280A) with docetaxel in
patients with previously treated non-small cell lung cancer (NSCLC).
Patients were matched to those in
[os_data](https://genentech.github.io/jmpost/reference/os_data.md) using
baseline SLD, treatment arm, and best overall response. Only patients
with at least 3 tumor size measurements are included.

## Usage

``` r
tumor_data
```

## Format

A `tibble` with one row per tumor measurement and 4 columns:

- id:

  Patient identifier (factor), matched to
  [os_data](https://genentech.github.io/jmpost/reference/os_data.md)

- year:

  Time of measurement in years from treatment start (numeric)

- sld:

  Sum of longest diameters of target lesions in mm (numeric)

- arm:

  Treatment arm (factor): `"Docetaxel"` or `"MPDL3280A"`

## Source

Tumor size measurements extracted from the S1 dataset of: Ghaffari Laleh
N, Loeffler CML, Grajek J, Staňková K, Pearson AT, Muti HS, et al.
(2022). Classical mathematical models for prediction of response to
chemotherapy and immunotherapy. *PLoS Comput Biol* 18(2): e1009822.
<https://doi.org/10.1371/journal.pcbi.1009822>

Data preparation code adapted from: Sabanes Bove D and Mercier F.
<https://rconis.github.io/tgi-os-training/>

## See also

[os_data](https://genentech.github.io/jmpost/reference/os_data.md)
