# OAK Study Overall Survival Data

Baseline covariates and survival outcomes from the OAK clinical trial, a
Phase III study comparing atezolizumab (MPDL3280A) with docetaxel in
patients with previously treated non-small cell lung cancer (NSCLC).
Patients were matched to those in
[tumor_data](https://genentech.github.io/jmpost/reference/tumor_data.md)
using baseline SLD, treatment arm, and best overall response.

## Usage

``` r
os_data
```

## Format

A `tibble` with one row per patient and 12 columns:

- id:

  Patient identifier (factor), matched to
  [tumor_data](https://genentech.github.io/jmpost/reference/tumor_data.md)

- arm:

  Treatment arm (factor): `"Docetaxel"` or `"MPDL3280A"`

- ecog:

  ECOG performance status at baseline (factor): `0` or `1`

- age:

  Age at baseline in years (numeric)

- race:

  Patient race (factor)

- sex:

  Patient sex (factor): `"F"` or `"M"`

- sld:

  Baseline sum of longest diameters of target lesions in mm (numeric)

- response:

  Best overall response (factor): `"CR"`, `"PR"`, `"SD"`, `"PD"`, or
  `"NE"`

- pfs_time:

  Progression-free survival time in years (numeric)

- pfs_event:

  Progression-free survival event indicator (logical): `TRUE` = event
  observed

- os_time:

  Overall survival time in years (numeric)

- os_event:

  Overall survival event indicator (logical): `TRUE` = death observed

## Source

Supplementary Table 8 from: Rittmeyer A, Barlesi F, Waterkamp D, et al.
(2017). Atezolizumab versus docetaxel in patients with previously
treated non-small-cell lung cancer (OAK): a phase 3, open-label,
multicentre randomised controlled trial. *The Lancet* 389(10066):
255–265. <https://doi.org/10.1038/s41591-018-0134-3>

Data preparation code adapted from: Sabanes Bove D and Mercier F.
<https://rconis.github.io/tgi-os-training/>

## See also

[tumor_data](https://genentech.github.io/jmpost/reference/tumor_data.md)
