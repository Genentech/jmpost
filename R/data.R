#' OAK Study Tumor Growth Data
#'
#' Longitudinal tumor size measurements from the OAK clinical trial, a
#' Phase III study comparing atezolizumab (MPDL3280A) with docetaxel in
#' patients with previously treated non-small cell lung cancer (NSCLC).
#' Patients were matched to those in [os_data] using baseline SLD, treatment
#' arm, and best overall response. Only patients with at least 3 tumor size
#' measurements are included.
#'
#' @format A tibble with one row per tumor measurement and 4 columns:
#' \describe{
#'   \item{id}{Patient identifier (factor), matched to [os_data]}
#'   \item{year}{Time of measurement in years from treatment start (numeric)}
#'   \item{sld}{Sum of longest diameters of target lesions in mm (numeric)}
#'   \item{arm}{Treatment arm (factor): `"Docetaxel"` or `"MPDL3280A"`}
#' }
#' @source Tumor size measurements extracted from the S1 dataset of:
#'   Ghaffari Laleh N, Loeffler CML, Grajek J, Staňková K, Pearson AT,
#'   Muti HS, et al. (2022). Classical mathematical models for prediction of
#'   response to chemotherapy and immunotherapy.
#'   *PLoS Comput Biol* 18(2): e1009822.
#'   <https://doi.org/10.1371/journal.pcbi.1009822>
#'
#'   Data preparation code adapted from:
#'   Sabanes Bove D and Mercier F.
#'   <https://rconis.github.io/tgi-os-training/>
#' @seealso [os_data]
"tumor_data"

#' OAK Study Overall Survival Data
#'
#' Baseline covariates and survival outcomes from the OAK clinical trial, a
#' Phase III study comparing atezolizumab (MPDL3280A) with docetaxel in
#' patients with previously treated non-small cell lung cancer (NSCLC).
#' Patients were matched to those in [tumor_data] using baseline SLD, treatment
#' arm, and best overall response.
#'
#' @format A tibble with one row per patient and 12 columns:
#' \describe{
#'   \item{id}{Patient identifier (factor), matched to [tumor_data]}
#'   \item{arm}{Treatment arm (factor): `"Docetaxel"` or `"MPDL3280A"`}
#'   \item{ecog}{ECOG performance status at baseline (factor): `0` or `1`}
#'   \item{age}{Age at baseline in years (numeric)}
#'   \item{race}{Patient race (factor)}
#'   \item{sex}{Patient sex (factor): `"F"` or `"M"`}
#'   \item{sld}{Baseline sum of longest diameters of target lesions in mm (numeric)}
#'   \item{response}{Best overall response (factor): `"CR"`, `"PR"`, `"SD"`, `"PD"`, or `"NE"`}
#'   \item{pfs_time}{Progression-free survival time in years (numeric)}
#'   \item{pfs_event}{Progression-free survival event indicator (logical): `TRUE` = event observed}
#'   \item{os_time}{Overall survival time in years (numeric)}
#'   \item{os_event}{Overall survival event indicator (logical): `TRUE` = death observed}
#' }
#' @source Supplementary Table 8 from:
#'   Rittmeyer A, Barlesi F, Waterkamp D, et al. (2017). Atezolizumab versus
#'   docetaxel in patients with previously treated non-small-cell lung cancer
#'   (OAK): a phase 3, open-label, multicentre randomised controlled trial.
#'   *The Lancet* 389(10066): 255–265.
#'   <https://doi.org/10.1038/s41591-018-0134-3>
#'
#'   Data preparation code adapted from:
#'   Sabanes Bove D and Mercier F.
#'   <https://rconis.github.io/tgi-os-training/>
#' @seealso [tumor_data]
"os_data"
