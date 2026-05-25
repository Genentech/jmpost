## Code to prepare datasets based on training code presented at https://rconis.github.io/tgi-os-training/
## Extracted from _load_data.dm
## Downloaded from
### https://github.com/RCONIS/tgi-os-training/blob/5591861acdebc4f70c5747316b6c684b6c5ea8eb/session-os/_load_data.qmd
## Created by Daniel Sabanes Bove and Francois Mercier

## Datasets were published as supplment to:
## Ghaffari Laleh N, Loeffler CML, Grajek J, Staňková K, Pearson AT, Muti HS, et al. (2022)
##  Classical mathematical models for prediction of response to chemotherapy and immunotherapy.
##  PLoS Comput Biol 18(2): e1009822. https://doi.org/10.1371/journal.pcbi.1009822

library(dplyr)
library(janitor)
library(readxl)
library(here)
library(fuzzyjoin)
library(forcats)
tumor_data <- here("data-raw/journal.pcbi.1009822.s006.xlsx") |>
    read_excel(sheet = "Study4") |>
    clean_names() |>
    mutate(
        id = factor(as.character(patient_anonmyized)),
        day = as.integer(treatment_day),
        year = day / 365.25,
        target_lesion_long_diam_mm = case_match(
            target_lesion_long_diam_mm,
            "TOO SMALL TO MEASURE" ~ "2",
            "NOT EVALUABLE" ~ NA_character_,
            .default = target_lesion_long_diam_mm
        ),
        sld = as.numeric(target_lesion_long_diam_mm),
        sld = ifelse(sld == 0, 2, sld),
        study = factor(gsub("^Study_(\\d+)_Arm_\\d+$", "\\1", study_arm)),
        arm = factor(gsub("^Study_\\d+_Arm_(\\d+)$", "\\1", study_arm)),
        arm = fct_recode(arm, "Docetaxel" = "1", "MPDL3280A" = "2")
    ) |>
    select(id, year, sld, arm)


head(tumor_data)

os_data <- here("data-raw/41591_2018_134_MOESM3_ESM.xlsx") |>
    read_excel(sheet = "OAK_Clinical_Data") |>
    clean_names() |>
    rename(
        age = bage,
        race = race2,
        ecog = ecoggr,
        arm = trt01p,
        response = bcor
    ) |>
    mutate(
        id = factor(as.character(pt_id)),
        sld = as.numeric(na_if(bl_sld, ".")),
        pfs_time = as.numeric(pfs) / 12,
        pfs_event = as.numeric(pfs_cnsr) == 0,
        os_time = as.numeric(os) / 12,
        os_event = as.numeric(os_cnsr) == 0,
        race = factor(race),
        ecog = factor(ecog),
        arm = factor(arm),
        response = factor(
            response,
            levels = c("CR", "PR", "SD", "PD", "NE")
        ),
        sex = factor(sex)
    ) |>
    select(
        id,
        arm,
        ecog,
        age,
        race,
        sex,
        sld,
        response,
        pfs_time,
        pfs_event,
        os_time,
        os_event
    )

head(os_data)

get_baseline <- function(sld, year) {
    which_base <- tail(which(year <= 0), 1L)
    if (length(which_base) == 0) {
        which_base <- which.min(year)
    }
    sld[which_base]
}

get_contig_below_thresh <- function(sld, year, bsld, thresh) {
    rle_res <- with(
        rle(((sld[year > 0] - bsld) / bsld) < 0.2),
        lengths[values]
    )
    if (length(rle_res) == 0) {
        0
    } else {
        max(rle_res)
    }
}

tumor_data_summary <- tumor_data |>
    group_by(id) |>
    arrange(year) |>
    summarize(
        arm = arm[1L],
        bsld = get_baseline(sld, year),
        last_year = tail(year, 1L),
        nadir = min(sld[year >= 0], na.rm = TRUE),
        max_cfn = max((sld[year >= 0] - nadir) / nadir, na.rm = TRUE),
        min_cfb = min((sld[year >= 0] - bsld) / bsld, na.rm = TRUE),
        contig_below_0.2 = get_contig_below_thresh(
            sld,
            year,
            bsld,
            thresh = 0.2
        ),
        approx_response = case_when(
            min_cfb <= -0.3 ~ "PR",
            contig_below_0.2 >= 2 ~ "SD",
            max_cfn >= 0.2 ~ "PD",
            .default = "NE"
        )
    )
head(tumor_data_summary)

os_data_keys <- os_data |>
    select(id, arm, sld, pfs_time, os_time, response)
head(os_data_keys)


dist_match <- function(v1, v2) {
    dist <- abs(v1 - v2)
    data.frame(include = (dist <= 0.05))
}

less_match <- function(lower, upper) {
    data.frame(include = lower <= upper)
}

tumor_os_data_joined <- tumor_data_summary |>
    fuzzy_left_join(
        os_data_keys,
        by = c(
            "arm" = "arm",
            "bsld" = "sld",
            "last_year" = "os_time",
            "approx_response" = "response"
        ),
        match_fun = list(
            `==`,
            dist_match,
            less_match,
            `==`
        )
    )
nrow(tumor_os_data_joined)


tumor_os_data_joined_subset <- tumor_os_data_joined |>
    na.omit() |>
    filter(!duplicated(id.y)) |>
    filter(!duplicated(id.x))
nrow(tumor_os_data_joined_subset)

tgi_os_join_keys <- tumor_os_data_joined_subset |>
    select(id.x, id.y, arm.x) |>
    rename(
        id_tgi = id.x,
        id_os = id.y,
        arm = arm.x
    )
table(tgi_os_join_keys$arm)


tumor_data <- tumor_data |>
    inner_join(
        tgi_os_join_keys,
        by = c("id" = "id_tgi", "arm" = "arm")
    ) |>
    select(-id) |>
    rename(id = id_os)

os_data <- os_data |>
    inner_join(
        tgi_os_join_keys,
        by = c("id" = "id_os", "arm" = "arm")
    ) |>
    select(-id_tgi)

usethis::use_data(tumor_data, overwrite = TRUE)
usethis::use_data(os_data, overwrite = TRUE)
