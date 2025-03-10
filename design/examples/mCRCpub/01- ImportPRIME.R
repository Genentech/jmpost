#' ####################################################
#'
#'   Data preparation
#'   PRIME study (from PDS)
#'
#'   Q3-2024
#'   Francois Mercier
#'
#' ####################################################


#' sessionInfo()

#' ----------------------------------
#' Structure analysis data set:
#' ----------------------------------
#' For time to event sub-model (event.df):
#' ***************************
#' SUBJID (chr)
#' STUDY (chr)
#' ATRT (chr)
#'
#' EVENTYR (num) - Time to event (in years)
#' EVENTFL (0/1) - Flag =1 if the ind died, 0 if censored
#'
#' For longi (TGI) sub-model (biom.df):
#' ***************************
#' SUBJID (chr)
#' STUDY (chr)
#' ATRT (chr)
#'
#' BIOMVAL (num) - Biomarker (here, SLD in mm) value
#' BIOMYR (num) - Biomarker measurement time (in years)
#' ----------------------------------


#' ===============================================
#' Import and Select
#' ===============================================================

#' Event data frame
#' -----------------
kras<-haven::read_sas("./design/examples/mCRCpub/data/PRIME/biomark_pds2019.sas7bdat")
kras0<-kras |> select("SUBJID", "BMMTR1")

adsl<-haven::read_sas("./design/examples/mCRCpub/data/PRIME/adsl_pds2019.sas7bdat")
event.df0<-adsl |>
    left_join(kras0, by="SUBJID") |>
    filter(BMMTR1=="Wild-type") |>
    mutate(STUDY="1", EVENTYR=DTHDY/365.25, EVENTFL=DTH) |>
  select("SUBJID", "STUDY", "ATRT", "EVENTYR", "EVENTFL")

UID.event0<-unique(event.df0$SUBJID)
#' length(UID.event0)
#' 514


#' Biomarker (SLD) data frame
#' -----------------
adtr<-haven::read_sas("./design/examples/mCRCpub/data/PRIME/adls_pds2019.sas7bdat")
biom.df0<-adtr |>
  filter(LSCAT=="Target lesion", !is.na(LSSLD)) |>
  mutate(STUDY="1", BIOMYR=VISITDY/365.25, BIOMVAL=LSSLD) |>
  group_by(SUBJID, VISITDY) |> slice(1) |> ungroup() |>
  select("SUBJID", "BIOMYR", "BIOMVAL")

UID.biom0<-unique(biom.df0$SUBJID)
#' length(UID.biom0)
#' 488


#' Retain matching patients
#' -----------------
retainID<-intersect(UID.event0, UID.biom0)
#' 263

event.df<-event.df0 |> filter(SUBJID %in% retainID)
#' length(unique(event.df$SUBJID))
#' 263
#' saveRDS(event.df, file="./design/examples/mCRCpub/data/PRIME/PRIMEOSads.rds")

desn0<-event.df0 |>
    filter(SUBJID %in% retainID) |>
    select(SUBJID, STUDY, ATRT)

biom.df<-biom.df0 |>
    filter(SUBJID %in% retainID) |>
    left_join(desn0, by="SUBJID")
#' length(unique(biom.df$SUBJID))
#' 263
#' saveRDS(biom.df, file="./design/examples/mCRCpub/data/PRIME/PRIMETGIads.rds")





