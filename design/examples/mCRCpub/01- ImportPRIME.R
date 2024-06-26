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

#' ===============================================
#' Import and Select
#' ===============================================================

#' UID values
#' ---------------------------------

kras<-haven::read_sas("./design/examples/mCRCpub/data/biomark_pds2019.sas7bdat")
kras0<-kras |> select("SUBJID", "BMMTR1")

adsl<-haven::read_sas("./design/examples/mCRCpub/data/adsl_pds2019.sas7bdat")
adsl0<-adsl |>
  select("SUBJID", "ATRT", "DTHDY", "DTH") |>
  mutate(STUDY="1", DTHYR=DTHDY/365.25) |>
  left_join(kras0, by="SUBJID") |>
  filter(BMMTR1=="Wild-type")
UIDlist<-unique(adsl0$SUBJID)
#' length(UIDlist)
#' 514

adtr<-haven::read_sas("./design/examples/mCRCpub/data/adls_pds2019.sas7bdat")
keepinadtr0<-c("SUBJID", "VISITDY", "VISIT", "LSSLD")

adtr0<-adtr |>
  filter(LSCAT=="Target lesion") |>
  select(one_of(keepinadtr0)) |>
  mutate(VISITYR=VISITDY/365.25) |>
  group_by(SUBJID, VISITDY) |>
    slice(1) |>
  ungroup() |>
  left_join(adsl0, by="SUBJID") |>
  filter(!is.na(LSSLD), SUBJID %in% UIDlist)
#' length(unique(adtr0$SUBJID))
#' 263

saveRDS(adtr0, file="./design/examples/mCRCpub/data/PRIMEads.rds")



