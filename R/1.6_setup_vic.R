# Data import and processsing

# All data is reprojected (if necessary) to lat/long

source("R/0_utilities.R")
source("R/1.1_setup_proj_boundaries.R")


# ---------------------------------------------------------------------
# Import VIC data

# Import basin-averaged daily VIC data
etqswe_kings_canesm2_hist <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_CanESM2_Historical_Q_ET_SWE_1950-2005.csv")
etqswe_kings_canesm2_45 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_CanESM2_RCP45_Q_ET_SWE_2006-2099.csv")
etqswe_kings_canesm2_85 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_CanESM2_RCP85_Q_ET_SWE_2006-2099.csv")

etqswe_kings_ccsm4_hist <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_CCSM4_Historical_Q_ET_SWE_1950-2005.csv")
etqswe_kings_ccsm4_45 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_CCSM4_RCP45_Q_ET_SWE_2006-2099.csv")
etqswe_kings_ccsm4_85 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_CCSM4_RCP85_Q_ET_SWE_2006-2099.csv")

etqswe_kings_cnrm_hist <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_CNRM-CM5_Historical_Q_ET_SWE_1950-2005.csv")
etqswe_kings_cnrm_45 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_CNRM-CM5_RCP45_Q_ET_SWE_2006-2099.csv")
etqswe_kings_cnrm_85 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_CNRM-CM5_RCP85_Q_ET_SWE_2006-2099.csv")

etqswe_kings_hadgemcc_hist <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_HadGEM2-CC365_Historical_Q_ET_SWE_1950-2005.csv")
etqswe_kings_hadgemcc_45 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_HadGEM2-CC365_RCP45_Q_ET_SWE_2006-2099.csv")
etqswe_kings_hadgemcc_85 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_HadGEM2-CC365_RCP85_Q_ET_SWE_2006-2099.csv")

etqswe_kings_hadgemec_hist <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_HadGEM2-ES365_Historical_Q_ET_SWE_1950-2005.csv")
etqswe_kings_hadgemec_45 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_HadGEM2-ES365_RCP45_Q_ET_SWE_2006-2099.csv")
etqswe_kings_hadgemec_85 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_HadGEM2-ES365_RCP85_Q_ET_SWE_2006-2099.csv")

etqswe_kings_miroc5_hist <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_MIROC5_Historical_Q_ET_SWE_1950-2005.csv")
etqswe_kings_miroc5_45 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_MIROC5_RCP45_Q_ET_SWE_2006-2099.csv")
etqswe_kings_miroc5_85 <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_MIROC5_RCP85_Q_ET_SWE_2006-2099.csv")

# Observed values
etqswe_kings_obs <- readr::read_csv("../../VIC_Modeling_SS/BasinAvg/Kings_Obs_Q_ET_SWE_1950-2011.csv")

# Consolidate basin-averaged daily data to lists
etqswe_kings <- list(
  kings_canesm2_hist=etqswe_kings_canesm2_hist,
  kings_ccsm4_hist=etqswe_kings_ccsm4_hist,
  kings_cnrm_hist=etqswe_kings_cnrm_hist,
  kings_hadgemcc_hist=etqswe_kings_hadgemcc_hist,
  kings_hadgemec_hist=etqswe_kings_hadgemec_hist,
  kings_miroc5_hist=etqswe_kings_miroc5_hist,
  kings_canesm2_45=etqswe_kings_canesm2_45,kings_canesm2_85=etqswe_kings_canesm2_85,
  kings_ccsm4_45=etqswe_kings_ccsm4_45,kings_ccsm4_85=etqswe_kings_ccsm4_85,
  kings_cnrm_45=etqswe_kings_cnrm_45,kings_cnrm_85=etqswe_kings_cnrm_85,
  kings_hadgemcc_45=etqswe_kings_hadgemcc_45,kings_hadgemcc_85=etqswe_kings_hadgemcc_85,
  kings_hadgemec_45=etqswe_kings_hadgemec_45,kings_hadgemec_85=etqswe_kings_hadgemec_85,
  kings_miroc5_45=etqswe_kings_miroc5_45,kings_miroc5_85=etqswe_kings_miroc5_85,
  kings_magicalgcm_obs=etqswe_kings_obs
)


# ----
# Sierra (aka WY average ET and Peak SWE - distributed)

wy_et_canesm2_hist <- readr::read_csv("../../VIC_Modeling_SS/Sierra//CanESM2_Historical_1951_2005_WY_ET.csv")
wy_et_canesm2_45 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//CanESM2_RCP45_2007_2099_WY_ET.csv")
wy_et_canesm2_85 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//CanESM2_RCP85_2007_2099_WY_ET.csv")
wy_et_ccsm4_hist <- readr::read_csv("../../VIC_Modeling_SS/Sierra//CCSM4_Historical_1951_2005_WY_ET.csv")
wy_et_ccsm4_45 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//CCSM4_RCP45_2007_2099_WY_ET.csv")
wy_et_ccsm4_85 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//CCSM4_RCP85_2007_2099_WY_ET.csv")
wy_et_cnrm_hist <- readr::read_csv("../../VIC_Modeling_SS/Sierra//CNRM-CM5_Historical_1951_2005_WY_ET.csv")
wy_et_cnrm_45 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//CNRM-CM5_RCP45_2007_2099_WY_ET.csv")
wy_et_cnrm_85 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//CNRM-CM5_RCP85_2007_2099_WY_ET.csv")
wy_et_hadgemcc_hist <- readr::read_csv("../../VIC_Modeling_SS/Sierra//HadGEM2-CC365_Historical_1951_2005_WY_ET.csv")
wy_et_hadgemcc_45 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//HadGEM2-CC365_RCP45_2007_2099_WY_ET.csv")
wy_et_hadgemcc_85 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//HadGEM2-CC365_RCP85_2007_2099_WY_ET.csv")
wy_et_hadgemec_hist <- readr::read_csv("../../VIC_Modeling_SS/Sierra//HadGEM2-ES365_Historical_1951_2005_WY_ET.csv")
wy_et_hadgemec_45 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//HadGEM2-ES365_RCP45_2007_2099_WY_ET.csv")
wy_et_hadgemec_85 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//HadGEM2-ES365_RCP85_2007_2099_WY_ET.csv")
wy_et_miroc5_hist <- readr::read_csv("../../VIC_Modeling_SS/Sierra//MIROC5_Historical_1951_2005_WY_ET.csv")
wy_et_miroc5_45 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//MIROC5_RCP45_2007_2099_WY_ET.csv")
wy_et_miroc5_85 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//MIROC5_RCP85_2007_2099_WY_ET.csv")

wy_swe_canesm2_hist <- readr::read_csv("../../VIC_Modeling_SS/Sierra//CanESM2_Historical_1951_2005_WY_PEAK_SWE.csv")
wy_swe_canesm2_45 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//CanESM2_RCP45_2007_2099_WY_PEAK_SWE.csv")
wy_swe_canesm2_85 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//CanESM2_RCP85_2007_2099_WY_PEAK_SWE.csv")
wy_swe_ccsm4_hist <- readr::read_csv("../../VIC_Modeling_SS/Sierra//CCSM4_Historical_1951_2005_WY_PEAK_SWE.csv")
wy_swe_ccsm4_45 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//CCSM4_RCP45_2007_2099_WY_PEAK_SWE.csv")
wy_swe_ccsm4_85 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//CCSM4_RCP85_2007_2099_WY_PEAK_SWE.csv")
wy_swe_cnrm_hist <- readr::read_csv("../../VIC_Modeling_SS/Sierra//CNRM-CM5_Historical_1951_2005_WY_PEAK_SWE.csv")
wy_swe_cnrm_45 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//CNRM-CM5_RCP45_2007_2099_WY_PEAK_SWE.csv")
wy_swe_cnrm_85 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//CNRM-CM5_RCP85_2007_2099_WY_PEAK_SWE.csv")
wy_swe_hadgemcc_hist <- readr::read_csv("../../VIC_Modeling_SS/Sierra//HadGEM2-CC365_Historical_1951_2005_WY_PEAK_SWE.csv")
wy_swe_hadgemcc_45 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//HadGEM2-CC365_RCP45_2007_2099_WY_PEAK_SWE.csv")
wy_swe_hadgemcc_85 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//HadGEM2-CC365_RCP85_2007_2099_WY_PEAK_SWE.csv")
wy_swe_hadgemec_hist <- readr::read_csv("../../VIC_Modeling_SS/Sierra//HadGEM2-ES365_Historical_1951_2005_WY_PEAK_SWE.csv")
wy_swe_hadgemec_45 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//HadGEM2-ES365_RCP45_2007_2099_WY_PEAK_SWE.csv")
wy_swe_hadgemec_85 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//HadGEM2-ES365_RCP85_2007_2099_WY_PEAK_SWE.csv")
wy_swe_miroc5_hist <- readr::read_csv("../../VIC_Modeling_SS/Sierra//MIROC5_Historical_1951_2005_WY_PEAK_SWE.csv")
wy_swe_miroc5_45 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//MIROC5_RCP45_2007_2099_WY_PEAK_SWE.csv")
wy_swe_miroc5_85 <- readr::read_csv("../../VIC_Modeling_SS/Sierra//MIROC5_RCP85_2007_2099_WY_PEAK_SWE.csv")


vic_maps <- list(
  wy_et_canesm2_hist=wy_et_canesm2_hist,
  wy_et_canesm2_45=wy_et_canesm2_45,
  wy_et_canesm2_85=wy_et_canesm2_85,
  wy_et_ccsm4_hist=wy_et_ccsm4_hist,
  wy_et_ccsm4_45=wy_et_ccsm4_45,
  wy_et_ccsm4_85=wy_et_ccsm4_85,
  wy_et_cnrm_hist=wy_et_cnrm_hist,
  wy_et_cnrm_45=wy_et_cnrm_45,
  wy_et_cnrm_85=wy_et_cnrm_85,
  wy_et_hadgemcc_hist=wy_et_hadgemcc_hist,
  wy_et_hadgemcc_45=wy_et_hadgemcc_45,
  wy_et_hadgemcc_85=wy_et_hadgemcc_85,
  wy_et_hadgemec_hist=wy_et_hadgemec_hist,
  wy_et_hadgemec_45=wy_et_hadgemec_45,
  wy_et_hadgemec_85=wy_et_hadgemec_85,
  wy_et_miroc5_hist=wy_et_miroc5_hist,
  wy_et_miroc5_45=wy_et_miroc5_45,
  wy_et_miroc5_85=wy_et_miroc5_85,
  
  wy_swe_canesm2_hist=wy_swe_canesm2_hist,
  wy_swe_canesm2_45=wy_swe_canesm2_45,
  wy_swe_canesm2_85=wy_swe_canesm2_85,
  wy_swe_ccsm4_hist=wy_swe_ccsm4_hist,
  wy_swe_ccsm4_45=wy_swe_ccsm4_45,
  wy_swe_ccsm4_85=wy_swe_ccsm4_85,
  wy_swe_cnrm_hist=wy_swe_cnrm_hist,
  wy_swe_cnrm_45=wy_swe_cnrm_45,
  wy_swe_cnrm_85=wy_swe_cnrm_85,
  wy_swe_hadgemcc_hist=wy_swe_hadgemcc_hist,
  wy_swe_hadgemcc_45=wy_swe_hadgemcc_45,
  wy_swe_hadgemcc_85=wy_swe_hadgemcc_85,
  wy_swe_hadgemec_hist=wy_swe_hadgemec_hist,
  wy_swe_hadgemec_45=wy_swe_hadgemec_45,
  wy_swe_hadgemec_85=wy_swe_hadgemec_85,
  wy_swe_miroc5_hist=wy_swe_miroc5_hist,
  wy_swe_miroc5_45=wy_swe_miroc5_45,
  wy_swe_miroc5_85=wy_swe_miroc5_85
)
