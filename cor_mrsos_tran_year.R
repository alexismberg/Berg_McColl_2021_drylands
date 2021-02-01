################################
#.libPaths("/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
#library(ncdf, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(colorRamps, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(maps, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(ncdf4, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(fields, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(akima, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(LSD, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(RColorBrewer, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")

### All models
list_models_evspsbl_PCMDI <- c("ACCESS1-0", "ACCESS1-3", "bcc-csm1-1", "bcc-csm1-1-m", "BNU-ESM",  "CCSM4", "CESM1-BGC", "CESM1-CAM5", "CESM1-CAM5-1-FV2", "CESM1-FASTCHEM", "CESM1-WACCM", "CMCC-CESM", "CMCC-CM", "CMCC-CMS", "CNRM-CM5", "CNRM-CM5-2", "CSIRO-Mk3-6-0","CanCM4", "CanESM2", "FGOALS-g2", "FGOALS-s2", "FIO-ESM", "GFDL-CM3", "GFDL-ESM2G", "GFDL-ESM2M", "GISS-E2-H", "GISS-E2-H-CC", "GISS-E2-R", "GISS-E2-R-CC", "HadCM3", "HadGEM2-AO", "HadGEM2-CC", "HadGEM2-ES", "inmcm4", "IPSL-CM5A-LR", "IPSL-CM5A-MR", "IPSL-CM5B-LR", "MIROC-ESM", "MIROC-ESM-CHEM", "MIROC4h", "MIROC5", "MPI-ESM-LR", "MPI-ESM-MR", "MPI-ESM-P", "MRI-CGCM3", "MRI-ESM1", "NorESM1-M", "NorESM1-ME")
list_models_evspsbl_PCMDI2 <- c("ACCESS1_0", "ACCESS1_3", "bcc_csm1_1", "bcc_csm1_1_m", "BNU_ESM",  "CCSM4", "CESM1_BGC", "CESM1_CAM5", "CESM1_CAM5_1_FV2", "CESM1_FASTCHEM", "CESM1_WACCM", "CMCC_CESM", "CMCC_CM", "CMCC_CMS", "CNRM_CM5", "CNRM_CM5_2", "CSIRO_Mk3_6_0","CanCM4", "CanESM2", "FGOALS_g2", "FGOALS_s2", "FIO_ESM", "GFDL_CM3", "GFDL_ESM2G", "GFDL_ESM2M", "GISS_E2_H", "GISS_E2_H_CC", "GISS_E2_R", "GISS_E2_R_CC", "HadCM3", "HadGEM2_AO", "HadGEM2_CC", "HadGEM2_ES", "inmcm4", "IPSL_CM5A_LR", "IPSL_CM5A_MR", "IPSL_CM5B_LR", "MIROC_ESM", "MIROC_ESM_CHEM", "MIROC4h", "MIROC5", "MPI_ESM_LR", "MPI_ESM_MR", "MPI_ESM_P", "MRI_CGCM3", "MRI_ESM1", "NorESM1_M", "NorESM1_ME")


### List of models for which we have good ET partitioning - that is, the sum of Tran+Esoil+canopy interception is ~equal to ET:
list_models_goodpartitionET_fut <- c("bcc-csm1-1", "bcc-csm1-1-m", "BNU-ESM","CCSM4",   "CESM1-BGC","CESM1-CAM5", "CanESM2","FIO-ESM",
  "GFDL-ESM2G", "GFDL-ESM2M", "GISS-E2-H","GISS-E2-H-CC", "GISS-E2-R","GISS-E2-R-CC", "inmcm4", "IPSL-CM5A-LR",
 "IPSL-CM5A-MR", "IPSL-CM5B-LR", "MIROC-ESM","MIROC-ESM-CHEM", "MIROC5", "MRI-CGCM3","NorESM1-M","NorESM1-ME"  )
### Underscore instead of hyphen:
list_models_goodpartitionET_fut2 <- c("bcc_csm1_1", "bcc_csm1_1_m", "BNU_ESM","CCSM4",   "CESM1_BGC","CESM1_CAM5", "CanESM2","FIO_ESM", 
  "GFDL_ESM2G", "GFDL_ESM2M", "GISS_E2_H","GISS_E2_H_CC", "GISS_E2_R","GISS_E2_R_CC", "inmcm4", "IPSL_CM5A_LR",
 "IPSL_CM5A_MR", "IPSL_CM5B_LR", "MIROC_ESM","MIROC_ESM_CHEM", "MIROC5", "MRI_CGCM3","NorESM1_M","NorESM1_ME"  )

### List of models for which we have both soil moisture (mrsos) and good tran
list_models_tran_and_mrsos_PCMDI_fut <- c("bcc-csm1-1", "bcc-csm1-1-m", "BNU-ESM","CCSM4",   "CESM1-BGC","CESM1-CAM5", "CanESM2","GFDL-ESM2G",
  "GFDL-ESM2M", "GISS-E2-H","GISS-E2-H-CC", "GISS-E2-R",  "GISS-E2-R-CC", "inmcm4", "IPSL-CM5A-LR", "IPSL-CM5A-MR",
 "IPSL-CM5B-LR", "MIROC-ESM","MIROC-ESM-CHEM", "MIROC5", "MRI-CGCM3","NorESM1-M","NorESM1-ME")
list_models_tran_and_mrsos_PCMDI_fut2 <- c("bcc_csm1_1", "bcc_csm1_1_m", "BNU_ESM","CCSM4",   "CESM1_BGC","CESM1_CAM5", "CanESM2","GFDL_ESM2G",
  "GFDL_ESM2M", "GISS_E2_H","GISS_E2_H_CC", "GISS_E2_R",  "GISS_E2_R_CC", "inmcm4", "IPSL_CM5A_LR", "IPSL_CM5A_MR",
 "IPSL_CM5B_LR", "MIROC_ESM","MIROC_ESM_CHEM", "MIROC5", "MRI_CGCM3","NorESM1_M","NorESM1_ME")


#### Loading Tran:
for (m in 1:length(list_models_goodpartitionET_fut)){
print(list_models_goodpartitionET_fut[m])
load(paste("tran_",list_models_goodpartitionET_fut2[m],"_year.RData", sep=""))
assign(paste("tran_",list_models_goodpartitionET_fut2[m],"_year",sep=""), bob) }

### Let's create mrso and evspsbl in 2x2 for every model, and calculate correlation:
for (m in 1:length(list_models_evspsbl_PCMDI2)){
if (list_models_evspsbl_PCMDI[m] %in% list_models_tran_and_mrsos_PCMDI_fut) {
lat <- get(paste("lat_", list_models_evspsbl_PCMDI2[m], sep=""))
lon <- get(paste("lon_", list_models_evspsbl_PCMDI2[m], sep=""))
load(paste("mrsos_",list_models_evspsbl_PCMDI2[m],"_year.RData", sep=""));mrsos_year <- bob
tran_year <- get(paste("tran_",list_models_evspsbl_PCMDI2[m],"_year", sep=""))
#####
tran_year[which(is.na(tran_year)==T)] <- 0 ; mrsos_year[which(is.na(mrsos_year)==T)] <- 0
mrsos_year_2x2 <- array(NA, dim=c(181,91,56))
tran_year_2x2 <- array(NA, dim=c(181,91,56));tran_year_2x2 <- array(NA, dim=c(181,91,56))
print("regridding year")
for (t in 1:56){
mrsos_year_2x2[,,t] <- bicubic.grid(lon, lat, mrsos_year[,,t], xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
tran_year_2x2[,,t] <- bicubic.grid(lon, lat, tran_year[,,t], xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z   } 
print("computing correlation")
for (i in 1:181){
for (j in 1:91) {
if (mrsos_year_2x2[i,j,1]!=0) {corr_mrsos_tran_year_2x2[i,j] <- cor(mrsos_year_2x2[i,j,21:50],tran_year_2x2[i,j,21:50], use="complete.obs")}}}}
assign(paste("corr_mrsos_tran_year_", list_models_evspsbl_PCMDI2[m],"_2x2", sep=""), corr_mrsos_tran_year_2x2) }


corr_mrsos_tran_year_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_tran_and_mrsos_PCMDI_fut2)))
for (m in 1:length(list_models_tran_and_mrsos_PCMDI_fut2)){
print(list_models_tran_and_mrsos_PCMDI_fut2[m])
corr_mrsos_tran_year_2x2 <- get(paste("corr_mrsos_tran_year_",list_models_tran_and_mrsos_PCMDI_fut2[m],"_2x2", sep=""))
corr_mrsos_tran_year_2x2_allmodels[,,m] <- corr_mrsos_tran_year_2x2*mask_2x2_NAs
rm(corr_mrsos_tran_year_2x2)}


##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
################ Future:


### Loading Tran:
for (m in 1:length(list_models_goodpartitionET_fut)){
print(list_models_goodpartitionET_fut[m])
load(paste("tran_",list_models_goodpartitionET_fut2[m],"_year_rcp85.RData", sep=""))
assign(paste("tran_",list_models_goodpartitionET_fut2[m],"_year_rcp85",sep=""), tran_year_rcp85)
}

### Let's create mrso and evspsbl in 2x2 for every model and calculate correlation:
for (m in 1:length(list_models_evspsbl_PCMDI2)){
corr_mrsos_tran_year_rcp85_2x2 <- array(NA, dim=c(181,91))
if ((list_models_evspsbl_PCMDI[m] %in% list_models_goodpartitionET_fut) && (list_models_evspsbl_PCMDI[m] %in% list_models_mrsos_PCMDI_fut)){
print(list_models_evspsbl_PCMDI2[m])
lat <- get(paste("lat_", list_models_evspsbl_PCMDI2[m], sep=""))
lon <- get(paste("lon_", list_models_evspsbl_PCMDI2[m], sep=""))
load(paste("mrsos_",list_models_evspsbl_PCMDI2[m],"_year_rcp85.RData", sep=""));mrsos_year_rcp85 <- bob
tran_year_rcp85 <- get(paste("tran_",list_models_evspsbl_PCMDI2[m],"_year_rcp85", sep=""))
#####
tran_year_rcp85[which(is.na(tran_year_rcp85)==T)] <- 0 ; mrsos_year_rcp85[which(is.na(mrsos_year_rcp85)==T)] <- 0
mrsos_year_rcp85_2x2 <- array(NA, dim=c(181,91,30))
tran_year_rcp85_2x2 <- array(NA, dim=c(181,91,30))
print("regridding year_rcp85")
for (t in 1:30){
mrsos_year_rcp85_2x2[,,t] <- bicubic.grid(lon, lat, mrsos_year_rcp85[,,t], xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
tran_year_rcp85_2x2[,,t] <- bicubic.grid(lon, lat, tran_year_rcp85[,,t], xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z   }
print("computing correlation")
for (i in 1:181){
for (j in 1:91) {
if ((mrsos_year_rcp85_2x2[i,j,1]!=0) && ( mrsos_year_rcp85_2x2[i,j,1]!=0)) {corr_mrsos_tran_year_rcp85_2x2[i,j] <- cor(mrsos_year_rcp85_2x2[i,j,],tran_year_rcp85_2x2[i,j,], use="complete.obs")}}}}
assign(paste("corr_mrsos_tran_year_rcp85_", list_models_evspsbl_PCMDI2[m],"_2x2", sep=""), corr_mrsos_tran_year_rcp85_2x2) }

corr_mrsos_tran_year_rcp85_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_tran_and_mrsos_PCMDI_fut2)))
for (m in 1:length(list_models_tran_and_mrsos_PCMDI_fut2)){
print(list_models_tran_and_mrsos_PCMDI_fut2[m])
corr_mrsos_tran_year_rcp85_2x2 <- get(paste("corr_mrsos_tran_year_rcp85_",list_models_tran_and_mrsos_PCMDI_fut2[m],"_2x2", sep=""))
corr_mrsos_tran_year_rcp85_2x2_allmodels[,,m] <- corr_mrsos_tran_year_rcp85_2x2*mask_2x2_NAs
rm(corr_mrsos_tran_year_rcp85_2x2)}


