################################ Libraries needed - eplace with your own paths.
#.libPaths("/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
#library(ncdf, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(colorRamps, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(maps, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(ncdf4, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(fields, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(akima, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(LSD, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")

################################################################################################################################
################################################################################################################################
### Color scale for plots
col_custom <- colorRampPalette(c("darkblue","blue","cyan","white","orange","red","darkred"))


### Variables we read:
list_var <- c("rsds", "rsus", "rlds", "rlus", "sfcWind", "ps", "huss", "tas","pr","evspsbl", "hfss", "hfls", "ts", "mrso", "mrsos")
## Domain:
dom <- c(  rep("atmos", 13), c("land", "land"))


###################### Defining models for which we have enough variables for Penman-Monteith PET
list_models_rsds<-sort(c("GISS-E2-H-CC", "GFDL-CM3", "BNU-ESM", "GFDL-CM2p1", "GISS-E2-R", "NorESM1-ME", "MRI-ESM1", "inmcm4", "CSIRO-Mk3-6-0", "FIO-ESM", "CanCM4", "ACCESS1-0", "MIROC-ESM-CHEM", "IPSL-CM5A-MR", "CESM1-CAM5", "NorESM1-M", "CESM1-FASTCHEM", "CESM1-BGC", "GFDL-ESM2M", "MIROC5", "GFDL-ESM2G", "MRI-CGCM3", "MPI-ESM-P", "GISS-E2-R-CC", "CanESM2", "HadCM3", "MPI-ESM-MR", "CMCC-CESM", "CCSM4", "FGOALS-g2", "GISS-E2-H", "MIROC4h", "IPSL-CM5B-LR", "MIROC-ESM", "bcc-csm1-1", "HadGEM2-ES", "CESM1-WACCM", "IPSL-CM5A-LR", "MPI-ESM-LR", "CMCC-CMS", "bcc-csm1-1-m", "ACCESS1-3", "CMCC-CM", "HadGEM2-CC", "CNRM-CM5"))

list_models_rsus<- sort(c("GISS-E2-H-CC", "GFDL-CM3", "BNU-ESM", "GFDL-CM2p1", "GISS-E2-R", "NorESM1-ME", "MRI-ESM1", "inmcm4", "CSIRO-Mk3-6-0", "FIO-ESM", "CanCM4", "ACCESS1-0", "MIROC-ESM-CHEM", "IPSL-CM5A-MR", "CESM1-CAM5", "NorESM1-M", "CESM1-FASTCHEM", "CESM1-BGC", "GFDL-ESM2M", "MIROC5", "GFDL-ESM2G", "MRI-CGCM3", "MPI-ESM-P", "GISS-E2-R-CC", "CanESM2", "HadCM3", "MPI-ESM-MR", "CMCC-CESM", "CCSM4", "FGOALS-g2", "GISS-E2-H", "MIROC4h", "IPSL-CM5B-LR", "MIROC-ESM", "bcc-csm1-1", "HadGEM2-ES", "CESM1-WACCM", "IPSL-CM5A-LR", "MPI-ESM-LR", "CMCC-CMS", "bcc-csm1-1-m", "ACCESS1-3", "CMCC-CM", "HadGEM2-CC", "CNRM-CM5"))
## we removed MIROC4h from the list, because there is no hfls for it.

list_models_rlds <- sort(c("GISS-E2-H-CC", "GFDL-CM3", "BNU-ESM", "GFDL-CM2p1", "NorESM1-ME", "GISS-E2-R", "MRI-ESM1", "inmcm4", "CSIRO-Mk3-6-0", "CanCM4", "ACCESS1-0", "MIROC-ESM-CHEM", "IPSL-CM5A-MR", "CESM1-CAM5", "NorESM1-M", "CESM1-FASTCHEM", "CESM1-BGC", "GFDL-ESM2M", "MIROC5", "GFDL-ESM2G", "MRI-CGCM3", "CanESM2", "MPI-ESM-P", "GISS-E2-R-CC", "HadCM3", "MPI-ESM-MR", "CMCC-CESM", "CCSM4", "FGOALS-g2", "GISS-E2-H", "MIROC4h", "IPSL-CM5B-LR", "MIROC-ESM", "bcc-csm1-1", "HadGEM2-ES", "CESM1-WACCM", "IPSL-CM5A-LR", "MPI-ESM-LR", "CMCC-CMS", "bcc-csm1-1-m", "ACCESS1-3", "CMCC-CM", "HadGEM2-CC", "CNRM-CM5"))

list_models_rlus <-sort(c("GISS-E2-H-CC", "GFDL-CM3", "BNU-ESM", "GFDL-CM2p1", "NorESM1-ME", "GISS-E2-R", "MRI-ESM1", "inmcm4", "CSIRO-Mk3-6-0", "CanCM4", "ACCESS1-0", "MIROC-ESM-CHEM", "IPSL-CM5A-MR", "CESM1-CAM5", "NorESM1-M", "CESM1-FASTCHEM", "CESM1-BGC", "GFDL-ESM2M", "MIROC5", "GFDL-ESM2G", "MRI-CGCM3", "CanESM2", "MPI-ESM-P", "GISS-E2-R-CC", "HadCM3", "MPI-ESM-MR", "CMCC-CESM", "CCSM4", "FGOALS-g2", "GISS-E2-H", "MIROC4h", "IPSL-CM5B-LR", "MIROC-ESM", "bcc-csm1-1", "HadGEM2-ES", "CESM1-WACCM", "IPSL-CM5A-LR", "MPI-ESM-LR", "CMCC-CMS", "bcc-csm1-1-m", "ACCESS1-3", "CMCC-CM", "HadGEM2-CC", "CNRM-CM5"))


list_models_sfcWind <-sort(c("CMCC-CM", "BNU-ESM", "CMCC-CESM", "MRI-CGCM3", "MPI-ESM-MR", "bcc-csm1-1-m", "HadGEM2-AO", "MPI-ESM-LR", "CESM1-CAM5", "GISS-E2-H-CC", "IPSL-CM5A-MR", "MRI-ESM1", "MIROC5", "CMCC-CMS", "MIROC-ESM", "MIROC-ESM-CHEM", "MPI-ESM-P", "GISS-E2-R-CC", "CanESM2", "GFDL-ESM2M", "HadGEM2-ES", "FGOALS-s2", "GISS-E2-H", "GFDL-CM3", "ACCESS1-0", "HadGEM2-CC", "ACCESS1-3", "HadCM3", "GFDL-ESM2G", "CSIRO-Mk3-6-0", "IPSL-CM5A-LR", "inmcm4", "bcc-csm1-1", "GISS-E2-R", "CNRM-CM5", "IPSL-CM5B-LR"))

list_models_tas <-sort(c("GISS-E2-H-CC", "GFDL-CM3", "BNU-ESM", "GISS-E2-R", "NorESM1-ME", "MRI-ESM1", "inmcm4", "CSIRO-Mk3-6-0", "FIO-ESM", "CanCM4", "ACCESS1-0", "MIROC-ESM-CHEM", "IPSL-CM5A-MR", "CESM1-CAM5", "NorESM1-M", "CESM1-FASTCHEM", "CESM1-BGC", "GFDL-ESM2M", "MIROC5", "GFDL-ESM2G", "MRI-CGCM3", "MPI-ESM-P", "GISS-E2-R-CC", "CanESM2", "HadGEM2-AO", "HadCM3", "MPI-ESM-MR", "CMCC-CESM", "CESM1-CAM5-1-FV2", "CCSM4", "FGOALS-s2", "FGOALS-g2", "GISS-E2-H", "MIROC4h", "IPSL-CM5B-LR", "MIROC-ESM", "bcc-csm1-1", "HadGEM2-ES", "CESM1-WACCM", "IPSL-CM5A-LR", "MPI-ESM-LR", "ACCESS1-3", "bcc-csm1-1-m", "CMCC-CMS", "CMCC-CM", "HadGEM2-CC", "CNRM-CM5"))

list_models_ps <- sort(c("GISS-E2-H-CC", "GFDL-CM3", "BNU-ESM", "GFDL-CM2p1", "GISS-E2-R", "NorESM1-ME", "MRI-ESM1", "inmcm4", "CSIRO-Mk3-6-0", "FIO-ESM", "ACCESS1-0", "MIROC-ESM-CHEM", "IPSL-CM5A-MR", "CESM1-CAM5", "NorESM1-M", "CESM1-FASTCHEM", "CESM1-BGC", "GFDL-ESM2M", "MIROC5", "GFDL-ESM2G", "MRI-CGCM3", "MPI-ESM-P", "GISS-E2-R-CC", "CanESM2", "HadGEM2-AO", "HadCM3", "MPI-ESM-MR", "CMCC-CESM", "CESM1-CAM5-1-FV2", "CCSM4", "FGOALS-s2", "FGOALS-g2", "GISS-E2-H", "IPSL-CM5B-LR", "MIROC-ESM", "bcc-csm1-1", "HadGEM2-ES", "CNRM-CM5-2", "CESM1-WACCM", "IPSL-CM5A-LR", "MPI-ESM-LR", "ACCESS1-3", "bcc-csm1-1-m", "CMCC-CMS", "CMCC-CM", "HadGEM2-CC", "CNRM-CM5"))


list_models_huss <- sort(c("CESM1-CAM5-1-FV2", "GISS-E2-R", "BNU-ESM", "CESM1-BGC", "MRI-CGCM3", "FGOALS-g2", "bcc-csm1-1-m", "HadGEM2-AO", "CESM1-CAM5", "GISS-E2-H-CC", "IPSL-CM5A-MR", "MRI-ESM1", "MIROC5", "NorESM1-ME", "MIROC-ESM", "MIROC-ESM-CHEM", "GISS-E2-R-CC", "CanESM2", "CCSM4", "GFDL-ESM2M", "HadGEM2-ES", "FGOALS-s2", "GISS-E2-H", "GFDL-CM3", "ACCESS1-0", "HadGEM2-CC", "NorESM1-M", "ACCESS1-3", "GFDL-ESM2G", "CESM1-FASTCHEM", "IPSL-CM5A-LR", "inmcm4", "HadCM3", "CSIRO-Mk3-6-0", "bcc-csm1-1", "CESM1-WACCM", "FIO-ESM", "CNRM-CM5", "IPSL-CM5B-LR"))

test_models_common <- array(NA, dim=c(47,8) )
for (m in 1:length(list_models_tas)) {
for (v in 1:8) { # Vars 
print(list_var[v])
list_models <- get(paste("list_models_", list_var[v],sep=""))
if ( list_models_tas[m] %in% list_models) { test_models_common[m,v] <- 1 }
rm(list_models) }}


##### This is the list of models:
list_models_common <- list_models_tas[which(rowSums(test_models_common) ==8)]
## Replacing hyphens by underscores for naming variables: 
list_models_common2 <- c( "ACCESS1_0","ACCESS1_3","bcc_csm1_1",  "bcc_csm1_1_m","BNU_ESM",  "CanESM2",  "CESM1_CAM5",  "CNRM_CM5","CSIRO_Mk3_6_0" , "GFDL_CM3", "GFDL_ESM2G",  "GFDL_ESM2M", "GISS_E2_H","GISS_E2_H_CC","GISS_E2_R","GISS_E2_R_CC" ,"HadCM3","HadGEM2_CC",  "HadGEM2_ES",  "inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC_ESM",  "MIROC_ESM_CHEM", "MIROC5","MRI_CGCM3","MRI_ESM1"  )


####################################################################################################################
###################################### Getting land masks, assigning lon/lat variables

## Models for which we have the land fraction variable:
list_models_sftlf <-  sort(c("CNRM-CM5", "bcc-csm1-1-m", "HadGEM2-ES", "CESM1-WACCM", "FGOALS-s2", "CanAM4", "BNU-ESM", "CSIRO-Mk3-6-0", "GFDL-ESM2G", "ACCESS1-3", "IPSL-CM5B-LR", "HadGEM2-CC", "CMCC-CM", "MIROC4h", "CanESM2", "CESM1-FASTCHEM", "MIROC-ESM-CHEM", "inmcm4", "GFDL-CM3", "MPI-ESM-MR", "GFDL-HIRAM-C180", "MIROC-ESM", "HadCM3", "GISS-E2-H", "CESM1-BGC", "ACCESS1-0", "NorESM1-M", "CMCC-CESM", "CanCM4", "CNRM-CM5-2", "CCSM4", "CMCC-CMS", "MPI-ESM-P", "MRI-CGCM3", "IPSL-CM5A-MR", "MPI-ESM-LR", "CESM1-CAM5", "GFDL-CM2p1", "GISS-E2-R", "MIROC5", "bcc-csm1-1", "FGOALS-g2", "IPSL-CM5A-LR", "GFDL-ESM2M"))
list_models_sftlf2 <-  sort(c("CNRM_CM5", "bcc_csm1_1_m", "HadGEM2_ES", "CESM1_WACCM", "FGOALS_s2", "CanAM4", "BNU_ESM", "CSIRO_Mk3_6_0", "GFDL_ESM2G", "ACCESS1_3", "IPSL_CM5B_LR", "HadGEM2_CC", "CMCC_CM", "MIROC4h", "CanESM2", "CESM1_FASTCHEM", "MIROC_ESM_CHEM", "inmcm4", "GFDL_CM3", "MPI_ESM_MR", "GFDL_HIRAM_C180", "MIROC_ESM", "HadCM3", "GISS_E2_H", "CESM1_BGC", "ACCESS1_0", "NorESM1_M", "CMCC_CESM", "CanCM4", "CNRM_CM5_2", "CCSM4", "CMCC_CMS", "MPI_ESM_P", "MRI_CGCM3", "IPSL_CM5A_MR", "MPI_ESM_LR", "CESM1_CAM5", "GFDL_CM2p1", "GISS_E2_R", "MIROC5", "bcc_csm1_1", "FGOALS_g2", "IPSL_CM5A_LR", "GFDL_ESM2M"))

for (m in 1:length(list_models_sftlf)){       #models 
print(list_models_sftlf[m])
data <-  nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.fixed/.atmos/", list_models_sftlf[m],"/.sftlf.nc/dods", sep=""))
lat <- ncvar_get(data, "lat")
lon<- ncvar_get(data, "lon")
mask  <- ncvar_get(data, "sftlf")
if (max(mask, na.rm=T) == 100) {mask <- mask/100}
mask[which(mask < 0.99)] <-  NA
assign(paste("lat_", list_models_sftlf2[m], sep=""), lat) 
assign(paste("lon_", list_models_sftlf2[m], sep=""), lon) 
nc_close(data)
print("Removing Antartica !!!" )
lowlat <- min(which(lat > -60))
mask[,1:lowlat]<- NA
## let's define Greenland
mask_greenland <- array(1, dim=c(length(lon), length(lat)))
for (i in 1:length(lon)){
for (j in 1:length(lat)){
if ( ( lat[j] > 60) && (lon[i] > 360-60) && ( lon[i] < 335 )) {mask_greenland[i,j] <- NA }
if ( ( lat[j] > 66) && (lon[i] > 360-60) && ( lon[i] < 355 )) {mask_greenland[i,j] <- NA }
if ( ( lat[j] > 70) && (lon[i] > 360-70) && ( lon[i] < 355 )) {mask_greenland[i,j] <- NA }
}}
print("Removing Greenland !!!")
mask <- mask * mask_greenland
assign(paste("mask_",list_models_sftlf2[m], sep=""),mask)
rm(mask);rm(lat);rm(lon); rm(lowlat);rm(mask_greenland)  }
mask_GISS_E2_H_CC <- mask_GISS_E2_H
mask_GISS_E2_R_CC <- mask_GISS_E2_R
lat_GISS_E2_H_CC <- lat_GISS_E2_H
lat_GISS_E2_R_CC <- lat_GISS_E2_R
lon_GISS_E2_H_CC <- lon_GISS_E2_H
lon_GISS_E2_R_CC <- lon_GISS_E2_R

######### Special models
#MRI-ESM1
data <-  nc_open("/n/holystore01/LABS/mccoll_lab/Lab/aberg/DATA/CMIP5/sftlf_fx_MRI-ESM1_esmFixClim1_r0i0p0.nc")
mask  <- ncvar_get(data, "sftlf")/100
mask[which(mask<0.99)] <-  NA
lat <- ncvar_get(data, "lat")
lon<- ncvar_get(data, "lon")
assign("lon_MRI_ESM1", lon) 
assign("lat_MRI_ESM1", lat) 
nc_close(data)
## removing Antartica !!!
lowlat <- min(which(lat > -60))
mask[,1:lowlat]<- NA
## let's define Greenland
mask_greenland <- array(1, dim=c(length(lon), length(lat)))
for (i in 1:length(lon)){
for (j in 1:length(lat)){
if ( ( lat[j] > 60) && (lon[i] > 360-60) && ( lon[i] < 335 )) {mask_greenland[i,j] <- NA }
if ( ( lat[j] > 66) && (lon[i] > 360-60) && ( lon[i] < 355 )) {mask_greenland[i,j] <- NA }
if ( ( lat[j] > 70) && (lon[i] > 360-70) && ( lon[i] < 355 )) {mask_greenland[i,j] <- NA }
}}
### REMOVING GREENLAND !!!
mask <- mask * mask_greenland
assign("mask_MRI_ESM1", mask) 
rm(mask);rm(lat);rm(lon); rm(lowlat);rm(mask_greenland)

## all plot on one big panel plot
png("mask_allmodels.png", width =2000, height = 2000)
layout(matrix(1:30,5,6))
par(mar=c(2,2,2,2))
for (m in 1:28){
print(list_models_common[m])
lat <- get(paste("lat_",list_models_common2[m], sep=""))
lon <- get(paste("lon_",list_models_common2[m], sep=""))
mask <- get(paste("mask_",list_models_common2[m], sep=""))
image.plot(lon - 180, lat[], mask[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),], zlim=c(1,24), col=matlab.like(24))
text(-20,-40,list_models_common2[m],cex=3) 
map(add=T)  }
rm(lat); rm(lon);rm(mask)
dev.off()

#### Mask on 2x2
buff<-mask_CMCC_CM; buff[which(is.na(buff))]<- 0
mask_2x2 <- bicubic.grid(lon_CMCC_CM, lat_CMCC_CM, buff, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )
mask_2x2_NAs <- mask_2x2$z
mask_2x2_NAs[which(mask_2x2_NAs==0)] <- NA
mask_2x2_NAs[which(mask_2x2_NAs < 1)] <- 1
mask_2x2_NAs[which(mask_2x2_NAs > 1)] <- 1

buff <- mask_CCSM4; buff[which(is.na(buff))]<- 0
mask_2x2 <- bicubic.grid(lon_CCSM4, lat_CCSM4, buff, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )

png("bob1.png")
image.plot(lon_CCSM4 - 180,lat_CCSM4,mask_CCSM4[c((length(lon_CCSM4)/2+1):length(lon_CCSM4),1:(length(lon_CCSM4)/2)),]) ;map(add=T);dev.off()
png("bob2.png")
image.plot(mask_2x2$x-180,mask_2x2$y, mask_2x2$z[c((length(mask_2x2$x)/2+1):length(mask_2x2$x),1:(length(mask_2x2$x)/2)),]);map(add=T); dev.off()
mask_2x2$z[which(mask_2x2$z< 0.6)]<- NA;
#mask_2x2$z[24,33] <- NA
#mask_2x2$z[131, 54] <- NA
#mask_2x2$z[137, 57] <- NA
#mask_2x2$z[145, 56] <- 1

png("bob3.png", width=1000, height=1000)
image.plot(mask_2x2$x-180,mask_2x2$y, mask_2x2$z[c((length(mask_2x2$x)/2+1):length(mask_2x2$x),1:(length(mask_2x2$x)/2)),]) ;map(add=T) ; dev.off()
mask_2x2_NAs <- mask_2x2$z
mask_2x2_NAs[which(mask_2x2_NAs==0)] <- NA
mask_2x2_NAs[which(mask_2x2_NAs < 1)] <- 1
mask_2x2_NAs[which(mask_2x2_NAs > 1)] <- 1

########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
########################################################################################################################################
################################################################################################################################
################################### Getting monthly variables, computing PET from Penman-Monteith

### We compute things at the annual scale, using monthly data 
## Penman monteith equation for open-water PET:
## EP = delta/(delta + gamma)*Rn + gamma/(gamma+delta)*6.43*(1+0.536*U)*D
## source: sheffield et al., 2012, Nature (paper on drought trends).
## http://www.fxsolver.com/browse/formulas/Evaporation+-+Penman+Equation+%28Shuttleworth+modification%29
## 
## delta = delta is the slope of the plot of saturated vapour pressure against air temperature
## delta= VPsat*4098/(Tmean+237.3)^2 ,  in kPa/C
## with VPsat(T) = 0.611 * e [ 17.27 * tas / (tas + 237.3) ] (kPa) see eq.13 in Allen et al 1998
## gamma = pschometric constant  = 0.665*10-3 * P    , in KPa/C, where 0.665=cp/epsilon*lambda=1.013/(0.622*2.45)
## with P = 101.3 *((293 - 0.0065*z)/293)^5.26  with z in meters, P in kPa .e.g., see Eq. 7 Allen & al. 1998 [kPa].
## or we can take P from model outputs, ps (then, we need to convert it to kPa)
## Rn is net radiation in W.m-2 - no, in J.m-2.s-1
## U is 2-m wind speed (m.s-1)
## D is the vapour-pressure deficit  (kPa) = VPsat(T) - ea 
## with ea actual water vapor partial pressure = w/(w+0.622)*P where w=q/1-q) ~q
#Allen, R.G., Pereira, L.S., Raes, D. & Smith, M. (1998) Crop evapotranspiration 
#Guidelines for computing crop water requirements. FAO Irrigation and drainage paper 56. FAO, Rome, 300, 6541.


#vec_JJA<-6:8
#for (a in 1:55) {vec_JJA <- append(vec_JJA, (12*a+6):(12*a+8))}
#vec_DJF<-12:14
#for (a in 2:54) {vec_DJF <- append(vec_DJF, (12*a):(12*a+2))}

#### Looping on models, getting masks and lon/lat
for (m in 1:28){ 
if (list_models_common[m] %in% list_models_lai){ 
print(list_models_common[m])
lat <- get(paste("lat_",list_models_common2[m], sep=""))
lon <- get(paste("lon_",list_models_common2[m], sep=""))
mask <- get(paste("mask_",list_models_common2[m], sep=""))
### Looping on variables
for (v in 1:13) { print(list_var[v])
data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.historical/.",dom[v],"/.mon/.", list_var[v], "/",
 list_models_common[m],"/.r1i1p1/.",list_var[v],"/dods", sep=""))
variable <- ncvar_get(data, list_var[v])
buff<- dim(variable)[3]
variable<- variable[,,(buff-30*12+1):buff] 
assign(list_var[v], variable ) 
nc_close(data); rm(variable); rm(buff) }
############### We need wind speed at 2m for open-water Penman-Monteith PET, and to fix the grid for a few models: 
if (m %in% c(1:2,17:19)){ print("Fixing wind grid...")
lat <- get(paste("lat_",list_models_common2[m], sep=""))
lon <- get(paste("lon_",list_models_common2[m], sep=""))
bob <- array(NA, dim=c(length(lon), length(lat), 672))
for (t in 1:672){ 
bob[,,t] <- bicubic.grid(lon, seq(-90,90,length.out=dim(sfcWind)[2]), sfcWind[,,t] , xlim=c(0,lon[length(lon)]), ylim=c(-90,90), dx=360/length(lon), dy=180/(length(lat)-1) )$z}
sfcWind <- bob }; rm(bob);
sfcWind_2m <- sfcWind*4.87/(log(67.8*10-5.42) )
############################## Calculating PET
### Latent heat of vaporatization 
#Lv <- 2500.8-2.36*(tas-273.16)+0.0016*(tas-273.16)^2-0.00006*(tas-273.16)^3 
Lv <- 2501 - 2.36*(tas - 273.16)
#in J/g. Multiply 1000/1000000=1/1000 to get MJ/kg
Lv <- Lv/1000 # in MJ/kg
Lv <- 2.501 # fixed, in MJ/Kg
#Lv <- Lv / 1000000 #now in MJ/kg
Lv2 <- Lv * 1000000 # J/kg
#### Other stuff:
vpsat  <-  0.611 * exp( 17.27 * (tas-273.16) / (tas-273.16 + 237.3) )  # in kPa
delta <- vpsat*4098/(((tas-273.16)+237.3)^2)   # in kPa/C
## gamma=cp*P/(Lv*MWratio) where MWratio is the ratio of molecular weight of water vapor/dry air=0.622
#gamma <- 0.665*10^-3* (ps/1000)   # in kPa/C
gamma <- 1.013e-3/(Lv*0.622) * (ps/1000) ## accounting for varying Lv if needed
netrad <- (rsds+rlds-rsus-rlus)   # in W.m-2. Multiply by 86400/1000000 to get MJ.m-2.d-1; We can replace that by hfls+hfss 
w <- huss/(1-huss)
es <-  w/(w+0.622)*(ps/1000) # in kPa, so the VPD is in kPa, too. 
rho_3D <- ps /(287.058*1.01*tas)
cp <-1.013e3 #J.g-1.K-1 = 1013J.kg-1.K-1 ; rho is in kg.m-3; vpsat is in Pa=kg.m-1.s-2, chU in m.s-1;
### numerator=J.kg-1.K-1*kg.m-3*Pa*m.s-1=Pa.K-1.J.m-2.s-1
h <- 0.5
zom <- h*0.123 ; zoh <- h*0.0123; d <- 0.67*h 
Ch <- 0.41^2 / (log((10 - d)/zom)*log((2-d)/zoh)) ## means we have to take wind speed at 10m
rs <- 70
##################################################################################
############## Open Water Penman - fron Shuttleworth 1993 handbook, equation 4.2.30
PET <- 1/(Lv)*( delta/(delta + gamma)*(hfls+hfss)*0.0864 + gamma/(gamma+delta)*(6.43*(1+0.536*sfcWind_2m)*(vpsat-es)))

############## Penman-Monteith:
PETpm <- 1/Lv2 *( (delta*(hfls+hfss)+ cp*rho_3D*(vpsat-es)*Ch*sfcWind)/(delta + gamma*(1+rs*Ch*sfcWind)))
#in mm/s:
PETpm <- PETpm* 86400 # in mm/d
############# Saving data
print("Saving data...")
#### Edit with your own path:
save(pr, file=paste("pr_",list_models_common2[m],".RData", sep=""))
save(evspsbl, file=paste("evspsbl_",list_models_common2[m],".RData", sep=""))
save(PET, file=paste("PET_",list_models_common2[m],".RData", sep=""))
save(PETpm, file=paste("PETpm_",list_models_common2[m],".RData", sep=""))
save(sfcWind_2m, file=paste("sfcWind_2m_",list_models_common2[m],".RData", sep=""))
save(hfls, file=paste("hfls_",list_models_common2[m],".RData", sep=""))
save(ts, file=paste("ts_",list_models_common2[m],".RData", sep=""))
save(tas, file=paste("tas_",list_models_common2[m],".RData", sep=""))
save(vpsat, file=paste("vpsat_",list_models_common2[m],".RData", sep=""))
save(netrad, file=paste("netrad_",list_models_common2[m],".RData", sep=""))
save(rlds, file=paste("rlds_",list_models_common2[m],".RData", sep=""))
save(rlus, file=paste("rlus_",list_models_common2[m],".RData", sep=""))
save(hfss, file=paste("hfss_",list_models_common2[m],".RData", sep=""))
rm(hfss);rm(hfls);rm(rlds); rm(rlus);rm(rsus); rm(rsds);rm(sfcWind); rm(sfcWind_2m); rm(ps);rm(huss); rm(w); rm(es); rm(Lv)
rm(delta); rm(gamma); rm(rho); rm(rho_3D) ; rm(PET); rm(PETpm); rm(tas); rm(vpsat); rm(netrad)     
}

################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
#################################### Now getting future values:

list_models_rsds_fut <-   sort(c("inmcm4", "bcc-csm1-1", "IPSL-CM5A-LR", "GISS-E2-R", "CMCC-CMS", "CSIRO-Mk3-6-0", "MRI-CGCM3", "HadGEM2-CC", "CMCC-CESM", "IPSL-CM5B-LR", "CanESM2", "FIO-ESM", "NorESM1-M", "FGOALS-g2", "MIROC-ESM", "GISS-E2-H-CC", "MPI-ESM-MR", "CMCC-CM", "CESM1-BGC", "MRI-ESM1", "GFDL-ESM2M", "GFDL-CM3", "GFDL-ESM2G", "BNU-ESM", "HadGEM2-ES", "IPSL-CM5A-MR", "NorESM1-ME", "ACCESS1-3", "CCSM4", "MIROC-ESM-CHEM", "ACCESS1-0", "MPI-ESM-LR", "CESM1-WACCM", "MIROC5", "bcc-csm1-1-m", "CNRM-CM5", "CESM1-CAM5", "GISS-E2-H", "GISS-E2-R-CC"))

list_models_rsus_fut <- sort(c("inmcm4", "bcc-csm1-1", "IPSL-CM5A-LR", "GISS-E2-R", "CMCC-CMS", "CSIRO-Mk3-6-0", "MRI-CGCM3", "HadGEM2-CC", "CMCC-CESM", "IPSL-CM5B-LR", "CanESM2", "FIO-ESM", "NorESM1-M", "FGOALS-g2", "MIROC-ESM", "GISS-E2-H-CC", "MPI-ESM-MR", "CMCC-CM", "CESM1-BGC", "MRI-ESM1", "GFDL-ESM2M", "GFDL-CM3", "GFDL-ESM2G", "BNU-ESM", "HadGEM2-ES", "IPSL-CM5A-MR", "NorESM1-ME", "ACCESS1-3", "CCSM4", "MIROC-ESM-CHEM", "ACCESS1-0", "MPI-ESM-LR", "CESM1-WACCM", "MIROC5", "bcc-csm1-1-m", "CNRM-CM5", "CESM1-CAM5", "GISS-E2-H", "GISS-E2-R-CC"))

list_models_rlds_fut <- sort(c("inmcm4", "bcc-csm1-1", "IPSL-CM5A-LR", "GISS-E2-R", "CMCC-CMS", "CSIRO-Mk3-6-0", "MRI-CGCM3", "HadGEM2-CC", "CMCC-CESM", "IPSL-CM5B-LR", "CanESM2", "NorESM1-M", "FGOALS-g2", "MIROC-ESM", "GISS-E2-H-CC", "MPI-ESM-MR", "CMCC-CM", "CESM1-BGC", "MRI-ESM1", "GFDL-ESM2M", "GFDL-CM3", "GFDL-ESM2G", "BNU-ESM", "HadGEM2-ES", "IPSL-CM5A-MR", "NorESM1-ME", "ACCESS1-3", "CCSM4", "MIROC-ESM-CHEM", "ACCESS1-0", "MPI-ESM-LR", "CESM1-WACCM", "MIROC5", "bcc-csm1-1-m", "CNRM-CM5", "CESM1-CAM5", "GISS-E2-H", "GISS-E2-R-CC"))

list_models_rlus_fut <-sort(c("inmcm4", "bcc-csm1-1", "IPSL-CM5A-LR", "GISS-E2-R", "CMCC-CMS", "CSIRO-Mk3-6-0", "MRI-CGCM3", "HadGEM2-CC", "CMCC-CESM", "IPSL-CM5B-LR", "CanESM2", "NorESM1-M", "FGOALS-g2", "MIROC-ESM", "GISS-E2-H-CC", "MPI-ESM-MR", "CMCC-CM", "CESM1-BGC", "MRI-ESM1", "GFDL-ESM2M", "GFDL-CM3", "GFDL-ESM2G", "BNU-ESM", "HadGEM2-ES", "IPSL-CM5A-MR", "NorESM1-ME", "ACCESS1-3", "CCSM4", "MIROC-ESM-CHEM", "ACCESS1-0", "MPI-ESM-LR", "CESM1-WACCM", "MIROC5", "bcc-csm1-1-m", "CNRM-CM5", "CESM1-CAM5", "GISS-E2-H", "GISS-E2-R-CC"))

list_models_sfcWind_fut <-sort(c("GISS-E2-R-CC", "inmcm4", "IPSL-CM5A-LR", "GISS-E2-R", "bcc-csm1-1", "CSIRO-Mk3-6-0", "MRI-CGCM3", "HadGEM2-CC", "CMCC-CESM", "IPSL-CM5B-LR", "CMCC-CMS", "CanESM2", "MIROC-ESM", "GISS-E2-H-CC", "MPI-ESM-MR", "CMCC-CM", "MRI-ESM1", "GFDL-ESM2M", "GFDL-CM3", "GFDL-ESM2G", "BNU-ESM", "HadGEM2-ES", "IPSL-CM5A-MR", "MPI-ESM-LR", "ACCESS1-0", "ACCESS1-3", "MIROC-ESM-CHEM", "MIROC5", "FGOALS-s2", "bcc-csm1-1-m", "CNRM-CM5", "CESM1-CAM5", "HadGEM2-AO", "GISS-E2-H"))

list_models_tas_fut <-sort(c("GISS-E2-R-CC", "inmcm4", "IPSL-CM5A-LR", "CESM1-CAM5-1-FV2", "GISS-E2-R", "CSIRO-Mk3-6-0", "MRI-CGCM3", "HadGEM2-CC", "CMCC-CESM", "IPSL-CM5B-LR", "bcc-csm1-1", "FIO-ESM", "NorESM1-M", "FGOALS-g2", "MIROC-ESM", "GISS-E2-H-CC", "MPI-ESM-MR", "CMCC-CM", "CESM1-BGC", "MRI-ESM1", "GFDL-ESM2M", "GFDL-CM3", "CMCC-CMS", "GFDL-ESM2G", "BNU-ESM", "HadGEM2-ES", "IPSL-CM5A-MR", "NorESM1-ME", "ACCESS1-3", "CCSM4", "MIROC-ESM-CHEM", "ACCESS1-0", "MPI-ESM-LR", "CanESM2", "CESM1-WACCM", "MIROC5", "FGOALS-s2", "bcc-csm1-1-m", "CNRM-CM5", "CESM1-CAM5", "HadGEM2-AO", "GISS-E2-H"))
list_models_ps_fut <- sort(c("GISS-E2-R-CC", "inmcm4", "IPSL-CM5A-LR", "CESM1-CAM5-1-FV2", "GISS-E2-R", "CSIRO-Mk3-6-0", "MRI-CGCM3", "HadGEM2-CC", "CMCC-CESM", "IPSL-CM5B-LR", "bcc-csm1-1", "FIO-ESM", "NorESM1-M", "FGOALS-g2", "MIROC-ESM", "GISS-E2-H-CC", "MPI-ESM-MR", "CMCC-CM", "CESM1-BGC", "MRI-ESM1", "GFDL-ESM2M", "GFDL-CM3", "CMCC-CMS", "GFDL-ESM2G", "BNU-ESM", "HadGEM2-ES", "IPSL-CM5A-MR", "NorESM1-ME", "ACCESS1-3", "CCSM4", "MIROC-ESM-CHEM", "ACCESS1-0", "MPI-ESM-LR", "CanESM2", "CESM1-WACCM", "MIROC5", "FGOALS-s2", "bcc-csm1-1-m", "CNRM-CM5", "CESM1-CAM5", "HadGEM2-AO", "GISS-E2-H"))

list_models_huss_fut <- sort(c("GISS-E2-R", "BNU-ESM", "CESM1-BGC", "MRI-CGCM3", "FGOALS-g2", "bcc-csm1-1-m", "HadGEM2-AO", "CESM1-CAM5", "GISS-E2-H-CC", "IPSL-CM5A-MR", "MRI-ESM1", "MIROC5", "NorESM1-ME", "MIROC-ESM", "MIROC-ESM-CHEM", "GISS-E2-R-CC", "CanESM2", "CCSM4", "GFDL-ESM2M", "HadGEM2-ES", "FGOALS-s2", "GISS-E2-H", "GFDL-CM3", "ACCESS1-0", "HadGEM2-CC", "NorESM1-M", "ACCESS1-3", "GFDL-ESM2G", "CESM1-FASTCHEM", "IPSL-CM5A-LR", "inmcm4", "HadCM3", "CSIRO-Mk3-6-0", "bcc-csm1-1", "CESM1-WACCM", "FIO-ESM", "CNRM-CM5", "IPSL-CM5B-LR"))


test_models_common_fut <- array(NA, dim=c(length(list_models_tas_fut),8) )
for (m in 1:length(list_models_tas_fut)) {
for (v in 1:8) { # Vars 
print(list_var[v])
list_models <- get(paste("list_models_", list_var[v],"_fut",sep=""))
if ( list_models_tas_fut[m] %in% list_models) { test_models_common_fut[m,v] <- 1 }
rm(list_models) }}

####### LIst of models available in the future:
list_models_common_fut <- list_models_tas_fut[which(rowSums(test_models_common_fut) ==8)]
list_models_common_fut2 <- c("ACCESS1_0", "ACCESS1_3", "bcc_csm1_1", "bcc_csm1_1_m",  "BNU_ESM","CanESM2", "CESM1_CAM5", "CNRM_CM5", "CSIRO_Mk3_6_0","GFDL_CM3", "GFDL_ESM2G", "GFDL_ESM2M","GISS_E2_H",  "GISS_E2_H_CC", "GISS_E2_R", "GISS_E2_R_CC", "HadGEM2_CC", "HadGEM2_ES", "inmcm4", "IPSL_CM5A_LR",  "IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC_ESM", "MIROC_ESM_CHEM","MIROC5",  "MRI_CGCM3", "MRI_ESM1")
## it's one less than the present:

########################################################
################################# Getting variables, computing PET:
for (m in 1:27){
print(list_models_common_fut[m])
for (v in 1:13){
print(list_var[v])
data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.rcp85/.",dom[v],"/.mon/.", list_var[v], "/", list_models_common_fut[m],"/.r1i1p1/.",list_var[v],"/dods", sep=""))
variable <- ncvar_get(data, list_var[v])
names(data$dim$T)
buff<- dim(variable)[3]
if (buff > 1140) {buff <- 1140}
variable<- variable[,,(buff-30*12+1):buff] 
assign(list_var[v], variable ) 
nc_close(data)
rm(variable); rm(buff)}
############### Fixing wind, if necessary 
if (m %in% c(1:2,17:19)){ print("Fixing wind grid...")
lat <- get(paste("lat_",list_models_common_fut2[m], sep=""))
lon <- get(paste("lon_",list_models_common_fut2[m], sep=""))
bob <- array(NA, dim=c(length(lon), length(lat), 360))
for (t in 1:360){ 
bob[,,t] <- bicubic.grid(lon, seq(-90,90,length.out=dim(sfcWind)[2]), sfcWind[,,t] , xlim=c(0,lon[length(lon)]), ylim=c(-90,90), dx=360/length(lon), dy=180/(length(lat)-1) )$z}
sfcWind <- bob }; rm(bob);
sfcWind_2m <- sfcWind*4.87/(log(67.8*10-5.42) )
############## Calculating PET 
### Latent heat of vaporatization
Lv <- 2500.8-2.36*(tas-273.16)+0.0016*(tas-273.16)^2-0.00006*(tas-273.16)^3
#in J/g. Multiply 1000/1000000=1/1000 to get MJ/kg
Lv <- Lv/1000 #MJ/kg
Lv2 <- Lv * 1000000 # J/kg
###################################3
vpsat  <-  0.611 * exp( 17.27 * (tas-273.16) / (tas-273.16 + 237.3) )  # in kPa
delta <- vpsat*4098/(((tas-273.16)+237.3)^2)   # in kPa/C
gamma <- 0.665*10^-3* (ps/1000)   # in kPa/C
#gamma <- 1.013e-3/(Lv*0.622) * (ps/1000) ## accounting for varying Lv
netrad <- (rsds+rlds-rsus-rlus)   # in W.m-2. Multiply by 86400/1000000 to get MJ.m-2.d-1 
w <- huss/(1-huss)
es <-  w/(w+0.622)*(ps/1000) # in kPa, so the VPD is in kPa, too. 
Lv <- 2500.8-2.36*(tas-273.16)+0.0016*(tas-273.16)^2-0.00006*(tas-273.16)^3 
#in J/g. Multiply 1000/1000000=1/1000 to get MJ/kg
Lv <- Lv/1000
print("PET")
rho_3D <- ps /(287.058*1.01*tas)
cp <-1.013e3 #J.g-1.K-1 = 1013J.kg-1.K-1 ; rho is in kg.m-3; vpsaat is in Pa=kg.m-1.s-2, chU in m.s-1;
#numerator=J.kg-1.K-1*kg.m-3*Pa*m.s-1=Pa.K-1.J.m-2.s-1
h <- 0.5
zom <- h*0.123 ; zoh <- h*0.0123; d <- 0.67*h
Ch <- 0.41^2 / (log((10-d)/zom)*log((2-d)/zoh))
rs <- 70
############## Open Water Penman - fron Shuttleworth 1993 handbook, equation 4.2.30
PET <- 1/(Lv)*( delta/(delta + gamma)*(hfls+hfss)*0.0864 + gamma/(gamma+delta)*(6.43*(1+0.536*sfcWind_2m)*(vpsat-es)))
############## Penman-Monteith:
PETpm <- 1/Lv2 *( (delta*(hfls+hfss)+ cp*rho_3D*(vpsat-es)*Ch*sfcWind)/(delta + gamma*(1+rs*Ch*sfcWind)))
#in mm/s:
PETpm <- PETpm* 86400 # in mm/d
############# Saving data - Edit with your own path:
save(pr, file=paste("pr_",list_models_common2[m],"_rcp85.RData", sep=""))
save(evspsbl, file=paste("evspsbl_",list_models_common2[m],"_rcp85.RData", sep=""))
save(PET, file=paste("PET_",list_models_common2[m],"_rcp85.RData", sep=""))
save(PETpm, file=paste("PETpm_",list_models_common2[m],"_rcp85.RData", sep=""))
save(sfcWind_2m, file=paste("sfcWind_2m_",list_models_common2[m],"_rcp85.RData", sep=""))
save(tas, file=paste("tas_",list_models_common_fut2[m],"_rcp85.RData", sep=""))
save(vpsat, file=paste("vpsat_",list_models_common_fut2[m],"_rcp85.RData", sep=""))
save(netrad, file=paste("netrad_",list_models_common_fut2[m],"_rcp85.RData", sep=""))
save(rlds, file=paste("rlds_",list_models_common_fut2[m],"_rcp85.RData", sep=""))
save(rlus, file=paste("rlus_",list_models_common_fut2[m],"_rcp85.RData", sep=""))
save(hfss, file=paste("hfss_",list_models_common_fut2[m],"_rcp85.RData", sep=""))
rm(hfss);rm(hfls);rm(rlds); rm(rlus);rm(rsus); rm(rsds);rm(sfcWind); rm(sfcWind_2m); rm(ps);rm(huss); rm(w); rm(es);
rm(Lv);  rm(delta); rm(gamma); rm(rho); rm(rho_3D); rm(zsurf) ; rm(PET); rm(PETpm); rm(tas); rm(vpsat); rm(netrad)   
}

#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
#####################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
######################## Computing mean summer/winter/year values
vec_JJA <- 6:8
for (a in 1:55) {vec_JJA <- append(vec_JJA, (12*a+6):(12*a+8))}
vec_DJF <- 12:14
for (a in 1:54) {vec_DJF <- append(vec_DJF, (12*a+12):(12*a+14))}

for (m in 1:28){
if (list_models_common[m] %in% list_models_common_fut) {
print(list_models_common[m])
load(paste("tas_",list_models_common2[m],".RData", sep="")) 
assign(paste("mean_tas_",list_models_common2[m],"_year_pres",sep=""), apply(tas[,,253:612],c(1,2), mean, na.rm=T))
assign(paste("mean_tas_",list_models_common2[m],"_JJA_pres",sep=""), apply(tas[,,253:612][,,vec_JJA],c(1,2), mean, na.rm=T))
assign(paste("mean_tas_",list_models_common2[m],"_DJF_pres",sep=""), apply(tas[,,253:612][,,vec_DJF],c(1,2), mean, na.rm=T))
rm(tas)
load(paste("tas_",list_models_common2[m],"_rcp85.RData", sep="")) 
assign(paste("mean_tas_",list_models_common2[m],"_year_fut",sep=""),  apply(tas[,,],c(1,2), mean, na.rm=T))
assign(paste("mean_tas_",list_models_common2[m],"_JJA_fut",sep=""),   apply(tas[,,][,,vec_JJA],c(1,2), mean, na.rm=T))
assign(paste("mean_tas_",list_models_common2[m],"_DJF_fut",sep=""),   apply(tas[,,][,,vec_DJF],c(1,2), mean, na.rm=T))
rm(tas)}}

for (m in 1:28){
if (list_models_common[m] %in% list_models_common_fut) {
print(list_models_common[m])
load(paste("PET_",list_models_common2[m],".RData", sep="")) 
assign(paste("mean_PET_",list_models_common2[m],"_year_pres",sep=""), apply(PET[,,253:612],c(1,2), mean, na.rm=T))
assign(paste("mean_PET_",list_models_common2[m],"_JJA_pres",sep=""), apply(PET[,,253:612][,,vec_JJA],c(1,2), mean, na.rm=T))
assign(paste("mean_PET_",list_models_common2[m],"_DJF_pres",sep=""), apply(PET[,,253:612][,,vec_DJF],c(1,2), mean, na.rm=T))
rm(PET)
load(paste("PET_",list_models_common2[m],"_rcp85.RData", sep="")) 
assign(paste("mean_PET_",list_models_common2[m],"_year_fut",sep=""),  apply(PET[,,],c(1,2), mean, na.rm=T))
assign(paste("mean_PET_",list_models_common2[m],"_JJA_fut",sep=""),   apply(PET[,,][,,vec_JJA],c(1,2), mean, na.rm=T))
assign(paste("mean_PET_",list_models_common2[m],"_DJF_fut",sep=""),   apply(PET[,,][,,vec_DJF],c(1,2), mean, na.rm=T))
rm(PET)}}

for (m in 1:28){
if (list_models_common[m] %in% list_models_common_fut) {
print(list_models_common[m])
load(paste("netrad_",list_models_common2[m],".RData", sep="")) 
assign(paste("mean_netrad_",list_models_common2[m],"_year_pres",sep=""), apply(netrad[,,253:612],c(1,2), mean, na.rm=T))
assign(paste("mean_netrad_",list_models_common2[m],"_JJA_pres",sep=""), apply(netrad[,,253:612][,,vec_JJA],c(1,2), mean, na.rm=T))
assign(paste("mean_netrad_",list_models_common2[m],"_DJF_pres",sep=""), apply(netrad[,,253:612][,,vec_DJF],c(1,2), mean, na.rm=T))
rm(netrad)
load(paste("netrad_",list_models_common2[m],"_rcp85.RData", sep="")) 
assign(paste("mean_netrad_",list_models_common2[m],"_year_fut",sep=""),  apply(netrad[,,],c(1,2), mean, na.rm=T))
assign(paste("mean_netrad_",list_models_common2[m],"_JJA_fut",sep=""),   apply(netrad[,,][,,vec_JJA],c(1,2), mean, na.rm=T))
assign(paste("mean_netrad_",list_models_common2[m],"_DJF_fut",sep=""),   apply(netrad[,,][,,vec_DJF],c(1,2), mean, na.rm=T))
rm(netrad)}}

for (m in 1:28){
if (list_models_common[m] %in% list_models_common_fut) {
print(list_models_common[m])
load(paste("rsds_",list_models_common2[m],".RData", sep="")) 
assign(paste("mean_rsds_",list_models_common2[m],"_year_pres",sep=""), apply(rsds[,,253:612],c(1,2), mean, na.rm=T))
assign(paste("mean_rsds_",list_models_common2[m],"_JJA_pres",sep=""), apply(rsds[,,253:612][,,vec_JJA],c(1,2), mean, na.rm=T))
assign(paste("mean_rsds_",list_models_common2[m],"_DJF_pres",sep=""), apply(rsds[,,253:612][,,vec_DJF],c(1,2), mean, na.rm=T))
rm(rsds)
load(paste("rsds_",list_models_common2[m],"_rcp85.RData", sep="")) 
assign(paste("mean_rsds_",list_models_common2[m],"_year_fut",sep=""),  apply(rsds[,,],c(1,2), mean, na.rm=T))
assign(paste("mean_rsds_",list_models_common2[m],"_JJA_fut",sep=""),   apply(rsds[,,][,,vec_JJA],c(1,2), mean, na.rm=T))
assign(paste("mean_rsds_",list_models_common2[m],"_DJF_fut",sep=""),   apply(rsds[,,][,,vec_DJF],c(1,2), mean, na.rm=T))
rm(rsds)}}

for (m in 1:28){
if (list_models_common[m] %in% list_models_common_fut) {
print(list_models_common[m])
load(paste("rlds_",list_models_common2[m],".RData", sep="")) 
assign(paste("mean_rlds_",list_models_common2[m],"_year_pres",sep=""), apply(rlds[,,253:612],c(1,2), mean, na.rm=T))
assign(paste("mean_rlds_",list_models_common2[m],"_JJA_pres",sep=""), apply(rlds[,,253:612][,,vec_JJA],c(1,2), mean, na.rm=T))
assign(paste("mean_rlds_",list_models_common2[m],"_DJF_pres",sep=""), apply(rlds[,,253:612][,,vec_DJF],c(1,2), mean, na.rm=T))
rm(rlds)
load(paste("rlds_",list_models_common2[m],"_rcp85.RData", sep="")) 
assign(paste("mean_rlds_",list_models_common2[m],"_year_fut",sep=""),  apply(rlds[,,],c(1,2), mean, na.rm=T))
assign(paste("mean_rlds_",list_models_common2[m],"_JJA_fut",sep=""),   apply(rlds[,,][,,vec_JJA],c(1,2), mean, na.rm=T))
assign(paste("mean_rlds_",list_models_common2[m],"_DJF_fut",sep=""),   apply(rlds[,,][,,vec_DJF],c(1,2), mean, na.rm=T))
rm(rlds)}}

for (m in 1:28){
if (list_models_common[m] %in% list_models_common_fut) {
print(list_models_common[m])
load(paste("rlus_",list_models_common2[m],".RData", sep="")) 
assign(paste("mean_rlus_",list_models_common2[m],"_year_pres",sep=""), apply(rlus[,,253:612],c(1,2), mean, na.rm=T))
assign(paste("mean_rlus_",list_models_common2[m],"_JJA_pres",sep=""), apply(rlus[,,253:612][,,vec_JJA],c(1,2), mean, na.rm=T))
assign(paste("mean_rlus_",list_models_common2[m],"_DJF_pres",sep=""), apply(rlus[,,253:612][,,vec_DJF],c(1,2), mean, na.rm=T))
rm(rlus)
load(paste("rlus_",list_models_common2[m],"_rcp85.RData", sep="")) 
assign(paste("mean_rlus_",list_models_common2[m],"_year_fut",sep=""),  apply(rlus[,,],c(1,2), mean, na.rm=T))
assign(paste("mean_rlus_",list_models_common2[m],"_JJA_fut",sep=""),   apply(rlus[,,][,,vec_JJA],c(1,2), mean, na.rm=T))
assign(paste("mean_rlus_",list_models_common2[m],"_DJF_fut",sep=""),   apply(rlus[,,][,,vec_DJF],c(1,2), mean, na.rm=T))
rm(rlus)}}

for (m in 1:28){
if (list_models_common[m] %in% list_models_common) {
print(list_models_common[m])
load(paste("evspsbl_",list_models_common2[m],".RData", sep="")) 
evspsbl <- variable; rm(variable)
assign(paste("mean_evspsbl_",list_models_common2[m],"_year_pres",sep=""), apply(evspsbl[,,253:612],c(1,2), mean, na.rm=T))
assign(paste("mean_evspsbl_",list_models_common2[m],"_JJA_pres",sep=""), apply(evspsbl[,,253:612][,,vec_JJA],c(1,2), mean, na.rm=T))
assign(paste("mean_evspsbl_",list_models_common2[m],"_DJF_pres",sep=""), apply(evspsbl[,,253:612][,,vec_DJF],c(1,2), mean, na.rm=T))
rm(evspsbl)}
if (list_models_common[m] %in% list_models_common_fut) {
print(list_models_common[m])
load(paste("evspsbl_",list_models_common2[m],"_rcp85.RData", sep="")) 
evspsbl <- variable
assign(paste("mean_evspsbl_",list_models_common2[m],"_year_fut",sep=""),  apply(evspsbl[,,],c(1,2), mean, na.rm=T))
assign(paste("mean_evspsbl_",list_models_common2[m],"_JJA_fut",sep=""),   apply(evspsbl[,,][,,vec_JJA],c(1,2), mean, na.rm=T))
assign(paste("mean_evspsbl_",list_models_common2[m],"_DJF_fut",sep=""),   apply(evspsbl[,,][,,vec_DJF],c(1,2), mean, na.rm=T))
rm(evspsbl); rm(variable)
}}

for (m in 1:28){
if (list_models_common[m] %in% list_models_common_fut) {
print(list_models_common[m])
load(paste("hfss_",list_models_common2[m],".RData", sep="")) 
assign(paste("mean_hfss_",list_models_common2[m],"_year_pres",sep=""), apply(hfss[,,253:612],c(1,2), mean, na.rm=T))
assign(paste("mean_hfss_",list_models_common2[m],"_JJA_pres",sep=""), apply(hfss[,,253:612][,,vec_JJA],c(1,2), mean, na.rm=T))
assign(paste("mean_hfss_",list_models_common2[m],"_DJF_pres",sep=""), apply(hfss[,,253:612][,,vec_DJF],c(1,2), mean, na.rm=T))
rm(hfss)
load(paste("hfss_",list_models_common2[m],"_rcp85.RData", sep="")) 
assign(paste("mean_hfss_",list_models_common2[m],"_year_fut",sep=""),  apply(hfss[,,],c(1,2), mean, na.rm=T))
assign(paste("mean_hfss_",list_models_common2[m],"_JJA_fut",sep=""),   apply(hfss[,,][,,vec_JJA],c(1,2), mean, na.rm=T))
assign(paste("mean_hfss_",list_models_common2[m],"_DJF_fut",sep=""),   apply(hfss[,,][,,vec_DJF],c(1,2), mean, na.rm=T))
rm(hfss); rm(hfss)
}}

for (m in 1:28){
if (list_models_common[m] %in% list_models_common_fut) {
print(list_models_common[m])
load(paste("pr_",list_models_common2[m],".RData", sep="")) 
pr <- variable; rm(variable)
assign(paste("mean_pr_",list_models_common2[m],"_year_pres",sep=""), apply(pr[,,253:612],c(1,2), mean, na.rm=T))
assign(paste("mean_pr_",list_models_common2[m],"_JJA_pres",sep=""), apply(pr[,,253:612][,,vec_JJA],c(1,2), mean, na.rm=T))
assign(paste("mean_pr_",list_models_common2[m],"_DJF_pres",sep=""), apply(pr[,,253:612][,,vec_DJF],c(1,2), mean, na.rm=T))
rm(pr)
load(paste("pr_",list_models_common2[m],"_rcp85.RData", sep="")) 
pr <- variable
assign(paste("mean_pr_",list_models_common2[m],"_year_fut",sep=""),  apply(pr[,,],c(1,2), mean, na.rm=T))
assign(paste("mean_pr_",list_models_common2[m],"_JJA_fut",sep=""),   apply(pr[,,][,,vec_JJA],c(1,2), mean, na.rm=T))
assign(paste("mean_pr_",list_models_common2[m],"_DJF_fut",sep=""),   apply(pr[,,][,,vec_DJF],c(1,2), mean, na.rm=T))
rm(pr); rm(variable)
}}

################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################## Regridding:
library(akima)
for (m in 1:28){
print(list_models_common2[m])
if (list_models_common[m] %in% list_models_common_fut) {
lat <- get(paste("lat_", list_models_common2[m], sep=""))
lon <- get(paste("lon_", list_models_common2[m], sep="")) 

mean_tas_year_pres <- get( paste("mean_tas_",list_models_common2[m],"_year_pres", sep="")) 
mean_tas_year_pres[which(is.na(mean_tas_year_pres)==T)] <- 0 
mean_tas_year_pres_2x2 <- bicubic.grid(lon, lat, mean_tas_year_pres, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_tas_",list_models_common2[m],"_year_pres_2x2", sep=""), mean_tas_year_pres_2x2)

mean_tas_year_fut <- get( paste("mean_tas_",list_models_common2[m],"_year_fut", sep="")) 
mean_tas_year_fut[which(is.na(mean_tas_year_fut)==T)] <- 0 
mean_tas_year_fut_2x2 <- bicubic.grid(lon, lat, mean_tas_year_fut, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_tas_",list_models_common2[m],"_year_fut_2x2", sep=""), mean_tas_year_fut_2x2)
}}

for (m in 1:28){
print(list_models_common2[m])
if (list_models_common[m] %in% list_models_common_fut) {
lat <- get(paste("lat_", list_models_common2[m], sep=""))
lon <- get(paste("lon_", list_models_common2[m], sep="")) 

mean_evspsbl_year_pres <- get( paste("mean_evspsbl_",list_models_common2[m],"_year_pres", sep="")) 
mean_evspsbl_year_pres[which(is.na(mean_evspsbl_year_pres)==T)] <- 0 
mean_evspsbl_year_pres_2x2 <- bicubic.grid(lon, lat, mean_evspsbl_year_pres, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_evspsbl_",list_models_common2[m],"_year_pres_2x2", sep=""), mean_evspsbl_year_pres_2x2)

mean_evspsbl_year_fut <- get( paste("mean_evspsbl_",list_models_common2[m],"_year_fut", sep="")) 
mean_evspsbl_year_fut[which(is.na(mean_evspsbl_year_fut)==T)] <- 0 
mean_evspsbl_year_fut_2x2 <- bicubic.grid(lon, lat, mean_evspsbl_year_fut, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_evspsbl_",list_models_common2[m],"_year_fut_2x2", sep=""), mean_evspsbl_year_fut_2x2)
}}



for (m in 1:28){
print(list_models_common2[m])
if (list_models_common[m] %in% list_models_common_fut) {
lat <- get(paste("lat_", list_models_common2[m], sep=""))
lon <- get(paste("lon_", list_models_common2[m], sep="")) 
mean_hfss_year_pres <- get( paste("mean_hfss_",list_models_common2[m],"_year_pres", sep="")) 
mean_hfss_year_pres[which(is.na(mean_hfss_year_pres)==T)] <- 0 
mean_hfss_year_pres_2x2 <- bicubic.grid(lon, lat, mean_hfss_year_pres, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_hfss_",list_models_common2[m],"_year_pres_2x2", sep=""), mean_hfss_year_pres_2x2)

mean_hfss_year_fut <- get( paste("mean_hfss_",list_models_common2[m],"_year_fut", sep="")) 
mean_hfss_year_fut[which(is.na(mean_hfss_year_fut)==T)] <- 0 
mean_hfss_year_fut_2x2 <- bicubic.grid(lon, lat, mean_hfss_year_fut, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_hfss_",list_models_common2[m],"_year_fut_2x2", sep=""), mean_hfss_year_fut_2x2)
}}


for (m in 1:28){
print(list_models_common2[m])
if (list_models_common[m] %in% list_models_common_fut) {
lat <- get(paste("lat_", list_models_common2[m], sep=""))
lon <- get(paste("lon_", list_models_common2[m], sep="")) 

mean_pr_year_pres <- get( paste("mean_pr_",list_models_common2[m],"_year_pres", sep="")) 
mean_pr_year_pres[which(is.na(mean_pr_year_pres)==T)] <- 0 
mean_pr_year_pres_2x2 <- bicubic.grid(lon, lat, mean_pr_year_pres, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_pr_",list_models_common2[m],"_year_pres_2x2", sep=""), mean_pr_year_pres_2x2)

mean_pr_year_fut <- get( paste("mean_pr_",list_models_common2[m],"_year_fut", sep="")) 
mean_pr_year_fut[which(is.na(mean_pr_year_fut)==T)] <- 0 
mean_pr_year_fut_2x2 <- bicubic.grid(lon, lat, mean_pr_year_fut, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_pr_",list_models_common2[m],"_year_fut_2x2", sep=""), mean_pr_year_fut_2x2)
}}


for (m in 1:28){
print(list_models_common2[m])
if (list_models_common[m] %in% list_models_common_fut) {
lat <- get(paste("lat_", list_models_common2[m], sep=""))
lon <- get(paste("lon_", list_models_common2[m], sep="")) 

mean_PET_year_pres <- get( paste("mean_PET_",list_models_common2[m],"_year_pres", sep="")) 
mean_PET_year_pres[which(is.na(mean_PET_year_pres)==T)] <- 0 
mean_PET_year_pres_2x2 <- bicubic.grid(lon, lat, mean_PET_year_pres, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_PET_",list_models_common2[m],"_year_pres_2x2", sep=""), mean_PET_year_pres_2x2)

mean_PET_year_fut <- get( paste("mean_PET_",list_models_common2[m],"_year_fut", sep="")) 
mean_PET_year_fut[which(is.na(mean_PET_year_fut)==T)] <- 0 
mean_PET_year_fut_2x2 <- bicubic.grid(lon, lat, mean_PET_year_fut, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_PET_",list_models_common2[m],"_year_fut_2x2", sep=""), mean_PET_year_fut_2x2)
}}

for (m in 1:28){
print(list_models_common2[m])
if (list_models_common[m] %in% list_models_common_fut) {
lat <- get(paste("lat_", list_models_common2[m], sep=""))
lon <- get(paste("lon_", list_models_common2[m], sep="")) 

mean_netrad_year_pres <- get( paste("mean_netrad_",list_models_common2[m],"_year_pres", sep="")) 
mean_netrad_year_pres[which(is.na(mean_netrad_year_pres)==T)] <- 0 
mean_netrad_year_pres_2x2 <- bicubic.grid(lon, lat, mean_netrad_year_pres, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_netrad_",list_models_common2[m],"_year_pres_2x2", sep=""), mean_netrad_year_pres_2x2)

mean_netrad_year_fut <- get( paste("mean_netrad_",list_models_common2[m],"_year_fut", sep="")) 
mean_netrad_year_fut[which(is.na(mean_netrad_year_fut)==T)] <- 0 
mean_netrad_year_fut_2x2 <- bicubic.grid(lon, lat, mean_netrad_year_fut, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_netrad_",list_models_common2[m],"_year_fut_2x2", sep=""), mean_netrad_year_fut_2x2)
}}


for (m in 1:28){
print(list_models_common2[m])
if (list_models_common[m] %in% list_models_common_fut) {
lat <- get(paste("lat_", list_models_common2[m], sep=""))
lon <- get(paste("lon_", list_models_common2[m], sep="")) 

mean_rsds_year_pres <- get( paste("mean_rsds_",list_models_common2[m],"_year_pres", sep="")) 
mean_rsds_year_pres[which(is.na(mean_rsds_year_pres)==T)] <- 0 
mean_rsds_year_pres_2x2 <- bicubic.grid(lon, lat, mean_rsds_year_pres, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_rsds_",list_models_common2[m],"_year_pres_2x2", sep=""), mean_rsds_year_pres_2x2)

mean_rsds_year_fut <- get( paste("mean_rsds_",list_models_common2[m],"_year_fut", sep="")) 
mean_rsds_year_fut[which(is.na(mean_rsds_year_fut)==T)] <- 0 
mean_rsds_year_fut_2x2 <- bicubic.grid(lon, lat, mean_rsds_year_fut, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_rsds_",list_models_common2[m],"_year_fut_2x2", sep=""), mean_rsds_year_fut_2x2)
}}


for (m in 1:28){
print(list_models_common2[m])
if (list_models_common[m] %in% list_models_common_fut) {
lat <- get(paste("lat_", list_models_common2[m], sep=""))
lon <- get(paste("lon_", list_models_common2[m], sep="")) 

mean_rlds_year_pres <- get( paste("mean_rlds_",list_models_common2[m],"_year_pres", sep="")) 
mean_rlds_year_pres[which(is.na(mean_rlds_year_pres)==T)] <- 0 
mean_rlds_year_pres_2x2 <- bicubic.grid(lon, lat, mean_rlds_year_pres, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_rlds_",list_models_common2[m],"_year_pres_2x2", sep=""), mean_rlds_year_pres_2x2)

mean_rlds_year_fut <- get( paste("mean_rlds_",list_models_common2[m],"_year_fut", sep="")) 
mean_rlds_year_fut[which(is.na(mean_rlds_year_fut)==T)] <- 0 
mean_rlds_year_fut_2x2 <- bicubic.grid(lon, lat, mean_rlds_year_fut, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_rlds_",list_models_common2[m],"_year_fut_2x2", sep=""), mean_rlds_year_fut_2x2)
}}


for (m in 1:28){
print(list_models_common2[m])
if (list_models_common[m] %in% list_models_common_fut) {
lat <- get(paste("lat_", list_models_common2[m], sep=""))
lon <- get(paste("lon_", list_models_common2[m], sep="")) 

mean_rlus_year_pres <- get( paste("mean_rlus_",list_models_common2[m],"_year_pres", sep="")) 
mean_rlus_year_pres[which(is.na(mean_rlus_year_pres)==T)] <- 0 
mean_rlus_year_pres_2x2 <- bicubic.grid(lon, lat, mean_rlus_year_pres, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_rlus_",list_models_common2[m],"_year_pres_2x2", sep=""), mean_rlus_year_pres_2x2)

mean_rlus_year_fut <- get( paste("mean_rlus_",list_models_common2[m],"_year_fut", sep="")) 
mean_rlus_year_fut[which(is.na(mean_rlus_year_fut)==T)] <- 0 
mean_rlus_year_fut_2x2 <- bicubic.grid(lon, lat, mean_rlus_year_fut, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_rlus_",list_models_common2[m],"_year_fut_2x2", sep=""), mean_rlus_year_fut_2x2)
}}

## Pr/PET
for (m in 1:28){
print(list_models_common2[m])
if (list_models_common[m] %in% list_models_common_fut) {
lat <- get(paste("lat_", list_models_common2[m], sep=""))
lon <- get(paste("lon_", list_models_common2[m], sep="")) 

mean_pr_year_pres <- get( paste("mean_pr_",list_models_common2[m],"_year_pres", sep="")) *86400
mean_PET_year_pres <- get( paste("mean_PET_",list_models_common2[m],"_year_pres", sep="")) 
mean_PET_year_pres[which(is.na(mean_PET_year_pres)==T)] <- 0; mean_pr_year_pres[which(is.na(mean_pr_year_pres)==T)] <- 0 
mean_PrPET_year_pres_2x2 <- bicubic.grid(lon, lat, mean_pr_year_pres/mean_PET_year_pres, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_PrPET_",list_models_common2[m],"_year_pres_2x2", sep=""), mean_PrPET_year_pres_2x2)

mean_pr_year_fut <- get( paste("mean_pr_",list_models_common2[m],"_year_fut", sep="")) *86400
mean_PET_year_fut <- get( paste("mean_PET_",list_models_common2[m],"_year_fut", sep="")) 
mean_PET_year_fut[which(is.na(mean_PET_year_fut)==T)] <- 0; mean_pr_year_fut[which(is.na(mean_pr_year_fut)==T)] <- 0 
mean_PrPET_year_fut_2x2 <- bicubic.grid(lon, lat, mean_pr_year_fut/mean_PET_year_fut, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_PrPET_",list_models_common2[m],"_year_fut_2x2", sep=""), mean_PrPET_year_fut_2x2)
}}


##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################

############### Let's put all models in a matrix:
#### Tas
mean_tas_year_pres_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_tas_2x2 <- get(paste("mean_tas_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_tas_year_pres_2x2_allmodels[,,m] <- mean_tas_2x2  #*mask_2x2_NAs
rm(mean_tas_2x2)}

mean_tas_year_fut_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_tas_2x2 <- get(paste("mean_tas_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_tas_year_fut_2x2_allmodels[,,m] <- mean_tas_2x2 #*mask_2x2_NAs
rm(mean_tas_2x2)}


############### Let's put all models in a matrix:
#### Pr
mean_pr_year_pres_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_pr_2x2 <- get(paste("mean_pr_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_pr_year_pres_2x2_allmodels[,,m] <- mean_pr_2x2*mask_2x2_NAs
rm(mean_pr_2x2)}

mean_pr_year_fut_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_pr_2x2 <- get(paste("mean_pr_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_pr_year_fut_2x2_allmodels[,,m] <- mean_pr_2x2*mask_2x2_NAs
rm(mean_pr_2x2)}


#### Evspsbl
mean_evspsbl_year_pres_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_evspsbl_2x2 <- get(paste("mean_evspsbl_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_evspsbl_year_pres_2x2_allmodels[,,m] <- mean_evspsbl_2x2*mask_2x2_NAs
rm(mean_evspsbl_2x2)}

mean_evspsbl_year_fut_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_evspsbl_2x2 <- get(paste("mean_evspsbl_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_evspsbl_year_fut_2x2_allmodels[,,m] <- mean_evspsbl_2x2*mask_2x2_NAs
rm(mean_evspsbl_2x2)}


#### Hfss
mean_hfss_year_pres_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_hfss_2x2 <- get(paste("mean_hfss_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_hfss_year_pres_2x2_allmodels[,,m] <- mean_hfss_2x2*mask_2x2_NAs
rm(mean_hfss_2x2)}

mean_hfss_year_fut_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_hfss_2x2 <- get(paste("mean_hfss_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_hfss_year_fut_2x2_allmodels[,,m] <- mean_hfss_2x2*mask_2x2_NAs
rm(mean_hfss_2x2)}

################################################
###### Netrad
mean_netrad_year_pres_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_netrad_2x2 <- get(paste("mean_netrad_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_netrad_year_pres_2x2_allmodels[,,m] <- mean_netrad_2x2*mask_2x2_NAs
rm(mean_netrad_2x2)}

mean_netrad_year_fut_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_netrad_2x2 <- get(paste("mean_netrad_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_netrad_year_fut_2x2_allmodels[,,m] <- mean_netrad_2x2*mask_2x2_NAs
rm(mean_netrad_2x2)}


###############################################
###### Rsds
mean_rsds_year_pres_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_rsds_2x2 <- get(paste("mean_rsds_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_rsds_year_pres_2x2_allmodels[,,m] <- mean_rsds_2x2*mask_2x2_NAs
rm(mean_rsds_2x2)}

mean_rsds_year_fut_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_rsds_2x2 <- get(paste("mean_rsds_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_rsds_year_fut_2x2_allmodels[,,m] <- mean_rsds_2x2*mask_2x2_NAs
rm(mean_rsds_2x2)}

###############################################
###### Rlds
mean_rlds_year_pres_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_rlds_2x2 <- get(paste("mean_rlds_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_rlds_year_pres_2x2_allmodels[,,m] <- mean_rlds_2x2*mask_2x2_NAs
rm(mean_rlds_2x2)}

mean_rlds_year_fut_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_rlds_2x2 <- get(paste("mean_rlds_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_rlds_year_fut_2x2_allmodels[,,m] <- mean_rlds_2x2*mask_2x2_NAs
rm(mean_rlds_2x2)}

###############################################
###### Rlus
mean_rlus_year_pres_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_rlus_2x2 <- get(paste("mean_rlus_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_rlus_year_pres_2x2_allmodels[,,m] <- mean_rlus_2x2*mask_2x2_NAs
rm(mean_rlus_2x2)}

mean_rlus_year_fut_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_rlus_2x2 <- get(paste("mean_rlus_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_rlus_year_fut_2x2_allmodels[,,m] <- mean_rlus_2x2*mask_2x2_NAs
rm(mean_rlus_2x2)}

#############################
#### PET
mean_PET_year_pres_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_PET_2x2 <- get(paste("mean_PET_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_PET_year_pres_2x2_allmodels[,,m] <- mean_PET_2x2*mask_2x2_NAs
rm(mean_PET_2x2)}

mean_PET_year_fut_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_PET_2x2 <- get(paste("mean_PET_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_PET_year_fut_2x2_allmodels[,,m] <- mean_PET_2x2*mask_2x2_NAs
rm(mean_PET_2x2)}


#### PrPET
mean_PrPET_year_pres_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_PrPET_2x2 <- get(paste("mean_PrPET_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_PrPET_year_pres_2x2_allmodels[,,m] <- mean_PrPET_2x2*mask_2x2_NAs
rm(mean_PrPET_2x2)}

mean_PrPET_year_fut_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_PrPET_2x2 <- get(paste("mean_PrPET_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_PrPET_year_fut_2x2_allmodels[,,m] <- mean_PrPET_2x2*mask_2x2_NAs
rm(mean_PrPET_2x2)}


######################################################################################################
######################################################################################################
################# Change:
mean_tas_year_change_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_tas_2x2_pres <- get(paste("mean_tas_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_tas_2x2_fut <- get(paste("mean_tas_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_tas_year_change_2x2_allmodels[,,m] <- (mean_tas_2x2_fut - mean_tas_2x2_pres) #*mask_2x2_NAs
rm(mean_tas_2x2_pres); rm(mean_tas_2x2_fut)}

mean_pr_year_change_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_pr_2x2_pres <- get(paste("mean_pr_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_pr_2x2_fut <- get(paste("mean_pr_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_pr_year_change_2x2_allmodels[,,m] <- (mean_pr_2x2_fut - mean_pr_2x2_pres)*mask_2x2_NAs
rm(mean_pr_2x2_pres); rm(mean_pr_2x2_fut)}

mean_PET_year_change_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_PET_2x2_pres <- get(paste("mean_PET_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_PET_2x2_fut <- get(paste("mean_PET_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_PET_year_change_2x2_allmodels[,,m] <- (mean_PET_2x2_fut - mean_PET_2x2_pres)*mask_2x2_NAs
rm(mean_PET_2x2_pres); rm(mean_PET_2x2_fut)}

mean_evspsbl_year_change_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_evspsbl_2x2_pres <- get(paste("mean_evspsbl_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_evspsbl_2x2_fut <- get(paste("mean_evspsbl_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_evspsbl_year_change_2x2_allmodels[,,m] <- (mean_evspsbl_2x2_fut - mean_evspsbl_2x2_pres)*mask_2x2_NAs
rm(mean_evspsbl_2x2_pres); rm(mean_evspsbl_2x2_fut)}

mean_hfss_year_change_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_hfss_2x2_pres <- get(paste("mean_hfss_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_hfss_2x2_fut <- get(paste("mean_hfss_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_hfss_year_change_2x2_allmodels[,,m] <- (mean_hfss_2x2_fut - mean_hfss_2x2_pres)*mask_2x2_NAs
rm(mean_hfss_2x2_pres); rm(mean_hfss_2x2_fut)}

mean_netrad_year_change_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_netrad_2x2_pres <- get(paste("mean_netrad_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_netrad_2x2_fut <- get(paste("mean_netrad_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_netrad_year_change_2x2_allmodels[,,m] <- (mean_netrad_2x2_fut - mean_netrad_2x2_pres)*mask_2x2_NAs
rm(mean_netrad_2x2_pres); rm(mean_netrad_2x2_fut)}

mean_rsds_year_change_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_rsds_2x2_pres <- get(paste("mean_rsds_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_rsds_2x2_fut <- get(paste("mean_rsds_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_rsds_year_change_2x2_allmodels[,,m] <- (mean_rsds_2x2_fut - mean_rsds_2x2_pres)*mask_2x2_NAs
rm(mean_rsds_2x2_pres); rm(mean_rsds_2x2_fut)}

mean_rlds_year_change_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_rlds_2x2_pres <- get(paste("mean_rlds_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_rlds_2x2_fut <- get(paste("mean_rlds_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_rlds_year_change_2x2_allmodels[,,m] <- (mean_rlds_2x2_fut - mean_rlds_2x2_pres)*mask_2x2_NAs
rm(mean_rlds_2x2_pres); rm(mean_rlds_2x2_fut)}

mean_rlus_year_change_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_rlus_2x2_pres <- get(paste("mean_rlus_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_rlus_2x2_fut <- get(paste("mean_rlus_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_rlus_year_change_2x2_allmodels[,,m] <- (mean_rlus_2x2_fut - mean_rlus_2x2_pres)*mask_2x2_NAs
rm(mean_rlus_2x2_pres); rm(mean_rlus_2x2_fut)}


mean_PrPET_year_change_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_PrPET_2x2_pres <- get(paste("mean_PrPET_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_PrPET_2x2_fut <- get(paste("mean_PrPET_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_PrPET_year_change_2x2_allmodels[,,m] <- (mean_PrPET_2x2_fut - mean_PrPET_2x2_pres)*mask_2x2_NAs
rm(mean_PrPET_2x2_pres); rm(mean_PrPET_2x2_fut)}

