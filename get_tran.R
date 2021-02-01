################################

#### List of models for which we have tran:
list_models_tran_PCMDI <- c("bcc-csm1-1","bcc-csm1-1-m","BNU-ESM","CCSM4","CESM1-BGC","CESM1-CAM5", "CESM1-FASTCHEM","CESM1-WACCM","CMCC-CESM","CNRM-CM5","CNRM-CM5-2","CanESM2","FGOALS-g2", "FGOALS-s2","FIO-ESM","GFDL-CM3","GFDL-ESM2G","GFDL-ESM2M","GISS-E2-H","GISS-E2-H-CC","GISS-E2-R","GISS-E2-R-CC","HadGEM2-AO","inmcm4","IPSL-CM5A-LR","IPSL-CM5A-MR","IPSL-CM5B-LR","MIROC-ESM","MIROC-ESM-CHEM","MIROC4h","MIROC5","MPI-ESM-LR","MPI-ESM-MR","MPI-ESM-P","MRI-CGCM3","MRI-ESM1","NorESM1-M","NorESM1-ME")
list_models_tran_PCMDI2 <- c("bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CCSM4","CESM1_BGC","CESM1_CAM5", "CESM1_FASTCHEM","CESM1_WACCM","CMCC_CESM","CNRM_CM5","CNRM_CM5_2","CanESM2","FGOALS_g2", "FGOALS_s2","FIO_ESM","GFDL_CM3","GFDL_ESM2G","GFDL_ESM2M","GISS_E2_H","GISS_E2_H_CC","GISS_E2_R","GISS_E2_R_CC","HadGEM2_AO","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC_ESM","MIROC_ESM_CHEM","MIROC4h","MIROC5","MPI_ESM_LR","MPI_ESM_MR","MPI_ESM_P","MRI_CGCM3","MRI_ESM1","NorESM1_M","NorESM1_ME")

list_var_evap <- c("evspsbl", "tran", "evspsblsoi", "evspsblveg")


########################################################### Getting Tran #############################
v=2; print(list_var_evap[v])
names_models <-  get(paste("list_models_",list_var_evap[v], "_PCMDI", sep=""))
names_models2 <-  get(paste("list_models_",list_var_evap[v], "_PCMDI2", sep=""))
for (m in 1:length(names_models)){       #models 
print(names_models[m])
data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.historical/.land/.mon/.", list_var_evap[v], "/.", names_models[m],"/.r1i1p1/.",list_var_evap[v],"/dods", sep=""))
lat <- ncvar_get(data, "lat")
lon <- ncvar_get(data, "lon")
assign(paste("lat_",names_models2[m], sep=""), lat)
assign(paste("lon_",names_models2[m], sep=""), lon) 
bob <- ncvar_get(data, list_var_evap[v])[,,(data$dim$T$len-56*12+1):data$dim$T$len] 
nc_close(data) 
buff <- array(NA, dim=c(dim(bob)[1],dim(bob)[2], 56))
for (t in 1:56){buff[,,t] <- apply(bob[ ,,  (12*(t-1)+1):(12*(t-1)+12)], c(1,2), mean, na.rm=T)}
assign(paste(list_var_evap[v],"_",names_models2[m],"_year" , sep=""), buff )
rm(bob); rm(buff) }

## For CESM1-CAM5, Tran is the same as Esoil... You'll have to download one from PCMDI, but it is wrong it is actually Esoil + Tran (see correction below).
v=2; print(list_var_evap[v])
 names_models <-  get(paste("list_models_",list_var_evap[v], "_PCMDI", sep=""))
names_models2 <-  get(paste("list_models_",list_var_evap[v], "_PCMDI2",sep=""))
m=6; print(names_models[m])
data <- nc_open(paste("/home/air3/ab5/CMIP5_data/", list_var_evap[v],"/", names_models[m],"/tran_Lmon_", names_models[m], "_historical_r1i1p1_185001-200512.nc", sep=""))
lat <- ncvar_get(data, "lat")
lon <- ncvar_get(data, "lon")
assign(paste("lat_",names_models2[m], sep=""), lat)
assign(paste("lon_",names_models2[m], sep=""), lon) 
bob <- ncvar_get(data, list_var_evap[v])[,,(data$dim$time$len-56*12+1):data$dim$time$len] 
nc_close(data)
buff <- array(NA, dim=c(dim(bob)[1],dim(bob)[2], 56))
for (t in 1:56){buff[,,t] <- apply(bob[ ,,  (12*(t-1)+1):(12*(t-1)+12)], c(1,2), mean, na.rm=T)}
assign(paste(list_var_evap[v],"_",names_models2[m],"_year" , sep=""), buff )
rm(bob); rm(buff) }


plus_models <-  c("CNRM-CM5",  "CNRM-CM5-2","IPSL-CM5A-LR", "IPSL-CM5A-MR", "IPSL-CM5B-LR")
plus_models2 <-  c("CNRM_CM5" , "CNRM_CM5_2", "IPSL_CM5A_LR", "IPSL_CM5A_MR", "IPSL_CM5B_LR")
for (m in 1:length(plus_models)){       #models 
print(plus_models[m])
data <- nc_open(paste("/home/air3/ab5/CMIP5_data/", list_var_evap[v],"/", plus_models[m],"/tran_Lmon_",  plus_models[m], "_historical_r1i1p1_allmonths.nc", sep=""))
lat <- ncvar_get(data, "lat")
lon <- ncvar_get(data, "lon")
assign(paste("lat_",plus_models2[m], sep=""), lat)
assign(paste("lon_",plus_models2[m], sep=""), lon) 
bob <- ncvar_get(data, list_var_evap[v])[,,(data$dim$time$len-56*12+1):data$dim$time$len] 
nc_close(data)
buff <- array(NA, dim=c(dim(bob)[1],dim(bob)[2], 56))
for (t in 1:56){buff[,,t] <- apply(bob[ ,,  (12*(t-1)+1):(12*(t-1)+12)], c(1,2), mean, na.rm=T)}
assign(paste(list_var_evap[v],"_",names_models2[m],"_year" , sep=""), buff )
rm(bob); rm(buff) }

########################################################## Evspsblsoi #############################################
### Download the data from PCMDI
### We take Esoil to correct some NCAR models:

list_models_evspsblsoi_PCMDI<-c("ACCESS1-0","ACCESS1-3","bcc-csm1-1","bcc-csm1-1-m","BNU-ESM","CCSM4","CESM1-BGC","CESM1-CAM5","CESM1-FASTCHEM","CESM1-WACCM","CMCC-CESM","CMCC-CM","CNRM-CM5","CNRM-CM5-2","CanESM2","FGOALS-g2","FGOALS-s2","FIO-ESM","GFDL-ESM2G","GFDL-ESM2M","GISS-E2-H","GISS-E2-H-CC","GISS-E2-R","GISS-E2-R-CC","HadGEM2-AO","inmcm4","IPSL-CM5A-LR","IPSL-CM5A-MR","IPSL-CM5B-LR","MIROC-ESM","MIROC-ESM-CHEM","MIROC4h","MIROC5","MRI-CGCM3","MRI-ESM1","NorESM1-M","NorESM1-ME")
list_models_evspsblsoi_PCMDI2<-c("ACCESS1_0","ACCESS1_3","bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CCSM4","CESM1_BGC","CESM1_CAM5","CESM1_FASTCHEM","CESM1_WACCM","CMCC_CESM","CMCC_CM","CNRM_CM5","CNRM_CM5_2","CanESM2","FGOALS_g2","FGOALS_s2","FIO_ESM","GFDL_ESM2G","GFDL_ESM2M","GISS_E2_H","GISS_E2_H_CC","GISS_E2_R","GISS_E2_R_CC","HadGEM2_AO","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC_ESM","MIROC_ESM_CHEM","MIROC4h","MIROC5","MRI_CGCM3","MRI_ESM1","NorESM1_M","NorESM1_ME")

v <- 3
print(list_var_evap[v])
for (m in 1:length(list_models_evspsblsoi_PCMDI)){       #models 
data <- nc_open(paste("/home/air3/ab5/CMIP5_data/",list_var_evap[v], "/", list_models_evspsblsoi_PCMDI[m],"/",list_var_evap[v],"_Lmon_",list_models_evspsblsoi_PCMDI[m],"_historical_r1i1p1_allmonths.nc", sep=""))
bob <- ncvar_get(data, list_var_evap[v])[,,(data$dim$time$len-56*12+1):data$dim$time$len] 
nc_close(data) 
buff <- array(NA, dim=c(dim(bob)[1],dim(bob)[2], 56))
for (t in 1:56){buff[,,t] <- apply(bob[ ,,  (12*(t-1)+1):(12*(t-1)+12)], c(1,2), mean, na.rm=T)}
assign(paste(list_var_evap[v],"_",names_models2[m],"_year" , sep=""), buff )
rm(bob); rm(buff) }


## We have to correct GFDL-ESM2M  - We take it from LDEO - also FGOALS-g2:
v <- 3
for (m in c(16, 20)){
print(list_models_evspsblsoi_PCMDI[m])
## We need to get that model:
data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.historical/.land/.mon/.", list_var_evap[v], "/.", list_models_evspsblsoi_PCMDI[m],"/.r1i1p1/.",list_var_evap[v],"/dods", sep=""))
bob <- ncvar_get(data, list_var_evap[v])[,,(data$dim$T$len-56*12+1):data$dim$T$len] 
nc_close(data)
buff <- array(NA, dim=c(dim(bob)[1],dim(bob)[2], 56))
for (t in 1:56){buff[,,t] <- apply(bob[ ,,  (12*(t-1)+1):(12*(t-1)+12)], c(1,2), mean, na.rm=T)}
assign(paste(list_var_evap[v],"_",names_models2[m],"_year" , sep=""), buff )
rm(bob); rm(buff) 

########################################################## Evspsblveg #############################################
### Not needed here.

#################################################################################################
### We have to correct some NCAR models: what we have is TRAN+EVSPSBLSOI. Substracting EVSPSBLSOI
tran_CCSM4 <- tran_CCSM4 - evspsblsoi_CCSM4
tran_CESM1_BGC <- tran_CESM1_BGC - evspsblsoi_CESM1_BGC
tran_CESM1_FASTCHEM <- tran_CESM1_FASTCHEM - evspsblsoi_CESM1_FASTCHEM
tran_CESM1_WACCM <- tran_CESM1_WACCM - evspsblsoi_CESM1_WACCM
tran_CESM1_CAM5 <- tran_CESM1_CAM5 - evspsblsoi_CESM1_CAM5
tran_CESM1_CAM5[which(tran_CESM1_CAM5 < 0 )] <- 0

for (m in 1:length(list_models_tran_PCMDI)){
print(list_models_tran_PCMDI[m])
tran_year <- get(paste("tran_",list_models_tran_PCMDI2[m],"_year" , sep=""))
save(tran_year, file=paste("tran_", list_models_tran_PCMDI_fut2[m],"_year.RData", sep="")) }

