######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
########## MRSO 
v <- 14
list_models_mrso<-c("ACCESS1-0","ACCESS1-3","bcc-csm1-1","bcc-csm1-1-m","BNU-ESM","CanESM2","CCSM4","CESM1-BGC","CESM1-CAM5", "CESM1-WACCM","CMCC-CM","CNRM-CM5","CSIRO-Mk3-6-0","FGOALS-g2", "GFDL-CM3","GFDL-ESM2G","GFDL-ESM2M","GISS-E2-H","GISS-E2-R","HadGEM2-CC","HadGEM2-ES","inmcm4","IPSL-CM5A-LR","IPSL-CM5A-MR",    "IPSL-CM5B-LR","MIROC-ESM","MIROC-ESM-CHEM","MIROC5","MPI-ESM-LR","MRI-CGCM3","NorESM1-M","NorESM1-ME")

list_models_mrso2 <- c("ACCESS1_0","ACCESS1_3","bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CanESM2","CCSM4","CESM1_BGC","CESM1_CAM5","CESM1_WACCM", "CMCC_CM","CNRM_CM5","CSIRO_Mk3_6_0","FGOALS_g2","GFDL_CM3","GFDL_ESM2G", "GFDL_ESM2M","GISS_E2_H","GISS_E2_R","HadGEM2_CC","HadGEM2_ES","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC_ESM","MIROC_ESM_CHEM","MIROC5","MPI_ESM_LR","MRI_CGCM3","NorESM1_M","NorESM1_ME")

#### Present
for (m in 1:length(list_models_mrso2)){       #models 
print(list_models_mrso[m])
print(list_var[v])
if (list_models_mrso[m] %in% c("CESM1-WACCM", "CNRM-CM5","CSIRO_Mk3_6_0")) {
data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.historical/.",dom[v],"/.mon/.", list_var[v], "/", list_models_mrso[m],"/.r2i1p1/.",list_var[v],"/dods", sep=""))} else {
data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.historical/.",dom[v],"/.mon/.", list_var[v], "/", list_models_mrso[m],"/.r1i1p1/.",list_var[v],"/dods", sep=""))}
bob <- ncvar_get(data, list_var[v])[,,(data$dim$T$len-56*12+1):data$dim$T$len] 
nc_close(data) 
buff <- array(NA, dim=c(dim(bob)[1],dim(bob)[2], 56))
for (t in 1:56){buff[,,t] <- apply(bob[ ,,  (12*(t-1)+1):(12*t)], c(1,2), mean, na.rm=T)}
#assign(paste(list_var[v],"_",list_models_mrso2[m],"_year" , sep=""), buff )
rm(bob);rm(buff)   }

### Future:
for (m in 1:length(list_models_mrso2)){       #models 
print(list_models_mrso[m])
print(list_var[v])
if (list_models_mrso[m] %in% c("CESM1-WACCM", "CNRM-CM5","CSIRO_Mk3_6_0")) {
data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.rcp85/.",dom[v],"/.mon/.", list_var[v], "/", list_models_mrso[m],"/.r2i1p1/.",list_var[v],"/dods", sep=""))} else {
data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.rcp85/.",dom[v],"/.mon/.", list_var[v], "/", list_models_mrso[m],"/.r1i1p1/.",list_var[v],"/dods", sep=""))}
print(data$dim$T$len) 
if (data$dim$T$len > 1140) {
bob <- ncvar_get(data, list_var[v])[,,781:1140];
print("fixing...") 
nc_close(data) 
# now, JJA and DJF means
buff <- array(NA, dim=c(dim(bob)[1],dim(bob)[2], 30))
for (t in 1:30){buff[,,t] <- apply(bob[ ,,  (12*(t-1)+1):(12*t)], c(1,2), mean, na.rm=T)}
assign(paste(list_var[v],"_",list_models_mrso2[m],"_year_rcp85" , sep=""), buff )
rm(bob)}}

######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
########## MRSOS 

v <- 15
list_models_mrsos<-c("ACCESS1-0","ACCESS1-3","bcc-csm1-1","bcc-csm1-1-m","BNU-ESM","CanESM2","CCSM4","CESM1-BGC","CESM1-CAM5", "CESM1-WACCM","CNRM-CM5","CSIRO-Mk3-6-0","FGOALS-g2", "GFDL-CM3","GFDL-ESM2G","GFDL-ESM2M","GISS-E2-H","GISS-E2-R","HadGEM2-CC","HadGEM2-ES","inmcm4","IPSL-CM5A-LR","IPSL-CM5A-MR",    "IPSL-CM5B-LR","MIROC-ESM","MIROC-ESM-CHEM","MIROC5","MRI-CGCM3","NorESM1-M","NorESM1-ME")
list_models_mrsos2 <- c("ACCESS1_0","ACCESS1_3","bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CanESM2","CCSM4","CESM1_BGC","CESM1_CAM5","CESM1_WACCM", "CNRM_CM5","CSIRO_Mk3_6_0","FGOALS_g2","GFDL_CM3","GFDL_ESM2G", "GFDL_ESM2M","GISS_E2_H","GISS_E2_R","HadGEM2_CC","HadGEM2_ES","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC_ESM","MIROC_ESM_CHEM","MIROC5","MRI_CGCM3","NorESM1_M","NorESM1_ME")

## Present
for (m in 1:length(list_models_mrsos2)){       #models 
print(list_models_mrsos[m])
print(list_var[v])
if (list_models_mrsos[m] %in% c("GISS-E2-H")) {data <- nc_open("/home/air3/ab5/CMIP5_data/mrsos/GISS-E2-H/mrsos_Lmon_GISS-E2-H_historical_r1i1p1_195001-200512.nc")} else if (list_models_mrsos[m] %in% c("CESM1-WACCM", "CNRM-CM5","CSIRO_Mk3_6_0")) {
data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.historical/.",dom[v],"/.mon/.", list_var[v], "/", list_models_mrsos[m],"/.r2i1p1/.",list_var[v],"/dods", sep=""))} else {data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.historical/.",dom[v],"/.mon/.", list_var[v], "/", list_models_mrsos[m],"/.r1i1p1/.",list_var[v],"/dods", sep=""))}
lat <- ncvar_get(data, "lat")
lon <- ncvar_get(data, "lon")
if (list_models_mrsos[m] %in% c("GISS-E2-H")){bob <- ncvar_get(data, list_var[v])[,,(data$dim$time$len-56*12+1):data$dim$time$len] } else {
bob <- ncvar_get(data, list_var[v])[,,(data$dim$T$len-56*12+1):data$dim$T$len] }
#assign(paste(list_var[v],"_",list_models_mrsos2[m], sep=""), bob ) 
nc_close(data) 
buff <- array(NA, dim=c(dim(bob)[1],dim(bob)[2], 56))
for (t in 1:30){buff[,,t] <- apply(bob[ ,,  (12*(t-1)+1):(12*t)], c(1,2), mean, na.rm=T)}
assign(paste(list_var[v],"_",list_models_mrsos2[m],"_year" , sep=""), buff )
rm(bob);rm(buff)}

## Future
for (m in 1:length(list_models_mrsos2)){       #models 
print(list_models_mrsos[m])
print(list_var[v])
if (list_models_mrsos[m] %in% c("CESM1-WACCM", "CNRM-CM5","CSIRO_Mk3_6_0")) {
data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.rcp85/.",dom[v],"/.mon/.", list_var[v], "/", list_models_mrsos[m],"/.r2i1p1/.",list_var[v],"/dods", sep=""))} else {
data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.rcp85/.",dom[v],"/.mon/.", list_var[v], "/", list_models_mrsos[m],"/.r1i1p1/.",list_var[v],"/dods", sep=""))}
print(data$dim$T$len)
if (data$dim$T$len > 1140) {
print("fixing...") 
bob <- ncvar_get(data, list_var[v])[,,781:1140];
#bob <- ncvar_get(data, list_var[v])[,,(data$dim$T$len-30*12+1):data$dim$T$len] 
#assign(paste(list_var[v],"_",list_models_mrsos2[m], sep=""), bob ) 
nc_close(data) 
# now, JJA and DJF means
buff <- array(NA, dim=c(dim(bob)[1],dim(bob)[2], 30))
for (t in 1:30){buff[,,t] <- apply(bob[ ,,  (12*(t-1)+1):(12*t)], c(1,2), mean, na.rm=T)}
assign(paste(list_var[v],"_",list_models_mrsos2[m],"_year_rcp85" , sep=""), buff )
rm(bob);rm(buff) }
}


######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
