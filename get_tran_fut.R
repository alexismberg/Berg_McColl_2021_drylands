
### List of models with tran in the future:
list_models_tran_PCMDI_fut <- c("bcc-csm1-1","bcc-csm1-1-m","BNU-ESM","CCSM4","CESM1-BGC","CESM1-CAM5","CESM1-WACCM","CMCC-CESM","CanESM2","FIO-ESM","GFDL-CM3","GFDL-ESM2G","GFDL-ESM2M","GISS-E2-H","GISS-E2-H-CC","GISS-E2-R","GISS-E2-R-CC","HadGEM2-AO","inmcm4","IPSL-CM5A-LR","IPSL-CM5A-MR","IPSL-CM5B-LR","MIROC-ESM","MIROC-ESM-CHEM","MIROC5","MPI-ESM-LR","MPI-ESM-MR","MRI-CGCM3","MRI-ESM1","NorESM1-M","NorESM1-ME")
list_models_tran_PCMDI_fut2 <- c("bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CCSM4","CESM1_BGC","CESM1_CAM5","CESM1_WACCM","CMCC_CESM","CanESM2","FIO_ESM","GFDL_CM3","GFDL_ESM2G","GFDL_ESM2M","GISS_E2_H","GISS_E2_H_CC","GISS_E2_R","GISS_E2_R_CC","HadGEM2_AO","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC_ESM","MIROC_ESM_CHEM","MIROC5","MPI_ESM_LR","MPI_ESM_MR","MRI_CGCM3","MRI_ESM1","NorESM1_M","NorESM1_ME")

list_var_evap <- c("evspsbl", "tran", "evspsblsoi", "evspsblveg")
####################################################################################################################
####################################################################################################################
####################################################################################################################
########################################################### Getting Tran ###########################################
v=2; print(list_var_evap[v])
names_models <-  get(paste("list_models_",list_var_evap[v], "_PCMDI_fut", sep=""))
names_models2 <-  get(paste("list_models_",list_var_evap[v], "_PCMDI_fut2", sep=""))
for (m in 1:length(names_models)){
print(names_models[m])
if (names_models[m] %in% c("CESM1-WACCM")) {data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.rcp85/.land/.mon/.", list_var_evap[v], "/.", names_models[m],"/.r2i1p1/.",list_var_evap[v],"/dods", sep=""))}
else { data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.rcp85/.land/.mon/.", list_var_evap[v], "/.", names_models[m],"/.r1i1p1/.",list_var_evap[v],"/dods", sep="")) }
if (data$dim$T$len > 1140) {bob <- ncvar_get(data, list_var_evap[v])[,,781:1140] } else {
bob <- ncvar_get(data, list_var_evap[v])[,,(data$dim$T$len-30*12+1):data$dim$T$len]}
nc_close(data) 
buff <- array(NA, dim=c(dim(bob)[1],dim(bob)[2], 30))
for (t in 1:30){buff[,,t] <- apply(bob[ ,,  (12*(t-1)+1):(12*(t-1)+12)], c(1,2), mean, na.rm=T)}
assign(paste(list_var_evap[v],"_",names_models2[m],"_year_rcp85" , sep=""), buff )
rm(buff)   }

### Let's fix bcc-csm1-1-m, and take tran for 2070-2099
v=2; print(list_var_evap[v])
names_models <-  get(paste("list_models_",list_var_evap[v], "_PCMDI_fut", sep=""))
names_models2 <-  get(paste("list_models_",list_var_evap[v], "_PCMDI_fut2", sep=""))
m=1;print(names_models[m])
data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.rcp85/.land/.mon/.", list_var_evap[v], "/.", names_models[m],"/.r1i1p1/.",list_var_evap[v],"/dods", sep="")) 
bob <- ncvar_get(data, list_var_evap[v])[,,769:1128] ; nc_close(data) 
buff <- array(NA, dim=c(dim(bob)[1],dim(bob)[2], 30))
for (t in 1:30){buff[,,t] <- apply(bob[ ,,  (12*(t-1)+1):(12*(t-1)+12)], c(1,2), mean, na.rm=T)}
assign(paste(list_var_evap[v],"_",names_models2[m],"_year_rcp85" , sep=""), buff )
rm(buff)   

####################################################################################################################
########################################################## Evspsblsoi #############################################
list_models_evspsblsoi_PCMDI_fut <- c("ACCESS1-0","ACCESS1-3","bcc-csm1-1","bcc-csm1-1-m","BNU-ESM","CCSM4","CESM1-BGC","CESM1-CAM5","CESM1-WACCM","CMCC-CESM","CMCC-CM","CNRM-CM5","CanESM2","FGOALS-g2","FGOALS-s2","FIO-ESM","GFDL-ESM2G","GFDL-ESM2M","GISS-E2-H","GISS-E2-H-CC","GISS-E2-R","GISS-E2-R-CC","HadGEM2-AO","inmcm4","IPSL-CM5A-LR","IPSL-CM5A-MR","IPSL-CM5B-LR","MIROC-ESM","MIROC-ESM-CHEM","MIROC5","MRI-CGCM3","MRI-ESM1","NorESM1-M","NorESM1-ME")

list_models_evspsblsoi_PCMDI_fut2<- c("ACCESS1_0","ACCESS1_3","bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CCSM4","CESM1_BGC","CESM1_CAM5","CESM1_WACCM","CMCC_CESM","CMCC_CM","CNRM_CM5","CanESM2","FGOALS_g2","FGOALS_s2","FIO_ESM","GFDL_ESM2G","GFDL_ESM2M","GISS_E2_H","GISS_E2_H_CC","GISS_E2_R","GISS_E2_R_CC","HadGEM2_AO","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC_ESM","MIROC_ESM_CHEM","MIROC5","MRI_CGCM3","MRI_ESM1","NorESM1_M","NorESM1_ME")

v <- 3
print(list_var_evap[v])
for (m in 11:length(list_models_evspsblsoi_PCMDI_fut)){      
print(list_models_evspsblsoi_PCMDI_fut[m])
if (list_models_evspsblsoi_PCMDI_fut[m] %in% c("CESM1-WACCM")) {data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.rcp85/.land/.mon/.",list_var_evap[v], "/.", list_models_evspsblsoi_PCMDI_fut[m],"/.r2i1p1/.",list_var_evap[v],"/dods", sep=""))}  else {data <- nc_open(paste("/n/holystore01/LABS/mccoll_lab/Lab/aberg/DATA/CMIP5/",list_var_evap[v], "/rcp85/", list_models_evspsblsoi_PCMDI_fut[m],"/",list_var_evap[v],
"_Lmon_",list_models_evspsblsoi_PCMDI_fut[m],"_rcp85_r1i1p1_200601-210012.nc", sep=""))}
bab <- ncvar_get(data, list_var_evap[v]); duration <- dim(bab)[3]
if (duration > 1140) {bob <- ncvar_get(data, list_var_evap[v])[,,781:1140] } else {
bob <- ncvar_get(data, list_var_evap[v])[,,(duration-30*12+1):duration]}
nc_close(data) 
buff <- array(NA, dim=c(dim(bob)[1],dim(bob)[2], 30))
for (t in 1:30){buff[,,t] <- apply(bob[ ,,  (12*(t-1)+1):(12*(t-1)+12)], c(1,2), mean, na.rm=T)}
assign(paste(list_var_evap[v],"_",list_models_evspsblsoi_PCMDI_fut2[m],"_year_rcp85" , sep=""), buff )
rm(buff)}

## Now all the data we need is at LDEO anyways... Let's fix these models:
v <- 3; print(list_var_evap[v])
for (m in 1:length(list_models_evspsblsoi_PCMDI_fut)){      
print(list_models_evspsblsoi_PCMDI_fut[m])
if (list_models_evspsblsoi_PCMDI_fut[m] %in% c("bcc-csm1-1", "bcc-csm1-1-m","CCSM4", "MIROC-ESM","NorESM1-ME")) {
print(list_models_evspsblsoi_PCMDI_fut[m])
data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.rcp85/.land/.mon/.",list_var_evap[v], "/.", list_models_evspsblsoi_PCMDI_fut[m],"/.r1i1p1/.",list_var_evap[v],"/dods", sep="" ))  
bab <- ncvar_get(data, list_var_evap[v]); duration <- dim(bab)[3]
if (duration > 1140) {bob <- ncvar_get(data, list_var_evap[v])[,,781:1140] } else {
bob <- ncvar_get(data, list_var_evap[v])[,,(duration-30*12+1):duration]}
# Because for BCC models evspsbl only goes to 2099 (not 2100), we have to take different years, aka 2070-2099. We did that #for mrsos and for evspsbl, too:
if (list_models_mrsos_PCMDI_fut[m] %in% c( "bcc-csm1-1", "bcc-csm1-1-m")) {bob <- ncvar_get(data, #list_var_evap[v])[,,769:1128] }
nc_close(data) 
buff <- array(NA, dim=c(dim(bob)[1],dim(bob)[2], 30))
for (t in 1:30){buff[,,t] <- apply(bob[ ,,  (12*(t-1)+1):(12*(t-1)+12)], c(1,2), mean, na.rm=T)}
assign(paste(list_var_evap[v],"_",list_models_evspsblsoi_PCMDI_fut2[m],"_year_rcp85" , sep=""), buff )
rm(buff)}}

####################################################################################################################
####################################################################################################################
########################################################## Evspsblveg #############################################
list_models_evspsblveg_PCMDI_fut <- c("ACCESS1-0","ACCESS1-3","bcc-csm1-1","bcc-csm1-1-m","BNU-ESM","CCSM4","CESM1-BGC","CESM1-CAM5","CESM1-WACCM", "CNRM-CM5","CanESM2","FGOALS-g2","FGOALS-s2","FIO-ESM","GFDL-ESM2G","GFDL-ESM2M","GISS-E2-H","GISS-E2-H-CC","GISS-E2-R","GISS-E2-R-CC","HadGEM2-AO","HadGEM2-CC","HadGEM2-ES","inmcm4","IPSL-CM5A-LR","IPSL-CM5A-MR","IPSL-CM5B-LR","MIROC-ESM","MIROC-ESM-CHEM","MIROC5","MRI-CGCM3","NorESM1-M","NorESM1-ME")
list_models_evspsblveg_PCMDI_fut2 <-c("ACCESS1_0","ACCESS1_3","bcc_csm1_1","bcc_csm1_1_m","BNU_ESM","CCSM4","CESM1_BGC","CESM1_CAM5","CESM1_WACCM" ,  "CNRM_CM5","CanESM2","FGOALS_g2","FGOALS_s2","FIO_ESM","GFDL_ESM2G","GFDL_ESM2M","GISS_E2_H","GISS_E2_H_CC","GISS_E2_R","GISS_E2_R_CC","HadGEM2_AO","HadGEM2_CC","HadGEM2_ES","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC_ESM","MIROC_ESM_CHEM","MIROC5","MRI_CGCM3","NorESM1_M","NorESM1_ME")
## we removed "MRI-ESM1": in theory, the data exists, but not accessible

v <- 4
print(list_var_evap[v])
for (m in 1:length(list_models_evspsblveg_PCMDI_fut)){      
print(list_models_evspsblveg_PCMDI_fut[m])
if (list_models_evspsblveg_PCMDI_fut[m] %in% c("CESM1-WACCM")) {data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.rcp85/.land/.mon/.",list_var_evap[v], "/.", list_models_evspsblveg_PCMDI_fut[m],"/.r2i1p1/.",list_var_evap[v],"/dods", sep=""))}  else {data <- nc_open(paste("/n/holystore01/LABS/mccoll_lab/Lab/aberg/DATA/CMIP5/",list_var_evap[v], "/rcp85/", list_models_evspsblveg_PCMDI_fut[m],"/",list_var_evap[v],"_Lmon_",list_models_evspsblveg_PCMDI_fut[m],"_rcp85_r1i1p1_allmonths.nc", sep=""))}
bab <- ncvar_get(data, list_var_evap[v]); duration <- dim(bab)[3]
if (duration > 1140) {bob <- ncvar_get(data, list_var_evap[v])[,,781:1140] } else {
bob <- ncvar_get(data, list_var_evap[v])[,,(duration-30*12+1):duration]}
nc_close(data) 
buff <- array(NA, dim=c(dim(bob)[1],dim(bob)[2], 30))
for (t in 1:30){buff[,,t] <- apply(bob[ ,,  (12*(t-1)+1):(12*(t-1)+12)], c(1,2), mean, na.rm=T)}
assign(paste(list_var_evap[v],"_",list_models_evspsblveg_PCMDI_fut2[m],"_year_rcp85" , sep=""), buff )
rm(bob);  rm(buff)}

## Now all the data we need is at LDEO anyways... Let's fix these models:
v <- 4; print(list_var_evap[v])
for (m in 1:length(list_models_evspsblveg_PCMDI_fut)){      
print(list_models_evspsblveg_PCMDI_fut[m])
if (list_models_evspsblveg_PCMDI_fut[m] %in% c("bcc-csm1-1", "bcc-csm1-1-m","CCSM4", "HadGEM2-CC")) {
print(list_models_evspsblveg_PCMDI_fut[m])
data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.rcp85/.land/.mon/.",list_var_evap[v], "/.", list_models_evspsblveg_PCMDI_fut[m],"/.r1i1p1/.",list_var_evap[v],"/dods", sep="" ))  
bab <- ncvar_get(data, list_var_evap[v]); duration <- dim(bab)[3]
if (duration > 1140) {bob <- ncvar_get(data, list_var_evap[v])[,,781:1140] } else {
bob <- ncvar_get(data, list_var_evap[v])[,,(duration-30*12+1):duration]}
# Because for BCC models evspsbl only goes to 2099 (not 2100), we have to take different years, aka 2070-2099. We did that for mrsos and for #evspsbl, too:
if (list_models_mrsos_PCMDI_fut[m] %in% c( "bcc-csm1-1", "bcc-csm1-1-m") {bob <- ncvar_get(data, list_var_evap[v])[,,769:1128] }
nc_close(data) 
buff <- array(NA, dim=c(dim(bob)[1],dim(bob)[2], 30))
for (t in 1:30){buff[,,t] <- apply(bob[ ,,  (12*(t-1)+1):(12*(t-1)+12)], c(1,2), mean, na.rm=T)}
assign(paste(list_var_evap[v],"_",list_models_evspsblveg_PCMDI_fut2[m],"_year_rcp85" , sep=""), buff )
rm(buff)}}


## For Hadley models we need another fix:
v <- 4; print(list_var_evap[v])
for (m in 1:length(list_models_evspsblveg_PCMDI_fut)){      
print(list_models_evspsblveg_PCMDI_fut[m])
if (list_models_evspsblveg_PCMDI_fut[m] %in% c("HadGEM2-CC", "HadGEM2-ES")) {
print(list_models_evspsblveg_PCMDI_fut[m])
data <- nc_open(paste("/n/holystore01/LABS/mccoll_lab/Lab/aberg/DATA/CMIP5/",list_var_evap[v], "/rcp85/", list_models_evspsblveg_PCMDI_fut[m],"/",list_var_evap[v],"_Lmon_",list_models_evspsblveg_PCMDI_fut[m],"_rcp85_r1i1p1_allmonths.nc", sep=""))
bob <- ncvar_get(data, list_var_evap[v])[,,769:1128]
nc_close(data) 
buff <- array(NA, dim=c(dim(bob)[1],dim(bob)[2], 30))
for (t in 1:30){buff[,,t] <- apply(bob[ ,,  (12*(t-1)+1):(12*(t-1)+12)], c(1,2), mean, na.rm=T)}
assign(paste(list_var_evap[v],"_",list_models_evspsblveg_PCMDI_fut2[m],"_year_rcp85" , sep=""), buff )
rm(bob);rm(bill); rm(bill2);rm(buff)}}


#########################################################################################################################
#########################################################################################################################
### We have to correct some NCAR models: what we have is TRAN+EVSPSBLSOI. Substracting EVSPSBLSOI
##BGC is okay.

#CCSM4
tran_CCSM4_year_rcp85 <- evspsbl_CCSM4_year_rcp85 - evspsblsoi_CCSM4_year_rcp85 - evspsblveg_CCSM4_year_rcp85
#CESM1_CAM5
tran_CESM1_CAM5_year_rcp85 <- evspsbl_CESM1_CAM5_year_rcp85 - evspsblsoi_CESM1_CAM5_year_rcp85 - evspsblveg_CESM1_CAM5_year_rcp85

for (m in 1:length(list_models_tran_PCMDI_fut)){
print(list_models_tran_PCMDI_fut[m])
tran_year_rcp85 <- get(paste("tran_",list_models_tran_PCMDI_fut2[m],"_year_rcp85" , sep=""))
save(tran_year_rcp85, file=paste("tran_", list_models_tran_PCMDI_fut2[m],"_year_rcp85.RData", sep="")) }
