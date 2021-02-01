
list_var <- append(list_var, "lai")
dom <- append(dom, "land")

list_models_lai <- c("ACCESS1-0","ACCESS1-3","bcc-csm1-1","bcc-csm1-1-m","CanESM2","CCSM4","CESM1-BGC","CESM1-CAM5","CESM1-FASTCHEM","CESM1-WACCM","FIO-ESM","GFDL-CM3","GFDL-ESM2G","GFDL-ESM2M","HadGEM2-CC","HadGEM2-ES","inmcm4","IPSL-CM5A-LR","IPSL-CM5A-MR","IPSL-CM5B-LR","MIROC-ESM","MIROC-ESM-CHEM","MIROC4h","MIROC5","MPI-ESM-LR",
"MPI-ESM-MR","MPI-ESM-P","MRI-ESM1","NorESM1-M","NorESM1-ME")
list_models_lai2 <- c("ACCESS1_0","ACCESS1_3","bcc_csm1_1","bcc_csm1_1_m","CanESM2","CCSM4","CESM1_BGC","CESM1_CAM5","CESM1_FASTCHEM","CESM1_WACCM","FIO_ESM","GFDL_CM3","GFDL_ESM2G","GFDL_ESM2M","HadGEM2_CC","HadGEM2_ES","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC_ESM","MIROC_ESM_CHEM","MIROC4h","MIROC5","MPI_ESM_LR",
"MPI_ESM_MR","MPI_ESM_P","MRI_ESM1","NorESM1_M","NorESM1_ME")


for (m in 1:length(list_models_lai)){
print(list_models_lai[m])
for (v in 14:14){
print(list_var[v])
data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.historical/.",dom[v],"/.mon/.", list_var[v], "/", list_models_lai[m],"/.r1i1p1/.",list_var[v],"/dods", sep=""))
variable <- ncvar_get(data, list_var[v])
if (list_models_lai[m] == "inmcm4") {variable <- variable/100}
assign(paste("lat_", list_models_lai2[m],sep=""), ncvar_get(data, "lat"))
assign(paste("lon_", list_models_lai2[m],sep=""), ncvar_get(data, "lon"))
lon <- ncvar_get(data, "lon")
buff<- dim(variable)[3]
variable<- variable[,,(buff-56*12+1):buff] 
assign(list_var[v], variable ) 
nc_close(data)
rm(variable); rm(buff)}
save(lai, file=paste("lai_",list_models_lai2[m],".RData", sep=""))
}

############################################################################
################### Future LAI
list_models_lai_fut <-c("ACCESS1-0","ACCESS1-3","bcc-csm1-1","bcc-csm1-1-m","CanESM2","CCSM4","CESM1-BGC","CESM1-CAM5","CESM1-WACCM","FIO-ESM","GFDL-CM3","GFDL-ESM2G","GFDL-ESM2M","HadGEM2-CC","HadGEM2-ES","inmcm4","IPSL-CM5A-LR","IPSL-CM5A-MR","IPSL-CM5B-LR","MIROC-ESM","MIROC-ESM-CHEM","MIROC5","MPI-ESM-LR","MPI-ESM-MR","MRI-ESM1","NorESM1-M","NorESM1-ME")
list_models_lai_fut2 <-c("ACCESS1_0","ACCESS1_3","bcc_csm1_1","bcc_csm1_1_m","CanESM2","CCSM4","CESM1_BGC","CESM1_CAM5","CESM1_WACCM","FIO_ESM","GFDL_CM3","GFDL_ESM2G","GFDL_ESM2M","HadGEM2_CC","HadGEM2_ES","inmcm4","IPSL_CM5A_LR","IPSL_CM5A_MR","IPSL_CM5B_LR","MIROC_ESM","MIROC_ESM_CHEM","MIROC5","MPI_ESM_LR","MPI_ESM_MR","MRI_ESM1","NorESM1_M","NorESM1_ME")


for (m in 1:length(list_models_lai_fut)){
print(list_models_lai_fut[m])
for (v in 14:14){
print(list_var[v])
if  (list_models_lai_fut[m] %in% c("CCSM4", "CESM1-WACCM"))
{ data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.rcp85/.",dom[v],"/.mon/.", list_var[v], "/", list_models_lai_fut[m],"/.r2i1p1/.",list_var[v],"/dods", sep="")) } else {data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.rcp85/.",dom[v],"/.mon/.", list_var[v], "/", list_models_lai_fut[m],"/.r1i1p1/.",list_var[v],"/dods", sep=""))}
variable <- ncvar_get(data, list_var[v])
if (list_models_lai_fut[m] == "inmcm4") {variable <- variable/100}
buff<- dim(variable)[3]
if (buff > 1140) {buff <- 1140}
variable<- variable[,,(buff-30*12+1):buff] 
assign(list_var[v], variable ) 
nc_close(data)
rm(variable); rm(buff)}
if (list_models_lai_fut[m] == "inmcm4")
save(lai, file=paste("lai_",list_models_lai_fut2[m],"_rcp85.RData", sep=""))
}


for (m in 1:length(list_models_lai)){
if (list_models_lai[m] %in% list_models_lai_fut) {
print(list_models_lai[m])
load(paste("lai_",list_models_lai2[m],".RData", sep="")) 
assign(paste("mean_lai_",list_models_lai2[m],"_year_pres",sep=""), apply(lai[,,253:612],c(1,2), mean, na.rm=T))
rm(lai)
load(paste("lai_",list_models_lai2[m],"_rcp85.RData", sep="")) 
assign(paste("mean_lai_",list_models_lai2[m],"_year_fut",sep=""),  apply(lai[,,],c(1,2), mean, na.rm=T))
rm(lai)}}

### Regridding
for (m in 1:length(list_models_lai)){
print(list_models_lai2[m])
if (list_models_lai[m] %in% list_models_lai_fut) {
lat <- get(paste("lat_", list_models_lai2[m], sep=""))
lon <- get(paste("lon_", list_models_lai2[m], sep="")) 
mean_lai_year_pres <- get( paste("mean_lai_",list_models_lai2[m],"_year_pres", sep="")) 
mean_lai_year_pres[which(is.na(mean_lai_year_pres)==T)] <- 0 
mean_lai_year_pres_2x2 <- bicubic.grid(lon, lat, mean_lai_year_pres, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_lai_",list_models_lai2[m],"_year_pres_2x2", sep=""), mean_lai_year_pres_2x2)

mean_lai_year_fut <- get( paste("mean_lai_",list_models_lai2[m],"_year_fut", sep="")) 
mean_lai_year_fut[which(is.na(mean_lai_year_fut)==T)] <- 0 
mean_lai_year_fut_2x2 <- bicubic.grid(lon, lat, mean_lai_year_fut, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z 
assign(paste("mean_lai_",list_models_lai2[m],"_year_fut_2x2", sep=""), mean_lai_year_fut_2x2)
}}


############### Let's put all models in a matrix:
#### Lai

mean_lai_year_pres_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_lai_fut)))
for (m in 1:length(list_models_lai_fut)){
print(list_models_lai_fut[m])
mean_lai_2x2 <- get(paste("mean_lai_",list_models_lai_fut2[m],"_year_pres_2x2", sep=""))
mean_lai_year_pres_2x2_allmodels[,,m] <- mean_lai_2x2*mask_2x2_NAs
rm(mean_lai_2x2)}

mean_lai_year_fut_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_lai_fut)))
for (m in 1:length(list_models_lai_fut)){
print(list_models_lai_fut[m])
mean_lai_2x2 <- get(paste("mean_lai_",list_models_lai_fut2[m],"_year_fut_2x2", sep=""))
mean_lai_year_fut_2x2_allmodels[,,m] <- mean_lai_2x2*mask_2x2_NAs
rm(mean_lai_2x2)}


################################################ Change
mean_lai_year_change_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_lai_fut)))
for (m in 1:length(list_models_lai_fut)){
print(list_models_lai_fut[m])
mean_lai_2x2_pres <- get(paste("mean_lai_",list_models_lai_fut2[m],"_year_pres_2x2", sep=""))
mean_lai_2x2_fut <- get(paste("mean_lai_",list_models_lai_fut2[m],"_year_fut_2x2", sep=""))
mean_lai_year_change_2x2_allmodels[,,m] <- (mean_lai_2x2_fut - mean_lai_2x2_pres)*mask_2x2_NAs
rm(mean_lai_2x2_pres); rm(mean_lai_2x2_fut)}


save(list_models_lai_fut, file="list_models_lai_fut.RData")
save(mean_lai_year_change_2x2_allmodels, file="mean_lai_year_change_2x2_allmodels.RData")


###############################################################################
###############################################################################

##### Designing a common list of models between list_models_lai, list_models_lai_fut, and list_models_common_fut
list_models_lai_and_common <- list_models_tas[which( (list_models_tas %in% list_models_lai) & (list_models_tas %in% list_models_lai_fut) &
 (list_models_tas %in% list_models_common_fut))]
list_models_lai_and_common2 <- c( "ACCESS1_0","ACCESS1_3","bcc_csm1_1", "bcc_csm1_1_m",
"CanESM2","CESM1_CAM5", "GFDL_CM3", "GFDL_ESM2G", "GFDL_ESM2M", "HadGEM2_CC", "HadGEM2_ES", "inmcm4",
 "IPSL_CM5A_LR", "IPSL_CM5A_MR", "IPSL_CM5B_LR", "MIROC_ESM",  "MIROC_ESM_CHEM", "MIROC5", "MRI_ESM1")

#Removing models with constant LAI:
list_models_lai_and_common <- list_models_lai_and_common[-c(1,2,18)]
list_models_lai_and_common2 <- list_models_lai_and_common2[-c(1,2,18)]
## That's 16 models only...

