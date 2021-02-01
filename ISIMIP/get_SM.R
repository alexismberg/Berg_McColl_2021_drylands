################################
#.libPaths("/n/holystore01/LABS/mccoll_lab/Lab/aberg///R/x86_64-redhat-linux-gnu-library/3.2")
library(ncdf, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg///R/x86_64-redhat-linux-gnu-library/3.2")
library(colorRamps, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg///R/x86_64-redhat-linux-gnu-library/3.2")
library(maps)#, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg///R/x86_64-redhat-linux-gnu-library/3.2")
library(ncdf4)#, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg///R/x86_64-redhat-linux-gnu-library/3.2")
library(fields)#, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg///R/x86_64-redhat-linux-gnu-library/3.2")
library(akima)#, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg///R/x86_64-redhat-linux-gnu-library/3.2")
#library(LSD)#, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg///R/x86_64-redhat-linux-gnu-library/3.2")
library(RColorBrewer, lib.loc="/n/holystore01/LABS/mccoll_lab/Lab/aberg///R/x86_64-redhat-linux-gnu-library/3.2")

### We are only going to take 2 simulations: 2005soc and 2005soc_2005CO2
## That way we will compare with and without CO2 (for fixed 2005soc).
## To study the effect of land-use change, we'll have to look at 2005soc vs rcp60 for a subset of models...
## That gives us four models: ORCHIDEE, CARAIB, LPJ-GUESS, VISIT
## For HIST we take simulation with full HIST scenario for society and CO2. Same 4 models...

################################################################
#### SOILMOIST from lpj_guess:
forcing <- c("gfdl-esm2m", "ipsl-cm5a-lr", "hadgem2-es", "miroc5")
forcing2 <- c("gfdl_esm2m", "ipsl_cm5a_lr", "hadgem2_es", "miroc5")
########### LPJ_GUESS
###  HIST
for (m in 1:4){ print(forcing[m])
data <- nc_open(paste("/n/holystore01/LABS/mccoll_lab/Lab/aberg/DATA/ISIMIP/HIST/lpj-guess_",
forcing[m],"_ewembi_historical_histsoc_co2_soilmoist_global_yearly_1861_2005.nc", sep=""))
soilmoist_lpj_guess_historical_histsoc_co2 <- ncvar_get(data, "soilmoist")[,360:1,1,]
nc_close(data)
assign(paste("soilmoist_",forcing2[m],"_lpj_guess_historical_histsoc_co2_year",sep=""), soilmoist_lpj_guess_historical_histsoc_co2)
rm(soilmoist_lpj_guess_historical_histsoc_co2) }
### RCP60, C02=2005
for (m in 1:4){ print(forcing[m])
data <- nc_open(paste("/n/holystore01/LABS/mccoll_lab/Lab/aberg/DATA/ISIMIP/RCP60/lpj-guess_",
forcing[m],"_ewembi_rcp60_2005soc_2005co2_soilmoist_global_yearly_2006_2099.nc", sep=""))
soilmoist_lpj_guess_rcp60_2005soc_2005co2 <- ncvar_get(data, "soilmoist")[,360:1,1,]
nc_close(data); 
assign(paste("soilmoist_",forcing2[m],"_lpj_guess_rcp60_2005soc_2005co2_year",sep=""), soilmoist_lpj_guess_rcp60_2005soc_2005co2)
rm(soilmoist_lpj_guess_rcp60_2005soc_2005co2 )}
### RCP60, CO2=RCP60
for (m in 1:4){ print(forcing[m])
data <- nc_open(paste("/n/holystore01/LABS/mccoll_lab/Lab/aberg/DATA/ISIMIP/RCP60/lpj-guess_",
forcing[m],"_ewembi_rcp60_2005soc_co2_soilmoist_global_yearly_2006_2099.nc", sep=""))
soilmoist_lpj_guess_rcp60_2005soc_co2 <- ncvar_get(data, "soilmoist")[,360:1,1,]
nc_close(data)
assign(paste("soilmoist_",forcing2[m],"_lpj_guess_rcp60_2005soc_co2_year",sep=""), soilmoist_lpj_guess_rcp60_2005soc_co2)
rm(soilmoist_lpj_guess_rcp60_2005soc_co2 ) }

########### ORCHIDEE
###  HIST
for (m in 1:4){ print(forcing[m])
data <- nc_open(paste("/n/holystore01/LABS/mccoll_lab/Lab/aberg/DATA/ISIMIP/HIST/orchidee_",
forcing[m],"_ewembi_historical_histsoc_co2_soilmoist_global_yearly_1861_2005.nc", sep=""))
soilmoist_orchidee_historical_histsoc_co2 <- apply(ncvar_get(data, "soilmoist")[,360:1,1:9,], c(1,2,4), sum,na.rm=T)
nc_close(data)
assign(paste("soilmoist_",forcing2[m],"_orchidee_historical_histsoc_co2_year",sep=""), soilmoist_orchidee_historical_histsoc_co2)
rm(soilmoist_orchidee_historical_histsoc_co2) }
### RCP60, C02=2005
for (m in 1:4){ print(forcing[m])
data <- nc_open(paste("/n/holystore01/LABS/mccoll_lab/Lab/aberg/DATA/ISIMIP/RCP60/orchidee_",
forcing[m],"_ewembi_rcp60_2005soc_2005co2_soilmoist_global_yearly_2006_2099.nc", sep=""))
soilmoist_orchidee_rcp60_2005soc_2005co2 <- apply(ncvar_get(data, "soilmoist")[,360:1,1:9,], c(1,2,4), sum,na.rm=T)
nc_close(data);
assign(paste("soilmoist_",forcing2[m],"_orchidee_rcp60_2005soc_2005co2_year",sep=""), soilmoist_orchidee_rcp60_2005soc_2005co2)
rm(soilmoist_orchidee_rcp60_2005soc_2005co2 )}
### RCP60, CO2=RCP60
for (m in 1:4){ print(forcing[m])
data <- nc_open(paste("/n/holystore01/LABS/mccoll_lab/Lab/aberg/DATA/ISIMIP/RCP60/orchidee_",
forcing[m],"_ewembi_rcp60_2005soc_co2_soilmoist_global_yearly_2006_2099.nc", sep=""))
soilmoist_orchidee_rcp60_2005soc_co2 <- apply(ncvar_get(data, "soilmoist")[,360:1,1:9,], c(1,2,4), sum,na.rm=T)
nc_close(data)
assign(paste("soilmoist_",forcing2[m],"_orchidee_rcp60_2005soc_co2_year",sep=""), soilmoist_orchidee_rcp60_2005soc_co2)
rm(soilmoist_orchidee_rcp60_2005soc_co2 ) }

########### CARAIB
###  HIST
for (m in 1:4){ print(forcing[m])
data <- nc_open(paste("/n/holystore01/LABS/mccoll_lab/Lab/aberg/DATA/ISIMIP/HIST/caraib_",
forcing[m],"_ewembi_historical_histsoc_co2_soilmoist_global_yearly_1861_2005.nc", sep=""))
soilmoist_caraib_historical_histsoc_co2 <- ncvar_get(data, "soilmoist")[,360:1,]
nc_close(data)
assign(paste("soilmoist_",forcing2[m],"_caraib_historical_histsoc_co2_year",sep=""), soilmoist_caraib_historical_histsoc_co2)
rm(soilmoist_caraib_historical_histsoc_co2) }
### RCP60, C02=2005
for (m in 1:4){ print(forcing[m])
data <- nc_open(paste("/n/holystore01/LABS/mccoll_lab/Lab/aberg/DATA/ISIMIP/RCP60/caraib_",
forcing[m],"_ewembi_rcp60_2005soc_2005co2_soilmoist_global_yearly_2006_2099.nc", sep=""))
soilmoist_caraib_rcp60_2005soc_2005co2 <- ncvar_get(data, "soilmoist")[,360:1,]
nc_close(data);
assign(paste("soilmoist_",forcing2[m],"_caraib_rcp60_2005soc_2005co2_year",sep=""), soilmoist_caraib_rcp60_2005soc_2005co2)
rm(soilmoist_caraib_rcp60_2005soc_2005co2 )}
### RCP60, CO2=RCP60
for (m in 1:4){ print(forcing[m])
data <- nc_open(paste("/n/holystore01/LABS/mccoll_lab/Lab/aberg/DATA/ISIMIP/RCP60/caraib_",
forcing[m],"_ewembi_rcp60_2005soc_co2_soilmoist_global_yearly_2006_2099.nc", sep=""))
soilmoist_caraib_rcp60_2005soc_co2 <- ncvar_get(data, "soilmoist")[,360:1,]
nc_close(data)
assign(paste("soilmoist_",forcing2[m],"_caraib_rcp60_2005soc_co2_year",sep=""), soilmoist_caraib_rcp60_2005soc_co2)
rm(soilmoist_caraib_rcp60_2005soc_co2 ) }



#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
########################
### Multi model means (forcings...)
#forcing <- c("gfdl-esm2m", "ipsl-cm5a-lr", "hadgem2-es", "miroc5")
### just edit the lines below with the four different veg. models:
## LPJ_GUESS
soilmoist_MMM_lpj_guess_historical_histsoc_co2_year <- array(NA, dim=dim(soilmoist_miroc5_lpj_guess_historical_histsoc_co2_year))
for (i in 1:length(lon_lpj_guess)) {print(i)
for (j in 1:length(lat_lpj_guess)) {
bob <- rbind(soilmoist_gfdl_esm2m_lpj_guess_historical_histsoc_co2_year[i,j,],soilmoist_ipsl_cm5a_lr_lpj_guess_historical_histsoc_co2_year[i,j,],
soilmoist_hadgem2_es_lpj_guess_historical_histsoc_co2_year[i,j,],soilmoist_miroc5_lpj_guess_historical_histsoc_co2_year[i,j,])
soilmoist_MMM_lpj_guess_historical_histsoc_co2_year[i,j,] <- colMeans(bob, na.rm=T) ; rm(bob) }}

soilmoist_MMM_lpj_guess_rcp60_2005soc_co2_year <- array(NA, dim=dim(soilmoist_miroc5_lpj_guess_rcp60_2005soc_co2_year))
for (i in 1:length(lon_lpj_guess)) {print(i)
for (j in 1:length(lat_lpj_guess)) {
bob <- rbind(soilmoist_gfdl_esm2m_lpj_guess_rcp60_2005soc_co2_year[i,j,],soilmoist_ipsl_cm5a_lr_lpj_guess_rcp60_2005soc_co2_year[i,j,],
soilmoist_hadgem2_es_lpj_guess_rcp60_2005soc_co2_year[i,j,],soilmoist_miroc5_lpj_guess_rcp60_2005soc_co2_year[i,j,])
soilmoist_MMM_lpj_guess_rcp60_2005soc_co2_year[i,j,] <- colMeans(bob, na.rm=T)  ; rm(bob)}}

soilmoist_MMM_lpj_guess_rcp60_2005soc_2005co2_year <- array(NA, dim=dim(soilmoist_miroc5_lpj_guess_rcp60_2005soc_2005co2_year))
for (i in 1:length(lon_lpj_guess)) {print(i)
for (j in 1:length(lat_lpj_guess)) {
bob <- rbind(soilmoist_gfdl_esm2m_lpj_guess_rcp60_2005soc_2005co2_year[i,j,],soilmoist_ipsl_cm5a_lr_lpj_guess_rcp60_2005soc_2005co2_year[i,j,],
soilmoist_hadgem2_es_lpj_guess_rcp60_2005soc_2005co2_year[i,j,],soilmoist_miroc5_lpj_guess_rcp60_2005soc_2005co2_year[i,j,])
soilmoist_MMM_lpj_guess_rcp60_2005soc_2005co2_year[i,j,] <- colMeans(bob, na.rm=T) ; rm(bob) }}

## ORCHIDEE
soilmoist_MMM_orchidee_historical_histsoc_co2_year <- array(NA, dim=dim(soilmoist_miroc5_orchidee_historical_histsoc_co2_year))
for (i in 1:length(lon_orchidee)) {print(i)
for (j in 1:length(lat_orchidee)) {
bob <- rbind(soilmoist_gfdl_esm2m_orchidee_historical_histsoc_co2_year[i,j,],soilmoist_ipsl_cm5a_lr_orchidee_historical_histsoc_co2_year[i,j,],
soilmoist_hadgem2_es_orchidee_historical_histsoc_co2_year[i,j,],soilmoist_miroc5_orchidee_historical_histsoc_co2_year[i,j,])
soilmoist_MMM_orchidee_historical_histsoc_co2_year[i,j,] <- colMeans(bob, na.rm=T) ; rm(bob) }}

soilmoist_MMM_orchidee_rcp60_2005soc_co2_year <- array(NA, dim=dim(soilmoist_miroc5_orchidee_rcp60_2005soc_co2_year))
for (i in 1:length(lon_orchidee)) {print(i)
for (j in 1:length(lat_orchidee)) {
bob <- rbind(soilmoist_gfdl_esm2m_orchidee_rcp60_2005soc_co2_year[i,j,],soilmoist_ipsl_cm5a_lr_orchidee_rcp60_2005soc_co2_year[i,j,],
soilmoist_hadgem2_es_orchidee_rcp60_2005soc_co2_year[i,j,],soilmoist_miroc5_orchidee_rcp60_2005soc_co2_year[i,j,])
soilmoist_MMM_orchidee_rcp60_2005soc_co2_year[i,j,] <- colMeans(bob, na.rm=T)  ; rm(bob)}}

soilmoist_MMM_orchidee_rcp60_2005soc_2005co2_year <- array(NA, dim=dim(soilmoist_miroc5_orchidee_rcp60_2005soc_2005co2_year))
for (i in 1:length(lon_orchidee)) {print(i)
for (j in 1:length(lat_orchidee)) {
bob <- rbind(soilmoist_gfdl_esm2m_orchidee_rcp60_2005soc_2005co2_year[i,j,],soilmoist_ipsl_cm5a_lr_orchidee_rcp60_2005soc_2005co2_year[i,j,],
soilmoist_hadgem2_es_orchidee_rcp60_2005soc_2005co2_year[i,j,],soilmoist_miroc5_orchidee_rcp60_2005soc_2005co2_year[i,j,])
soilmoist_MMM_orchidee_rcp60_2005soc_2005co2_year[i,j,] <- colMeans(bob, na.rm=T) ; rm(bob) }}

## CARAIB
soilmoist_MMM_caraib_historical_histsoc_co2_year <- array(NA, dim=dim(soilmoist_miroc5_caraib_historical_histsoc_co2_year))
for (i in 1:length(lon_caraib)) {print(i)
for (j in 1:length(lat_caraib)) {
bob <- rbind(soilmoist_gfdl_esm2m_caraib_historical_histsoc_co2_year[i,j,],soilmoist_ipsl_cm5a_lr_caraib_historical_histsoc_co2_year[i,j,],
soilmoist_hadgem2_es_caraib_historical_histsoc_co2_year[i,j,],soilmoist_miroc5_caraib_historical_histsoc_co2_year[i,j,])
soilmoist_MMM_caraib_historical_histsoc_co2_year[i,j,] <- colMeans(bob, na.rm=T) ; rm(bob) }}
  
soilmoist_MMM_caraib_rcp60_2005soc_co2_year <- array(NA, dim=dim(soilmoist_miroc5_caraib_rcp60_2005soc_co2_year))
for (i in 1:length(lon_caraib)) {print(i)
for (j in 1:length(lat_caraib)) {
bob <- rbind(soilmoist_gfdl_esm2m_caraib_rcp60_2005soc_co2_year[i,j,],soilmoist_ipsl_cm5a_lr_caraib_rcp60_2005soc_co2_year[i,j,],
soilmoist_hadgem2_es_caraib_rcp60_2005soc_co2_year[i,j,],soilmoist_miroc5_caraib_rcp60_2005soc_co2_year[i,j,])
soilmoist_MMM_caraib_rcp60_2005soc_co2_year[i,j,] <- colMeans(bob, na.rm=T)  ; rm(bob)}}

soilmoist_MMM_caraib_rcp60_2005soc_2005co2_year <- array(NA, dim=dim(soilmoist_miroc5_caraib_rcp60_2005soc_2005co2_year))
for (i in 1:length(lon_caraib)) {print(i)
for (j in 1:length(lat_caraib)) {
bob <- rbind(soilmoist_gfdl_esm2m_caraib_rcp60_2005soc_2005co2_year[i,j,],soilmoist_ipsl_cm5a_lr_caraib_rcp60_2005soc_2005co2_year[i,j,],
soilmoist_hadgem2_es_caraib_rcp60_2005soc_2005co2_year[i,j,],soilmoist_miroc5_caraib_rcp60_2005soc_2005co2_year[i,j,])
soilmoist_MMM_caraib_rcp60_2005soc_2005co2_year[i,j,] <- colMeans(bob, na.rm=T) ; rm(bob) }}




### Save data:
save(soilmoist_gfdl_esm2m_lpj_guess_rcp60_2005soc_2005co2_year, file="save_RData/soilmoist_gfdl_esm2m_lpj_guess_rcp60_2005soc_2005co2_year.RData")
save(soilmoist_ipsl_cm5a_lr_lpj_guess_rcp60_2005soc_2005co2_year, file="save_RData/soilmoist_ipsl_cm5a_lr_lpj_guess_rcp60_2005soc_2005co2_year.RData")
save(soilmoist_hadgem2_es_lpj_guess_rcp60_2005soc_2005co2_year, file="save_RData/soilmoist_hadgem2_es_lpj_guess_rcp60_2005soc_2005co2_year.RData")
save(soilmoist_miroc5_lpj_guess_rcp60_2005soc_2005co2_year, file="save_RData/soilmoist_miroc5_lpj_guess_rcp60_2005soc_2005co2_year.RData")

save(soilmoist_gfdl_esm2m_orchidee_rcp60_2005soc_2005co2_year, file="save_RData/soilmoist_gfdl_esm2m_orchidee_rcp60_2005soc_2005co2_year.RData")
save(soilmoist_ipsl_cm5a_lr_orchidee_rcp60_2005soc_2005co2_year, file="save_RData/soilmoist_ipsl_cm5a_lr_orchidee_rcp60_2005soc_2005co2_year.RData")
save(soilmoist_hadgem2_es_orchidee_rcp60_2005soc_2005co2_year, file="save_RData/soilmoist_hadgem2_es_orchidee_rcp60_2005soc_2005co2_year.RData")
save(soilmoist_miroc5_orchidee_rcp60_2005soc_2005co2_year, file="save_RData/soilmoist_miroc5_orchidee_rcp60_2005soc_2005co2_year.RData")

save(soilmoist_gfdl_esm2m_caraib_rcp60_2005soc_2005co2_year, file="save_RData/soilmoist_gfdl_esm2m_caraib_rcp60_2005soc_2005co2_year.RData")
save(soilmoist_ipsl_cm5a_lr_caraib_rcp60_2005soc_2005co2_year, file="save_RData/soilmoist_ipsl_cm5a_lr_caraib_rcp60_2005soc_2005co2_year.RData")
save(soilmoist_hadgem2_es_caraib_rcp60_2005soc_2005co2_year, file="save_RData/soilmoist_hadgem2_es_caraib_rcp60_2005soc_2005co2_year.RData")
save(soilmoist_miroc5_caraib_rcp60_2005soc_2005co2_year, file="save_RData/soilmoist_miroc5_caraib_rcp60_2005soc_2005co2_year.RData")

#save(soilmoist_gfdl_esm2m_visit_rcp60_2005soc_2005co2_year, file="save_RData/soilmoist_gfdl_esm2m_visit_rcp60_2005soc_2005co2_year.RData")
#save(soilmoist_ipsl_cm5a_lr_visit_rcp60_2005soc_2005co2_year, file="save_RData/soilmoist_ipsl_cm5a_lr_visit_rcp60_2005soc_2005co2_year.RData")
#save(soilmoist_hadgem2_es_visit_rcp60_2005soc_2005co2_year, file="save_RData/soilmoist_hadgem2_es_visit_rcp60_2005soc_2005co2_year.RData")
#save(soilmoist_miroc5_visit_rcp60_2005soc_2005co2_year, file="save_RData/soilmoist_miroc5_visit_rcp60_2005soc_2005co2_year.RData")



save(soilmoist_gfdl_esm2m_lpj_guess_rcp60_2005soc_co2_year, file="save_RData/soilmoist_gfdl_esm2m_lpj_guess_rcp60_2005soc_co2_year.RData")
save(soilmoist_ipsl_cm5a_lr_lpj_guess_rcp60_2005soc_co2_year, file="save_RData/soilmoist_ipsl_cm5a_lr_lpj_guess_rcp60_2005soc_co2_year.RData")
save(soilmoist_hadgem2_es_lpj_guess_rcp60_2005soc_co2_year, file="save_RData/soilmoist_hadgem2_es_lpj_guess_rcp60_2005soc_co2_year.RData")
save(soilmoist_miroc5_lpj_guess_rcp60_2005soc_co2_year, file="save_RData/soilmoist_miroc5_lpj_guess_rcp60_2005soc_co2_year.RData")

save(soilmoist_gfdl_esm2m_orchidee_rcp60_2005soc_co2_year, file="save_RData/soilmoist_gfdl_esm2m_orchidee_rcp60_2005soc_co2_year.RData")
save(soilmoist_ipsl_cm5a_lr_orchidee_rcp60_2005soc_co2_year, file="save_RData/soilmoist_ipsl_cm5a_lr_orchidee_rcp60_2005soc_co2_year.RData")
save(soilmoist_hadgem2_es_orchidee_rcp60_2005soc_co2_year, file="save_RData/soilmoist_hadgem2_es_orchidee_rcp60_2005soc_co2_year.RData")
save(soilmoist_miroc5_orchidee_rcp60_2005soc_co2_year, file="save_RData/soilmoist_miroc5_orchidee_rcp60_2005soc_co2_year.RData")

save(soilmoist_gfdl_esm2m_caraib_rcp60_2005soc_co2_year, file="save_RData/soilmoist_gfdl_esm2m_caraib_rcp60_2005soc_co2_year.RData")
save(soilmoist_ipsl_cm5a_lr_caraib_rcp60_2005soc_co2_year, file="save_RData/soilmoist_ipsl_cm5a_lr_caraib_rcp60_2005soc_co2_year.RData")
save(soilmoist_hadgem2_es_caraib_rcp60_2005soc_co2_year, file="save_RData/soilmoist_hadgem2_es_caraib_rcp60_2005soc_co2_year.RData")
save(soilmoist_miroc5_caraib_rcp60_2005soc_co2_year, file="save_RData/soilmoist_miroc5_caraib_rcp60_2005soc_co2_year.RData")

#save(soilmoist_gfdl_esm2m_visit_rcp60_2005soc_co2_year, file="save_RData/soilmoist_gfdl_esm2m_visit_rcp60_2005soc_co2_year.RData")
#save(soilmoist_ipsl_cm5a_lr_visit_rcp60_2005soc_co2_year, file="save_RData/soilmoist_ipsl_cm5a_lr_visit_rcp60_2005soc_co2_year.RData")
#save(soilmoist_hadgem2_es_visit_rcp60_2005soc_co2_year, file="save_RData/soilmoist_hadgem2_es_visit_rcp60_2005soc_co2_year.RData")
#save(soilmoist_miroc5_visit_rcp60_2005soc_co2_year, file="save_RData/soilmoist_miroc5_visit_rcp60_2005soc_co2_year.RData")


save(soilmoist_gfdl_esm2m_lpj_guess_historical_histsoc_co2_year, file="save_RData/soilmoist_gfdl_esm2m_lpj_guess_historical_histsoc_co2_year.RData")
save(soilmoist_ipsl_cm5a_lr_lpj_guess_historical_histsoc_co2_year, file="save_RData/soilmoist_ipsl_cm5a_lr_lpj_guess_historical_histsoc_co2_year.RData")
save(soilmoist_hadgem2_es_lpj_guess_historical_histsoc_co2_year, file="save_RData/soilmoist_hadgem2_es_lpj_guess_historical_histsoc_co2_year.RData")
save(soilmoist_miroc5_lpj_guess_historical_histsoc_co2_year, file="save_RData/soilmoist_miroc5_lpj_guess_historical_histsoc_co2_year.RData")

save(soilmoist_gfdl_esm2m_orchidee_historical_histsoc_co2_year, file="save_RData/soilmoist_gfdl_esm2m_orchidee_historical_histsoc_co2_year.RData")
save(soilmoist_ipsl_cm5a_lr_orchidee_historical_histsoc_co2_year, file="save_RData/soilmoist_ipsl_cm5a_lr_orchidee_historical_histsoc_co2_year.RData")
save(soilmoist_hadgem2_es_orchidee_historical_histsoc_co2_year, file="save_RData/soilmoist_hadgem2_es_orchidee_historical_histsoc_co2_year.RData")
save(soilmoist_miroc5_orchidee_historical_histsoc_co2_year, file="save_RData/soilmoist_miroc5_orchidee_historical_histsoc_co2_year.RData")

save(soilmoist_gfdl_esm2m_caraib_historical_histsoc_co2_year, file="save_RData/soilmoist_gfdl_esm2m_caraib_historical_histsoc_co2_year.RData")
save(soilmoist_ipsl_cm5a_lr_caraib_historical_histsoc_co2_year, file="save_RData/soilmoist_ipsl_cm5a_lr_caraib_historical_histsoc_co2_year.RData")
save(soilmoist_hadgem2_es_caraib_historical_histsoc_co2_year, file="save_RData/soilmoist_hadgem2_es_caraib_historical_histsoc_co2_year.RData")
save(soilmoist_miroc5_caraib_historical_histsoc_co2_year, file="save_RData/soilmoist_miroc5_caraib_historical_histsoc_co2_year.RData")

#save(soilmoist_gfdl_esm2m_visit_historical_histsoc_co2_year, file="save_RData/soilmoist_gfdl_esm2m_visit_historical_histsoc_co2_year.RData")
#save(soilmoist_ipsl_cm5a_lr_visit_historical_histsoc_co2_year, file="save_RData/soilmoist_ipsl_cm5a_lr_visit_historical_histsoc_co2_year.RData")
#save(soilmoist_hadgem2_es_visit_historical_histsoc_co2_year, file="save_RData/soilmoist_hadgem2_es_visit_historical_histsoc_co2_year.RData")
#save(soilmoist_miroc5_visit_historical_histsoc_co2_year, file="save_RData/soilmoist_miroc5_visit_historical_histsoc_co2_year.RData")




