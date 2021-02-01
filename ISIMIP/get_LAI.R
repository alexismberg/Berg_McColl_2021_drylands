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
#### LAI from lpj_guess:
forcing <- c("gfdl-esm2m", "ipsl-cm5a-lr", "hadgem2-es", "miroc5"); forcing2 <- c("gfdl_esm2m", "ipsl_cm5a_lr", "hadgem2_es", "miroc5")
########### LPJ_GUESS
###  HIST
for (m in 1:4){ print(forcing[m])
data <- nc_open(paste("/n/mccoll_lab/aberg/DATA/ISIMIP/HIST/lpj-guess_",forcing[m],"_ewembi_historical_histsoc_co2_lai_global_monthly_1861_2005.nc", sep=""))
lat_lpj_guess <- ncvar_get(data, "lat")[360:1]
lon_lpj_guess <- ncvar_get(data, "lon")
lai_lpj_guess_historical_histsoc_co2 <- ncvar_get(data, "lai")[,360:1,]
nc_close(data)
number_years <- dim(lai_lpj_guess_historical_histsoc_co2)[3]/12
bob <- array(NA, dim=c(dim(lai_lpj_guess_historical_histsoc_co2)[1:2], number_years))
for (a in 1:number_years){print(a)
bob[,,a] <- apply(lai_lpj_guess_historical_histsoc_co2[,,(12*(a-1)+1):(12*a)], c(1,2), mean)}
assign(paste("lai_",forcing2[m],"_lpj_guess_historical_histsoc_co2_year",sep=""),bob)
rm(bob); rm(lai_lpj_guess_historical_histsoc_co2) }
### RCP60, C02=2005
for (m in 1:4){ print(forcing[m])
data <- nc_open(paste("/n/mccoll_lab/aberg/DATA/ISIMIP/RCP60/lpj-guess_",forcing[m],"_ewembi_rcp60_2005soc_2005co2_lai_global_monthly_2006_2099.nc", sep=""))
lai_lpj_guess_rcp60_2005soc_2005co2 <- ncvar_get(data, "lai")[,360:1,]
nc_close(data); 
number_years <- dim(lai_lpj_guess_rcp60_2005soc_2005co2 )[3]/12
bob <- array(NA, dim=c(dim(lai_lpj_guess_rcp60_2005soc_2005co2 )[1:2], number_years))
for (a in 1:number_years){print(a)
bob[,,a] <- apply(lai_lpj_guess_rcp60_2005soc_2005co2 [,,(12*(a-1)+1):(12*a)], c(1,2), mean)}
assign(paste("lai_",forcing2[m],"_lpj_guess_rcp60_2005soc_2005co2_year",sep=""),bob)
rm(bob); rm(lai_lpj_guess_rcp60_2005soc_2005co2 )}
### RCP60, CO2=RCP60
for (m in 1:4){ print(forcing[m])
data <- nc_open(paste("/n/mccoll_lab/aberg/DATA/ISIMIP/RCP60/lpj-guess_",forcing[m],"_ewembi_rcp60_2005soc_co2_lai_global_monthly_2006_2099.nc", sep=""))
lai_lpj_guess_rcp60_2005soc_co2 <- ncvar_get(data, "lai")[,360:1,]
nc_close(data)
number_years <- dim(lai_lpj_guess_rcp60_2005soc_co2 )[3]/12
bob <- array(NA, dim=c(dim(lai_lpj_guess_rcp60_2005soc_co2 )[1:2], number_years))
for (a in 1:number_years){print(a)
bob[,,a] <- apply(lai_lpj_guess_rcp60_2005soc_co2 [,,(12*(a-1)+1):(12*a)], c(1,2), mean)}
assign(paste("lai_",forcing2[m],"_lpj_guess_rcp60_2005soc_co2_year",sep=""),bob)
rm(bob); rm(lai_lpj_guess_rcp60_2005soc_co2 ) }

########### ORCHIDEE
###  HIST
for (m in 1:4){ print(forcing[m])
data <- nc_open(paste("/n/mccoll_lab/aberg/DATA/ISIMIP/HIST/orchidee_",forcing[m],"_ewembi_historical_histsoc_co2_lai_global_monthly_1861_2005.nc", 
sep=""))
lat_orchidee <- ncvar_get(data, "lat")[360:1]
lon_orchidee <- ncvar_get(data, "lon")
lai_orchidee_historical_histsoc_co2 <- ncvar_get(data, "lai")[,360:1,]
nc_close(data)
number_years <- dim(lai_orchidee_historical_histsoc_co2)[3]/12
bob <- array(NA, dim=c(dim(lai_orchidee_historical_histsoc_co2)[1:2], number_years))
for (a in 1:number_years){print(a)
bob[,,a] <- apply(lai_orchidee_historical_histsoc_co2[,,(12*(a-1)+1):(12*a)], c(1,2), mean)}
assign(paste("lai_",forcing2[m],"_orchidee_historical_histsoc_co2_year",sep=""),bob)
rm(bob); rm(lai_orchidee_historical_histsoc_co2) }
### RCP60, C02=2005
for (m in 1:4){ print(forcing[m])
data <- nc_open(paste("/n/mccoll_lab/aberg/DATA/ISIMIP/RCP60/orchidee_",forcing[m],"_ewembi_rcp60_2005soc_2005co2_lai_global_monthly_2006_2099.nc", 
sep=""))
lai_orchidee_rcp60_2005soc_2005co2 <- ncvar_get(data, "lai")[,360:1,]
nc_close(data);
number_years <- dim(lai_orchidee_rcp60_2005soc_2005co2 )[3]/12
bob <- array(NA, dim=c(dim(lai_orchidee_rcp60_2005soc_2005co2 )[1:2], number_years))
for (a in 1:number_years){print(a)
bob[,,a] <- apply(lai_orchidee_rcp60_2005soc_2005co2 [,,(12*(a-1)+1):(12*a)], c(1,2), mean)}
assign(paste("lai_",forcing2[m],"_orchidee_rcp60_2005soc_2005co2_year",sep=""),bob)
rm(bob); rm(lai_orchidee_rcp60_2005soc_2005co2 )}
### RCP60, CO2=RCP60
for (m in 1:4){ print(forcing[m])
data <- nc_open(paste("/n/mccoll_lab/aberg/DATA/ISIMIP/RCP60/orchidee_",forcing[m],"_ewembi_rcp60_2005soc_co2_lai_global_monthly_2006_2099.nc", sep=""))
lai_orchidee_rcp60_2005soc_co2 <- ncvar_get(data, "lai")[,360:1,]
nc_close(data)
number_years <- dim(lai_orchidee_rcp60_2005soc_co2 )[3]/12
bob <- array(NA, dim=c(dim(lai_orchidee_rcp60_2005soc_co2 )[1:2], number_years))
for (a in 1:number_years){print(a)
bob[,,a] <- apply(lai_orchidee_rcp60_2005soc_co2 [,,(12*(a-1)+1):(12*a)], c(1,2), mean)}
assign(paste("lai_",forcing2[m],"_orchidee_rcp60_2005soc_co2_year",sep=""),bob)
rm(bob); rm(lai_orchidee_rcp60_2005soc_co2 ) }

########### CARAIB
###  HIST
for (m in 1:4){ print(forcing[m])
data <- nc_open(paste("/n/mccoll_lab/aberg/DATA/ISIMIP/HIST/caraib_",forcing[m],"_ewembi_historical_histsoc_co2_lai_global_monthly_1861_2005.nc", sep=""))
lat_caraib <- ncvar_get(data, "lat")[360:1]
lon_caraib <- ncvar_get(data, "lon")
lai_caraib_historical_histsoc_co2 <- ncvar_get(data, "lai")[,360:1,]
nc_close(data)
number_years <- dim(lai_caraib_historical_histsoc_co2)[3]/12
bob <- array(NA, dim=c(dim(lai_caraib_historical_histsoc_co2)[1:2], number_years))
for (a in 1:number_years){print(a)
bob[,,a] <- apply(lai_caraib_historical_histsoc_co2[,,(12*(a-1)+1):(12*a)], c(1,2), mean)}
assign(paste("lai_",forcing2[m],"_caraib_historical_histsoc_co2_year",sep=""),bob)
rm(bob); rm(lai_caraib_historical_histsoc_co2) }
### RCP60, C02=2005
for (m in 1:4){ print(forcing[m])
data <- nc_open(paste("/n/mccoll_lab/aberg/DATA/ISIMIP/RCP60/caraib_",forcing[m],"_ewembi_rcp60_2005soc_2005co2_lai_global_monthly_2006_2099.nc", sep=""))
lai_caraib_rcp60_2005soc_2005co2 <- ncvar_get(data, "lai")[,360:1,]
nc_close(data);
number_years <- dim(lai_caraib_rcp60_2005soc_2005co2 )[3]/12
bob <- array(NA, dim=c(dim(lai_caraib_rcp60_2005soc_2005co2 )[1:2], number_years))
for (a in 1:number_years){print(a)
bob[,,a] <- apply(lai_caraib_rcp60_2005soc_2005co2 [,,(12*(a-1)+1):(12*a)], c(1,2), mean)}
assign(paste("lai_",forcing2[m],"_caraib_rcp60_2005soc_2005co2_year",sep=""),bob)
rm(bob); rm(lai_caraib_rcp60_2005soc_2005co2 )}
### RCP60, CO2=RCP60
for (m in 1:4){ print(forcing[m])
data <- nc_open(paste("/n/mccoll_lab/aberg/DATA/ISIMIP/RCP60/caraib_",forcing[m],"_ewembi_rcp60_2005soc_co2_lai_global_monthly_2006_2099.nc", sep=""))
lai_caraib_rcp60_2005soc_co2 <- ncvar_get(data, "lai")[,360:1,]
nc_close(data)
number_years <- dim(lai_caraib_rcp60_2005soc_co2 )[3]/12
bob <- array(NA, dim=c(dim(lai_caraib_rcp60_2005soc_co2 )[1:2], number_years))
for (a in 1:number_years){print(a)
bob[,,a] <- apply(lai_caraib_rcp60_2005soc_co2 [,,(12*(a-1)+1):(12*a)], c(1,2), mean)}
assign(paste("lai_",forcing2[m],"_caraib_rcp60_2005soc_co2_year",sep=""),bob)
rm(bob); rm(lai_caraib_rcp60_2005soc_co2 ) }


########### VISIT
###  HIST
for (m in 1:4){ print(forcing[m])
data <- nc_open(paste("/n/mccoll_lab/aberg/DATA/ISIMIP/HIST/visit_",forcing[m],"_ewembi_historical_histsoc_co2_lai_global_monthly_1861_2005.nc", sep=""))
lat_visit <- ncvar_get(data, "lat")[360:1]
lon_visit <- ncvar_get(data, "lon")
lai_visit_historical_histsoc_co2 <- ncvar_get(data, "lai")[,360:1,]
nc_close(data)
number_years <- dim(lai_visit_historical_histsoc_co2)[3]/12
bob <- array(NA, dim=c(dim(lai_visit_historical_histsoc_co2)[1:2], number_years))
for (a in 1:number_years){print(a)
bob[,,a] <- apply(lai_visit_historical_histsoc_co2[,,(12*(a-1)+1):(12*a)], c(1,2), mean)}
assign(paste("lai_",forcing2[m],"_visit_historical_histsoc_co2_year",sep=""),bob)
rm(bob); rm(lai_visit_historical_histsoc_co2) }
### RCP60, C02=2005
for (m in 1:4){ print(forcing[m])
data <- nc_open(paste("/n/mccoll_lab/aberg/DATA/ISIMIP/RCP60/visit_",forcing[m],"_ewembi_rcp60_2005soc_2005co2_lai_global_monthly_2006_2099.nc", sep=""))
lai_visit_rcp60_2005soc_2005co2 <- ncvar_get(data, "lai")[,360:1,]
nc_close(data);
number_years <- dim(lai_visit_rcp60_2005soc_2005co2 )[3]/12
bob <- array(NA, dim=c(dim(lai_visit_rcp60_2005soc_2005co2 )[1:2], number_years))
for (a in 1:number_years){print(a)
bob[,,a] <- apply(lai_visit_rcp60_2005soc_2005co2 [,,(12*(a-1)+1):(12*a)], c(1,2), mean)}
assign(paste("lai_",forcing2[m],"_visit_rcp60_2005soc_2005co2_year",sep=""),bob)
rm(bob); rm(lai_visit_rcp60_2005soc_2005co2 )}
### RCP60, CO2=RCP60
for (m in 1:4){ print(forcing[m])
data <- nc_open(paste("/n/mccoll_lab/aberg/DATA/ISIMIP/RCP60/visit_",forcing[m],"_ewembi_rcp60_2005soc_co2_lai_global_monthly_2006_2099.nc", sep=""))
lai_visit_rcp60_2005soc_co2 <- ncvar_get(data, "lai")[,360:1,]
nc_close(data)
number_years <- dim(lai_visit_rcp60_2005soc_co2 )[3]/12
bob <- array(NA, dim=c(dim(lai_visit_rcp60_2005soc_co2 )[1:2], number_years))
for (a in 1:number_years){print(a)
bob[,,a] <- apply(lai_visit_rcp60_2005soc_co2 [,,(12*(a-1)+1):(12*a)], c(1,2), mean)}
assign(paste("lai_",forcing2[m],"_visit_rcp60_2005soc_co2_year",sep=""),bob)
rm(bob); rm(lai_visit_rcp60_2005soc_co2 ) }



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
lai_MMM_lpj_guess_historical_histsoc_co2_year <- array(NA, dim=dim(lai_miroc5_lpj_guess_historical_histsoc_co2_year))
for (i in 1:length(lon_lpj_guess)) {print(i)
for (j in 1:length(lat_lpj_guess)) {
bob <- rbind(lai_gfdl_esm2m_lpj_guess_historical_histsoc_co2_year[i,j,],lai_ipsl_cm5a_lr_lpj_guess_historical_histsoc_co2_year[i,j,],
lai_hadgem2_es_lpj_guess_historical_histsoc_co2_year[i,j,],lai_miroc5_lpj_guess_historical_histsoc_co2_year[i,j,])
lai_MMM_lpj_guess_historical_histsoc_co2_year[i,j,] <- colMeans(bob, na.rm=T) ; rm(bob) }}

lai_MMM_lpj_guess_rcp60_2005soc_co2_year <- array(NA, dim=dim(lai_miroc5_lpj_guess_rcp60_2005soc_co2_year))
for (i in 1:length(lon_lpj_guess)) {print(i)
for (j in 1:length(lat_lpj_guess)) {
bob <- rbind(lai_gfdl_esm2m_lpj_guess_rcp60_2005soc_co2_year[i,j,],lai_ipsl_cm5a_lr_lpj_guess_rcp60_2005soc_co2_year[i,j,],
lai_hadgem2_es_lpj_guess_rcp60_2005soc_co2_year[i,j,],lai_miroc5_lpj_guess_rcp60_2005soc_co2_year[i,j,])
lai_MMM_lpj_guess_rcp60_2005soc_co2_year[i,j,] <- colMeans(bob, na.rm=T)  ; rm(bob)}}

lai_MMM_lpj_guess_rcp60_2005soc_2005co2_year <- array(NA, dim=dim(lai_miroc5_lpj_guess_rcp60_2005soc_2005co2_year))
for (i in 1:length(lon_lpj_guess)) {print(i)
for (j in 1:length(lat_lpj_guess)) {
bob <- rbind(lai_gfdl_esm2m_lpj_guess_rcp60_2005soc_2005co2_year[i,j,],lai_ipsl_cm5a_lr_lpj_guess_rcp60_2005soc_2005co2_year[i,j,],
lai_hadgem2_es_lpj_guess_rcp60_2005soc_2005co2_year[i,j,],lai_miroc5_lpj_guess_rcp60_2005soc_2005co2_year[i,j,])
lai_MMM_lpj_guess_rcp60_2005soc_2005co2_year[i,j,] <- colMeans(bob, na.rm=T) ; rm(bob) }}

## ORCHIDEE
lai_MMM_orchidee_historical_histsoc_co2_year <- array(NA, dim=dim(lai_miroc5_orchidee_historical_histsoc_co2_year))
for (i in 1:length(lon_orchidee)) {print(i)
for (j in 1:length(lat_orchidee)) {
bob <- rbind(lai_gfdl_esm2m_orchidee_historical_histsoc_co2_year[i,j,],lai_ipsl_cm5a_lr_orchidee_historical_histsoc_co2_year[i,j,],
lai_hadgem2_es_orchidee_historical_histsoc_co2_year[i,j,],lai_miroc5_orchidee_historical_histsoc_co2_year[i,j,])
lai_MMM_orchidee_historical_histsoc_co2_year[i,j,] <- colMeans(bob, na.rm=T) ; rm(bob) }}

lai_MMM_orchidee_rcp60_2005soc_co2_year <- array(NA, dim=dim(lai_miroc5_orchidee_rcp60_2005soc_co2_year))
for (i in 1:length(lon_orchidee)) {print(i)
for (j in 1:length(lat_orchidee)) {
bob <- rbind(lai_gfdl_esm2m_orchidee_rcp60_2005soc_co2_year[i,j,],lai_ipsl_cm5a_lr_orchidee_rcp60_2005soc_co2_year[i,j,],
lai_hadgem2_es_orchidee_rcp60_2005soc_co2_year[i,j,],lai_miroc5_orchidee_rcp60_2005soc_co2_year[i,j,])
lai_MMM_orchidee_rcp60_2005soc_co2_year[i,j,] <- colMeans(bob, na.rm=T)  ; rm(bob)}}

lai_MMM_orchidee_rcp60_2005soc_2005co2_year <- array(NA, dim=dim(lai_miroc5_orchidee_rcp60_2005soc_2005co2_year))
for (i in 1:length(lon_orchidee)) {print(i)
for (j in 1:length(lat_orchidee)) {
bob <- rbind(lai_gfdl_esm2m_orchidee_rcp60_2005soc_2005co2_year[i,j,],lai_ipsl_cm5a_lr_orchidee_rcp60_2005soc_2005co2_year[i,j,],
lai_hadgem2_es_orchidee_rcp60_2005soc_2005co2_year[i,j,],lai_miroc5_orchidee_rcp60_2005soc_2005co2_year[i,j,])
lai_MMM_orchidee_rcp60_2005soc_2005co2_year[i,j,] <- colMeans(bob, na.rm=T) ; rm(bob) }}

## VISIT
lai_MMM_visit_historical_histsoc_co2_year <- array(NA, dim=dim(lai_miroc5_visit_historical_histsoc_co2_year))
for (i in 1:length(lon_visit)) {print(i)
for (j in 1:length(lat_visit)) {
bob <- rbind(lai_gfdl_esm2m_visit_historical_histsoc_co2_year[i,j,],lai_ipsl_cm5a_lr_visit_historical_histsoc_co2_year[i,j,],
lai_hadgem2_es_visit_historical_histsoc_co2_year[i,j,],lai_miroc5_visit_historical_histsoc_co2_year[i,j,])
lai_MMM_visit_historical_histsoc_co2_year[i,j,] <- colMeans(bob, na.rm=T) ; rm(bob) }}
  
lai_MMM_visit_rcp60_2005soc_co2_year <- array(NA, dim=dim(lai_miroc5_visit_rcp60_2005soc_co2_year))
for (i in 1:length(lon_visit)) {print(i)
for (j in 1:length(lat_visit)) {
bob <- rbind(lai_gfdl_esm2m_visit_rcp60_2005soc_co2_year[i,j,],lai_ipsl_cm5a_lr_visit_rcp60_2005soc_co2_year[i,j,],
lai_hadgem2_es_visit_rcp60_2005soc_co2_year[i,j,],lai_miroc5_visit_rcp60_2005soc_co2_year[i,j,])
lai_MMM_visit_rcp60_2005soc_co2_year[i,j,] <- colMeans(bob, na.rm=T)  ; rm(bob)}}

lai_MMM_visit_rcp60_2005soc_2005co2_year <- array(NA, dim=dim(lai_miroc5_visit_rcp60_2005soc_2005co2_year))
for (i in 1:length(lon_visit)) {print(i)
for (j in 1:length(lat_visit)) {
bob <- rbind(lai_gfdl_esm2m_visit_rcp60_2005soc_2005co2_year[i,j,],lai_ipsl_cm5a_lr_visit_rcp60_2005soc_2005co2_year[i,j,],
lai_hadgem2_es_visit_rcp60_2005soc_2005co2_year[i,j,],lai_miroc5_visit_rcp60_2005soc_2005co2_year[i,j,])
lai_MMM_visit_rcp60_2005soc_2005co2_year[i,j,] <- colMeans(bob, na.rm=T) ; rm(bob) }}

## CARAIB
lai_MMM_caraib_historical_histsoc_co2_year <- array(NA, dim=dim(lai_miroc5_caraib_historical_histsoc_co2_year))
for (i in 1:length(lon_caraib)) {print(i)
for (j in 1:length(lat_caraib)) {
bob <- rbind(lai_gfdl_esm2m_caraib_historical_histsoc_co2_year[i,j,],lai_ipsl_cm5a_lr_caraib_historical_histsoc_co2_year[i,j,],
lai_hadgem2_es_caraib_historical_histsoc_co2_year[i,j,],lai_miroc5_caraib_historical_histsoc_co2_year[i,j,])
lai_MMM_caraib_historical_histsoc_co2_year[i,j,] <- colMeans(bob, na.rm=T) ; rm(bob) }}
  
lai_MMM_caraib_rcp60_2005soc_co2_year <- array(NA, dim=dim(lai_miroc5_caraib_rcp60_2005soc_co2_year))
for (i in 1:length(lon_caraib)) {print(i)
for (j in 1:length(lat_caraib)) {
bob <- rbind(lai_gfdl_esm2m_caraib_rcp60_2005soc_co2_year[i,j,],lai_ipsl_cm5a_lr_caraib_rcp60_2005soc_co2_year[i,j,],
lai_hadgem2_es_caraib_rcp60_2005soc_co2_year[i,j,],lai_miroc5_caraib_rcp60_2005soc_co2_year[i,j,])
lai_MMM_caraib_rcp60_2005soc_co2_year[i,j,] <- colMeans(bob, na.rm=T)  ; rm(bob)}}
lai_MMM_caraib_rcp60_2005soc_2005co2_year <- array(NA, dim=dim(lai_miroc5_caraib_rcp60_2005soc_2005co2_year))
for (i in 1:length(lon_caraib)) {print(i)
for (j in 1:length(lat_caraib)) {
bob <- rbind(lai_gfdl_esm2m_caraib_rcp60_2005soc_2005co2_year[i,j,],lai_ipsl_cm5a_lr_caraib_rcp60_2005soc_2005co2_year[i,j,],
lai_hadgem2_es_caraib_rcp60_2005soc_2005co2_year[i,j,],lai_miroc5_caraib_rcp60_2005soc_2005co2_year[i,j,])
lai_MMM_caraib_rcp60_2005soc_2005co2_year[i,j,] <- colMeans(bob, na.rm=T) ; rm(bob) }}


##############################################
### Save data:
save(lai_gfdl_esm2m_lpj_guess_rcp60_2005soc_2005co2_year, file="save_RData/lai_gfdl_esm2m_lpj_guess_rcp60_2005soc_2005co2_year.RData")
save(lai_ipsl_cm5a_lr_lpj_guess_rcp60_2005soc_2005co2_year, file="save_RData/lai_ipsl_cm5a_lr_lpj_guess_rcp60_2005soc_2005co2_year.RData")
save(lai_hadgem2_es_lpj_guess_rcp60_2005soc_2005co2_year, file="save_RData/lai_hadgem2_es_lpj_guess_rcp60_2005soc_2005co2_year.RData")
save(lai_miroc5_lpj_guess_rcp60_2005soc_2005co2_year, file="save_RData/lai_miroc5_lpj_guess_rcp60_2005soc_2005co2_year.RData")

save(lai_gfdl_esm2m_orchidee_rcp60_2005soc_2005co2_year, file="save_RData/lai_gfdl_esm2m_orchidee_rcp60_2005soc_2005co2_year.RData")
save(lai_ipsl_cm5a_lr_orchidee_rcp60_2005soc_2005co2_year, file="save_RData/lai_ipsl_cm5a_lr_orchidee_rcp60_2005soc_2005co2_year.RData")
save(lai_hadgem2_es_orchidee_rcp60_2005soc_2005co2_year, file="save_RData/lai_hadgem2_es_orchidee_rcp60_2005soc_2005co2_year.RData")
save(lai_miroc5_orchidee_rcp60_2005soc_2005co2_year, file="save_RData/lai_miroc5_orchidee_rcp60_2005soc_2005co2_year.RData")

save(lai_gfdl_esm2m_caraib_rcp60_2005soc_2005co2_year, file="save_RData/lai_gfdl_esm2m_caraib_rcp60_2005soc_2005co2_year.RData")
save(lai_ipsl_cm5a_lr_caraib_rcp60_2005soc_2005co2_year, file="save_RData/lai_ipsl_cm5a_lr_caraib_rcp60_2005soc_2005co2_year.RData")
save(lai_hadgem2_es_caraib_rcp60_2005soc_2005co2_year, file="save_RData/lai_hadgem2_es_caraib_rcp60_2005soc_2005co2_year.RData")
save(lai_miroc5_caraib_rcp60_2005soc_2005co2_year, file="save_RData/lai_miroc5_caraib_rcp60_2005soc_2005co2_year.RData")

save(lai_gfdl_esm2m_visit_rcp60_2005soc_2005co2_year, file="save_RData/lai_gfdl_esm2m_visit_rcp60_2005soc_2005co2_year.RData")
save(lai_ipsl_cm5a_lr_visit_rcp60_2005soc_2005co2_year, file="save_RData/lai_ipsl_cm5a_lr_visit_rcp60_2005soc_2005co2_year.RData")
save(lai_hadgem2_es_visit_rcp60_2005soc_2005co2_year, file="save_RData/lai_hadgem2_es_visit_rcp60_2005soc_2005co2_year.RData")
save(lai_miroc5_visit_rcp60_2005soc_2005co2_year, file="save_RData/lai_miroc5_visit_rcp60_2005soc_2005co2_year.RData")



save(lai_gfdl_esm2m_lpj_guess_rcp60_2005soc_co2_year, file="save_RData/lai_gfdl_esm2m_lpj_guess_rcp60_2005soc_co2_year.RData")
save(lai_ipsl_cm5a_lr_lpj_guess_rcp60_2005soc_co2_year, file="save_RData/lai_ipsl_cm5a_lr_lpj_guess_rcp60_2005soc_co2_year.RData")
save(lai_hadgem2_es_lpj_guess_rcp60_2005soc_co2_year, file="save_RData/lai_hadgem2_es_lpj_guess_rcp60_2005soc_co2_year.RData")
save(lai_miroc5_lpj_guess_rcp60_2005soc_co2_year, file="save_RData/lai_miroc5_lpj_guess_rcp60_2005soc_co2_year.RData")

save(lai_gfdl_esm2m_orchidee_rcp60_2005soc_co2_year, file="save_RData/lai_gfdl_esm2m_orchidee_rcp60_2005soc_co2_year.RData")
save(lai_ipsl_cm5a_lr_orchidee_rcp60_2005soc_co2_year, file="save_RData/lai_ipsl_cm5a_lr_orchidee_rcp60_2005soc_co2_year.RData")
save(lai_hadgem2_es_orchidee_rcp60_2005soc_co2_year, file="save_RData/lai_hadgem2_es_orchidee_rcp60_2005soc_co2_year.RData")
save(lai_miroc5_orchidee_rcp60_2005soc_co2_year, file="save_RData/lai_miroc5_orchidee_rcp60_2005soc_co2_year.RData")

save(lai_gfdl_esm2m_caraib_rcp60_2005soc_co2_year, file="save_RData/lai_gfdl_esm2m_caraib_rcp60_2005soc_co2_year.RData")
save(lai_ipsl_cm5a_lr_caraib_rcp60_2005soc_co2_year, file="save_RData/lai_ipsl_cm5a_lr_caraib_rcp60_2005soc_co2_year.RData")
save(lai_hadgem2_es_caraib_rcp60_2005soc_co2_year, file="save_RData/lai_hadgem2_es_caraib_rcp60_2005soc_co2_year.RData")
save(lai_miroc5_caraib_rcp60_2005soc_co2_year, file="save_RData/lai_miroc5_caraib_rcp60_2005soc_co2_year.RData")

save(lai_gfdl_esm2m_visit_rcp60_2005soc_co2_year, file="save_RData/lai_gfdl_esm2m_visit_rcp60_2005soc_co2_year.RData")
save(lai_ipsl_cm5a_lr_visit_rcp60_2005soc_co2_year, file="save_RData/lai_ipsl_cm5a_lr_visit_rcp60_2005soc_co2_year.RData")
save(lai_hadgem2_es_visit_rcp60_2005soc_co2_year, file="save_RData/lai_hadgem2_es_visit_rcp60_2005soc_co2_year.RData")
save(lai_miroc5_visit_rcp60_2005soc_co2_year, file="save_RData/lai_miroc5_visit_rcp60_2005soc_co2_year.RData")



save(lai_gfdl_esm2m_lpj_guess_historical_histsoc_co2_year, file="save_RData/lai_gfdl_esm2m_lpj_guess_historical_histsoc_co2_year.RData")
save(lai_ipsl_cm5a_lr_lpj_guess_historical_histsoc_co2_year, file="save_RData/lai_ipsl_cm5a_lr_lpj_guess_historical_histsoc_co2_year.RData")
save(lai_hadgem2_es_lpj_guess_historical_histsoc_co2_year, file="save_RData/lai_hadgem2_es_lpj_guess_historical_histsoc_co2_year.RData")
save(lai_miroc5_lpj_guess_historical_histsoc_co2_year, file="save_RData/lai_miroc5_lpj_guess_historical_histsoc_co2_year.RData")

save(lai_gfdl_esm2m_orchidee_historical_histsoc_co2_year, file="save_RData/lai_gfdl_esm2m_orchidee_historical_histsoc_co2_year.RData")
save(lai_ipsl_cm5a_lr_orchidee_historical_histsoc_co2_year, file="save_RData/lai_ipsl_cm5a_lr_orchidee_historical_histsoc_co2_year.RData")
save(lai_hadgem2_es_orchidee_historical_histsoc_co2_year, file="save_RData/lai_hadgem2_es_orchidee_historical_histsoc_co2_year.RData")
save(lai_miroc5_orchidee_historical_histsoc_co2_year, file="save_RData/lai_miroc5_orchidee_historical_histsoc_co2_year.RData")

save(lai_gfdl_esm2m_caraib_historical_histsoc_co2_year, file="save_RData/lai_gfdl_esm2m_caraib_historical_histsoc_co2_year.RData")
save(lai_ipsl_cm5a_lr_caraib_historical_histsoc_co2_year, file="save_RData/lai_ipsl_cm5a_lr_caraib_historical_histsoc_co2_year.RData")
save(lai_hadgem2_es_caraib_historical_histsoc_co2_year, file="save_RData/lai_hadgem2_es_caraib_historical_histsoc_co2_year.RData")
save(lai_miroc5_caraib_historical_histsoc_co2_year, file="save_RData/lai_miroc5_caraib_historical_histsoc_co2_year.RData")

save(lai_gfdl_esm2m_visit_historical_histsoc_co2_year, file="save_RData/lai_gfdl_esm2m_visit_historical_histsoc_co2_year.RData")
save(lai_ipsl_cm5a_lr_visit_historical_histsoc_co2_year, file="save_RData/lai_ipsl_cm5a_lr_visit_historical_histsoc_co2_year.RData")
save(lai_hadgem2_es_visit_historical_histsoc_co2_year, file="save_RData/lai_hadgem2_es_visit_historical_histsoc_co2_year.RData")
save(lai_miroc5_visit_historical_histsoc_co2_year, file="save_RData/lai_miroc5_visit_historical_histsoc_co2_year.RData")


