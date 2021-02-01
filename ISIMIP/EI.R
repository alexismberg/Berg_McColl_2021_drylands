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



## LPJ_GUESS
corr_sm_tran_MMM_lpj_guess_historical_histsoc_co2 <- array(NA, dim=dim(corr_sm_tran_miroc5_lpj_guess_historical_histsoc_co2))
for (i in 1:length(lon_lpj_guess)) {print(i)
for (j in 1:length(lat_lpj_guess)) {
bob <- c(corr_sm_tran_gfdl_esm2m_lpj_guess_historical_histsoc_co2[i,j],corr_sm_tran_ipsl_cm5a_lr_lpj_guess_historical_histsoc_co2[i,j],
corr_sm_tran_hadgem2_es_lpj_guess_historical_histsoc_co2[i,j],corr_sm_tran_miroc5_lpj_guess_historical_histsoc_co2[i,j])
corr_sm_tran_MMM_lpj_guess_historical_histsoc_co2[i,j] <- mean(bob, na.rm=T) ; rm(bob) }}

corr_sm_tran_MMM_lpj_guess_rcp60_2005soc_co2 <- array(NA, dim=dim(corr_sm_tran_miroc5_lpj_guess_rcp60_2005soc_co2))
for (i in 1:length(lon_lpj_guess)) {print(i)
for (j in 1:length(lat_lpj_guess)) {
bob <- c(corr_sm_tran_gfdl_esm2m_lpj_guess_rcp60_2005soc_co2[i,j],corr_sm_tran_ipsl_cm5a_lr_lpj_guess_rcp60_2005soc_co2[i,j],
corr_sm_tran_hadgem2_es_lpj_guess_rcp60_2005soc_co2[i,j],corr_sm_tran_miroc5_lpj_guess_rcp60_2005soc_co2[i,j])
corr_sm_tran_MMM_lpj_guess_rcp60_2005soc_co2[i,j] <- mean(bob, na.rm=T)  ; rm(bob)}}

corr_sm_tran_MMM_lpj_guess_rcp60_2005soc_2005co2 <- array(NA, dim=dim(corr_sm_tran_miroc5_lpj_guess_rcp60_2005soc_2005co2))
for (i in 1:length(lon_lpj_guess)) {print(i)
for (j in 1:length(lat_lpj_guess)) {
bob <- c(corr_sm_tran_gfdl_esm2m_lpj_guess_rcp60_2005soc_2005co2[i,j],corr_sm_tran_ipsl_cm5a_lr_lpj_guess_rcp60_2005soc_2005co2[i,j],
corr_sm_tran_hadgem2_es_lpj_guess_rcp60_2005soc_2005co2[i,j],corr_sm_tran_miroc5_lpj_guess_rcp60_2005soc_2005co2[i,j])
corr_sm_tran_MMM_lpj_guess_rcp60_2005soc_2005co2[i,j] <- mean(bob, na.rm=T) ; rm(bob) }}


## ORCHIDEE
corr_sm_tran_MMM_orchidee_historical_histsoc_co2 <- array(NA, dim=dim(corr_sm_tran_miroc5_orchidee_historical_histsoc_co2))
for (i in 1:length(lon_orchidee)) {print(i)
for (j in 1:length(lat_orchidee)) {
bob <- c(corr_sm_tran_gfdl_esm2m_orchidee_historical_histsoc_co2[i,j],corr_sm_tran_ipsl_cm5a_lr_orchidee_historical_histsoc_co2[i,j],
corr_sm_tran_hadgem2_es_orchidee_historical_histsoc_co2[i,j],corr_sm_tran_miroc5_orchidee_historical_histsoc_co2[i,j])
corr_sm_tran_MMM_orchidee_historical_histsoc_co2[i,j] <- mean(bob, na.rm=T) ; rm(bob) }}

corr_sm_tran_MMM_orchidee_rcp60_2005soc_co2 <- array(NA, dim=dim(corr_sm_tran_miroc5_orchidee_rcp60_2005soc_co2))
for (i in 1:length(lon_orchidee)) {print(i)
for (j in 1:length(lat_orchidee)) {
bob <- c(corr_sm_tran_gfdl_esm2m_orchidee_rcp60_2005soc_co2[i,j],corr_sm_tran_ipsl_cm5a_lr_orchidee_rcp60_2005soc_co2[i,j],
corr_sm_tran_hadgem2_es_orchidee_rcp60_2005soc_co2[i,j],corr_sm_tran_miroc5_orchidee_rcp60_2005soc_co2[i,j])
corr_sm_tran_MMM_orchidee_rcp60_2005soc_co2[i,j] <- mean(bob, na.rm=T)  ; rm(bob)}}

corr_sm_tran_MMM_orchidee_rcp60_2005soc_2005co2 <- array(NA, dim=dim(corr_sm_tran_miroc5_orchidee_rcp60_2005soc_2005co2))
for (i in 1:length(lon_orchidee)) {print(i)
for (j in 1:length(lat_orchidee)) {
bob <- c(corr_sm_tran_gfdl_esm2m_orchidee_rcp60_2005soc_2005co2[i,j],corr_sm_tran_ipsl_cm5a_lr_orchidee_rcp60_2005soc_2005co2[i,j],
corr_sm_tran_hadgem2_es_orchidee_rcp60_2005soc_2005co2[i,j],corr_sm_tran_miroc5_orchidee_rcp60_2005soc_2005co2[i,j])
corr_sm_tran_MMM_orchidee_rcp60_2005soc_2005co2[i,j] <- mean(bob, na.rm=T) ; rm(bob) }}


## CARAIB
corr_sm_tran_MMM_caraib_historical_histsoc_co2 <- array(NA, dim=dim(corr_sm_tran_miroc5_caraib_historical_histsoc_co2))
for (i in 1:length(lon_caraib)) {print(i)
for (j in 1:length(lat_caraib)) {
bob <- c(corr_sm_tran_gfdl_esm2m_caraib_historical_histsoc_co2[i,j],corr_sm_tran_ipsl_cm5a_lr_caraib_historical_histsoc_co2[i,j],
corr_sm_tran_hadgem2_es_caraib_historical_histsoc_co2[i,j],corr_sm_tran_miroc5_caraib_historical_histsoc_co2[i,j])
corr_sm_tran_MMM_caraib_historical_histsoc_co2[i,j] <- mean(bob, na.rm=T) ; rm(bob) }}

corr_sm_tran_MMM_caraib_rcp60_2005soc_co2 <- array(NA, dim=dim(corr_sm_tran_miroc5_caraib_rcp60_2005soc_co2))
for (i in 1:length(lon_caraib)) {print(i)
for (j in 1:length(lat_caraib)) {
bob <- c(corr_sm_tran_gfdl_esm2m_caraib_rcp60_2005soc_co2[i,j],corr_sm_tran_ipsl_cm5a_lr_caraib_rcp60_2005soc_co2[i,j],
corr_sm_tran_hadgem2_es_caraib_rcp60_2005soc_co2[i,j],corr_sm_tran_miroc5_caraib_rcp60_2005soc_co2[i,j])
corr_sm_tran_MMM_caraib_rcp60_2005soc_co2[i,j] <- mean(bob, na.rm=T)  ; rm(bob)}}

corr_sm_tran_MMM_caraib_rcp60_2005soc_2005co2 <- array(NA, dim=dim(corr_sm_tran_miroc5_caraib_rcp60_2005soc_2005co2))
for (i in 1:length(lon_caraib)) {print(i)
for (j in 1:length(lat_caraib)) {
bob <- c(corr_sm_tran_gfdl_esm2m_caraib_rcp60_2005soc_2005co2[i,j],corr_sm_tran_ipsl_cm5a_lr_caraib_rcp60_2005soc_2005co2[i,j],
corr_sm_tran_hadgem2_es_caraib_rcp60_2005soc_2005co2[i,j],corr_sm_tran_miroc5_caraib_rcp60_2005soc_2005co2[i,j])
corr_sm_tran_MMM_caraib_rcp60_2005soc_2005co2[i,j] <- mean(bob, na.rm=T) ; rm(bob) }}




#############################################################################
######### Calculating EI:
# We have to average LAI years, but the correlation is already a time-average.

a<- 4.72
b<- 0.15
EI_MMM_lpj_guess_historical_histsoc_co2 <- apply(lai_MMM_lpj_guess_historical_histsoc_co2_year[,,111:140],c(1,2),mean, na.rm=T) -
 (a*corr_sm_tran_MMM_lpj_guess_historical_histsoc_co2+b)
EI_MMM_lpj_guess_rcp60_2005soc_co2 <- apply(lai_MMM_lpj_guess_rcp60_2005soc_co2_year[,,65:94],c(1,2),mean, na.rm=T) - 
(a*corr_sm_tran_MMM_lpj_guess_rcp60_2005soc_co2+b)
EI_MMM_lpj_guess_rcp60_2005soc_2005co2 <- apply(lai_MMM_lpj_guess_rcp60_2005soc_2005co2_year[,,65:94],c(1,2),mean, na.rm=T) - 
(a*corr_sm_tran_MMM_lpj_guess_rcp60_2005soc_2005co2+b)

EI_MMM_orchidee_historical_histsoc_co2 <- apply(lai_MMM_orchidee_historical_histsoc_co2_year[,,111:140],c(1,2),mean, na.rm=T) -
 (a*corr_sm_tran_MMM_orchidee_historical_histsoc_co2+b)
EI_MMM_orchidee_rcp60_2005soc_co2 <- apply(lai_MMM_orchidee_rcp60_2005soc_co2_year[,,65:94],c(1,2),mean, na.rm=T) -
(a*corr_sm_tran_MMM_orchidee_rcp60_2005soc_co2+b)
EI_MMM_orchidee_rcp60_2005soc_2005co2 <- apply(lai_MMM_orchidee_rcp60_2005soc_2005co2_year[,,65:94],c(1,2),mean, na.rm=T) -
(a*corr_sm_tran_MMM_orchidee_rcp60_2005soc_2005co2+b)

EI_MMM_caraib_historical_histsoc_co2 <- apply(lai_MMM_caraib_historical_histsoc_co2_year[,,111:140],c(1,2),mean, na.rm=T) -
 (a*corr_sm_tran_MMM_caraib_historical_histsoc_co2+b)
EI_MMM_caraib_rcp60_2005soc_co2 <- apply(lai_MMM_caraib_rcp60_2005soc_co2_year[,,65:94],c(1,2),mean, na.rm=T) -
(a*corr_sm_tran_MMM_caraib_rcp60_2005soc_co2+b)
EI_MMM_caraib_rcp60_2005soc_2005co2 <- apply(lai_MMM_caraib_rcp60_2005soc_2005co2_year[,,65:94],c(1,2),mean, na.rm=T) -
(a*corr_sm_tran_MMM_caraib_rcp60_2005soc_2005co2+b)


############################################################################################
### We just take the distribution over 4 GCMs x 3 DGVMs = 12 models. 
area_tot <- sum(cell_area*mask, na.rm=T)
frac_drylands_hist <- array(NA, dim=c(3,4))
frac_drylands_rcp60_co2 <-  array(NA, dim=c(3,4))
frac_drylands_rcp60_noco2 <-  array(NA, dim=c(3,4))
for (m in 1:3) {  print(m) #models
for (n in 1:4) {  print(n) #forcings
##Pres
lai_pres <- get(paste("lai_",forcing2[n],"_",list_models[m],"_historical_histsoc_co2_year",sep=""))
mean_lai_pres <- mask*apply(lai_pres[,,111:140], c(1,2), mean, na.rm=T)
mean_corrsmtran_pres <-  mask*get(paste("corr_sm_tran_",forcing2[n],"_",list_models[m],"_historical_histsoc_co2",sep=""))
mean_EI_pres <- mean_lai_pres -   ( a * mean_corrsmtran_pres +  b  )
frac_drylands_hist[m,n] <- sum(   (cell_area*mask)[which(mean_EI_pres < 0)],na.rm=T)/area_tot * 100
## Fut with CO2
lai_fut_co2 <- get(paste("lai_",forcing2[n],"_",list_models[m],"_rcp60_2005soc_co2_year",sep=""))
mean_lai_fut_co2 <- mask*apply(lai_fut_co2[,,65:94], c(1,2), mean, na.rm=T)
mean_corrsmtran_fut_co2 <-  mask*get(paste("corr_sm_tran_",forcing2[n],"_",list_models[m],"_rcp60_2005soc_co2",sep=""))
mean_EI_fut_co2 <- mean_lai_fut_co2 -   ( a * mean_corrsmtran_fut_co2 +  b  )
frac_drylands_rcp60_co2[m,n] <- sum(   (cell_area*mask)[which(mean_EI_fut_co2 < 0)],na.rm=T)/area_tot * 100
## Fut without CO2
lai_fut_noco2 <- get(paste("lai_",forcing2[n],"_",list_models[m],"_rcp60_2005soc_2005co2_year",sep=""))
mean_lai_fut_noco2 <-mask* apply(lai_fut_noco2[,,65:94], c(1,2), mean, na.rm=T)
mean_corrsmtran_fut_noco2 <-  mask*get(paste("corr_sm_tran_",forcing2[n],"_",list_models[m],"_rcp60_2005soc_2005co2",sep=""))
mean_EI_fut_noco2 <- mean_lai_fut_noco2 -   ( a * mean_corrsmtran_fut_noco2 +  b  )
frac_drylands_rcp60_noco2[m,n] <- sum(   (cell_area*mask)[which(mean_EI_fut_noco2 < 0)],na.rm=T)/area_tot * 100 }}

EI_MMM_fut_co2 <- array(NA, dim=c(720,360,3)); EI_MMM_fut_noco2 <- array(NA, dim=c(720,360,3));EI_MMM_pres <- array(NA, dim=c(720,360,3))
for (m in 1:3){print(m)
lon <- get(paste("lon_",list_models[m], sep=""));lat <- get(paste("lat_",list_models[m], sep=""))
lowlat <- min(which(lat > -60)); highlat <- min(which(lat > 80))
EI_MMM_fut_co2 [,,m] <- mask*get(paste("EI_MMM_", list_models[m], "_rcp60_2005soc_co2",sep=""))
EI_MMM_fut_noco2 [,,m] <- mask*get(paste("EI_MMM_", list_models[m], "_rcp60_2005soc_2005co2",sep=""))
EI_MMM_pres[,,m]<- mask*get(paste("EI_MMM_", list_models[m],"_historical_histsoc_co2",sep="")) }


#################################################################
#################################################################
#################################################################
#################################################################
#################################################################
## What we've done so far is plotting the mean of 3 EI calculated for each DGVM separately, averaging 4 GCMs for each DGVM...
## Let's calculate one MMM LAI and cor(SM,Tran), and calculate EI for this.


MMM_lai_pres <- array(NA, dim=c(720,360)); MMM_corr_sm_tran_pres <- array(NA, dim=c(720,360))
for (i in 1:720){print(i)
for (j in 1:360){
MMM_lai_pres[i,j] <- mean(rowMeans(cbind(lai_MMM_lpj_guess_historical_histsoc_co2_year[i,j,], lai_MMM_orchidee_historical_histsoc_co2_year[i,j,],
lai_MMM_caraib_historical_histsoc_co2_year[i,j,]), na.rm=T)[111:140],na.rm=T)
MMM_corr_sm_tran_pres[i,j] <- mean(c(corr_sm_tran_MMM_lpj_guess_historical_histsoc_co2[i,j], corr_sm_tran_MMM_orchidee_historical_histsoc_co2[i,j],
corr_sm_tran_MMM_caraib_historical_histsoc_co2[i,j]), na.rm=T)  }}

MMM_lai_fut_co2 <- array(NA, dim=c(720,360)); MMM_corr_sm_tran_fut_co2 <- array(NA, dim=c(720,360))
for (i in 1:720){print(i)
for (j in 1:360){
MMM_lai_fut_co2[i,j] <- mean(rowMeans(cbind(lai_MMM_lpj_guess_rcp60_2005soc_co2_year[i,j,], lai_MMM_orchidee_rcp60_2005soc_co2_year[i,j,],
lai_MMM_caraib_rcp60_2005soc_co2_year[i,j,]), na.rm=T)[65:94],na.rm=T)
MMM_corr_sm_tran_fut_co2[i,j] <- mean(c(corr_sm_tran_MMM_lpj_guess_rcp60_2005soc_co2[i,j], corr_sm_tran_MMM_orchidee_rcp60_2005soc_co2[i,j],
corr_sm_tran_MMM_caraib_rcp60_2005soc_co2[i,j]), na.rm=T)  }}

MMM_lai_fut_noco2 <- array(NA, dim=c(720,360)); MMM_corr_sm_tran_fut_noco2 <- array(NA, dim=c(720,360))
for (i in 1:720){print(i)
for (j in 1:360){
MMM_lai_fut_noco2[i,j] <- mean( rowMeans(cbind(lai_MMM_lpj_guess_rcp60_2005soc_2005co2_year[i,j,], lai_MMM_orchidee_rcp60_2005soc_2005co2_year[i,j,],
lai_MMM_caraib_rcp60_2005soc_2005co2_year[i,j,]), na.rm=T)[65:94],na.rm=T)
MMM_corr_sm_tran_fut_noco2[i,j] <- mean(c(corr_sm_tran_MMM_lpj_guess_rcp60_2005soc_2005co2[i,j], corr_sm_tran_MMM_orchidee_rcp60_2005soc_2005co2[i,j],
corr_sm_tran_MMM_caraib_rcp60_2005soc_2005co2[i,j]), na.rm=T)  }}

MMM_EI_pres <- mask*(MMM_lai_pres - (a*MMM_corr_sm_tran_pres+b))
MMM_EI_fut_co2 <- mask*(MMM_lai_fut_co2 - (a*MMM_corr_sm_tran_fut_co2+b))
MMM_EI_fut_noco2 <- mask*(MMM_lai_fut_noco2 - (a*MMM_corr_sm_tran_fut_noco2+b))


area_tot <- sum(cell_area*mask, na.rm=T)
MMM_frac_drylands_pres <- sum(   (cell_area*mask)[which(MMM_EI_pres < 0)],na.rm=T)/area_tot * 100
print(MMM_frac_drylands_pres)
MMM_frac_drylands_fut_co2 <- sum(   (cell_area*mask)[which(MMM_EI_fut_co2 < 0)],na.rm=T)/area_tot * 100
print(MMM_frac_drylands_fut_co2)
MMM_frac_drylands_fut_noco2 <- sum(   (cell_area*mask)[which(MMM_EI_fut_noco2 < 0)],na.rm=T)/area_tot * 100
print(MMM_frac_drylands_fut_noco2)


###########################################################################
### We need to calculate EI per model, so we can calculate model agreement:
EI_pres_allmodels <- array(NA, dim=c(720,360,3,4))
EI_fut_co2_allmodels <- array(NA, dim=c(720,360,3,4))
EI_fut_noco2_allmodels <- array(NA, dim=c(720,360,3,4))
for (m in 1:3) {  print(m) #models
for (n in 1:4) {  print(n) #forcings
##Pres
lai_pres <- get(paste("lai_",forcing2[n],"_",list_models[m],"_historical_histsoc_co2_year",sep=""))
mean_lai_pres <- apply(lai_pres[,,111:140], c(1,2), mean, na.rm=T)
mean_corrsmtran_pres <-  get(paste("corr_sm_tran_",forcing2[n],"_",list_models[m],"_historical_histsoc_co2",sep=""))
EI_pres_allmodels[,,m,n] <- mean_lai_pres -   ( a * mean_corrsmtran_pres +  b  )
## Fut with CO2
lai_fut_co2 <- get(paste("lai_",forcing2[n],"_",list_models[m],"_rcp60_2005soc_co2_year",sep=""))
mean_lai_fut_co2 <- apply(lai_fut_co2[,,65:94], c(1,2), mean, na.rm=T)
mean_corrsmtran_fut_co2 <-  get(paste("corr_sm_tran_",forcing2[n],"_",list_models[m],"_rcp60_2005soc_co2",sep=""))
EI_fut_co2_allmodels[,,m,n] <- mean_lai_fut_co2 -   ( a * mean_corrsmtran_fut_co2 +  b  )
## Fut without CO2
lai_fut_noco2 <- get(paste("lai_",forcing2[n],"_",list_models[m],"_rcp60_2005soc_2005co2_year",sep=""))
mean_lai_fut_noco2 <- apply(lai_fut_noco2[,,65:94], c(1,2), mean, na.rm=T)
mean_corrsmtran_fut_noco2 <-  get(paste("corr_sm_tran_",forcing2[n],"_",list_models[m],"_rcp60_2005soc_2005co2",sep=""))
EI_fut_noco2_allmodels[,,m,n]  <- mean_lai_fut_noco2 -   ( a * mean_corrsmtran_fut_noco2 +  b  )
}}

coord_sign_EI_year_change_co2 <- NULL
for (i in 1:720){ print(i)
for (j in 1:360){
if (is.na(EI_pres_allmodels[i,j,1,1])==F){
bob <- length(which(sign( (EI_fut_co2_allmodels - EI_pres_allmodels)[i,j,, ])==1))
bill  <- 12 - bob
if ((bob > 12*0.74) || (bill > 12*0.74) ){ coord_sign_EI_year_change_co2 <- rbind(coord_sign_EI_year_change_co2,c(lon[i],lat[j])) }
}}  }
for (i in 1:dim(coord_sign_EI_year_change_co2)[1]){
if  (coord_sign_EI_year_change_co2[i,1] > 180)  { coord_sign_EI_year_change_co2[i,1]<-coord_sign_EI_year_change_co2[i,1] -360 }}
coord_sign_EI_year_change_noco2 <- NULL
for (i in 1:720){ print(i)
for (j in 1:360){
if (is.na(EI_pres_allmodels[i,j,1,1])==F){
bob <- length(which(sign( (EI_fut_noco2_allmodels - EI_pres_allmodels)[i,j,, ])==1))
bill  <- 12 - bob
if ((bob > 12*0.74) || (bill > 12*0.74) ){ coord_sign_EI_year_change_noco2 <- rbind(coord_sign_EI_year_change_noco2,c(lon[i],lat[j])) }
}}}
for (i in 1:dim(coord_sign_EI_year_change_noco2)[1]){
if  (coord_sign_EI_year_change_noco2[i,1] > 180)  { coord_sign_EI_year_change_noco2[i,1]<-coord_sign_EI_year_change_noco2[i,1] -360  }}

######### What if we regrid, because otherwise plotting the stippling makes everything dark...
### We need to calculate EI per model, so we can calculate model agreement:
EI_pres_allmodels_1x1 <- array(NA, dim=c(361,181,3,4))
EI_fut_co2_allmodels_1x1 <- array(NA, dim=c(361,181,3,4))
EI_fut_noco2_allmodels_1x1 <- array(NA, dim=c(361,181,3,4))
for (m in 1:3) {  print(m) #models
for (n in 1:4) {  print(n) #forcings
##Pres
lai_pres <- get(paste("lai_",forcing2[n],"_",list_models[m],"_historical_histsoc_co2_year",sep=""))
mean_lai_pres <- apply(lai_pres[,,111:140], c(1,2), mean, na.rm=T)
mean_corrsmtran_pres <-  get(paste("corr_sm_tran_",forcing2[n],"_",list_models[m],"_historical_histsoc_co2",sep=""))
bob <- mean_lai_pres; bob[which(is.na(bob) ==T)] <- 0
mean_lai_pres_1x1 <- bicubic.grid(lon, lat, bob, xlim=c(-180,180), ylim=c(-90,90), dx=1, dy=1 )$z
bob <- mean_corrsmtran_pres; bob[which(is.na(bob)==T)] <- 0
mean_corrsmtran_pres_1x1 <-  bicubic.grid(lon, lat, bob, xlim=c(-180,180), ylim=c(-90,90), dx=1, dy=1 )$z
EI_pres_allmodels_1x1[,,m,n] <- mean_lai_pres_1x1 - ( a * mean_corrsmtran_pres_1x1 +  b  )

## Fut with CO2
lai_fut_co2 <- get(paste("lai_",forcing2[n],"_",list_models[m],"_rcp60_2005soc_co2_year",sep=""))
mean_lai_fut_co2 <- apply(lai_fut_co2[,,65:94], c(1,2), mean, na.rm=T)
mean_corrsmtran_fut_co2 <-  get(paste("corr_sm_tran_",forcing2[n],"_",list_models[m],"_rcp60_2005soc_co2",sep=""))
bob <- mean_lai_fut_co2; bob[which(is.na(bob)==T)] <- 0
mean_lai_fut_co2_1x1 <- bicubic.grid(lon, lat, bob, xlim=c(-180,180), ylim=c(-90,90), dx=1, dy=1 )$z
bob <- mean_corrsmtran_fut_co2; bob[which(is.na(bob)==T)] <- 0
mean_corrsmtran_fut_co2_1x1 <-  bicubic.grid(lon, lat, bob, xlim=c(-180,180), ylim=c(-90,90), dx=1, dy=1 )$z
EI_fut_co2_allmodels_1x1[,,m,n] <- mean_lai_fut_co2_1x1 - (a * mean_corrsmtran_fut_co2_1x1 +  b  )
## Fut without CO2
lai_fut_noco2 <- get(paste("lai_",forcing2[n],"_",list_models[m],"_rcp60_2005soc_2005co2_year",sep=""))
mean_lai_fut_noco2 <- apply(lai_fut_noco2[,,65:94], c(1,2), mean, na.rm=T)
mean_corrsmtran_fut_noco2 <-  get(paste("corr_sm_tran_",forcing2[n],"_",list_models[m],"_rcp60_2005soc_2005co2",sep=""))
bob <- mean_lai_fut_noco2; bob[which(is.na(bob)==T)] <- 0
mean_lai_fut_noco2_1x1 <- bicubic.grid(lon, lat, bob, xlim=c(-180,180), ylim=c(-90,90), dx=1, dy=1 )$z
bob <- mean_corrsmtran_fut_noco2; bob[which(is.na(bob)==T)] <- 0
mean_corrsmtran_fut_noco2_1x1 <-  bicubic.grid(lon, lat, bob, xlim=c(-180,180), ylim=c(-90,90), dx=1, dy=1 )$z
EI_fut_noco2_allmodels_1x1[,,m,n]  <- mean_lai_fut_noco2_1x1 - ( a * mean_corrsmtran_fut_noco2_1x1 +  b  )
}}

m=1
n=1
mean_corrsmtran_pres <-  get(paste("corr_sm_tran_",forcing2[n],"_",list_models[m],"_historical_histsoc_co2",sep=""))
bob <- mean_corrsmtran_pres; bob[which(is.na(bob)==T)] <- 0
mean_corrsmtran_pres_1x1 <-  bicubic.grid(lon, lat, bob, xlim=c(-180,180), ylim=c(-90,90), dx=1, dy=1 )$z
lon_1x1 <- bicubic.grid(lon, lat, bob, xlim=c(-180,180), ylim=c(-90,90), dx=1, dy=1 )$x
lat_1x1 <- bicubic.grid(lon, lat, bob, xlim=c(-180,180), ylim=c(-90,90), dx=1, dy=1 )$y

coord_sign_EI_year_change_co2_1x1 <- NULL
for (i in 1:361){ print(i)
for (j in 1:181){
if (round(EI_pres_allmodels_1x1[i,j,1,1],7) != -0.1522463 ){
bob <- length(which(sign( (EI_fut_co2_allmodels_1x1 - EI_pres_allmodels_1x1)[i,j,, ])==1))
bill  <- 12 - bob
if ((bob > 12*0.74) || (bill > 12*0.74) ){ coord_sign_EI_year_change_co2_1x1 <- rbind(coord_sign_EI_year_change_co2_1x1,c(lon_1x1[i],lat_1x1[j])) }
}}  }
for (i in 1:dim(coord_sign_EI_year_change_co2_1x1)[1]){
if  (coord_sign_EI_year_change_co2_1x1[i,1] > 180)  { coord_sign_EI_year_change_co2_1x1[i,1]<-coord_sign_EI_year_change_co2_1x1[i,1] - 360 }}
coord_sign_EI_year_change_noco2_1x1 <- NULL
for (i in 1:361){ print(i)
for (j in 1:181){
if ( round(EI_pres_allmodels_1x1[i,j,1,1],7) !=  -0.1522463) {
bob <- length(which(sign( (EI_fut_noco2_allmodels_1x1 - EI_pres_allmodels_1x1)[i,j,, ])==1))
bill  <- 12 - bob
if ((bob > 12*0.74) || (bill > 12*0.74) ){ coord_sign_EI_year_change_noco2_1x1 <- rbind(coord_sign_EI_year_change_noco2_1x1,c(lon_1x1[i],lat_1x1[j])) }
}}}
for (i in 1:dim(coord_sign_EI_year_change_noco2_1x1)[1]){
if  (coord_sign_EI_year_change_noco2_1x1[i,1] > 180)  { coord_sign_EI_year_change_noco2_1x1[i,1]<-coord_sign_EI_year_change_noco2_1x1[i,1] -360  }}


#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
## Now with 2x2

######### What if we regrid, because otherwise plotting the stippling makes everything dark...
### We need to calculate EI per model, so we can calculate model agreement:
EI_pres_allmodels_2x2 <- array(NA, dim=c(181,91,3,4))
EI_fut_co2_allmodels_2x2 <- array(NA, dim=c(181,91,3,4))
EI_fut_noco2_allmodels_2x2 <- array(NA, dim=c(181,91,3,4))
for (m in 1:3) {  print(m) #models
for (n in 1:4) {  print(n) #forcings
##Pres
lai_pres <- get(paste("lai_",forcing2[n],"_",list_models[m],"_historical_histsoc_co2_year",sep=""))
mean_lai_pres <- apply(lai_pres[,,111:140], c(1,2), mean, na.rm=T)
mean_corrsmtran_pres <-  get(paste("corr_sm_tran_",forcing2[n],"_",list_models[m],"_historical_histsoc_co2",sep=""))
bob <- mean_lai_pres; bob[which(is.na(bob) ==T)] <- 0
mean_lai_pres_2x2 <- bicubic.grid(lon, lat, bob, xlim=c(-180,180), ylim=c(-90,90), dx=2, dy=2 )$z
bob <- mean_corrsmtran_pres; bob[which(is.na(bob)==T)] <- 0
mean_corrsmtran_pres_2x2 <-  bicubic.grid(lon, lat, bob, xlim=c(-180,180), ylim=c(-90,90), dx=2, dy=2 )$z
EI_pres_allmodels_2x2[,,m,n] <- mean_lai_pres_2x2 - ( a * mean_corrsmtran_pres_2x2 +  b  )
## Fut with CO2
lai_fut_co2 <- get(paste("lai_",forcing2[n],"_",list_models[m],"_rcp60_2005soc_co2_year",sep=""))
mean_lai_fut_co2 <- apply(lai_fut_co2[,,65:94], c(1,2), mean, na.rm=T)
mean_corrsmtran_fut_co2 <-  get(paste("corr_sm_tran_",forcing2[n],"_",list_models[m],"_rcp60_2005soc_co2",sep=""))
bob <- mean_lai_fut_co2; bob[which(is.na(bob)==T)] <- 0
mean_lai_fut_co2_2x2 <- bicubic.grid(lon, lat, bob, xlim=c(-180,180), ylim=c(-90,90), dx=2, dy=2 )$z
bob <- mean_corrsmtran_fut_co2; bob[which(is.na(bob)==T)] <- 0
mean_corrsmtran_fut_co2_2x2 <-  bicubic.grid(lon, lat, bob, xlim=c(-180,180), ylim=c(-90,90), dx=2, dy=2 )$z
EI_fut_co2_allmodels_2x2[,,m,n] <- mean_lai_fut_co2_2x2 - (a * mean_corrsmtran_fut_co2_2x2 +  b  )
## Fut without CO2
lai_fut_noco2 <- get(paste("lai_",forcing2[n],"_",list_models[m],"_rcp60_2005soc_2005co2_year",sep=""))
mean_lai_fut_noco2 <- apply(lai_fut_noco2[,,65:94], c(1,2), mean, na.rm=T)
mean_corrsmtran_fut_noco2 <-  get(paste("corr_sm_tran_",forcing2[n],"_",list_models[m],"_rcp60_2005soc_2005co2",sep=""))
bob <- mean_lai_fut_noco2; bob[which(is.na(bob)==T)] <- 0
mean_lai_fut_noco2_2x2 <- bicubic.grid(lon, lat, bob, xlim=c(-180,180), ylim=c(-90,90), dx=2, dy=2 )$z
bob <- mean_corrsmtran_fut_noco2; bob[which(is.na(bob)==T)] <- 0
mean_corrsmtran_fut_noco2_2x2 <-  bicubic.grid(lon, lat, bob, xlim=c(-180,180), ylim=c(-90,90), dx=2, dy=2 )$z
EI_fut_noco2_allmodels_2x2[,,m,n]  <- mean_lai_fut_noco2_2x2 - ( a * mean_corrsmtran_fut_noco2_2x2 +  b  )
}}

m=1
n=1
mean_corrsmtran_pres <-  get(paste("corr_sm_tran_",forcing2[n],"_",list_models[m],"_historical_histsoc_co2",sep=""))
bob <- mean_corrsmtran_pres; bob[which(is.na(bob)==T)] <- 0
mean_corrsmtran_pres_2x2 <-  bicubic.grid(lon, lat, bob, xlim=c(-180,180), ylim=c(-90,90), dx=2, dy=2 )$z
lon_2x2 <- bicubic.grid(lon, lat, bob, xlim=c(-180,180), ylim=c(-90,90), dx=2, dy=2 )$x
lat_2x2 <- bicubic.grid(lon, lat, bob, xlim=c(-180,180), ylim=c(-90,90), dx=2, dy=2 )$y

coord_sign_EI_year_change_co2_2x2 <- NULL
for (i in 1:181){ print(i)
for (j in 1:91){
if (round(EI_pres_allmodels_2x2[i,j,1,1],7) != -0.1522463 ){
bob <- length(which(sign( (EI_fut_co2_allmodels_2x2 - EI_pres_allmodels_2x2)[i,j,, ])==1))
bill  <- 12 - bob
if ((bob > 12*0.74) || (bill > 12*0.74) ){ coord_sign_EI_year_change_co2_2x2 <- rbind(coord_sign_EI_year_change_co2_2x2,c(lon_2x2[i],lat_2x2[j])) }
}}  }
for (i in 1:dim(coord_sign_EI_year_change_co2_2x2)[1]){
if  (coord_sign_EI_year_change_co2_2x2[i,1] > 180)  { coord_sign_EI_year_change_co2_2x2[i,1]<-coord_sign_EI_year_change_co2_2x2[i,1] - 360 }}
coord_sign_EI_year_change_noco2_2x2 <- NULL
for (i in 1:181){ print(i)
for (j in 1:91){
if ( round(EI_pres_allmodels_2x2[i,j,1,1],7) !=  -0.1522463) {
bob <- length(which(sign( (EI_fut_noco2_allmodels_2x2 - EI_pres_allmodels_2x2)[i,j,, ])==1))
bill  <- 12 - bob
if ((bob > 12*0.74) || (bill > 12*0.74) ){ coord_sign_EI_year_change_noco2_2x2 <- rbind(coord_sign_EI_year_change_noco2_2x2,c(lon_2x2[i],lat_2x2[j])) }
}}}
for (i in 1:dim(coord_sign_EI_year_change_noco2_2x2)[1]){
if  (coord_sign_EI_year_change_noco2_2x2[i,1] > 180)  { coord_sign_EI_year_change_noco2_2x2[i,1]<-coord_sign_EI_year_change_noco2_2x2[i,1] -360  }}

