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


co2_pres_year<- c(310.75,311.1,311.5, 311.925,312.425,313,313.6,314.225,314.8475,315.5,316.2725,317.075,317.795,318.3975,318.925,319.6475,320.6475,321.605,322.635,323.9025,324.985,325.855,
327.14,328.6775,329.7425,330.585,331.7475,333.2725,334.8475,336.525,338.36,339.7275,340.7925,342.1975,343.7825,345.2825, 346.7975,348.645,350.7375,352.4875,353.855,355.0175,355.885,356.7775,358.1275,359.8375,361.4625,363.155,365.3225,367.3475,368.865,370.4675,372.5225,
374.76,376.8125,378.8125)
co2_pres_monthly <-rep(co2_pres_year[1],12) 
for (a in 2:56){
co2_pres_monthly <- append(co2_pres_monthly, rep(co2_pres_year[a],12)) }

for (m in 1:28){
if ((list_models_common[m] %in% list_models_lai)==FALSE) {
print(list_models_common[m])
lat <- get(paste("lat_",list_models_common2[m], sep=""))
lon <- get(paste("lon_",list_models_common2[m], sep=""))
mask <- get(paste("mask_",list_models_common2[m], sep=""))
############# Getting data
for (v in 1:13) { print(list_var[v])
data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.historical/.",dom[v],"/.mon/.", list_var[v], "/",
 list_models_common[m],"/.r1i1p1/.",list_var[v],"/dods", sep=""))
variable <- ncvar_get(data, list_var[v])
buff<- dim(variable)[3]
variable<- variable[,,(buff-56*12+1):buff]
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
Lv <- 2500.8-2.36*(tas-273.16)+0.0016*(tas-273.16)^2-0.00006*(tas-273.16)^3
#Lv <- 2501 - 2.36*(tas - 273.16)
#in J/g. Multiply 1000/1000000=1/1000 to get MJ/kg
Lv <- Lv/1000 # in MJ/kg
Lv <- 2.501 # in MJ/Kg
#Lv <- hfls / evspsbl # from the climate model, in J/kg
#Lv <- Lv / 1000000 #now in MJ/kg
Lv2 <- Lv * 1000000 # J/kg
#### Other stuff:
#vpsat  <-  0.611 * exp( 17.27 * (tas-273.16) / (tas-273.16 + 237.3) )  # in kPa
vpsat <-  0.611 * exp( 17.625 * (tas-273.16) / (tas-273.16 + 243.04) )  # in kPa
delta <- vpsat*4098/(((tas-273.16)+237.3)^2)   # in kPa/C
## gamma=cp*P/(Lv*MWratio) where MWratio is the ratio of molecular weight of water vapor/dry air=0.622
#gamma <- 0.665*10^-3* (ps/1000)   # in kPa/C
gamma <- 1.013e-3/(Lv*0.622) * (ps/1000) ## accounting for varying Lv
netrad <- (rsds+rlds-rsus-rlus)   # in W.m-2. Multiply by 86400/1000000 to get MJ.m-2.d-1
w <- huss/(1-huss)
es <-  w/(w+0.622)*(ps/1000) # in kPa, so the VPD is in kPa, too.
rho_3D <- ps /(287.058*1.01*tas)
cp <-1.013e3 #J.g-1.K-1 = 1013J.kg-1.K-1 ; rho is in kg.m-3; vpsat is in Pa=kg.m-1.s-2, chU in m.s-1;
### numerator=J.kg-1.K-1*kg.m-3*Pa*m.s-1=Pa.K-1.J.m-2.s-1

D <- (vpsat -es)

PETpmrc <- (0.408*delta*(0.0864*(hfls+hfss))+ gamma*900/tas*D*sfcWind_2m)/ (delta + gamma*(1+0.34*sfcWind_2m))
PETpmrc_co2 <- (0.408*delta*(0.0864*(hfls+hfss))+ gamma*900/tas*D*sfcWind_2m)/ (delta + gamma*(1+sfcWind_2m*(0.34+2.4*0.0001*(co2_pres_monthly-300))))
PETow <- 1/(Lv)*( delta/(delta + gamma)*(hfls+hfss)*0.0864 + gamma/(gamma+delta)*(6.43*(1+0.536*sfcWind_2m)*(vpsat-es)))
PETpm <- 1/Lv2 *( (delta*(hfls+hfss)+ cp*rho_3D*(vpsat-es)*Ch*sfcWind)/(delta + gamma*(1+rs*Ch*sfcWind)))
PETpm <- PETpm* 86400 # in mm/d

save(PETpmrc, file=paste("PETpmrc_",list_models_common2[m],".RData", sep=""))
save(PETpmrc_co2, file=paste("PETpmrc_co2_",list_models_common2[m],".RData", sep=""))

rm(hfss);rm(hfls);rm(rlds); rm(rlus);rm(rsus); rm(rsds);rm(sfcWind); rm(sfcWind_2m); rm(ps);rm(huss); rm(w); rm(es); rm(Lv)
rm(delta); rm(gamma); rm(rho); rm(rho_3D) ; rm(PETow); rm(PETpm); 
rm(tas); rm(vpsat); rm(netrad)
}}



#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
##### Future

co2_fut_year <- c(684.95429,692.90196,700.89416,708.93159,717.01548,725.13597,733.30667,741.52368,749.80466,758.1823,766.64451,775.17446,783.75141,792.36578,
801.0188,809.71464,818.42214,827.15719,835.95594,844.80471,853.72536,862.72597,871.7768,880.86435,889.98162,899.12407,908.28871,917.47137,926.66527,935.87437)

co2_fut_monthly <-rep(co2_fut_year[1],12)
for (a in 2:30){
co2_fut_monthly <- append(co2_fut_monthly, rep(co2_fut_year[a],12)) }
mask <- get(paste("mask_",list_models_common2[m], sep=""))
list_var <- c("rsds", "rsus", "rlds", "rlus", "sfcWind", "ps", "huss", "tas","pr" ,"evspsbl", "hfss", "hfls", "ts")
dom <- rep("atmos", 13)

for (m in 1:27){
print(list_models_common_fut[m])
for (v in 1:13){
print(list_var[v])
data <- nc_open(paste("http://strega.ldeo.columbia.edu:81/CMIP5/.byScenario/.rcp85/.",dom[v],"/.mon/.", list_var[v], "/",
list_models_common_fut[m],"/.r1i1p1/.",list_var[v],"/dods", sep=""))
variable <- ncvar_get(data, list_var[v]); buff<- dim(variable)[3]
if (buff > 1140) {buff <- 1140}
variable<- variable[,,(buff-30*12+1):buff]
assign(list_var[v], variable ); nc_close(data); rm(variable); rm(buff)}
############### Fixing wind, if necessary
if (m %in% c(1:2,17:19)){ print("Fixing wind grid...")
lat <- get(paste("lat_",list_models_common_fut2[m], sep=""))
lon <- get(paste("lon_",list_models_common_fut2[m], sep=""))
bob <- array(NA, dim=c(length(lon), length(lat), 360))
for (t in 1:360){bob[,,t] <- bicubic.grid(lon, seq(-90,90,length.out=dim(sfcWind)[2]), sfcWind[,,t] , xlim=c(0,lon[length(lon)]), ylim=c(-90,90), dx=360/length(lon), dy=180/(length(lat)-1) )$z}
sfcWind <- bob }
rm(bob); sfcWind_2m <- sfcWind*4.87/(log(67.8*10-5.42) )
############################## Calculating PET
### Latent heat of vaporatization
Lv <- 2500.8-2.36*(tas-273.16)+0.0016*(tas-273.16)^2-0.00006*(tas-273.16)^3
#Lv <- 2501 - 2.36*(tas - 273.16)
#in J/g. Multiply 1000/1000000=1/1000 to get MJ/kg
Lv <- Lv/1000 # in MJ/kg
Lv <- 2.501 # in MJ/Kg
#Lv <- hfls / evspsbl # from the climate model, in J/kg
#Lv <- Lv / 1000000 #now in MJ/kg
Lv2 <- Lv * 1000000 # J/kg
#### Other stuff:
#vpsat  <-  0.611 * exp( 17.27 * (tas-273.16) / (tas-273.16 + 237.3) )  # in kPa
vpsat <-  0.611 * exp( 17.625 * (tas-273.16) / (tas-273.16 + 243.04) )  # in kPa
delta <- vpsat*4098/(((tas-273.16)+237.3)^2)   # in kPa/C
## gamma=cp*P/(Lv*MWratio) where MWratio is the ratio of molecular weight of water vapor/dry air=0.622
#gamma <- 0.665*10^-3* (ps/1000)   # in kPa/C
gamma <- 1.013e-3/(Lv*0.622) * (ps/1000) ## accounting for varying Lv
netrad <- (rsds+rlds-rsus-rlus)   # in W.m-2. Multiply by 86400/1000000 to get MJ.m-2.d-1
w <- huss/(1-huss)
es <-  w/(w+0.622)*(ps/1000) # in kPa, so the VPD is in kPa, too.
rho_3D <- ps /(287.058*1.01*tas)
cp <-1.013e3 #J.g-1.K-1 = 1013J.kg-1.K-1 ; rho is in kg.m-3; vpsat is in Pa=kg.m-1.s-2, chU in m.s-1;
### numerator=J.kg-1.K-1*kg.m-3*Pa*m.s-1=Pa.K-1.J.m-2.s-1
D <- (vpsat -es)
PETpmrc <- (0.408*delta*(0.0864*(hfls+hfss))+ gamma*900/tas*D*sfcWind_2m)/ (delta + gamma*(1+0.34*sfcWind_2m))
PETpmrc_co2 <- (0.408*delta*(0.0864*(hfls+hfss))+ gamma*900/tas*D*sfcWind_2m)/ (delta + gamma*(1+sfcWind_2m*(0.34+2.4*0.0001*(co2_fut_monthly-300))))
PETow <- 1/(Lv)*( delta/(delta + gamma)*(hfls+hfss)*0.0864 + gamma/(gamma+delta)*(6.43*(1+0.536*sfcWind_2m)*(vpsat-es)))
PETpm <- 1/Lv2 *( (delta*(hfls+hfss)+ cp*rho_3D*(vpsat-es)*Ch*sfcWind)/(delta + gamma*(1+rs*Ch*sfcWind)))
PETpm <- PETpm* 86400 # in mm/d

save(PETpmrc, file=paste("PETpmrc_",list_models_common_fut2[m],"_rcp85.RData", sep=""))
save(PETpmrc_co2, file=paste("PETpmrc_co2_",list_models_common_fut2[m],"_rcp85.RData", sep=""))

rm(hfss);rm(hfls);rm(rlds); rm(rlus);rm(rsus); rm(rsds);rm(sfcWind); rm(sfcWind_2m); rm(ps);rm(huss); rm(w); rm(es); rm(Lv)
rm(delta); rm(gamma); rm(rho); rm(rho_3D) ; rm(PETow); rm(PETpm); 
rm(tas); rm(vpsat); rm(netrad)
}



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
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
######################## Computing mean annual values


for (m in 1:28){
if (list_models_common[m] %in% list_models_common_fut) {
print(list_models_common[m])
load(paste("PETpmrc_",list_models_common2[m],".RData", sep=""))
assign(paste("mean_PETpmrc_",list_models_common2[m],"_year_pres",sep=""), apply(PETpmrc[,,253:612],c(1,2), mean, na.rm=T))
rm(PETpmrc)
load(paste("PETpmrc_",list_models_common2[m],"_rcp85.RData", sep=""))
assign(paste("mean_PETpmrc_",list_models_common2[m],"_year_fut",sep=""),  apply(PETpmrc[,,],c(1,2), mean, na.rm=T))
rm(PETpmrc)}}

for (m in 1:28){
if (list_models_common[m] %in% list_models_common_fut) {
print(list_models_common[m])
load(paste("PETpmrc_co2_",list_models_common2[m],".RData", sep=""))
assign(paste("mean_PETpmrc_co2_",list_models_common2[m],"_year_pres",sep=""), apply(PETpmrc_co2[,,253:612],c(1,2), mean, na.rm=T))
rm(PETpmrc_co2)
load(paste("PETpmrc_co2_",list_models_common2[m],"_rcp85.RData", sep=""))
assign(paste("mean_PETpmrc_co2_",list_models_common2[m],"_year_fut",sep=""),  apply(PETpmrc_co2[,,],c(1,2), mean, na.rm=T))
rm(PETpmrc_co2)}}


##########################################################################
##################################### Regridding:
#### PETpmrc
for (m in 1:28){
print(list_models_common2[m])
if (list_models_common[m] %in% list_models_common_fut) {
lat <- get(paste("lat_", list_models_common2[m], sep=""))
lon <- get(paste("lon_", list_models_common2[m], sep=""))

mean_PETpmrc_year_pres <- get( paste("mean_PETpmrc_",list_models_common2[m],"_year_pres", sep=""))
mean_PETpmrc_year_pres[which(is.na(mean_PETpmrc_year_pres)==T)] <- 0
mean_PETpmrc_year_pres_2x2 <- bicubic.grid(lon, lat, mean_PETpmrc_year_pres, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
assign(paste("mean_PETpmrc_",list_models_common2[m],"_year_pres_2x2", sep=""), mean_PETpmrc_year_pres_2x2)

mean_PETpmrc_year_fut <- get( paste("mean_PETpmrc_",list_models_common2[m],"_year_fut", sep=""))
mean_PETpmrc_year_fut[which(is.na(mean_PETpmrc_year_fut)==T)] <- 0
mean_PETpmrc_year_fut_2x2 <- bicubic.grid(lon, lat, mean_PETpmrc_year_fut, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
assign(paste("mean_PETpmrc_",list_models_common2[m],"_year_fut_2x2", sep=""), mean_PETpmrc_year_fut_2x2)
}}

######### PrPET with PETpmrcpmrc
for (m in 1:28){
print(list_models_common2[m])
if (list_models_common[m] %in% list_models_common_fut) {
lat <- get(paste("lat_", list_models_common2[m], sep=""))
lon <- get(paste("lon_", list_models_common2[m], sep=""))

mean_pr_year_pres <- get( paste("mean_pr_",list_models_common2[m],"_year_pres", sep="")) *86400
mean_PETpmrc_year_pres <- get( paste("mean_PETpmrc_",list_models_common2[m],"_year_pres", sep=""))
mean_PETpmrc_year_pres[which(is.na(mean_PETpmrc_year_pres)==T)] <- 0; mean_pr_year_pres[which(is.na(mean_pr_year_pres)==T)] <- 0
mean_PrPETpmrc_year_pres_2x2 <- bicubic.grid(lon, lat, mean_pr_year_pres/mean_PETpmrc_year_pres, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
assign(paste("mean_PrPETpmrc_",list_models_common2[m],"_year_pres_2x2", sep=""), mean_PrPETpmrc_year_pres_2x2)

mean_pr_year_fut <- get( paste("mean_pr_",list_models_common2[m],"_year_fut", sep="")) *86400
mean_PETpmrc_year_fut <- get( paste("mean_PETpmrc_",list_models_common2[m],"_year_fut", sep=""))
mean_PETpmrc_year_fut[which(is.na(mean_PETpmrc_year_fut)==T)] <- 0; mean_pr_year_fut[which(is.na(mean_pr_year_fut)==T)] <- 0
mean_PrPETpmrc_year_fut_2x2 <- bicubic.grid(lon, lat, mean_pr_year_fut/mean_PETpmrc_year_fut, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
assign(paste("mean_PrPETpmrc_",list_models_common2[m],"_year_fut_2x2", sep=""), mean_PrPETpmrc_year_fut_2x2)
}}

##########################################################
#### PETpmrc_co2
for (m in 1:28){
print(list_models_common2[m])
if (list_models_common[m] %in% list_models_common_fut) {
lat <- get(paste("lat_", list_models_common2[m], sep=""))
lon <- get(paste("lon_", list_models_common2[m], sep=""))

mean_PETpmrc_co2_year_pres <- get( paste("mean_PETpmrc_co2_",list_models_common2[m],"_year_pres", sep=""))
mean_PETpmrc_co2_year_pres[which(is.na(mean_PETpmrc_co2_year_pres)==T)] <- 0
mean_PETpmrc_co2_year_pres_2x2 <- bicubic.grid(lon, lat, mean_PETpmrc_co2_year_pres, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
assign(paste("mean_PETpmrc_co2_",list_models_common2[m],"_year_pres_2x2", sep=""), mean_PETpmrc_co2_year_pres_2x2)

mean_PETpmrc_co2_year_fut <- get( paste("mean_PETpmrc_co2_",list_models_common2[m],"_year_fut", sep=""))
mean_PETpmrc_co2_year_fut[which(is.na(mean_PETpmrc_co2_year_fut)==T)] <- 0
mean_PETpmrc_co2_year_fut_2x2 <- bicubic.grid(lon, lat, mean_PETpmrc_co2_year_fut, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
assign(paste("mean_PETpmrc_co2_",list_models_common2[m],"_year_fut_2x2", sep=""), mean_PETpmrc_co2_year_fut_2x2)
}}


######### PrPET with PETpmrc_co2
for (m in 1:28){
print(list_models_common2[m])
if (list_models_common[m] %in% list_models_common_fut) {
lat <- get(paste("lat_", list_models_common2[m], sep=""))
lon <- get(paste("lon_", list_models_common2[m], sep=""))

mean_pr_year_pres <- get( paste("mean_pr_",list_models_common2[m],"_year_pres", sep="")) *86400
mean_PETpmrc_co2_year_pres <- get( paste("mean_PETpmrc_co2_",list_models_common2[m],"_year_pres", sep=""))
mean_PETpmrc_co2_year_pres[which(is.na(mean_PETpmrc_co2_year_pres)==T)] <- 0; mean_pr_year_pres[which(is.na(mean_pr_year_pres)==T)] <- 0
mean_PrPETpmrc_co2_year_pres_2x2 <- bicubic.grid(lon, lat, mean_pr_year_pres/mean_PETpmrc_co2_year_pres, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
assign(paste("mean_PrPETpmrc_co2_",list_models_common2[m],"_year_pres_2x2", sep=""), mean_PrPETpmrc_co2_year_pres_2x2)

mean_pr_year_fut <- get( paste("mean_pr_",list_models_common2[m],"_year_fut", sep="")) *86400
mean_PETpmrc_co2_year_fut <- get( paste("mean_PETpmrc_co2_",list_models_common2[m],"_year_fut", sep=""))
mean_PETpmrc_co2_year_fut[which(is.na(mean_PETpmrc_co2_year_fut)==T)] <- 0; mean_pr_year_fut[which(is.na(mean_pr_year_fut)==T)] <- 0
mean_PrPETpmrc_co2_year_fut_2x2 <- bicubic.grid(lon, lat, mean_pr_year_fut/mean_PETpmrc_co2_year_fut, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
assign(paste("mean_PrPETpmrc_co2_",list_models_common2[m],"_year_fut_2x2", sep=""), mean_PrPETpmrc_co2_year_fut_2x2)
}}

##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
############### Let's put all models in a matrix:

##Pres
mean_PETpmrc_year_pres_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_PETpmrc_2x2 <- get(paste("mean_PETpmrc_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_PETpmrc_year_pres_2x2_allmodels[,,m] <- mean_PETpmrc_2x2*mask_2x2_NAs
rm(mean_PETpmrc_2x2)}
mean_PrPETpmrc_year_pres_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_PrPETpmrc_2x2 <- get(paste("mean_PrPETpmrc_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_PrPETpmrc_year_pres_2x2_allmodels[,,m] <- mean_PrPETpmrc_2x2*mask_2x2_NAs
rm(mean_PrPETpmrc_2x2)}

mean_PETpmrc_co2_year_pres_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_PETpmrc_co2_2x2 <- get(paste("mean_PETpmrc_co2_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_PETpmrc_co2_year_pres_2x2_allmodels[,,m] <- mean_PETpmrc_co2_2x2*mask_2x2_NAs
rm(mean_PETpmrc_co2_2x2)}
mean_PrPETpmrc_co2_year_pres_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_PrPETpmrc_co2_2x2 <- get(paste("mean_PrPETpmrc_co2_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_PrPETpmrc_co2_year_pres_2x2_allmodels[,,m] <- mean_PrPETpmrc_co2_2x2*mask_2x2_NAs
rm(mean_PrPETpmrc_co2_2x2)}


## Fut
mean_PETpmrc_year_fut_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_PETpmrc_2x2 <- get(paste("mean_PETpmrc_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_PETpmrc_year_fut_2x2_allmodels[,,m] <- mean_PETpmrc_2x2*mask_2x2_NAs
rm(mean_PETpmrc_2x2)}
mean_PrPETpmrc_year_fut_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_PrPETpmrc_2x2 <- get(paste("mean_PrPETpmrc_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_PrPETpmrc_year_fut_2x2_allmodels[,,m] <- mean_PrPETpmrc_2x2*mask_2x2_NAs
rm(mean_PrPETpmrc_2x2)}

mean_PETpmrc_co2_year_fut_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_PETpmrc_co2_2x2 <- get(paste("mean_PETpmrc_co2_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_PETpmrc_co2_year_fut_2x2_allmodels[,,m] <- mean_PETpmrc_co2_2x2*mask_2x2_NAs
rm(mean_PETpmrc_co2_2x2)}
mean_PrPETpmrc_co2_year_fut_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){
print(list_models_common_fut[m])
mean_PrPETpmrc_co2_2x2 <- get(paste("mean_PrPETpmrc_co2_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_PrPETpmrc_co2_year_fut_2x2_allmodels[,,m] <- mean_PrPETpmrc_co2_2x2*mask_2x2_NAs
rm(mean_PrPETpmrc_co2_2x2)}


### Change:

mean_PrPETpmrc_co2_year_change_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){  print(list_models_common_fut[m])
mean_PrPETpmrc_co2_2x2_pres <- get(paste("mean_PrPETpmrc_co2_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_PrPETpmrc_co2_2x2_fut <- get(paste("mean_PrPETpmrc_co2_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_PrPETpmrc_co2_year_change_2x2_allmodels[,,m] <- (mean_PrPETpmrc_co2_2x2_fut - mean_PrPETpmrc_co2_2x2_pres)*mask_2x2_NAs
rm(mean_PrPETpmrc_co2_2x2_pres); rm(mean_PrPETpmrc_co2_2x2_fut)}

mean_PrPETpmrc_year_change_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_common_fut)))
for (m in 1:length(list_models_common_fut)){  print(list_models_common_fut[m])
mean_PrPETpmrc_2x2_pres <- get(paste("mean_PrPETpmrc_",list_models_common_fut2[m],"_year_pres_2x2", sep=""))
mean_PrPETpmrc_2x2_fut <- get(paste("mean_PrPETpmrc_",list_models_common_fut2[m],"_year_fut_2x2", sep=""))
mean_PrPETpmrc_year_change_2x2_allmodels[,,m] <- (mean_PrPETpmrc_2x2_fut - mean_PrPETpmrc_2x2_pres)*mask_2x2_NAs
rm(mean_PrPETpmrc_2x2_pres); rm(mean_PrPETpmrc_2x2_fut)}


save(mean_PrPETpmrc_co2_year_change_2x2_allmodels, file="mean_PrPETpmrc_co2_year_change_2x2_allmodels.Rdata")
save(mean_PrPETpmrc_year_change_2x2_allmodels, file="mean_PrPETpmrc_year_change_2x2_allmodels.Rdata")



