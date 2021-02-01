################################################################
#### Here we calculate correlation between mrsos and tran

#### esmFixclim1
for (m in 1:length(list_models2)) {
print(list_models[m])
lat <- get(paste("lat_", list_models2[m], sep=""))
lon <- get(paste("lon_", list_models2[m], sep=""))
mrsos_year <- get(paste("mrsos_", list_models2[m],"_year_esmFixclim1", sep="") )
tran_year <- get(paste("tran_", list_models2[m],"_year_esmFixclim1", sep="") )
corr_mrsos_tran_pres <- array(NA, dim=dim(tran_year)[1:2])
corr_mrsos_tran_fut <- array(NA, dim=dim(tran_year)[1:2])
for (i in 1:length(lon)){
for (j in 1:length(lat)){
if (is.na(mrsos_year[i,j,1])==F) {
corr_mrsos_tran_pres[i,j] <- cor(tran_year[i,j,1:30], mrsos_year[i,j,1:30], use="complete.obs")
corr_mrsos_tran_fut[i,j] <- cor(tran_year[i,j,111:140], mrsos_year[i,j,111:140], use="complete.obs") } }}
assign(paste("corr_mrsos_tran_", list_models2[m],"_year_pres_esmFixclim1", sep=""), corr_mrsos_tran_pres)
assign(paste("corr_mrsos_tran_", list_models2[m],"_year_fut_esmFixclim1", sep=""), corr_mrsos_tran_fut)
}
 
#### esmFdbk1
for (m in 1:length(list_models2)) {
print(list_models[m])
lat <- get(paste("lat_", list_models2[m], sep=""))
lon <- get(paste("lon_", list_models2[m], sep=""))
mrsos_year <- get(paste("mrsos_", list_models2[m],"_year_esmFdbk1", sep="") )
tran_year <- get(paste("tran_", list_models2[m],"_year_esmFdbk1", sep="") )
corr_mrsos_tran_pres <- array(NA, dim=dim(tran_year)[1:2])
corr_mrsos_tran_fut <- array(NA, dim=dim(tran_year)[1:2])
for (i in 1:length(lon)){
for (j in 1:length(lat)){
if (is.na(mrsos_year[i,j,1])==F) {
corr_mrsos_tran_pres[i,j] <- cor(tran_year[i,j,1:30], mrsos_year[i,j,1:30], use="complete.obs")
corr_mrsos_tran_fut[i,j] <- cor(tran_year[i,j,111:140], mrsos_year[i,j,111:140], use="complete.obs") } }}
assign(paste("corr_mrsos_tran_", list_models2[m],"_year_pres_esmFdbk1", sep=""), corr_mrsos_tran_pres)
assign(paste("corr_mrsos_tran_", list_models2[m],"_year_fut_esmFdbk1", sep=""), corr_mrsos_tran_fut)
}

#### 1pctCO2
for (m in 1:length(list_models2)) {
print(list_models[m])
lat <- get(paste("lat_", list_models2[m], sep=""))
lon <- get(paste("lon_", list_models2[m], sep=""))
mrsos_year <- get(paste("mrsos_", list_models2[m],"_year_1pctCO2", sep="") )
tran_year <- get(paste("tran_", list_models2[m],"_year_1pctCO2", sep="") )
corr_mrsos_tran_pres <- array(NA, dim=dim(tran_year)[1:2])
corr_mrsos_tran_fut <- array(NA, dim=dim(tran_year)[1:2])
for (i in 1:length(lon)){
for (j in 1:length(lat)){
if (is.na(mrsos_year[i,j,1])==F) {
corr_mrsos_tran_pres[i,j] <- cor(tran_year[i,j,1:30], mrsos_year[i,j,1:30], use="complete.obs")
corr_mrsos_tran_fut[i,j] <- cor(tran_year[i,j,111:140], mrsos_year[i,j,111:140], use="complete.obs") } }}
assign(paste("corr_mrsos_tran_", list_models2[m],"_year_pres_1pctCO2", sep=""), corr_mrsos_tran_pres)
assign(paste("corr_mrsos_tran_", list_models2[m],"_year_fut_1pctCO2", sep=""), corr_mrsos_tran_fut)
}


################################################################################
################################################################################
################################################################################
### Let's create 2x2 correlations:

### pres_1pctCO2
for (m in 1:length(list_models)){       #models
print(list_models2[m])
lat <- get(paste("lat_", list_models2[m], sep=""))
lon <- get(paste("lon_", list_models2[m], sep=""))
corr_mrsos_tran_year_pres_1pctCO2 <- get( paste("corr_mrsos_tran_",list_models2[m],"_year_pres_1pctCO2", sep=""))
corr_mrsos_tran_year_pres_1pctCO2[which(is.na(corr_mrsos_tran_year_pres_1pctCO2)==T)] <- 0
corr_mrsos_tran_year_pres_1pctCO2_2x2 <- bicubic.grid(lon, lat, corr_mrsos_tran_year_pres_1pctCO2, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
assign(paste("corr_mrsos_tran_",list_models2[m],"_year_pres_1pctCO2_2x2", sep=""), corr_mrsos_tran_year_pres_1pctCO2_2x2)
rm(lon); rm(lat); rm(corr_mrsos_tran_year_pres_1pctCO2); rm(corr_mrsos_tran_year_pres_1pctCO2_2x2)  }

### fut_1pctCO2
for (m in 1:length(list_models)){       #models
print(list_models2[m])
lat <- get(paste("lat_", list_models2[m], sep=""))
lon <- get(paste("lon_", list_models2[m], sep=""))
corr_mrsos_tran_year_fut_1pctCO2 <- get( paste("corr_mrsos_tran_",list_models2[m],"_year_fut_1pctCO2", sep=""))
corr_mrsos_tran_year_fut_1pctCO2[which(is.na(corr_mrsos_tran_year_fut_1pctCO2)==T)] <- 0
corr_mrsos_tran_year_fut_1pctCO2_2x2 <- bicubic.grid(lon, lat, corr_mrsos_tran_year_fut_1pctCO2, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
assign(paste("corr_mrsos_tran_",list_models2[m],"_year_fut_1pctCO2_2x2", sep=""), corr_mrsos_tran_year_fut_1pctCO2_2x2)
rm(lon); rm(lat); rm(corr_mrsos_tran_year_fut_1pctCO2); rm(corr_mrsos_tran_year_fut_1pctCO2_2x2)  }




### pres_esmFdbk1
for (m in 1:length(list_models)){       #models
print(list_models2[m])
lat <- get(paste("lat_", list_models2[m], sep=""))
lon <- get(paste("lon_", list_models2[m], sep=""))
corr_mrsos_tran_year_pres_esmFdbk1 <- get( paste("corr_mrsos_tran_",list_models2[m],"_year_pres_esmFdbk1", sep=""))
corr_mrsos_tran_year_pres_esmFdbk1[which(is.na(corr_mrsos_tran_year_pres_esmFdbk1)==T)] <- 0
corr_mrsos_tran_year_pres_esmFdbk1_2x2 <- bicubic.grid(lon, lat, corr_mrsos_tran_year_pres_esmFdbk1, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
assign(paste("corr_mrsos_tran_",list_models2[m],"_year_pres_esmFdbk1_2x2", sep=""), corr_mrsos_tran_year_pres_esmFdbk1_2x2)
rm(lon); rm(lat); rm(corr_mrsos_tran_year_pres_esmFdbk1); rm(corr_mrsos_tran_year_pres_esmFdbk1_2x2)  }

### fut_esmFdbk1
for (m in 1:length(list_models)){       #models
print(list_models2[m])
lat <- get(paste("lat_", list_models2[m], sep=""))
lon <- get(paste("lon_", list_models2[m], sep=""))
corr_mrsos_tran_year_fut_esmFdbk1 <- get( paste("corr_mrsos_tran_",list_models2[m],"_year_fut_esmFdbk1", sep=""))
corr_mrsos_tran_year_fut_esmFdbk1[which(is.na(corr_mrsos_tran_year_fut_esmFdbk1)==T)] <- 0
corr_mrsos_tran_year_fut_esmFdbk1_2x2 <- bicubic.grid(lon, lat, corr_mrsos_tran_year_fut_esmFdbk1, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
assign(paste("corr_mrsos_tran_",list_models2[m],"_year_fut_esmFdbk1_2x2", sep=""), corr_mrsos_tran_year_fut_esmFdbk1_2x2)
rm(lon); rm(lat); rm(corr_mrsos_tran_year_fut_esmFdbk1); rm(corr_mrsos_tran_year_fut_esmFdbk1_2x2)  }




### pres_esmFixclim1
for (m in 1:length(list_models)){       #models
print(list_models2[m])
lat <- get(paste("lat_", list_models2[m], sep=""))
lon <- get(paste("lon_", list_models2[m], sep=""))
corr_mrsos_tran_year_pres_esmFixclim1 <- get( paste("corr_mrsos_tran_",list_models2[m],"_year_pres_esmFixclim1", sep=""))
corr_mrsos_tran_year_pres_esmFixclim1[which(is.na(corr_mrsos_tran_year_pres_esmFixclim1)==T)] <- 0
corr_mrsos_tran_year_pres_esmFixclim1_2x2 <- bicubic.grid(lon, lat, corr_mrsos_tran_year_pres_esmFixclim1, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
assign(paste("corr_mrsos_tran_",list_models2[m],"_year_pres_esmFixclim1_2x2", sep=""), corr_mrsos_tran_year_pres_esmFixclim1_2x2)
rm(lon); rm(lat); rm(corr_mrsos_tran_year_pres_esmFixclim1); rm(corr_mrsos_tran_year_pres_esmFixclim1_2x2)  }

### fut_esmFixclim1
for (m in 1:length(list_models)){       #models
print(list_models2[m])
lat <- get(paste("lat_", list_models2[m], sep=""))
lon <- get(paste("lon_", list_models2[m], sep=""))
corr_mrsos_tran_year_fut_esmFixclim1 <- get( paste("corr_mrsos_tran_",list_models2[m],"_year_fut_esmFixclim1", sep=""))
corr_mrsos_tran_year_fut_esmFixclim1[which(is.na(corr_mrsos_tran_year_fut_esmFixclim1)==T)] <- 0
corr_mrsos_tran_year_fut_esmFixclim1_2x2 <- bicubic.grid(lon, lat, corr_mrsos_tran_year_fut_esmFixclim1, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
assign(paste("corr_mrsos_tran_",list_models2[m],"_year_fut_esmFixclim1_2x2", sep=""), corr_mrsos_tran_year_fut_esmFixclim1_2x2)
rm(lon); rm(lat); rm(corr_mrsos_tran_year_fut_esmFixclim1); rm(corr_mrsos_tran_year_fut_esmFixclim1_2x2)  }

################################################################################
################################################################################
################################################################################
################################################################################
### Let's create 2x2 correlations, but regridding FIRST and TRAN first...
### 1pctCO2
for (m in 1:length(list_models)){       #models
print(list_models2[m])
lat <- get(paste("lat_", list_models2[m], sep=""))
lon <- get(paste("lon_", list_models2[m], sep=""))
mrsos_year <- get(paste("mrsos_",list_models2[m],"_year_1pctCO2", sep=""))
tran_year <- get(paste("tran_",list_models2[m],"_year_1pctCO2", sep=""))
mrsos_year[is.na(mrsos_year)==T] <- 0; tran_year[is.na(tran_year)==T] <- 0
mrsos_year_2x2 <- array(NA, dim=c(181,91,dim(mrsos_year)[3] ));tran_year_2x2 <- array(NA, dim=c(181,91,dim(tran_year)[3] ))
for (t in 1:dim(mrsos_year)[3] ) { print(t)
mrsos_year_2x2[,,t]<- bicubic.grid(lon, lat, mrsos_year[,,t], xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
tran_year_2x2[,,t]<- bicubic.grid(lon, lat, tran_year[,,t], xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z  }
corr_mrsos_tran_year_pres_1pctCO2_2x2 <- array(NA, dim=c(181,91))
corr_mrsos_tran_year_fut_1pctCO2_2x2 <- array(NA, dim=c(181,91))
for (i in 1:181){print(i)
for (j in 1:91){
if (is.na(mrsos_year_2x2[i,j,1])==F) {
corr_mrsos_tran_year_pres_1pctCO2_2x2[i,j] <- cor(tran_year_2x2[i,j,1:30], mrsos_year_2x2[i,j,1:30], use="complete.obs")
corr_mrsos_tran_year_fut_1pctCO2_2x2[i,j] <- cor(tran_year_2x2[i,j,111:140], mrsos_year_2x2[i,j,111:140], use="complete.obs") }}}
assign(paste("corr_mrsos_tran_",list_models2[m],"_year_pres_1pctCO2_2x2", sep=""), corr_mrsos_tran_year_pres_1pctCO2_2x2)
assign(paste("corr_mrsos_tran_",list_models2[m],"_year_fut_1pctCO2_2x2", sep=""), corr_mrsos_tran_year_fut_1pctCO2_2x2)
rm(lon); rm(lat); rm(corr_mrsos_tran_year_fut_1pctCO2_2x2); rm(corr_mrsos_tran_year_pres_1pctCO2_2x2)  }

### esmFdbk1
for (m in 1:length(list_models)){       #models
print(list_models2[m])
lat <- get(paste("lat_", list_models2[m], sep=""))
lon <- get(paste("lon_", list_models2[m], sep=""))
mrsos_year <- get(paste("mrsos_",list_models2[m],"_year_esmFdbk1", sep=""))
tran_year <- get(paste("tran_",list_models2[m],"_year_esmFdbk1", sep=""))
mrsos_year[is.na(mrsos_year)==T] <- 0; tran_year[is.na(tran_year)==T] <- 0
mrsos_year_2x2 <- array(NA, dim=c(181,91,dim(mrsos_year)[3] ));tran_year_2x2 <- array(NA, dim=c(181,91,dim(tran_year)[3] ))
for (t in 1:dim(mrsos_year)[3] ) { print(t)
mrsos_year_2x2[,,t]<- bicubic.grid(lon, lat, mrsos_year[,,t], xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
tran_year_2x2[,,t]<- bicubic.grid(lon, lat, tran_year[,,t], xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z  }
corr_mrsos_tran_year_pres_esmFdbk1_2x2 <- array(NA, dim=c(181,91))
corr_mrsos_tran_year_fut_esmFdbk1_2x2 <- array(NA, dim=c(181,91))
for (i in 1:181){print(i)
for (j in 1:91){
if (is.na(mrsos_year_2x2[i,j,1])==F) {
corr_mrsos_tran_year_pres_esmFdbk1_2x2[i,j] <- cor(tran_year_2x2[i,j,1:30], mrsos_year_2x2[i,j,1:30], use="complete.obs")
corr_mrsos_tran_year_fut_esmFdbk1_2x2[i,j] <- cor(tran_year_2x2[i,j,111:140], mrsos_year_2x2[i,j,111:140], use="complete.obs") }}}
assign(paste("corr_mrsos_tran_",list_models2[m],"_year_pres_esmFdbk1_2x2", sep=""), corr_mrsos_tran_year_pres_esmFdbk1_2x2)
assign(paste("corr_mrsos_tran_",list_models2[m],"_year_fut_esmFdbk1_2x2", sep=""), corr_mrsos_tran_year_fut_esmFdbk1_2x2)
rm(lon); rm(lat); rm(corr_mrsos_tran_year_fut_esmFdbk1_2x2); rm(corr_mrsos_tran_year_pres_esmFdbk1_2x2)  }

### esmFixclim1
for (m in 1:length(list_models)){       #models
print(list_models2[m])
lat <- get(paste("lat_", list_models2[m], sep=""))
lon <- get(paste("lon_", list_models2[m], sep=""))
mrsos_year <- get(paste("mrsos_",list_models2[m],"_year_esmFixclim1", sep=""))
tran_year <- get(paste("tran_",list_models2[m],"_year_esmFixclim1", sep=""))
mrsos_year[is.na(mrsos_year)==T] <- 0; tran_year[is.na(tran_year)==T] <- 0
mrsos_year_2x2 <- array(NA, dim=c(181,91,dim(mrsos_year)[3] ));tran_year_2x2 <- array(NA, dim=c(181,91,dim(tran_year)[3] ))
for (t in 1:dim(mrsos_year)[3] ) { print(t)
mrsos_year_2x2[,,t]<- bicubic.grid(lon, lat, mrsos_year[,,t], xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
tran_year_2x2[,,t]<- bicubic.grid(lon, lat, tran_year[,,t], xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z  }
corr_mrsos_tran_year_pres_esmFixclim1_2x2 <- array(NA, dim=c(181,91))
corr_mrsos_tran_year_fut_esmFixclim1_2x2 <- array(NA, dim=c(181,91))
for (i in 1:181){print(i)
for (j in 1:91){
if (is.na(mrsos_year_2x2[i,j,1])==F) {
corr_mrsos_tran_year_pres_esmFixclim1_2x2[i,j] <- cor(tran_year_2x2[i,j,1:30], mrsos_year_2x2[i,j,1:30], use="complete.obs")
corr_mrsos_tran_year_fut_esmFixclim1_2x2[i,j] <- cor(tran_year_2x2[i,j,111:140], mrsos_year_2x2[i,j,111:140], use="complete.obs") }}}
assign(paste("corr_mrsos_tran_",list_models2[m],"_year_pres_esmFixclim1_2x2", sep=""), corr_mrsos_tran_year_pres_esmFixclim1_2x2)
assign(paste("corr_mrsos_tran_",list_models2[m],"_year_fut_esmFixclim1_2x2", sep=""), corr_mrsos_tran_year_fut_esmFixclim1_2x2)
rm(lon); rm(lat); rm(corr_mrsos_tran_year_fut_esmFixclim1_2x2); rm(corr_mrsos_tran_year_pres_esmFixclim1_2x2)  }



########################################################################
#### Let's put everything in a matrix:
########## 1pctCO2
corr_mrsos_tran_2x2_allmodels_year_pres_1pctCO2 <- array(NA, dim=c(181,91,length(list_models)))
for (m in 1:length(list_models)){
print(list_models[m])
corr_mrsos_tran_year_pres_1pctCO2_2x2 <- get(paste("corr_mrsos_tran_",list_models2[m],"_year_pres_1pctCO2_2x2", sep=""))
corr_mrsos_tran_2x2_allmodels_year_pres_1pctCO2[,,m] <- corr_mrsos_tran_year_pres_1pctCO2_2x2
rm(corr_mrsos_tran_year_pres_1pctCO2_2x2)}

corr_mrsos_tran_2x2_allmodels_year_fut_1pctCO2 <- array(NA, dim=c(181,91,length(list_models)))
for (m in 1:length(list_models)){
print(list_models[m])
corr_mrsos_tran_year_fut_1pctCO2_2x2 <- get(paste("corr_mrsos_tran_",list_models2[m],"_year_fut_1pctCO2_2x2", sep=""))
corr_mrsos_tran_2x2_allmodels_year_fut_1pctCO2[,,m] <- corr_mrsos_tran_year_fut_1pctCO2_2x2
rm(corr_mrsos_tran_year_fut_1pctCO2_2x2)}

####### esmFdbk1
corr_mrsos_tran_2x2_allmodels_year_pres_esmFdbk1 <- array(NA, dim=c(181,91,length(list_models)))
for (m in 1:length(list_models)){
print(list_models[m])
corr_mrsos_tran_year_pres_esmFdbk1_2x2 <- get(paste("corr_mrsos_tran_",list_models2[m],"_year_pres_esmFdbk1_2x2", sep=""))
corr_mrsos_tran_2x2_allmodels_year_pres_esmFdbk1[,,m] <- corr_mrsos_tran_year_pres_esmFdbk1_2x2
rm(corr_mrsos_tran_year_pres_esmFdbk1_2x2)}

corr_mrsos_tran_2x2_allmodels_year_fut_esmFdbk1 <- array(NA, dim=c(181,91,length(list_models)))
for (m in 1:length(list_models)){
print(list_models[m])
corr_mrsos_tran_year_fut_esmFdbk1_2x2 <- get(paste("corr_mrsos_tran_",list_models2[m],"_year_fut_esmFdbk1_2x2", sep=""))
corr_mrsos_tran_2x2_allmodels_year_fut_esmFdbk1[,,m] <- corr_mrsos_tran_year_fut_esmFdbk1_2x2
rm(corr_mrsos_tran_year_fut_esmFdbk1_2x2)}

####### esmFixclim1
corr_mrsos_tran_2x2_allmodels_year_pres_esmFixclim1 <- array(NA, dim=c(181,91,length(list_models)))
for (m in 1:length(list_models)){
print(list_models[m])
corr_mrsos_tran_year_pres_esmFixclim1_2x2 <- get(paste("corr_mrsos_tran_",list_models2[m],"_year_pres_esmFixclim1_2x2", sep=""))
corr_mrsos_tran_2x2_allmodels_year_pres_esmFixclim1[,,m] <- corr_mrsos_tran_year_pres_esmFixclim1_2x2
rm(corr_mrsos_tran_year_pres_esmFixclim1_2x2)}

corr_mrsos_tran_2x2_allmodels_year_fut_esmFixclim1 <- array(NA, dim=c(181,91,length(list_models)))
for (m in 1:length(list_models)){
print(list_models[m])
corr_mrsos_tran_year_fut_esmFixclim1_2x2 <- get(paste("corr_mrsos_tran_",list_models2[m],"_year_fut_esmFixclim1_2x2", sep=""))
corr_mrsos_tran_2x2_allmodels_year_fut_esmFixclim1[,,m] <- corr_mrsos_tran_year_fut_esmFixclim1_2x2
rm(corr_mrsos_tran_year_fut_esmFixclim1_2x2)}


