#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#################  Mean MRSOS, present
for (m in (1:length(list_models_mrsos)) ){    #models
print(list_models_mrsos[m])
mrsos_year <- get(paste("mrsos_",list_models_mrsos2[m],"_year" , sep=""))
assign(paste("mean_mrsos_", list_models_mrsos2[m],"_year", sep=""), apply(mrsos_year[,,22:51], c(1,2), mean, na.rm=T))
rm(mrsos_year)}

##############################
#################  Mean MRSOS future
for (m in (1:length(list_models_mrsos)) ){    #models
print(list_models_mrsos[m])
mrsos_year_rcp85 <- get(paste("mrsos_",list_models_mrsos2[m],"_year_rcp85" , sep=""))
assign(paste("mean_mrsos_", list_models_mrsos2[m],"_year_rcp85", sep=""), apply(mrsos_year_rcp85, c(1,2), mean, na.rm=T))
rm(mrsos_year_rcp85)}

################################################################################################
################################################################################################
############ Let's put everything on a 2x2 grid so that we can compute the MMM change

for (m in 1:length(list_models_mrsos2)){     
print(list_models_mrsos2[m])
lat <- get(paste("lat_", list_models_mrsos2[m], sep=""))
lon <- get(paste("lon_", list_models_mrsos2[m], sep=""))

mean_mrsos_year <- get( paste("mean_mrsos_",list_models_mrsos2[m],"_year", sep=""))
mean_mrsos_year[which(is.na(mean_mrsos_year)==T)] <- 0
mean_mrsos_year_2x2  <- bicubic.grid(lon, lat, mean_mrsos_year, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
assign(paste("mean_mrsos_year_", list_models_mrsos2[m],"_2x2", sep=""),mean_mrsos_year_2x2 )

mean_mrsos_year_rcp85 <- get( paste("mean_mrsos_",list_models_mrsos2[m],"_year_rcp85", sep=""))
mean_mrsos_year_rcp85[which(is.na(mean_mrsos_year_rcp85)==T)] <- 0
mean_mrsos_year_rcp85_2x2  <- bicubic.grid(lon, lat, mean_mrsos_year_rcp85, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
assign(paste("mean_mrsos_year_rcp85_", list_models_mrsos2[m],"_2x2", sep=""),mean_mrsos_year_rcp85_2x2 )
}


#################### In one matrix
## Year
mean_mrsos_year_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_mrsos2)))
for (m in 1:length(list_models_mrsos2)){
print(list_models_mrsos[m])
mean_mrsos_year_2x2 <- get(paste("mean_mrsos_year_",list_models_mrsos2[m],"_2x2", sep=""))
mean_mrsos_year_2x2_allmodels[,,m] <- mean_mrsos_year_2x2
rm(mean_mrsos_year_2x2)}

mean_mrsos_year_rcp85_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_mrsos2)))
for (m in 1:length(list_models_mrsos2)){
print(list_models_mrsos[m])
mean_mrsos_year_rcp85_2x2 <- get(paste("mean_mrsos_year_rcp85_",list_models_mrsos2[m],"_2x2", sep=""))
mean_mrsos_year_rcp85_2x2_allmodels[,,m] <- mean_mrsos_year_rcp85_2x2
rm(mean_mrsos_year_rcp85_2x2)}


###Changes
mean_mrsoschange_year_rcp85_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_mrsos)))
for (m in 1:length(list_models_mrsos)){
print(list_models_mrsos[m])
mean_mrsos_year_rcp85_2x2 <- get(paste("mean_mrsos_year_rcp85_",list_models_mrsos2[m],"_2x2", sep=""))
mean_mrsos_year_2x2 <- get(paste("mean_mrsos_year_",list_models_mrsos2[m],"_2x2", sep=""))
mean_mrsoschange_year_rcp85_2x2_allmodels[,,m] <- mean_mrsos_year_rcp85_2x2 *mask_2x2_NAs - mean_mrsos_year_2x2 *mask_2x2_NAs
rm(mean_mrsos_year_rcp85_2x2)}

mean_mrsoschangerel_year_rcp85_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_mrsos)))
for (m in 1:length(list_models_mrsos)){
print(list_models_mrsos[m])
mean_mrsos_year_rcp85_2x2 <- get(paste("mean_mrsos_year_rcp85_",list_models_mrsos2[m],"_2x2", sep=""))
mean_mrsos_year_2x2 <- get(paste("mean_mrsos_year_",list_models_mrsos2[m],"_2x2", sep=""))
mean_mrsoschangerel_year_rcp85_2x2_allmodels[,,m] <- (mean_mrsos_year_rcp85_2x2 *mask_2x2_NAs - mean_mrsos_year_2x2 *mask_2x2_NAs)/(mean_mrsos_year_2x2 *mask_2x2_NAs)*100
rm(mean_mrsos_year_rcp85_2x2)}


#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#######################################################################################################################
#################  Mean MRSO, present

for (m in (1:length(list_models_mrso)) ){    #models
print(list_models_mrso[m])
lat <- get(paste("lat_", list_models_mrso2[m], sep=""))
lon <- get(paste("lon_", list_models_mrso2[m], sep=""))
mrso_year <- get(paste("mrso_",list_models_mrso2[m],"_year" , sep=""))
assign(paste("mean_mrso_", list_models_mrso2[m],"_year", sep=""), apply(mrso_year[,,22:51], c(1,2), mean, na.rm=T))
}
 
##############################
#################  Mean MRSO future
for (m in (1:length(list_models_mrso)) ){    #models
print(list_models_mrso[m])
mrso_year_rcp85 <- get(paste("mrso_",list_models_mrso2[m],"_year_rcp85" , sep=""))
assign(paste("mean_mrso_", list_models_mrso2[m],"_year_rcp85", sep=""), apply(mrso_year_rcp85, c(1,2), mean, na.rm=T))
}

################################################################################################
################################################################################################
############ Let's put everything on a 2x2 grid so that we can compute the MMM change


for (m in 1:length(list_models_mrso2)){     
print(list_models_mrso2[m])
lat <- get(paste("lat_", list_models_mrso2[m], sep=""))
lon <- get(paste("lon_", list_models_mrso2[m], sep=""))
mean_mrso_year <- get( paste("mean_mrso_",list_models_mrso2[m],"_year", sep=""))
mean_mrso_year[which(is.na(mean_mrso_year)==T)] <- 0
mean_mrso_year_2x2  <- bicubic.grid(lon, lat, mean_mrso_year, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
assign(paste("mean_mrso_year_", list_models_mrso2[m],"_2x2", sep=""),mean_mrso_year_2x2 )

mean_mrso_year_rcp85 <- get( paste("mean_mrso_",list_models_mrso2[m],"_year_rcp85", sep=""))
mean_mrso_year_rcp85[which(is.na(mean_mrso_year_rcp85)==T)] <- 0
mean_mrso_year_rcp85_2x2  <- bicubic.grid(lon, lat, mean_mrso_year_rcp85, xlim=c(0,360), ylim=c(-90,90), dx=2, dy=2 )$z
assign(paste("mean_mrso_year_rcp85_", list_models_mrso2[m],"_2x2", sep=""),mean_mrso_year_rcp85_2x2 )
}

######### One matrix
## Year
mean_mrso_year_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_mrso2)))
for (m in 1:length(list_models_mrso2)){
print(list_models_mrso[m])
mean_mrso_year_2x2 <- get(paste("mean_mrso_year_",list_models_mrso2[m],"_2x2", sep=""))
mean_mrso_year_2x2_allmodels[,,m] <- mean_mrso_year_2x2
rm(mean_mrso_year_2x2)}

mean_mrso_year_rcp85_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_mrso2)))
for (m in 1:length(list_models_mrso2)){
print(list_models_mrso[m])
mean_mrso_year_rcp85_2x2 <- get(paste("mean_mrso_year_rcp85_",list_models_mrso2[m],"_2x2", sep=""))
mean_mrso_year_rcp85_2x2_allmodels[,,m] <- mean_mrso_year_rcp85_2x2
rm(mean_mrso_year_rcp85_2x2)}


########## Changes
mean_mrsochange_year_rcp85_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_mrso)))
for (m in 1:length(list_models_mrso)){
print(list_models_mrso[m])
mean_mrso_year_rcp85_2x2 <- get(paste("mean_mrso_year_rcp85_",list_models_mrso2[m],"_2x2", sep=""))
mean_mrso_year_2x2 <- get(paste("mean_mrso_year_",list_models_mrso2[m],"_2x2", sep=""))
mean_mrsochange_year_rcp85_2x2_allmodels[,,m] <- mean_mrso_year_rcp85_2x2 *mask_2x2_NAs - mean_mrso_year_2x2 *mask_2x2_NAs
rm(mean_mrso_year_rcp85_2x2)}

mean_mrsochangerel_year_rcp85_2x2_allmodels <- array(NA, dim=c(181,91,length(list_models_mrso)))
for (m in 1:length(list_models_mrso)){
print(list_models_mrso[m])
mean_mrso_year_rcp85_2x2 <- get(paste("mean_mrso_year_rcp85_",list_models_mrso2[m],"_2x2", sep=""))
mean_mrso_year_2x2 <- get(paste("mean_mrso_year_",list_models_mrso2[m],"_2x2", sep=""))
mean_mrsochangerel_year_rcp85_2x2_allmodels[,,m] <- (mean_mrso_year_rcp85_2x2 *mask_2x2_NAs - mean_mrso_year_2x2 *mask_2x2_NAs)/(mean_mrso_year_2x2 *mask_2x2_NAs)*100
rm(mean_mrso_year_rcp85_2x2)}



