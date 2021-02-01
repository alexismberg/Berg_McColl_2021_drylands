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

mean_PrPET <- apply( mean_PrPET_year_pres_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1, 2), mean, na.rm=T)
mean_lai <- apply(mean_lai_year_2x2_allmodels[,,which(list_models_lai %in% list_models_common_SMstress)], c(1, 2), mean, na.rm=T)
mean_lai_change <- apply(mean_lai_year_change_2x2_allmodels[,,which(list_models_lai_fut %in% list_models_common_SMstress)], c(1, 2), mean, na.rm=T)
mean_PrPET_fut <- apply( mean_PrPET_year_fut_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1, 2), mean, na.rm=T)
mean_prchange <- apply(mean_pr_year_change_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1,2), mean, na.rm=T)
mean_PrPETchange <- apply(mean_PrPET_year_change_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1,2), mean,
na.rm=T)
mean_lai_year_fut_2x2_allmodels <- mean_lai_year_change_2x2_allmodels + mean_lai_year_2x2_allmodels[,,which(list_models_lai %in%
list_models_lai_fut)]
mean_lai_fut <- apply( mean_lai_year_fut_2x2_allmodels[,,which(list_models_lai_fut %in% list_models_common_SMstress)], c(1, 2), mean, na.rm=T)
mean_corr_mrsos_tran <- apply(corr_mrsos_tran_year_2x2_allmodels[,,which(list_models_tran_and_mrsos_PCMDI_fut %in% list_models_common_SMstress)],
c(1,2), mean,na.rm=T)
mean_corr_mrsos_tran_fut <- apply(corr_mrsos_tran_year_rcp85_2x2_allmodels[,,
which(list_models_tran_and_mrsos_PCMDI_fut %in% list_models_common_SMstress)],c(1,2), mean,na.rm=T)
mean_corr_mrsos_tran_change <- mean_corr_mrsos_tran_fut - mean_corr_mrsos_tran

mean_pr <- 86400*apply( mean_pr_year_pres_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1, 2), mean, na.rm=T)
mean_pr_fut <- 86400*apply( mean_pr_year_fut_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1, 2), mean, na.rm=T)
mean_prchange <- 86400*apply(mean_pr_year_change_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1,2), mean,na.rm=T)


a <- a_MMM_withpr
b <- b_MMM_withpr
if (log_index==1) {
mean_index_drylands_pres <- array(NA, dim=dim(mean_pr))
for (i in 1:length(lon)){
for (j in 1:length(lat)){
if ( (is.na(mean_corr_mrsos_tran[i,j])==F) && (mean_corr_mrsos_tran[i,j] < 0)){
mean_index_drylands_pres[i,j] <- mean_pr[i,j]  }  ##-  (a*log(mean_corr_mrsos_tran[i,j])+b) }
else if ((is.na(mean_corr_mrsos_tran[i,j])==F) && (mean_corr_mrsos_tran[i,j] > 0)) {
mean_index_drylands_pres[i,j]  <-  mean_pr[i,j] -  (a*log(mean_corr_mrsos_tran[i,j])+b) } }}
mean_index_drylands_fut <- array(NA, dim=dim(mean_pr))
for (i in 1:length(lon)){
for (j in 1:length(lat)){
if ( (is.na(mean_corr_mrsos_tran_fut[i,j])==F) && (mean_corr_mrsos_tran_fut[i,j] < 0)){
mean_index_drylands_fut[i,j] <- mean_pr_fut[i,j] } ##-  (a*log(mean_corr_mrsos_tran[i,j])+b)}
else if ((is.na(mean_corr_mrsos_tran_fut[i,j])==F) && (mean_corr_mrsos_tran_fut[i,j] > 0)) {
mean_index_drylands_fut[i,j]  <-  mean_pr_fut[i,j] -  (a*log(mean_corr_mrsos_tran_fut[i,j])+b) } }}
} else if (log_index==0) {
mean_index_drylands_pres <- array(NA, dim=dim(mean_pr))
for (i in 1:length(lon)){
for (j in 1:length(lat)){
if ( (is.na(mean_corr_mrsos_tran[i,j])==F) ){
mean_index_drylands_pres[i,j] <- mean_pr[i,j]   - (a*(mean_corr_mrsos_tran[i,j])+b) }}}
mean_index_drylands_fut <- array(NA, dim=dim(mean_pr))
for (i in 1:length(lon)){
for (j in 1:length(lat)){
if ( (is.na(mean_corr_mrsos_tran_fut[i,j])==F)  ) {
mean_index_drylands_fut[i,j] <- mean_pr_fut[i,j]  - (a*(mean_corr_mrsos_tran_fut[i,j])+b)} }}
}



############ We need to count models that agree on sign of change
### AI
coord_sign_PrPET_year_change_2x2 <- NULL
for (i in 1:181){
for (j in 1:91){
bob <- length(which(sign(mean_PrPET_year_change_2x2_allmodels[i,j, which(list_models_common_fut %in% list_models_common_SMstress)  ])==1))
bill  <- length(which(sign(mean_PrPET_year_change_2x2_allmodels[i,j, which(list_models_common_fut %in% list_models_common_SMstress)  ])==-1))
if ((bob > 12*0.74) || (bill > 12*0.74) ){ coord_sign_PrPET_year_change_2x2 <- rbind(coord_sign_PrPET_year_change_2x2,c(lon[i],lat[j])) }
}}
for (i in 1:dim(coord_sign_PrPET_year_change_2x2)[1]){
if  (coord_sign_PrPET_year_change_2x2[i,1] > 180)  { coord_sign_PrPET_year_change_2x2[i,1]<-coord_sign_PrPET_year_change_2x2[i,1]-360  }}

### pr
coord_sign_pr_year_change_2x2 <- NULL
for (i in 1:181){
for (j in 1:91){
bob <- length(which(sign(mean_pr_year_change_2x2_allmodels[i,j, which(list_models_common_fut %in% list_models_common_SMstress)  ])==1))
bill  <- length(which(sign(mean_pr_year_change_2x2_allmodels[i,j, which(list_models_common_fut %in% list_models_common_SMstress)  ])==-1))
if ((bob > 12*0.74) || (bill > 12*0.74) ){ coord_sign_pr_year_change_2x2 <- rbind(coord_sign_pr_year_change_2x2,c(lon[i],lat[j])) }
}}
for (i in 1:dim(coord_sign_pr_year_change_2x2)[1]){
if  (coord_sign_pr_year_change_2x2[i,1] > 180)  { coord_sign_pr_year_change_2x2[i,1]<-coord_sign_pr_year_change_2x2[i,1]-360  }}

### Cor(SM,Tran)
corr_mrsos_tran_year_change_2x2_allmodels <- corr_mrsos_tran_year_rcp85_2x2_allmodels - corr_mrsos_tran_year_2x2_allmodels
coord_sign_corr_mrsos_tran_year_change_2x2 <- NULL
for (i in 1:181){
for (j in 1:91){
bob <- length(which(sign(corr_mrsos_tran_year_change_2x2_allmodels[i,j, which(list_models_tran_and_mrsos_PCMDI_fut %in% list_models_common_SMstress)  ])==1))
bill  <- length(which(sign(corr_mrsos_tran_year_change_2x2_allmodels[i,j, which(list_models_tran_and_mrsos_PCMDI_fut %in% list_models_common_SMstress)  ])==-1))
if ((bob > 12*0.74) || (bill > 12*0.74) ){ coord_sign_corr_mrsos_tran_year_change_2x2 <- rbind(coord_sign_corr_mrsos_tran_year_change_2x2,c(lon[i],lat[j])) }
}}
for (i in 1:dim(coord_sign_corr_mrsos_tran_year_change_2x2)[1]){
if  (coord_sign_corr_mrsos_tran_year_change_2x2[i,1] > 180)  { coord_sign_corr_mrsos_tran_year_change_2x2[i,1]<-
 coord_sign_corr_mrsos_tran_year_change_2x2[i,1]-360  }}

###### EIwithpr
## We need to make map of EIwithpr change per model... which we haven't made yet:
mean_EIwithpr_year_pres_2x2_allmodels <- array(NA, dim=dim(mean_PrPET_year_pres_2x2_allmodels))
mean_EIwithpr_year_fut_2x2_allmodels <- array(NA, dim=dim(mean_PrPET_year_pres_2x2_allmodels))
a <- a_MMM_withpr
b <- b_MMM_withpr
for (m in 1:length(list_models_common_SMstress)) {
mean_pr <- 86400*mean_pr_year_pres_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)][,,m]
mean_pr_fut <-  86400*mean_pr_year_fut_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)][,,m]
corr_mrsos_tran_year <- corr_mrsos_tran_year_2x2_allmodels[,,which(list_models_tran_and_mrsos_PCMDI_fut %in% list_models_common_SMstress)][,,m]
corr_mrsos_tran_year_fut <- corr_mrsos_tran_year_rcp85_2x2_allmodels[,,which(list_models_tran_and_mrsos_PCMDI_fut %in% list_models_common_SMstress)][,,m]
###  Maps for all models
mean_EIwithpr_year_pres_2x2_allmodels[,,m] <- mean_pr - (a*corr_mrsos_tran_year+b)
mean_EIwithpr_year_fut_2x2_allmodels[,,m] <- mean_pr_fut - (a*corr_mrsos_tran_year_fut+b)   }
mean_EIwithpr_year_change_2x2_allmodels <- mean_EIwithpr_year_fut_2x2_allmodels - mean_EIwithpr_year_pres_2x2_allmodels
coord_sign_EIwithpr_year_change_2x2 <- NULL
for (i in 1:181){
for (j in 1:91){
bob <- length(which(sign(mean_EIwithpr_year_change_2x2_allmodels[i,j, ])==1))
bill  <- length(which(sign(mean_EIwithpr_year_change_2x2_allmodels[i,j, ])==-1))
if ((bob > 12*0.74) || (bill > 12*0.74) ){ coord_sign_EIwithpr_year_change_2x2 <- rbind(coord_sign_EIwithpr_year_change_2x2,c(lon[i],lat[j])) }
}}
for (i in 1:dim(coord_sign_EIwithpr_year_change_2x2)[1]){
if  (coord_sign_EIwithpr_year_change_2x2[i,1] > 180)  { coord_sign_EIwithpr_year_change_2x2[i,1]<-coord_sign_EIwithpr_year_change_2x2[i,1]-360  }}



##### Figure 2
lat <- mask_2x2$y; lon <-  mask_2x2$x
lowlat <- min(which(lat > -60)) ; highlat <- min(which(lat > 80))
pdf("FigureS14.pdf",  width=14, height=8)
layout(matrix(1:4, 2, 2, byrow=T));par(mar=c(0,1,2,4), omi=c(0,0,0,0.4))
#### a: Change in AI and drylands expansion
par(mar=c(3,4,3,5.1))
bob <- mean_PrPETchange; bob[bob >1 ] <- 1; bob[bob < -1] <- -1
image.plot(lon-180-1, lat[lowlat:highlat], bob[ c( (length(lon)/2+1):length(lon),1:(length(lon)/2)),lowlat:highlat],  zlim=c(-1,1),
col=col_custom(40)[40:1], breaks=seq(-1,1,by=0.05), xaxt="n", yaxt="n", xlab="", ylab="",axis.args=list(cex.axis=1.5), cex.main=1.9,
main=expression(bold(paste(Delta,"AI"))))
increase_drylands <- array(0, dim=dim(mean_PrPET))
increase_drylands[which( (mean_PrPET >= 0.65) & (mean_PrPET_fut<0.65))] <- 1
increase_drylands[which( (mean_PrPET < 0.65) & (mean_PrPET_fut> 0.65))] <- -1
contour(seq(-181,179, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
increase_drylands[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(-0.3,0.3),
 col=c("blue","red"), lwd=1.5, add=T, drawlabels=F);map(add=T,interior=F)
mtext("a", side=3, adj=0, line=1, cex=1.5, font=2)
legend(-50,-28, lwd=1.5, col=c("blue","red"), c("drylands contraction","drylands expansion"), bty="n", cex=1.5)
points(coord_sign_PrPET_year_change_2x2,pch=4, col="black", cex=.01)

#### b: Change in EIwithpr 
bob <- mean_index_drylands_fut - mean_index_drylands_pres;bob[bob >3 ] <- 3; bob[bob < -3] <- -3
image.plot(lon-180, lat[lowlat:highlat], bob[ c( (length(lon)/2+1):length(lon),1:(length(lon)/2)),lowlat:highlat],  zlim=c(-3,3),
col=col_custom(20)[20:1], breaks=seq(-3,3,by=0.3), xaxt="n", yaxt="n", xlab="", ylab="",axis.args=list(cex.axis=1.5), cex.main=1.9,
main=expression(bold(paste(Delta,"EI'"))))
increase_drylands <- array(0, dim=dim(mean_PrPET))
increase_drylands[which( (  mean_index_drylands_pres > 0) & (mean_index_drylands_fut < 0))] <- 1 #expansion
increase_drylands[which( (  mean_index_drylands_pres < 0) & (mean_index_drylands_fut > 0))] <- -1 #contraction
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
increase_drylands[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(-0.3, 0.3),
col=c("blue","red"), lwd=1.5, add=T, drawlabels=F);
map(add=T,interior=F)
mtext("b", side=3, adj=0, line=1, cex=1.5, font=2)
legend(-50,-28,lwd=1.5, col=c("blue","red"), c("drylands contraction", "drylands expansion"), bty="n", cex=1.5)
points(coord_sign_EIwithpr_year_change_2x2,pch=4, col="black", cex=.01)

sum((mask_2x2_NAs*areacella_2x2)[which(  mean_index_drylands_pres < 0)], na.rm=T)/sum(mask_2x2_NAs*areacella_2x2, na.rm=T)
sum((mask_2x2_NAs*areacella_2x2)[which(  mean_index_drylands_fut < 0)], na.rm=T)/sum(mask_2x2_NAs*areacella_2x2, na.rm=T)
sum((mask_2x2_NAs*areacella_2x2)[which(  (  mean_index_drylands_pres < 0) & (mean_index_drylands_fut > 0))], na.rm=T)/sum(mask_2x2_NAs*areacella_2x2, na.rm=T)
sum((mask_2x2_NAs*areacella_2x2)[which(  (  mean_index_drylands_pres > 0) & (mean_index_drylands_fut < 0))], na.rm=T)/sum(mask_2x2_NAs*areacella_2x2, na.rm=T)

#### c
bob <- mean_prchange; bob[which(bob > 1.5)] <- 1.5
image.plot(lon-180, lat[lowlat:highlat], bob[ c( (length(lon)/2+1):length(lon),
1:(length(lon)/2)),lowlat:highlat],  zlim=c(-1.5,1.5),
col=col_custom(30)[30:1], breaks=seq(-1.5,1.5,by=0.1), xaxt="n", yaxt="n", xlab="", ylab="",axis.args=list(cex.axis=1.5), cex.main=1.9, main=expression(bold(paste(Delta,"Pr"))))
increase_drylands <- array(0, dim=dim(mean_PrPET))
increase_drylands[which( (mean_PrPET >= 0.65) & (mean_PrPET_fut<0.65))] <- 1
increase_drylands[which( (mean_PrPET < 0.65) & (mean_PrPET_fut> 0.65))] <- -1
#contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
#increase_drylands[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(-0.3,0.3),
# col=c("blue","red"), lwd=1.5, add=T, drawlabels=F);
map(add=T,interior=F)
#legend(-40,-30, lwd=1.5, col=c("blue","red"), c("drylands contraction","drylands expansion"), bty="n", cex=1.6)
mtext("c", side=3, adj=0, line=1, cex=1.5, font=2)
points(coord_sign_pr_year_change_2x2,pch=4, col="black", cex=.01)


#### d
image.plot(lon-180, lat[lowlat:highlat], mean_corr_mrsos_tran_change[ c( (length(lon)/2+1):length(lon),
1:(length(lon)/2)),lowlat:highlat],  zlim=c(-0.5,0.5),
col=col_custom(20), breaks=seq(-0.5,0.5,by=0.05), xaxt="n", yaxt="n", xlab="", ylab="",axis.args=list(cex.axis=1.5), cex.main=1.9,
main=expression(bold(paste(Delta,"cor(SM,Tran)"))))
increase_drylands <- array(0, dim=dim(mean_PrPET))
increase_drylands[which( (mean_PrPET >= 0.65) & (mean_PrPET_fut<0.65))] <- 1
increase_drylands[which( (mean_PrPET < 0.65) & (mean_PrPET_fut> 0.65))] <- -1
#contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
#increase_drylands[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(-0.3,0.3),
# col=c("blue","red"), lwd=1.5, add=T, drawlabels=F);
map(add=T,interior=F)
#legend(-40,-30, lwd=1.5, col=c("blue","red"), c("drylands contraction","drylands expansion"), bty="n", cex=1.6)
mtext("d", side=3, adj=0, line=1, cex=1.5, font=2)
points(coord_sign_corr_mrsos_tran_year_change_2x2,pch=4, col="black", cex=.01)
dev.off()





