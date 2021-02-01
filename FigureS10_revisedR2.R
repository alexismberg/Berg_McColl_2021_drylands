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



#list_models_common_SMstress
# [1] "bcc-csm1-1"     "bcc-csm1-1-m"   "CanESM2"        "CESM1-CAM5"
# [5] "GFDL-ESM2G"     "GFDL-ESM2M"     "inmcm4"         "IPSL-CM5A-LR"
# [9] "IPSL-CM5A-MR"   "IPSL-CM5B-LR"   "MIROC-ESM"      "MIROC-ESM-CHEM"

##Looks like soil in inmcm4 is only 5 meters not 10... they missed the last layer when summing up mrlsl!!!
#Here, our model depths will thus be :
depths <- c(3.573087,  3.573087 ,4.05, 3.8 , 10, 10, 5, 2, 2, 2, 14, 14)

mean_mrso <- mask_2x2_NAs*apply(mean_mrso_year_2x2_allmodels[,,which(list_models_mrso %in% list_models_common_SMstress)],c(1,2), mean,na.rm=T)
mean_mrso_fut <- mask_2x2_NAs*apply(mean_mrso_year_rcp85_2x2_allmodels[,,which(list_models_mrso %in% list_models_common_SMstress)],c(1,2), mean,na.rm=T)
mean_mrso_change <- mean_mrso_fut - mean_mrso

### We normalize total-column soil moisture by soil depth:
mean_mrsonorm_year_2x2_allmodels <- array(NA, dim=c(181,91,length(which(list_models_mrso %in% list_models_common_SMstress))))
mean_mrsonorm_year_rcp85_2x2_allmodels<-  array(NA, dim=c(181,91,length(which(list_models_mrso %in% list_models_common_SMstress))))
for (m in 1:length(list_models_common_SMstress)){
mean_mrsonorm_year_2x2_allmodels[,,m] <- mean_mrso_year_2x2_allmodels[,,which(list_models_mrso %in% list_models_common_SMstress)][,,m] / depths[m]/1000
mean_mrsonorm_year_rcp85_2x2_allmodels[,,m] <- mean_mrso_year_rcp85_2x2_allmodels[,,which(list_models_mrso %in% list_models_common_SMstress)][,,m] /
 depths[m]/1000}

## Calculating the multi-mean model mean quantities with the right models:
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
mean_mrsonorm <- mask_2x2_NAs*apply(mean_mrsonorm_year_2x2_allmodels[,,],c(1,2), mean,na.rm=T)
mean_mrsonorm_fut <- mask_2x2_NAs*apply(mean_mrsonorm_year_rcp85_2x2_allmodels[,,],c(1,2), mean,na.rm=T)
mean_mrsonorm_change <- mean_mrsonorm_fut - mean_mrsonorm

### Using all models:
#mean_mrsonorm <- mask_2x2_NAs*apply(mean_mrsonorm_year_2x2_allmodels[,,],c(1,2), mean,na.rm=T)
#mean_mrsonorm_fut <- mask_2x2_NAs*apply(mean_mrsonorm_year_rcp85_2x2_allmodels[,,],c(1,2), mean,na.rm=T)
#mean_mrsonorm_change <- mean_mrsonorm_fut - mean_mrsonorm


mean_mrsonorm_corr <- mean_mrsonorm
## Fixing mean soil moisture: correcting problematix coastal pixels:
for (i in 1:length(lon)){ #print(i)
for (j in 77:91){
if (    (is.na(mean_mrsonorm[i,j])==F) && (mean_mrsonorm[i,j] <0.25)) {mean_mrsonorm_corr[i,j] <- NA }}}
for (i in 2:(length(lon)-1)){# print(i)
for (j in 2:(length(lat)-1)){
if (is.na(mean_mrsonorm[i,j])==F){
if ((mean_mrsonorm[i,j] <0.25) &&  (length(which(is.na(mean_mrsonorm[(i-1):(i+1),(j-1):(j+1)])==T)) ==8))  {mean_mrsonorm_corr[i,j] <- NA }}}}
for (i in 2:(length(lon)-1)){ #print(i)
for (j in 2:(length(lat)-1)){
if (is.na(mean_mrsonorm[i,j])==F){
if ((mean_mrsonorm[i,j] <0.25) &&  (length(which(is.na( mean_mrsonorm[(i-1):(i+1),(j-1):(j+1)] )==T)) >= 3) &&
(length(which(is.na( mean_mrsonorm[(i-1):(i+1),(j-1):(j+1)] )==T)) < 8) &&
(mean( c(mean_mrsonorm[(i-1):(i+1),j+1], mean_mrsonorm[(i-1):(i+1),j-1],mean_mrsonorm[i-1,j],mean_mrsonorm[i+1,j]),na.rm=T)> mean_mrsonorm[i,j]+2)) {mean_mrsonorm_corr[i,j] <- NA } }}}

bib <- mean_mrsonorm_corr[47:84,41:51]
bib[which(bib < 0.25)] <- NA
mean_mrsonorm_corr[47:84,41:51] <- bib
mean_mrsonorm<- mean_mrsonorm_corr
mean_mrsonorm_change <- mean_mrsonorm_fut - mean_mrsonorm
mean_mrsonorm_fut[is.na(mean_mrsonorm)==T] <- NA

mean_pr <- 86400*apply( mean_pr_year_pres_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1, 2), mean, na.rm=T)
mean_pr_fut <- 86400*apply( mean_pr_year_fut_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1, 2), mean, na.rm=T)
mean_prchange <- 86400*apply(mean_pr_year_change_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1,2), mean,na.rm=T)

##### Figure 1
lat <- mask_2x2$y
lon <-  mask_2x2$x
lowlat <- min(which(lat > -60))
highlat <- min(which(lat > 80))
pdf("FigureS10_revisedR2.pdf",  width=13, height=13) 
layout(matrix(1:6, 3, 2, byrow=T));par(mar=c(3,4,3,8))
#### a
image.plot(lon-180, lat[lowlat:highlat], mean_mrsonorm[ c( (length(lon)/2+1):length(lon),1:(length(lon)/2)),lowlat:highlat],  zlim=c(0,0.5),
col=col_custom(25)[25:1], breaks=seq(0,0.50,by=0.02), xlab="", ylab="",axis.args=list(cex.axis=1.5), cex.main=1.9,main="Mean MRSO and drylands")
bob <- mean_PrPET; bob[bob >4 ] <- 4;  bob[is.na(bob)==T] <- 1000
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
bob[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(0.65),col="darkred", drawlabels=F, lwd=1.5, add=T)
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
mean_mrsonorm[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(0.22),drawlabels=F, col="black", lwd=1.5, add=T)
map(add=T,interior=F)
mtext("a", side=3, adj=0, line=1, cex=1.5, font=2)
legend(50, -35, lwd=1.5, col=c("black","darkred"), c("MRSO<0.22", "AI<0.65"), bty="n", cex=1.6)
#### b
par(mar=c(4,4.5,3,2))
plot(mean_PrPET, mean_mrsonorm, xlim=c(0,5), ylim=c(0,0.5), cex.axis=1.5, xlab="", ylab="",cex.lab=1.5,cex.main=1.9,main="MRSO vs AI", pch="."); 
mtext(side=1, line=2.2, cex=1.5, "AI")
mtext(side=2, line=2.2, cex=1.5, "MRSO")
abline(v=0.05, col="gold");abline(v=0.2, col="orange");abline(v=0.5, col="red");
abline(v=0.65, col="darkred")
abline(h=0.22, lty=2)
mtext("b", side=3, adj=0, line=1, cex=1.5, font=2)
legend("bottomright", lty=2, "Drylands threshold", bty="n", cex=1.6)
#### c
par(mar=c(3,4,3,4))
bob <- mean_PrPETchange; bob[bob >1 ] <- 1; bob[bob < -1] <- -1
image.plot(lon-180, lat[lowlat:highlat], bob[ c( (length(lon)/2+1):length(lon),
1:(length(lon)/2)),lowlat:highlat],  zlim=c(-1,1),
col=col_custom(40)[40:1], breaks=seq(-1,1,by=0.05), xlab="", ylab="",axis.args=list(cex.axis=1.5), cex.main=1.9,
main=expression(bold(paste(Delta,"AI and Drylands changes"))))
increase_drylands <- array(0, dim=dim(mean_PrPET))
increase_drylands[which( (mean_PrPET >= 0.65) & (mean_PrPET_fut<0.65))] <- 1.1
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
increase_drylands[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(0.1),
 col="red", lwd=1.2, add=T, drawlabels=F);map(add=T,interior=F);mtext("c", side=3, adj=0, line=1, cex=1.5, font=2)
legend("bottomright", lwd=1.5, col="red", c("drylands expansion"), bty="n", cex=1.6)
#### d
image.plot(lon-180, lat[lowlat:highlat], mean_mrsonorm_change[ c( (length(lon)/2+1):length(lon),
1:(length(lon)/2)),lowlat:highlat],  zlim=c(-.08,.08), col=col_custom(20)[20:1], breaks=seq(-.08,.08,by=0.008),legend.mar=9.1,
 xlab="", ylab="",axis.args=list(cex.axis=1.5), cex.main=1.9, main=expression(bold(paste(Delta,"MRSO and Drylands changes"))))
increase_drylands <- array(0, dim=dim(mean_PrPET))
increase_drylands[which( (mean_mrsonorm >= 0.22) & (mean_mrsonorm_fut< 0.22))] <- 1.1
increase_drylands[which( (mean_mrsonorm < 0.22) & (mean_mrsonorm_fut>= 0.22))] <- -1.1
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
increase_drylands[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(-0.1, 0.1),
col=c("blue","red"), lwd=1.2, add=T, drawlabels=F); map(add=T,interior=F);mtext("d", side=3, adj=0, line=1, cex=1.5, font=2)
legend("bottomright", lwd=1.5, col=c("blue","red"), c("drylands contraction", "drylands expansion"), bty="n", cex=1.6)
frac_drylands_EI_pres_MMM <- sum((areacella_2x2*mask_2x2_NAs)[which( mean_mrsonorm < 0.22)], na.rm=T)/sum((areacella_2x2*mask_2x2_NAs)[which( is.na(mean_mrsonorm)==F)], na.rm=T)*100
frac_drylands_EI_fut_MMM <- sum((areacella_2x2*mask_2x2_NAs)[which( mean_mrsonorm_fut < 0.22)], na.rm=T)/sum((areacella_2x2*mask_2x2_NAs)[which(is.na(mean_mrsonorm_fut)==F)],na.rm=T)*100
text(-150,-30, paste("pres=",round(frac_drylands_EI_pres_MMM,2),"%",sep=""), cex=1.6, pos=4)
text(-150,-40, paste("fut=",round(frac_drylands_EI_fut_MMM,2),"%",sep=""), cex=1.6, pos=4)

###### e: boxplots
##### For the barplot, or boxplot:
frac_drylands_AI_pres_allmodels <- NULL
frac_drylands_AI_fut_allmodels <- NULL
frac_drylands_EI_pres_allmodels <- NULL
frac_drylands_EI_fut_allmodels <- NULL
tot<- sum((mask_2x2_NAs*areacella_2x2), na.rm=T)
for (m in 1:length(list_models_common_SMstress)) {
mean_PrPET <-  mean_PrPET_year_pres_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)][,,m]
mean_PrPET_fut <-  mean_PrPET_year_fut_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)][,,m]
mrsonorm <- mean_mrsonorm_year_2x2_allmodels[,,m]
mrsonorm_fut <- mean_mrsonorm_year_rcp85_2x2_allmodels[,,m]
print(list_models_common_SMstress[m])
mrsonorm[which(is.na(mean_mrsonorm)==T)] <- NA; mrsonorm_fut[which(is.na(mean_mrsonorm_fut)==T)] <- NA;
##### Fraction of drylands defined based on AI:
pres<- sum((mask_2x2_NAs*areacella_2x2)[which(mean_PrPET <= 0.65)], na.rm=T)
fut<- sum((mask_2x2_NAs*areacella_2x2)[which(mean_PrPET_fut <=  0.65 )], na.rm=T)
frac_drylands_AI_pres_allmodels[m] <- pres/tot*100
frac_drylands_AI_fut_allmodels[m]  <- fut/tot*100
##### Fraction of drylands defined based on Ecohydro:
mean_EI_pres <- mrsonorm
mean_EI_fut<- mrsonorm_fut
mean_EI_pres[is.na(mean_PrPET)==T] <- NA
mean_EI_fut[is.na(mean_PrPET)==T] <- NA
## Fractions
pres <- sum((mask_2x2_NAs*areacella_2x2)[which(mean_EI_pres < 0.22 )], na.rm=T)
fut <- sum((mask_2x2_NAs*areacella_2x2)[which (mean_EI_fut < 0.22  )], na.rm=T)
frac_drylands_EI_pres_allmodels[m]  <- pres/tot*100
frac_drylands_EI_fut_allmodels[m]  <- fut/tot*100
rm(mean_PrPET); rm(mean_lai); rm(mean_PrPET_fut); rm(mean_lai_fut); rm(pres); rm(fut); rm(corr_mrsonorm_tran_year_fut) }



#### AI-based
boxplot(frac_drylands_AI_pres_allmodels, col="orange", xlim=c(0,5), ylab="Fraction of global land area (%)", ylim=c(0, 100),
cex.lab=1.5, cex.axis=1.3, range=0, at=1.2)
#points(mean(frac_drylands_AI_pres_allmodels), pch=21,lwd=3, col="orange");
axis(1, labels=c("AI-based", "MRSO-based"), at=c(1.5, 3.5), tick=T, cex.axis=1.5)
legend("bottomleft", fill=c("orange", "red"),  c("Present", "Future"), bty="n", cex=1.5)
boxplot(frac_drylands_AI_fut_allmodels, col="red", at=1.8, add=TRUE, yaxt="n", xaxt="n", range=0);
#points(mean(frac_drylands_AI_fut_allmodels), pch=21,lwd=3, col="red")
#### Ecohydro-based
boxplot(frac_drylands_EI_pres_allmodels, col="orange", at=3.2, add=TRUE,  yaxt="n", xaxt="n",range=0)
#points( mean(frac_drylands_EI_pres_allmodels), pch=21,lwd=3, col="orange")
boxplot(frac_drylands_EI_fut_allmodels, col="red", at=3.8, add=TRUE,yaxt="n", xaxt="n", range=0);
#points(mean(frac_drylands_EI_fut_allmodels), pch=21,lwd=3, col="red", )
points(cbind(c(1.2, 1.8, 3.2, 3.8), c(mean(frac_drylands_AI_pres_allmodels), mean(frac_drylands_AI_fut_allmodels),
mean(frac_drylands_EI_pres_allmodels), mean(frac_drylands_EI_fut_allmodels))), pch=21,lwd=2, col="black")
points(cbind(c(1.2, 1.8, 3.2, 3.8), c(mean(frac_drylands_AI_pres_MMM), mean(frac_drylands_AI_fut_MMM),
mean(frac_drylands_EI_pres_MMM), mean(frac_drylands_EI_fut_MMM))), pch=8,lwd=2, col="black")
mtext("e", side=3, adj=0, line=1, cex=1.5, font=2)
dev.off()



print(c(mean(frac_drylands_AI_pres_allmodels), mean(frac_drylands_AI_fut_allmodels),
mean(frac_drylands_EI_pres_allmodels), mean(frac_drylands_EI_fut_allmodels)))


print(c(median(frac_drylands_AI_pres_allmodels), median(frac_drylands_AI_fut_allmodels),
median(frac_drylands_EI_pres_allmodels), median(frac_drylands_EI_fut_allmodels)))

print( c(mean(frac_drylands_AI_pres_MMM), mean(frac_drylands_AI_fut_MMM),
mean(frac_drylands_EI_pres_MMM), mean(frac_drylands_EI_fut_MMM)))






