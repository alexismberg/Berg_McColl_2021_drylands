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

## we need those:
#mean_mrsos_year_2x2_allmodels
#mean_mrsos_year_rcp85_2x2_allmodels
#list_models_mrsos


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

## FIxing IPSL's SM?
#mean_mrsos_year_2x2_allmodels[,,22:24] <- mean_mrsos_year_2x2_allmodels[,,22:24] *2
#mean_mrsos_year_rcp85_2x2_allmodels[,,22:24] <- mean_mrsos_year_rcp85_2x2_allmodels[,,22:24] *2

mean_mrsos <- mask_2x2_NAs*apply(mean_mrsos_year_2x2_allmodels[,,which(list_models_mrsos %in% list_models_common_SMstress)],c(1,2), mean,na.rm=T)
mean_mrsos_fut <- mask_2x2_NAs*apply(mean_mrsos_year_rcp85_2x2_allmodels[,,which(list_models_mrsos %in% list_models_common_SMstress)],c(1,2), mean,na.rm=T)
mean_mrsos_change <- mean_mrsos_fut - mean_mrsos



mean_mrsos_corr <- mean_mrsos
## Fixing mean soil moisture: problematic coastal pixels
for (i in 1:length(lon)){ #print(i)
for (j in 77:91){
if (    (is.na(mean_mrsos[i,j])==F) && (mean_mrsos[i,j] <18)) {mean_mrsos_corr[i,j] <- NA }}}
for (i in 2:(length(lon)-1)){# print(i)
for (j in 2:(length(lat)-1)){
if (is.na(mean_mrsos[i,j])==F){
if ((mean_mrsos[i,j] <20) &&  (length(which(is.na(mean_mrsos[(i-1):(i+1),(j-1):(j+1)])==T)) ==8))  {mean_mrsos_corr[i,j] <- NA }}}}
for (i in 2:(length(lon)-1)){ #print(i)
for (j in 2:(length(lat)-1)){
if (is.na(mean_mrsos[i,j])==F){
if ((mean_mrsos[i,j] <20) &&  (length(which(is.na( mean_mrsos[(i-1):(i+1),(j-1):(j+1)] )==T)) >= 3) &&
(length(which(is.na( mean_mrsos[(i-1):(i+1),(j-1):(j+1)] )==T)) < 8) &&
(mean( c(mean_mrsos[(i-1):(i+1),j+1], mean_mrsos[(i-1):(i+1),j-1],mean_mrsos[i-1,j],mean_mrsos[i+1,j]),na.rm=T)> mean_mrsos[i,j]+2)) {mean_mrsos_corr[i,j] <- NA }
}}}
bib <- mean_mrsos_corr[47:84,41:51]
bib[which(bib < 19)] <- NA
mean_mrsos_corr[47:84,41:51] <- bib
mean_mrsos<- mean_mrsos_corr
mean_mrsos_change <- mean_mrsos_fut - mean_mrsos
mean_mrsos_fut[is.na(mean_mrsos)==T] <- NA

mean_pr <- 86400*apply( mean_pr_year_pres_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1, 2), mean, na.rm=T)
mean_pr_fut <- 86400*apply( mean_pr_year_fut_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1, 2), mean, na.rm=T)
mean_prchange <- 86400*apply(mean_pr_year_change_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1,2), mean,na.rm=T)


##### Figure 1
lat <- mask_2x2$y
lon <-  mask_2x2$x
lowlat <- min(which(lat > -60))
highlat <- min(which(lat > 80))
pdf("FigureS9_revisedR2.pdf",  width=13, height=13) 
layout(matrix(1:6, 3, 2, byrow=T));par(mar=c(3,4,3,5))
#### a
image.plot(lon-180, lat[lowlat:highlat], mean_mrsos[ c( (length(lon)/2+1):length(lon),1:(length(lon)/2)),lowlat:highlat],  zlim=c(0,50),
col=col_custom(25)[25:1], breaks=seq(0,50,by=2), xlab="", ylab="",axis.args=list(cex.axis=1.5), cex.main=1.9,main="Mean MRSOS and drylands")
bob <- mean_PrPET; bob[bob >4 ] <- 4;  bob[is.na(bob)==T] <- 1000
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
bob[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(0.65),col="darkred", drawlabels=F, lwd=1.5, add=T)
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
mean_mrsos[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(15),drawlabels=F, col="black", lwd=1.5, add=T)
map(add=T,interior=F)
mtext("a", side=3, adj=0, line=1, cex=1.5, font=2)
legend(50, -35, lwd=1.5, col=c("black","darkred"), c("MRSOS<15", "AI<0.65"), bty="n", cex=1.6)
#### b
par(mar=c(4,4.5,3,2))
plot(mean_PrPET, mean_mrsos, xlim=c(0,5), ylim=c(0,40), cex.axis=1.5, xlab="", ylab="",cex.lab=1.5,cex.main=1.9,main="MRSOS vs AI", pch="."); 
mtext(side=1, line=2.2, cex=1.5, "AI")
mtext(side=2, line=2.2, cex=1.5, "MRSOS")
abline(v=0.05, col="gold");abline(v=0.2, col="orange");abline(v=0.5, col="red");
abline(v=0.65, col="darkred")
abline(h=15, lty=2)
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
image.plot(lon-180, lat[lowlat:highlat], mean_mrsos_change[ c( (length(lon)/2+1):length(lon),
1:(length(lon)/2)),lowlat:highlat],  zlim=c(-5,5),
col=col_custom(20)[20:1], breaks=seq(-5,5,by=0.5), xlab="", ylab="",axis.args=list(cex.axis=1.5), cex.main=1.9,
main=expression(bold(paste(Delta,"MRSOS and Drylands changes"))))
increase_drylands <- array(0, dim=dim(mean_PrPET))
increase_drylands[which( (mean_mrsos >= 15) & (mean_mrsos_fut< 15))] <- 1.1
increase_drylands[which( (mean_mrsos < 15) & (mean_mrsos_fut>= 15))] <- -1.1
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
increase_drylands[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(-0.1, 0.1),
col=c("blue","red"), lwd=1.7, add=T, drawlabels=F); map(add=T,interior=F);mtext("d", side=3, adj=0, line=1, cex=1.5, font=2)
legend("bottomright", lwd=1.5, col=c("blue","red"), c("drylands contraction", "drylands expansion"), bty="n", cex=1.6)
frac_drylands_EI_pres_MMM <- sum((areacella_2x2*mask_2x2_NAs)[which( mean_mrsos < 15)], na.rm=T)/sum((areacella_2x2*mask_2x2_NAs)[which( is.na(mean_mrsos)==F)], na.rm=T)*100
frac_drylands_EI_fut_MMM<- sum((areacella_2x2*mask_2x2_NAs)[which( mean_mrsos_fut < 15)], na.rm=T)/sum((areacella_2x2*mask_2x2_NAs)[which(is.na(mean_mrsos_fut)==F)],na.rm=T)*100
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
mrsos <- mean_mrsos_year_2x2_allmodels[,,which(list_models_mrsos %in% list_models_common_SMstress)][,,m]
mrsos_fut <- mean_mrsos_year_rcp85_2x2_allmodels[,,which(list_models_mrsos %in% list_models_common_SMstress)][,,m]
print(list_models_common_SMstress[m])
mrsos[which(is.na(mean_mrsos)==T)] <- NA; mrsos_fut[which(is.na(mean_mrsos_fut)==T)] <- NA;
print(mean(mrsos, na.rm=T))
print(median(mrsos, na.rm=T))

#if (m %in% 8:10) {
#mrsos <- 2* mrsos
#mrsos_fut <- 2*mrsos_fut }


### Removing bad pixels:
mrsos[which(is.na(mean_mrsos)==T)] <- NA; mrsos_fut[which(is.na(mean_mrsos_fut)==T)] <- NA;
##### Fraction of drylands defined based on AI:
pres<- sum((mask_2x2_NAs*areacella_2x2)[which(mean_PrPET <= 0.65)], na.rm=T)
fut<- sum((mask_2x2_NAs*areacella_2x2)[which(mean_PrPET_fut <=  0.65 )], na.rm=T)
frac_drylands_AI_pres_allmodels[m] <- pres/tot*100
frac_drylands_AI_fut_allmodels[m]  <- fut/tot*100
##### Fraction of drylands defined based on Ecohydro:
mean_EI_pres <- mrsos
mean_EI_fut<- mrsos_fut
mean_EI_pres[is.na(mean_PrPET)==T] <- NA
mean_EI_fut[is.na(mean_PrPET)==T] <- NA
## Fractions
pres <- sum((mask_2x2_NAs*areacella_2x2)[which(mean_EI_pres < 15 )], na.rm=T)
fut <- sum((mask_2x2_NAs*areacella_2x2)[which (mean_EI_fut < 15  )], na.rm=T)
frac_drylands_EI_pres_allmodels[m]  <- pres/tot*100
frac_drylands_EI_fut_allmodels[m]  <- fut/tot*100
rm(mean_PrPET); rm(mean_lai); rm(mean_PrPET_fut); rm(mean_lai_fut); rm(pres); rm(fut); rm(corr_mrsos_tran_year_fut) }



#### AI-based
boxplot(frac_drylands_AI_pres_allmodels, col="orange", xlim=c(0,5), ylab="Fraction of global land area (%)", ylim=c(0, 100),
cex.lab=1.5, cex.axis=1.3, range=0, at=1.2)
#points(mean(frac_drylands_AI_pres_allmodels), pch=21,lwd=3, col="orange");
axis(1, labels=c("AI-based", "MRSOS-based"), at=c(1.5, 3.5), tick=T, cex.axis=1.5)
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








