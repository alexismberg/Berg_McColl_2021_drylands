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
mean_mrsos <- mask_2x2_NAs*apply(mean_mrsos_year_2x2_allmodels[,,which(list_models_mrsos %in% list_models_common_SMstress)],c(1,2), mean,na.rm=T)
mean_mrsos_fut <- mask_2x2_NAs*apply(mean_mrsos_year_rcp85_2x2_allmodels[,,which(list_models_mrsos %in% list_models_common_SMstress)],c(1,2), mean,na.rm=T)
mean_mrsos_change <- mean_mrsos_fut - mean_mrsos

mean_mrsos_corr <- mean_mrsos
## Fixing mean soil moisture: problematix coastal pixels
for (i in 1:length(lon)){ #print(i)
for (j in 77:91){
if (    (is.na(mean_mrsos[i,j])==F) && (mean_mrsos[i,j] <18)) {mean_mrsos_corr[i,j] <- NA }}}
for (i in 2:(length(lon)-1)){ #print(i)
for (j in 2:(length(lat)-1)){
if (is.na(mean_mrsos[i,j])==F){
if ((mean_mrsos[i,j] <20) &&  (length(which(is.na(mean_mrsos[(i-1):(i+1),(j-1):(j+1)])==T)) ==8))  {mean_mrsos_corr[i,j] <- NA }}}}
for (i in 2:(length(lon)-1)){# print(i)
for (j in 2:(length(lat)-1)){
if (is.na(mean_mrsos[i,j])==F){
if ((mean_mrsos[i,j] <20) &&  (length(which(is.na( mean_mrsos[(i-1):(i+1),(j-1):(j+1)] )==T)) >= 3) && 
(length(which(is.na( mean_mrsos[(i-1):(i+1),(j-1):(j+1)] )==T)) < 8) &&
(mean( c(mean_mrsos[(i-1):(i+1),j+1], mean_mrsos[(i-1):(i+1),j-1],mean_mrsos[i-1,j],mean_mrsos[i+1,j]),na.rm=T)> mean_mrsos[i,j]+2)) {mean_mrsos_corr[i,j] <- NA }}}}

bib <- mean_mrsos_corr[47:84,41:51]
bib[which(bib < 19)] <- NA
mean_mrsos_corr[47:84,41:51] <- bib
mean_mrsos<- mean_mrsos_corr
mean_mrsos_change <- mean_mrsos_fut - mean_mrsos
mean_mrsos_fut[is.na(mean_mrsos)==T]<- NA

mean_pr <- 86400*apply( mean_pr_year_pres_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1, 2), mean, na.rm=T)
mean_pr_fut <- 86400*apply( mean_pr_year_fut_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1, 2), mean, na.rm=T)
mean_prchange <- 86400*apply(mean_pr_year_change_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1,2), mean,na.rm=T)

### Threshold-based definition of Ei-drylands (see SI Text):
mean_index_drylands_pres <- array(NA, dim=dim(mean_lai))
for (i in 1:length(lon)){
for (j in 1:length(lat)){
if (is.na(mean_mrsos[i,j])==F) {
if ((mean_mrsos[i,j] < 16) && (mean_lai[i,j] < 2))  {mean_index_drylands_pres[i,j] <- (mean_lai[i,j]-2) + (mean_mrsos[i,j]-16)/10  }
if ((mean_mrsos[i,j] > 16) && (mean_lai[i,j] > 2))  {mean_index_drylands_pres[i,j] <- (mean_lai[i,j]-2) + (mean_mrsos[i,j]-16)/10  }
if ((mean_mrsos[i,j] > 16) && (mean_lai[i,j] < 2))  {mean_index_drylands_pres[i,j] <- max(0.001, mean_lai[i,j]-2 + (mean_mrsos[i,j]-16)/10)}
if ((mean_mrsos[i,j] < 16) && (mean_lai[i,j] > 2))  {mean_index_drylands_pres[i,j] <- max(0.001, mean_lai[i,j]-2 + (mean_mrsos[i,j]-16)/10)} 
}}}
mean_index_drylands_fut <- array(NA, dim=dim(mean_lai))
for (i in 1:length(lon)){
for (j in 1:length(lat)){
if (is.na(mean_mrsos_fut[i,j])==F) {
#if ((mean_mrsos_fut[i,j] < 16) && (mean_lai_fut[i,j] < 2))  {mean_index_drylands_fut[i,j] <- -5}
if ((mean_mrsos_fut[i,j] < 16) && (mean_lai_fut[i,j] < 2))  {mean_index_drylands_fut[i,j] <- (mean_lai_fut[i,j]-2) + (mean_mrsos_fut[i,j]-16)/10  }
if ((mean_mrsos_fut[i,j] > 16) && (mean_lai_fut[i,j] > 2))  {mean_index_drylands_fut[i,j] <- (mean_lai_fut[i,j]-2) + (mean_mrsos_fut[i,j]-16)/10  }
if ((mean_mrsos_fut[i,j] > 16) && (mean_lai_fut[i,j] < 2))  {mean_index_drylands_fut[i,j] <- max(0.001, mean_lai_fut[i,j]-2 + (mean_mrsos_fut[i,j]-16)/10)}
if ((mean_mrsos_fut[i,j] < 16) && (mean_lai_fut[i,j] > 2))  {mean_index_drylands_fut[i,j] <- max(0.001, mean_lai_fut[i,j]-2 + (mean_mrsos_fut[i,j]-16)/10)}
}}}   }



##Global land fractions of AI and EI* drylands: 
buff<-sum((mask_2x2_NAs*areacella_2x2)[which(mean_PrPET < 0.65)], na.rm=T)/ sum((mask_2x2_NAs*areacella_2x2), na.rm=T)* 100
print(paste("Frac drylands AI pres=",buff))
frac_drylands_AI_pres_MMM <- buff

buff<-sum((mask_2x2_NAs*areacella_2x2)[which(mean_PrPET_fut < 0.65)], na.rm=T)/ sum((mask_2x2_NAs*areacella_2x2), na.rm=T)* 100
print(paste("Frac drylands AI fut=",buff))
frac_drylands_AI_fut_MMM <- buff

buff<-sum((mask_2x2_NAs*areacella_2x2)[which(mean_index_drylands_pres < 0)], na.rm=T)/ sum((mask_2x2_NAs*areacella_2x2), na.rm=T)* 100
print(paste("Frac drylands EI pres=",buff))
frac_drylands_EI_pres_MMM <- buff

buff<-sum((mask_2x2_NAs*areacella_2x2)[which(mean_index_drylands_fut < 0)], na.rm=T)/ sum((mask_2x2_NAs*areacella_2x2), na.rm=T)* 100
print(paste("Frac drylands EI fut=",buff))
frac_drylands_EI_fut_MMM <- buff

### Global land means
weighted.mean(mean_index_drylands_pres, mask_2x2_NAs*areacella_2x2, na.rm=T)
weighted.mean(mean_index_drylands_fut, mask_2x2_NAs*areacella_2x2, na.rm=T)
weighted.mean(mean_PrPET, mask_2x2_NAs*areacella_2x2, na.rm=T)
weighted.mean(mean_PrPET_fut, mask_2x2_NAs*areacella_2x2, na.rm=T)

##################################################################################
##################################################################################
##################################################################################
##### Figure 1
lat <- mask_2x2$y; lon <-  mask_2x2$x
lowlat <- min(which(lat > -60)); highlat <- min(which(lat > 80))
pdf("FigureS11_revisedR2.pdf",  width=12, height=13)
layout(matrix(1:8, 4, 2, byrow=T));par(mar=c(2,3,3,4.5))
#### a
bob <- mean_PrPET; bob[bob >4 ] <- 4;
image.plot(lon-180, lat[lowlat:highlat], bob[ c( (length(lon)/2+1):length(lon),
1:(length(lon)/2)),lowlat:highlat],  zlim=c(0,4),col=brewer.pal(8, "YlGnBu"), breaks=seq(0,4,by=0.5),
xlab="", ylab="", xaxt="n", yaxt="n", axis.args=list(cex.axis=1.5), cex.main=1.9,main="Mean AI and drylands");map(add=T,interior=F)
bob[is.na(bob)==T] <- 1000
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
bob[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(0.05, 0.2, 0.5, 0.65),
 col=c("gold", "orange", "red", "darkred"), lwd=1, add=T, drawlabels=F); map(add=T,interior=F)
legend("bottomleft", col=c("gold", "orange", "red", "darkred"), c("AI=0.05", "AI=0.2", "AI=0.5", "AI=0.65"), bty="n", lwd=1.2, cex=1.6)
mtext("a", side=3, adj=0, line=1, cex=1.5, font=2)
#### b
image.plot(lon-180, lat[lowlat:highlat], mean_lai[ c( (length(lon)/2+1):length(lon),
1:(length(lon)/2)),lowlat:highlat],  zlim=c(0,8),
col=brewer.pal(8, "YlGnBu"), breaks=seq(0,8,by=1), xlab="", ylab="",xaxt="n", yaxt="n",
axis.args=list(cex.axis=1.5), cex.main=1.9,main="Mean LAI and drylands")
bob <- mean_PrPET; bob[bob >4 ] <- 4;  bob[is.na(bob)==T] <- 1000
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
bob[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(0.65),
 col="darkred", lwd=1, add=T, drawlabels=F); map(add=T,interior=F); mtext("b", side=3, adj=0, line=1, cex=1.5, font=2)
legend(-50,-40, ncol=1, col=c("darkred"), c("Drylands (AI=0.65)"), bty="n", lwd=1.2, cex=1.7)
#### c
image.plot(lon-180, lat[lowlat:highlat], mean_mrsos[ c( (length(lon)/2+1):length(lon),
1:(length(lon)/2)),lowlat:highlat],  zlim=c(-1,1),
col=col_custom(20)[20:1], breaks=seq(0,40,by=2), xlab="", ylab="",xaxt="n", yaxt="n",
axis.args=list(cex.axis=1.5), cex.main=1.9,main="Mean MRSOS and drylands")
bob <- mean_PrPET; bob[bob >4 ] <- 4;  bob[is.na(bob)==T] <- 1000
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
bob[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(0.65),
 col="darkred", lwd=1, add=T, drawlabels=F); map(add=T,interior=F); mtext("c", side=3, adj=0, line=1, cex=1.5, font=2)
legend(-50, -40, ncol=1, col=c("darkred"), c("Drylands, AI=0.65"), bty="n", lwd=1.2, cex=1.7)
#### d
par(mar=c(4,6.5,3,1))
plot(mean_PrPET, mean_lai, xlim=c(0,5), ylim=c(0,16),yaxt="n", cex.axis=1.5, xlab="", ylab="",cex.lab=1.5,cex.main=1.9,main="", pch=".");
mtext(side=1, line=2.2, cex=1.5, "AI", at=2.5)
mtext(side=2, line=3, cex=1.4, "LAI", at=3)
mtext(side=2, line=3, cex=1.4, "MRSOS", at=12)
axis(side=2, at=c(1:16),   labels=F) #     cex.axis=1.3, labels=c(1:7, c(0, 5,10,15, 20,25, 30, 35, 40)))
axis(side=2, at=c(1:8,10,12,14), cex.axis=1.4, labels=c(1:8, c(10, 20,35)), las=1)
abline(h=8, lwd=2); #abline(h=12, col="gray")
lines(type="p", mean_PrPET, mean_mrsos/40*8+8, pch=".")
abline(v=0.65, col="darkred");mtext("d", side=3, adj=0, line=1, cex=1.5, font=2)
abline(v=0.05, col="gold");abline(v=0.2, col="orange");abline(v=0.5, col="red");
legend("topright", ncol=1, col=c("gold", "orange", "red", "black"), c("AI=0.05", "AI=0.2", "AI=0.5", "AI=0.65"), bty="n", lwd=1.2, cex=1.5)
#### e: Binned plot, AI=f(LAI, Mrsos)
par(mar=c(4,4.5,3,5))
lat <- mask_2x2$y;lon <-  mask_2x2$x
lowlat <- min(which(lat > -60));highlat <- min(which(lat > 80)); mask <- mask_2x2_NAs
#bin_mrsos <- seq(0,50, length.out=21)
#bin_LAI <- seq(0,8, by=0.5)
PrPET_year_binned_acrossmodels <- array(NA, dim=c(length(bin_mrsos),length(bin_LAI), 4000))
w <- array(0, dim=c(length(bin_mrsos),length(bin_LAI)))
for (i in 1:length(lon)){ #print(i)
for (j in 1:length(lat)){
coord_x <- NULL; coord_y <- NULL
for (t in 1:(length(bin_mrsos)-1)) {
if ( (is.na(mean_mrsos[i,j]*mask[i,j]) == F) && (bin_mrsos[t] <  mean_mrsos[i,j]) &&
(mean_mrsos[i,j] < bin_mrsos[t+1]))  {coord_x <- t} }
for (p in 1:(length(bin_LAI)-1)) {
if ( (is.na(mask[i,j] * mean_lai[i,j] ) ==F) && (bin_LAI[p] < mean_lai[i,j] ) && ( mean_lai[i,j] < bin_LAI[p+1]))  {coord_y <- p} }
w[coord_x, coord_y] <- w[coord_x, coord_y]+1
PrPET_year_binned_acrossmodels[coord_x, coord_y, w[coord_x, coord_y]] <- mean_PrPET[i,j]}}
mean_PrPET_year_binned_acrossmodels <- apply(PrPET_year_binned_acrossmodels, c(1,2), mean, na.rm=T)
w_PrPET_year_binned_Pr_PET_acrossmodels  <- w
bill <-  mean_PrPET_year_binned_acrossmodels
bill[bill <= -5] <- -5; bill[bill >= 5] <- 5;
image.plot(bin_mrsos, bin_LAI , bill, zlim=c(0,5), breaks=seq(0, 5, by=0.1), ylim=range(bin_LAI), xlim=range(bin_mrsos),
main="AI",cex.main=1.9, col=col_custom3(50)[50:1], xlab="MRSOS", 
ylab="LAI", cex.axis=1.5, cex.lab=1.9,axis.args=list( cex.axis=1.5))
contour( bin_mrsos,bin_LAI, bill,levels=c(0.05, 0.2, 0.5, 0.65), col=c("gold", "orange", "red", "black"), lwd=2, add=T, drawlabels=F)
legend("topright", ncol=1, col=c("gold", "orange", "red", "black"), c("AI=0.05", "AI=0.2", "AI=0.5", "AI=0.65"), bty="n", lwd=1.2, cex=1.5)
abline(v=0)
mtext("e", side=3, adj=0, line=1, cex=1.5, font=2)
if (log_index==1) { curve(   (x<16)*2 + (x>16)*(2-10000*x), add=T, col="black", lwd=2, lty=2)} else
if (log_index==0) { curve(  a_MMM*(x) + b_MMM, add=T, col="black", lwd=2, lty=2)}

#### f
par(mar=c(2,3,3,4.5))
image.plot(lon-180, lat[lowlat:highlat], mean_index_drylands_pres[ c( (length(lon)/2+1):length(lon),
1:(length(lon)/2)),lowlat:highlat],  zlim=c(-10,10),
col=col_custom(20)[20:1], breaks=seq(-10,10,by=1), xlab="", ylab="",xaxt="n", yaxt="n",
axis.args=list(cex.axis=1.5), cex.main=1.9,main="Mean EI* and drylands")
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
mean_index_drylands_pres[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(0),
 col="blue", lwd=1, add=T, drawlabels=F); map(add=T,interior=F)
bob <- mean_PrPET; bob[bob >4 ] <- 4;  bob[is.na(bob)==T] <- 1000
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
bob[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(0.65),
 col="red", lwd=1, add=T, drawlabels=F); map(add=T,interior=F)
legend(-50,-30, ncol=1, col=c("blue", "red"), c("Drylands (EI*=0)", "Drylands (AI=0.65)"), bty="n", lwd=1.2, cex=1.7)
mtext("f", side=3, adj=0, line=1, cex=1.5, font=2)

##### g : Changes in the new index:
bob <- mean_index_drylands_fut - mean_index_drylands_pres;bob[bob >1.5 ] <- 1.5; bob[bob < -1.5] <- -1.5
image.plot(lon-180, lat[lowlat:highlat], bob[ c( (length(lon)/2+1):length(lon),1:(length(lon)/2)),lowlat:highlat],  zlim=c(-1.5,1.5),
col=col_custom(30)[30:1], breaks=seq(-1.5,1.5,by=0.1), xaxt="n", yaxt="n", xlab="", ylab="",axis.args=list(cex.axis=1.5), cex.main=1.9,
main=expression(bold(paste(Delta,"EI*"))))
increase_drylands <- array(0, dim=dim(mean_PrPET))
increase_drylands[which( (  mean_index_drylands_pres >= 0) & (mean_index_drylands_fut < 0))] <- 1 #expansion
increase_drylands[which( (  mean_index_drylands_pres < 0) & (mean_index_drylands_fut >= 0))] <- -1 #contraction
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
increase_drylands[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(-0.3, 0.3),
col=c("blue","red"), lwd=1.5, add=T, drawlabels=F);
map(add=T,interior=F)
mtext("g", side=3, adj=0, line=1, cex=1.5, font=2)
legend(-50,-28,lwd=1.5, col=c("blue","red"), c("drylands contraction", "drylands expansion"), bty="n", cex=1.5)
#points(coord_sign_EI_year_change_2x2,pch=4, col="black", cex=.01)

##### h: Boxplots:
##### For the barplot, or boxplot:
frac_drylands_AI_pres_allmodels <- NULL
frac_drylands_AI_fut_allmodels <- NULL
frac_drylands_EI_pres_allmodels <- NULL
frac_drylands_EI_fut_allmodels <- NULL
tot<- sum((mask_2x2_NAs*areacella_2x2), na.rm=T)
for (m in 1:length(list_models_common_SMstress)) {
mean_PrPET <-  mean_PrPET_year_pres_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)][,,m]
mean_lai <- mean_lai_year_2x2_allmodels[,,which(list_models_lai %in% list_models_common_SMstress)][,,m]
mean_PrPET_fut <-  mean_PrPET_year_fut_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)][,,m]
mean_lai_fut <-  mean_lai_year_fut_2x2_allmodels[,,which(list_models_lai_fut %in% list_models_common_SMstress)][,,m]
mrsos <- mean_mrsos_year_2x2_allmodels[,,which(list_models_mrsos %in% list_models_common_SMstress)][,,m]
mrsos_fut <- mean_mrsos_year_rcp85_2x2_allmodels[,,which(list_models_mrsos %in% list_models_common_SMstress)][,,m]
### Removing bad pixels:
mrsos[which(is.na(mean_mrsos)==T)] <- NA; mrsos_fut[which(is.na(mean_mrsos_fut)==T)] <- NA;

##### Fraction of drylands defined based on AI:
pres<- sum((mask_2x2_NAs*areacella_2x2)[which(mean_PrPET <= 0.65)], na.rm=T)
fut<- sum((mask_2x2_NAs*areacella_2x2)[which(mean_PrPET_fut <=  0.65 )], na.rm=T)
frac_drylands_AI_pres_allmodels[m] <- pres/tot*100
frac_drylands_AI_fut_allmodels[m]  <- fut/tot*100
##### Fraction of drylands defined based on Ecohydro:
mean_EI_pres <-array(NA, dim=dim(mean_PrPET)); mean_EI_fut<-array(NA, dim=dim(mean_PrPET));
for (i in 1:length(lon)){
for (j in 1:length(lat)){
if (is.na(mrsos[i,j])==F) {
if ((mrsos[i,j] < 16) && (mean_lai[i,j] < 2))  {mean_EI_pres[i,j] <- (mean_lai[i,j]-2) + (mrsos[i,j]-16)/10  }
if ((mrsos[i,j] > 16) && (mean_lai[i,j] > 2))  {mean_EI_pres[i,j] <- (mean_lai[i,j]-2) + (mrsos[i,j]-16)/10  }
if ((mrsos[i,j] > 16) && (mean_lai[i,j] < 2))  {mean_EI_pres[i,j] <- max(0.001, mean_lai[i,j]-2 + (mrsos[i,j]-16)/10)}
if ((mrsos[i,j] < 16) && (mean_lai[i,j] > 2))  {mean_EI_pres[i,j] <- max(0.001, mean_lai[i,j]-2 + (mrsos[i,j]-16)/10)}  }
if (is.na(mrsos_fut[i,j])==F) {
if ((mrsos_fut[i,j] < 16) && (mean_lai_fut[i,j] < 2))  {mean_EI_fut[i,j] <- (mean_lai_fut[i,j]-2) + (mrsos_fut[i,j]-16)/10  }
if ((mrsos_fut[i,j] > 16) && (mean_lai_fut[i,j] > 2))  {mean_EI_fut[i,j] <- (mean_lai_fut[i,j]-2) + (mrsos_fut[i,j]-16)/10  }
if ((mrsos_fut[i,j] > 16) && (mean_lai_fut[i,j] < 2))  {mean_EI_fut[i,j] <- max(0.001, mean_lai_fut[i,j]-2 + (mrsos_fut[i,j]-16)/10)}
if ((mrsos_fut[i,j] < 16) && (mean_lai_fut[i,j] > 2))  {mean_EI_fut[i,j] <- max(0.001, mean_lai_fut[i,j]-2 + (mrsos_fut[i,j]-16)/10)}  }
}}
mean_EI_pres[is.na(mean_PrPET)==T] <- NA
mean_EI_fut[is.na(mean_PrPET)==T] <- NA
## Fractions
pres <- sum((mask_2x2_NAs*areacella_2x2)[which(mean_EI_pres < 0 )], na.rm=T)
fut <- sum((mask_2x2_NAs*areacella_2x2)[which (mean_EI_fut < 0  )], na.rm=T)
frac_drylands_EI_pres_allmodels[m]  <- pres/tot*100
frac_drylands_EI_fut_allmodels[m]  <- fut/tot*100
rm(mean_PrPET); rm(mean_lai); rm(mean_PrPET_fut); rm(mean_lai_fut); rm(pres); rm(fut); rm(corr_mrsos_tran_year_fut); rm(corr_mrsos_tran_year) }

#### AI-based
boxplot(frac_drylands_AI_pres_allmodels, col="orange", xlim=c(0,5), ylab="Fraction of global land area (%)", ylim=c(0, 70),
cex.lab=1.5, cex.axis=1.3, range=0, at=1.2)
#points(mean(frac_drylands_AI_pres_allmodels), pch=21,lwd=3, col="orange");
axis(1, labels=c("AI-based", "EI*-based"), at=c(1.5, 3.5), tick=T, cex.axis=1.5)
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
mtext("h", side=3, adj=0, line=1, cex=1.5, font=2)
dev.off()



print(c(mean(frac_drylands_AI_pres_allmodels), mean(frac_drylands_AI_fut_allmodels),
mean(frac_drylands_EI_pres_allmodels), mean(frac_drylands_EI_fut_allmodels)))


print(c(median(frac_drylands_AI_pres_allmodels), median(frac_drylands_AI_fut_allmodels),
median(frac_drylands_EI_pres_allmodels), median(frac_drylands_EI_fut_allmodels)))

print( c(mean(frac_drylands_AI_pres_MMM), mean(frac_drylands_AI_fut_MMM),
mean(frac_drylands_EI_pres_MMM), mean(frac_drylands_EI_fut_MMM)))


