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
mean_PrPET <- apply( mean_PrPETpmrc_co2_year_pres_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1, 2), mean, na.rm=T)
mean_PrPET_fut <- apply( mean_PrPETpmrc_co2_year_fut_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1, 2), mean, na.rm=T)
mean_PrPETchange <- apply(mean_PrPETpmrc_year_co2_change_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1,2), mean,
na.rm=T)

##### Figure 2
lat <- mask_2x2$y; lon <-  mask_2x2$x
lowlat <- min(which(lat > -60)) ; highlat <- min(which(lat > 80))
pdf("FigureS7_new_withEI.pdf",  width=14, height=8)
layout(matrix(1:4, 2, 2, byrow=T)); par(mar=c(0,1,2,4), omi=c(0,0,0,0.4))
#### a: Change in AI and drylands expansion
par(mar=c(3,4,3,5.1))
bob <- mean_PrPETchange; bob[bob >1] <- 1; bob[bob < -1] <- -1
image.plot(lon-180, lat[lowlat:highlat], bob[ c( (length(lon)/2+1):length(lon),1:(length(lon)/2)),lowlat:highlat],  zlim=c(-1,1),
col=col_custom(40)[40:1], breaks=seq(-1,1,by=0.05), xaxt="n", yaxt="n", xlab="", ylab="",axis.args=list(cex.axis=1.5), cex.main=1.9,
main=expression(bold(paste(Delta,"AI"))))
increase_drylands <- array(0, dim=dim(mean_PrPET))
increase_drylands[which( (mean_PrPET >= 0.65) & (mean_PrPET_fut<0.65))] <- 1
increase_drylands[which( (mean_PrPET < 0.65) & (mean_PrPET_fut> 0.65))] <- -1
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
increase_drylands[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(-0.3,0.3),
 col=c("blue","red"), lwd=1.5, add=T, drawlabels=F);map(add=T,interior=F)
mtext("a", side=3, adj=0, line=1, cex=1.5, font=2)
legend(-50,-28, lwd=1.5, col=c("blue","red"), c("drylands contraction","drylands expansion"), bty="n", cex=1.5)

#### b: Change in EI 
bob <- mean_index_drylands_fut - mean_index_drylands_pres;bob[bob >3 ] <- 3; bob[bob < -3] <- -3
image.plot(lon-180, lat[lowlat:highlat], bob[ c( (length(lon)/2+1):length(lon),1:(length(lon)/2)),lowlat:highlat],  zlim=c(-3,3),
col=col_custom(20)[20:1], breaks=seq(-3,3,by=0.3), xaxt="n", yaxt="n", xlab="", ylab="",axis.args=list(cex.axis=1.5), cex.main=1.9,
main=expression(bold(paste(Delta,"EI"))))
increase_drylands <- array(0, dim=dim(mean_PrPET))
increase_drylands[which( (  mean_index_drylands_pres > 0) & (mean_index_drylands_fut < 0))] <- 1 #expansion
increase_drylands[which( (  mean_index_drylands_pres < 0) & (mean_index_drylands_fut > 0))] <- -1 #contraction
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
increase_drylands[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(-0.3, 0.3),
col=c("blue","red"), lwd=1.5, add=T, drawlabels=F);
map(add=T,interior=F)
mtext("b", side=3, adj=0, line=1, cex=1.5, font=2)
legend(-50,-28,lwd=1.5, col=c("blue","red"), c("drylands contraction", "drylands expansion"), bty="n", cex=1.5)

dev.off()





