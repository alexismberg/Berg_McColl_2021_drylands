#####################
#.libPaths("/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
#library(ncdf, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(colorRamps, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(maps, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(ncdf4, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(fields, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(akima, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(LSD, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")

################################################################################################################################
################################################################################################################################
################################################################################################################################
################################################################################################################################
### Here we calculate the Aridity Index in 1pctCO2, esmFdbk1 and esmFixclim1 from CMIP5
### Better run this in a separate directory from the main to avoid confusion:

## Go to get_data_PHYS_RAD.R first

mean_PrPET_pres_1pctCO2 <- mask_2x2_NAs*apply( mean_PrPET_2x2_allmodels_year_pres_1pctCO2[,,], c(1, 2), mean, na.rm=T)
mean_lai_pres_1pctCO2 <- mask_2x2_NAs*apply(mean_lai_2x2_allmodels_year_pres_1pctCO2[,,], c(1, 2), mean, na.rm=T)
mean_PrPET_fut_1pctCO2 <- mask_2x2_NAs*apply( mean_PrPET_2x2_allmodels_year_fut_1pctCO2[,,], c(1, 2), mean, na.rm=T)
mean_lai_fut_1pctCO2 <- mask_2x2_NAs*apply(mean_lai_2x2_allmodels_year_fut_1pctCO2[,,], c(1, 2), mean, na.rm=T)

mean_PrPET_pres_esmFdbk1 <- mask_2x2_NAs*apply( mean_PrPET_2x2_allmodels_year_pres_esmFdbk1[,,], c(1, 2), mean, na.rm=T)
mean_lai_pres_esmFdbk1 <- mask_2x2_NAs*apply(mean_lai_2x2_allmodels_year_pres_esmFdbk1[,,], c(1, 2), mean, na.rm=T)
mean_PrPET_fut_esmFdbk1 <- mask_2x2_NAs*apply( mean_PrPET_2x2_allmodels_year_fut_esmFdbk1[,,], c(1, 2), mean, na.rm=T)
mean_lai_fut_esmFdbk1 <- mask_2x2_NAs*apply(mean_lai_2x2_allmodels_year_fut_esmFdbk1[,,], c(1, 2), mean, na.rm=T)

mean_PrPET_pres_esmFixclim1 <- mask_2x2_NAs*apply( mean_PrPET_2x2_allmodels_year_pres_esmFixclim1[,,], c(1, 2), mean, na.rm=T)
mean_lai_pres_esmFixclim1 <- mask_2x2_NAs*apply(mean_lai_2x2_allmodels_year_pres_esmFixclim1[,,], c(1, 2), mean, na.rm=T)
mean_PrPET_fut_esmFixclim1 <- mask_2x2_NAs*apply( mean_PrPET_2x2_allmodels_year_fut_esmFixclim1[,,], c(1, 2), mean, na.rm=T)
mean_lai_fut_esmFixclim1 <- mask_2x2_NAs*apply(mean_lai_2x2_allmodels_year_fut_esmFixclim1[,,], c(1, 2), mean, na.rm=T)

mean_corrmrsostranchange_1pctCO2 <- mean_corrmrsostran_fut_1pctCO2 - mean_corrmrsostran_pres_1pctCO2
mean_corrmrsostranchange_esmFdbk1 <- mean_corrmrsostran_fut_esmFdbk1 - mean_corrmrsostran_pres_esmFdbk1
mean_corrmrsostranchange_esmFixclim1 <- mean_corrmrsostran_fut_esmFixclim1 - mean_corrmrsostran_pres_esmFixclim1

a <- a_MMM
b <- b_MMM

mean_EI_pres_1pctCO2 <- mean_lai_pres_1pctCO2 - ( a*mean_corrmrsostran_pres_1pctCO2 + b)
mean_EI_fut_1pctCO2 <- mean_lai_fut_1pctCO2 - ( a*mean_corrmrsostran_fut_1pctCO2 + b)
mean_EIchange_1pctCO2 <- mean_EI_fut_1pctCO2 - mean_EI_pres_1pctCO2

mean_EI_pres_esmFdbk1 <- mean_lai_pres_esmFdbk1 - ( a*mean_corrmrsostran_pres_esmFdbk1 + b)
mean_EI_fut_esmFdbk1 <- mean_lai_fut_esmFdbk1 - ( a*mean_corrmrsostran_fut_esmFdbk1 + b)
mean_EIchange_esmFdbk1 <- mean_EI_fut_esmFdbk1 - mean_EI_pres_esmFdbk1

mean_EI_pres_esmFixclim1 <- mean_lai_pres_esmFixclim1 - ( a*mean_corrmrsostran_pres_esmFixclim1 + b)
mean_EI_fut_esmFixclim1 <- mean_lai_fut_esmFixclim1 - ( a*mean_corrmrsostran_fut_esmFixclim1 + b)
mean_EIchange_esmFixclim1 <- mean_EI_fut_esmFixclim1 - mean_EI_pres_esmFixclim1


col_custom2=colorRampPalette(c("darkblue", "blue", "cyan", "white","orange", "red","darkred"))
pdf("Figure4_new_withEI.pdf", width=17 , height=9)
lowlat <- min(which(lat > -60))
highlat <- min(which(lat > 80))
layout(matrix(1:9,3,3, byrow=F));par(mar=c(0.5,2,2.5,7.5)); par(omi=c(0,0,0,0.3))

## AI
mean_PrPETchange_1pctCO2 <- mean_PrPET_fut_1pctCO2 - mean_PrPET_pres_1pctCO2
mean_PrPETchange_esmFdbk1 <- mean_PrPET_fut_esmFdbk1 - mean_PrPET_pres_esmFdbk1
mean_PrPETchange_esmFixclim1 <- mean_PrPET_fut_esmFixclim1 - mean_PrPET_pres_esmFixclim1
mean_PrPETchange_1pctCO2[which(mean_PrPETchange_1pctCO2 > 0.5)] <-0.5;
mean_PrPETchange_1pctCO2[which(mean_PrPETchange_1pctCO2 < -0.5)] <- -0.5;
mean_PrPETchange_esmFixclim1[which(mean_PrPETchange_esmFixclim1 > 0.5)] <-0.5
mean_PrPETchange_esmFixclim1[which(mean_PrPETchange_esmFixclim1 < -0.5)] <- -0.5;
mean_PrPETchange_esmFdbk1[which(mean_PrPETchange_esmFdbk1 > 0.5)] <-0.5;
mean_PrPETchange_esmFdbk1[which(mean_PrPETchange_esmFdbk1 < -0.5)] <- -0.5;
image.plot(lon - 180, lat[lowlat:highlat], (mean_PrPETchange_1pctCO2*mask_2x2_NAs)[ c( (length(lon)/2+1):length(lon),
1:(length(lon)/2)),lowlat:highlat],
zlim=c(-0.5,0.5), col=col_custom2(20)[20:1], main=expression(paste(Delta,"AI, CTL")),cex.main=2.5,font.main=2,axis.args=list(cex.axis=2.2), xlab="", 
ylab="",xaxt="n",yaxt="n", cex.axis=1.2) ; map(add=T, interior=F);
increase_drylands <- array(0, dim=dim(mean_PrPET_pres_1pctCO2)); increase_drylands[which( (mean_PrPET_pres_1pctCO2 >= 0.65) & (mean_PrPET_fut_1pctCO2 <
0.65))] <- 1.1
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
increase_drylands[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(0.1),
 col="red", lwd=1.5, add=T, drawlabels=F);map(add=T,interior=F)
mtext("a", side=3, adj=0, line=0.3, cex=1.5, font=2)
legend(-50,-35, lwd=1.5, col="red", c("drylands expansion"), bty="n", cex=1.8)
image.plot(lon - 180, lat[lowlat:highlat], (mean_PrPETchange_esmFdbk1*mask_2x2_NAs)[ c( (length(lon)/2+1):length(lon),1:(length(lon)/2)),
lowlat:highlat], zlim=c(-0.5,0.5), col=col_custom2(20)[20:1],
 main=expression(paste(Delta,"AI, RAD")), cex.main=2.5,font.main=2,axis.args=list(cex.axis=2.2), xlab="", 
ylab="",xaxt="n",yaxt="n", cex.axis=1.2) ; map(add=T, interior=F);
increase_drylands <- array(0, dim=dim(mean_PrPET_pres_esmFdbk1)); increase_drylands[which( (mean_PrPET_pres_esmFdbk1 >= 0.65)
&(mean_PrPET_fut_esmFdbk1 <0.65))] <- 1.1
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
increase_drylands[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(0.1),
 col="red", lwd=1.5, add=T, drawlabels=F);map(add=T,interior=F)
mtext("b", side=3, adj=0, line=0.3, cex=1.5, font=2)
legend(-50,-35, lwd=1.5, col="red", c("drylands expansion"), bty="n", cex=1.8)
image.plot(lon - 180, lat[lowlat:highlat], (mean_PrPETchange_esmFixclim1*mask_2x2_NAs)[ c( (length(lon)/2+1):length(lon),
1:(length(lon)/2)),lowlat:highlat],
zlim=c(-0.5,0.5),col=col_custom2(20)[20:1], main=expression(paste(Delta,"AI, PHYS")),cex.main=2.5,font.main=2,axis.args=list(cex.axis=2.2), xlab="", 
ylab="",xaxt="n",yaxt="n",cex.axis=1.2) ; map(add=T, interior=F);
increase_drylands <- array(0, dim=dim(mean_PrPET_pres_esmFixclim1)); increase_drylands[which( (mean_PrPET_pres_esmFixclim1 >= 0.65) &
(mean_PrPET_fut_esmFixclim1 < 0.65))] <- 1.1
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
increase_drylands[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(0.1),
 col="red", lwd=1.5, add=T, drawlabels=F);map(add=T,interior=F)
mtext("c", side=3, adj=0, line=0.3, cex=1.5, font=2)
legend(-50,-35, lwd=1.5, col="red", c("drylands expansion"), bty="n", cex=1.8)

## EI
mean_EIchange_1pctCO2[which(mean_EIchange_1pctCO2 > 3)] <-3;
mean_EIchange_1pctCO2[which(mean_EIchange_1pctCO2 < -3)] <- -3;
mean_EIchange_esmFixclim1[which(mean_EIchange_esmFixclim1 > 3)] <- 3
mean_EIchange_esmFixclim1[which(mean_EIchange_esmFixclim1 < -3)] <- -3;
mean_EIchange_esmFdbk1[which(mean_EIchange_esmFdbk1 > 3)] <- 3
mean_EIchange_esmFdbk1[which(mean_EIchange_esmFdbk1 < -3)] <- -3;
image.plot(lon - 180, lat[lowlat:highlat], (mean_EIchange_1pctCO2*mask_2x2_NAs)[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],
zlim=c(-3,3), col=col_custom2(20)[20:1], xlab="", ylab="",xaxt="n",yaxt="n",main=expression(paste(Delta,"EI, CTL")),cex.main=2.5,font.main=2,
axis.args=list(cex.axis=2.2),cex.axis=1.2); map(add=T, interior=F); 
mtext("d", side=3, adj=0, line=0.3, cex=1.5, font=2)
image.plot(lon - 180, lat[lowlat:highlat], (mean_EIchange_esmFdbk1*mask_2x2_NAs)[ c( (length(lon)/2+1):length(lon),
1:(length(lon)/2)),lowlat:highlat], zlim=c(-3,3), col=col_custom2(20)[20:1], main= expression(paste(Delta,"EI, RAD")), cex.main=2.5, font.main=2,
axis.args=list(cex.axis=2.2), xlab="", ylab="",xaxt="n",yaxt="n", cex.axis=1.2) ; map(add=T, interior=F);
mtext("e", side=3, adj=0, line=0.3, cex=1.5, font=2)
image.plot(lon - 180, lat[lowlat:highlat], (mean_EIchange_esmFixclim1*mask_2x2_NAs)[ c( (length(lon)/2+1):length(lon),
1:(length(lon)/2)),lowlat:highlat], zlim=c(-3,3), col=col_custom2(20)[20:1],  main=expression(paste(Delta,"EI, PHYS")), cex.main=2.5,font.main=2,
axis.args=list(cex.axis=2.2), xlab="", ylab="",xaxt="n",yaxt="n", cex.axis=1.2) ; map(add=T, interior=F);
mtext("f", side=3, adj=0, line=0.3, cex=1.5, font=2)

### XY plots:
mean_EI_pres_1pctCO2 <- mean_lai_pres_1pctCO2 - ( a*mean_corrmrsostran_pres_1pctCO2 + b)
mean_EI_fut_1pctCO2 <- mean_lai_fut_1pctCO2 - ( a*mean_corrmrsostran_fut_1pctCO2 + b)
mean_EIchange_1pctCO2 <- mean_EI_fut_1pctCO2 - mean_EI_pres_1pctCO2
mean_EI_pres_esmFdbk1 <- mean_lai_pres_esmFdbk1 - ( a*mean_corrmrsostran_pres_esmFdbk1 + b)
mean_EI_fut_esmFdbk1 <- mean_lai_fut_esmFdbk1 - ( a*mean_corrmrsostran_fut_esmFdbk1 + b)
mean_EIchange_esmFdbk1 <- mean_EI_fut_esmFdbk1 - mean_EI_pres_esmFdbk1
mean_EI_pres_esmFixclim1 <- mean_lai_pres_esmFixclim1 - ( a*mean_corrmrsostran_pres_esmFixclim1 + b)
mean_EI_fut_esmFixclim1 <- mean_lai_fut_esmFixclim1 - ( a*mean_corrmrsostran_fut_esmFixclim1 + b)
mean_EIchange_esmFixclim1 <- mean_EI_fut_esmFixclim1 - mean_EI_pres_esmFixclim1
par(mar=c(4,4.5,3,3))
### 1pctCO2
x<- mean_PrPETchange_1pctCO2[which( (mean_PrPET_fut_1pctCO2 < 0.65)  & (mean_PrPET_pres_1pctCO2 >= 0.65)   )]
y<- mean_EIchange_1pctCO2[which( (mean_PrPET_fut_1pctCO2 < 0.65)  & (mean_PrPET_pres_1pctCO2 >= 0.65)   )]
plot( x,y,col="gray40", xlim=c(-0.6,0.4), ylim=c(-4,4), xlab="",ylab="",cex.lab=1.5,cex.axis=1.5,cex.main=2.5,
main=expression(paste(Delta,"EI and ",Delta,"AI, CTL")), pch=3, cex=0.5); abline(v=0);abline(h=0)
mtext(side=1,line=3, cex=1.7, expression(paste(Delta,"AI")))
mtext(side=2,line=2.2, cex=1.7, expression(paste(Delta,"EI")))  
# Just new dryland pixels:
#lines( mean_PrPETchange_1pctCO2[which( (mean_PrPET_fut_1pctCO2 < 0.65  )],  
#mean_EIchange_1pctCO2[which((mean_PrPET_fut_1pctCO2 < 0.65)  & (mean_PrPET_pres_1pctCO2 >= 0.65) )], col="gray40", type="p",pch=3, cex=0.5)
abline(v=mean( mean_PrPETchange_1pctCO2[which( (mean_PrPET_fut_1pctCO2 < 0.65)  & (mean_PrPET_pres_1pctCO2 >= 0.65)   )], na.rm=T), col="gray40", lty=3)
#abline(v=mean( mean_PrPETchange_1pctCO2[which( mean_PrPET_fut_1pctCO2 < 0.65  )], na.rm=T), col="gray40", lty=3)
abline(h=mean( mean_EIchange_1pctCO2[which( (mean_PrPET_fut_1pctCO2 < 0.65)  & (mean_PrPET_pres_1pctCO2 >= 0.65)  )],na.rm=T), col="gray40", lty=3)
#abline(h=mean( mean_EIchange_1pctCO2[which( mean_PrPET_fut_1pctCO2 < 0.65  )],na.rm=T), col="gray40", lty=3)
mtext("g", side=3, at=-0.7, line=1, cex=1.6, font=2)
buff <- x; buff[buff>0.4]<- NA
histx<- hist(buff, breaks=seq(-0.6,0.4,length.out=30),plot=F)
buff <- y
histy<- hist(buff, breaks=seq(-4,4,  length.out=30),plot=F)
lines(histx$mids, (histx$density)/max( histx$density)-4, col="gray40")
lines(  (histy$density)/max(histy$density)*0.1-0.6, histy$mids, col="gray40")

### esmFdbk1
x<- mean_PrPETchange_esmFdbk1[which( (mean_PrPET_fut_esmFdbk1 < 0.65)  & (mean_PrPET_pres_esmFdbk1 >= 0.65)   )]
y<- mean_EIchange_esmFdbk1[which( (mean_PrPET_fut_esmFdbk1 < 0.65)  & (mean_PrPET_pres_esmFdbk1 >= 0.65)   )]
plot( x,y,col="gray40", xlim=c(-0.6,0.4), ylim=c(-4,4), xlab="",ylab="",cex.lab=1.5,cex.axis=1.5,cex.main=2.5,
main=expression(paste(Delta,"EI and ",Delta,"AI, RAD")), pch=3, cex=0.5); abline(v=0);abline(h=0)
mtext(side=1,line=3, cex=1.7, expression(paste(Delta,"AI")))
mtext(side=2,line=2.2, cex=1.7, expression(paste(Delta,"EI")))
# Just new dryland pixels:
#lines( mean_PrPETchange_esmFdbk1[which( (mean_PrPET_fut_esmFdbk1 < 0.65  )],
#mean_EIchange_esmFdbk1[which((mean_PrPET_fut_esmFdbk1 < 0.65)  & (mean_PrPET_pres_esmFdbk1 >= 0.65) )], col="gray40", type="p",pch=3, cex=0.5)
abline(v=mean( mean_PrPETchange_esmFdbk1[which( (mean_PrPET_fut_esmFdbk1 < 0.65)  & (mean_PrPET_pres_esmFdbk1 >= 0.65)   )], na.rm=T), col="gray40", lty=3)
#abline(v=mean( mean_PrPETchange_esmFdbk1[which( mean_PrPET_fut_esmFdbk1 < 0.65  )], na.rm=T), col="gray40", lty=3)
abline(h=mean( mean_EIchange_esmFdbk1[which( (mean_PrPET_fut_esmFdbk1 < 0.65)  & (mean_PrPET_pres_esmFdbk1 >= 0.65)  )],na.rm=T), col="gray40", lty=3)
#abline(h=mean( mean_EIchange_esmFdbk1[which( mean_PrPET_fut_esmFdbk1 < 0.65  )],na.rm=T), col="gray40", lty=3)
mtext("h", side=3, at=-0.7, line=1, cex=1.6, font=2)
buff <- x; buff[buff>0.4]<- NA
histx<- hist(buff, breaks=seq(-0.6,0.4,length.out=30),plot=F)
buff <- y
histy<- hist(buff, breaks=seq(-4,4,  length.out=30),plot=F)
lines(histx$mids, (histx$density)/max( histx$density)-4, col="gray40")
lines(  (histy$density)/max(histy$density)*0.1-0.6, histy$mids, col="gray40")

### esmFixclim1
x<- mean_PrPETchange_esmFixclim1[which( (mean_PrPET_fut_esmFixclim1 < 0.65)  & (mean_PrPET_pres_esmFixclim1 >= 0.65)   )]
y<- mean_EIchange_esmFixclim1[which( (mean_PrPET_fut_esmFixclim1 < 0.65)  & (mean_PrPET_pres_esmFixclim1 >= 0.65)   )]
plot( x,y,col="gray40", xlim=c(-0.6,0.4), ylim=c(-4,4), xlab="",ylab="",cex.lab=1.5,cex.axis=1.5,cex.main=2.5,
main=expression(paste(Delta,"EI and ",Delta,"AI, PHYS")), pch=3, cex=0.5); abline(v=0);abline(h=0)
mtext(side=1,line=3, cex=1.7, expression(paste(Delta,"AI")))
mtext(side=2,line=2.2, cex=1.7, expression(paste(Delta,"EI")))
# Just new dryland pixels:
#lines( mean_PrPETchange_esmFixclim1[which( (mean_PrPET_fut_esmFixclim1 < 0.65  )],
#mean_EIchange_esmFixclim1[which((mean_PrPET_fut_esmFixclim1 < 0.65)  & (mean_PrPET_pres_esmFixclim1 >= 0.65) )], col="gray40", type="p",pch=3, cex=0.5)
abline(v=mean( mean_PrPETchange_esmFixclim1[which( (mean_PrPET_fut_esmFixclim1 < 0.65)  & (mean_PrPET_pres_esmFixclim1 >= 0.65)   )], na.rm=T), col="gray40", lty=3)
#abline(v=mean( mean_PrPETchange_esmFixclim1[which( mean_PrPET_fut_esmFixclim1 < 0.65  )], na.rm=T), col="gray40", lty=3)
abline(h=mean( mean_EIchange_esmFixclim1[which( (mean_PrPET_fut_esmFixclim1 < 0.65)  & (mean_PrPET_pres_esmFixclim1 >= 0.65)  )],na.rm=T), col="gray40", lty=3)
#abline(h=mean( mean_EIchange_esmFixclim1[which( mean_PrPET_fut_esmFixclim1 < 0.65  )],na.rm=T), col="gray40", lty=3)
mtext("i", side=3, at=-0.7, line=1, cex=1.6, font=2)
buff <- x; buff[buff>0.4]<- NA
histx<- hist(buff, breaks=seq(-0.6,0.4,length.out=30),plot=F)
buff <- y
histy<- hist(buff, breaks=seq(-4,4,  length.out=30),plot=F)
lines(histx$mids, (histx$density)/max( histx$density)-4, col="gray40")
lines(  (histy$density)/max(histy$density)*0.1-0.6, histy$mids, col="gray40")


dev.off()
