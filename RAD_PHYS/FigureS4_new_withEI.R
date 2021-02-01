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
### We compare with LAI outputs.

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


col_custom2=colorRampPalette(c("darkblue", "blue", "cyan", "white","orange", "red","darkred"))
pdf("FigureS4_new_withEI.pdf", width=17 , height=7)
lowlat <- min(which(lat > -60))
highlat <- min(which(lat > 80))
layout(matrix(1:6,2,3, byrow=F));par(mar=c(0.5,2,2.5,7.5)); par(omi=c(0,0,0,0.3))
## LAI
mean_laichange_1pctCO2 <- mean_lai_fut_1pctCO2 - mean_lai_pres_1pctCO2
mean_laichange_esmFdbk1 <- mean_lai_fut_esmFdbk1 - mean_lai_pres_esmFdbk1
mean_laichange_esmFixclim1 <- mean_lai_fut_esmFixclim1 - mean_lai_pres_esmFixclim1
mean_laichange_1pctCO2[which(mean_laichange_1pctCO2 > 2)] <-2;mean_laichange_1pctCO2[which(mean_laichange_1pctCO2 < -2)] <- -2;
mean_laichange_esmFixclim1[which(mean_laichange_esmFixclim1 > 2)] <-2
mean_laichange_esmFixclim1[which(mean_laichange_esmFixclim1 < -2)] <- -2;
mean_laichange_esmFdbk1[which(mean_laichange_esmFdbk1 > 2)] <-2;mean_laichange_esmFdbk1[which(mean_laichange_esmFdbk1 < -2)] <- -2;
image.plot(lon - 180, lat[lowlat:highlat], (mean_laichange_1pctCO2*mask_2x2_NAs)[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],
zlim=c(-2,2), col=col_custom2(20)[20:1], xlab="", ylab="",xaxt="n",yaxt="n",main=expression(paste(Delta,"LAI, CTL")),cex.main=2,font.main=1,
axis.args=list(cex.axis=2.2),cex.axis=1.2); map(add=T, interior=F); mtext("a", side=3, adj=0, line=0.3, cex=1.5, font=2)
image.plot(lon - 180, lat[lowlat:highlat], (mean_laichange_esmFdbk1*mask_2x2_NAs)[ c( (length(lon)/2+1):length(lon),
1:(length(lon)/2)),lowlat:highlat], zlim=c(-2,2), col=col_custom2(20)[20:1], main= expression(paste(Delta,"LAI, RAD")), cex.main=2, font.main=1,
axis.args=list(cex.axis=2.2), xlab="", ylab="",xaxt="n",yaxt="n", cex.axis=1.2) ; map(add=T, interior=F);
mtext("b", side=3, adj=0, line=0.3, cex=1.5, font=2)
image.plot(lon - 180, lat[lowlat:highlat], (mean_laichange_esmFixclim1*mask_2x2_NAs)[ c( (length(lon)/2+1):length(lon),
1:(length(lon)/2)),lowlat:highlat], zlim=c(-2,2), col=col_custom2(20)[20:1],  main=expression(paste(Delta,"LAI, PHYS")), cex.main=2, font.main=1,
axis.args=list(cex.axis=2.2), xlab="", ylab="",xaxt="n",yaxt="n", cex.axis=1.2) ; map(add=T, interior=F);
mtext("c", side=3, adj=0, line=0.3, cex=1.5, font=2)

## Cor(SM,Tran)
mean_corrmrsostranchange_1pctCO2 <- mean_corrmrsostran_fut_1pctCO2 - mean_corrmrsostran_pres_1pctCO2
mean_corrmrsostranchange_esmFdbk1 <- mean_corrmrsostran_fut_esmFdbk1 - mean_corrmrsostran_pres_esmFdbk1
mean_corrmrsostranchange_esmFixclim1 <- mean_corrmrsostran_fut_esmFixclim1 - mean_corrmrsostran_pres_esmFixclim1
mean_corrmrsostranchange_1pctCO2[which(mean_corrmrsostranchange_1pctCO2 > 0.5)] <-0.5;
mean_corrmrsostranchange_1pctCO2[which(mean_corrmrsostranchange_1pctCO2 < -0.5)] <- -0.5;
mean_corrmrsostranchange_esmFixclim1[which(mean_corrmrsostranchange_esmFixclim1 > 0.5)] <- 0.5
mean_corrmrsostranchange_esmFixclim1[which(mean_corrmrsostranchange_esmFixclim1 < -0.5)] <- -0.5;
mean_corrmrsostranchange_esmFdbk1[which(mean_corrmrsostranchange_esmFdbk1 > 0.5)] <- 0.5
mean_corrmrsostranchange_esmFdbk1[which(mean_corrmrsostranchange_esmFdbk1 < -0.5)] <- -0.5;
image.plot(lon - 180, lat[lowlat:highlat], (mean_corrmrsostranchange_1pctCO2*mask_2x2_NAs)[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),
lowlat:highlat], zlim=c(-0.5,0.5),xlab="", ylab="",xaxt="n",yaxt="n", col=col_custom2(20), 
main= expression(paste(Delta,"cor(SM,Tran), CTL")),cex.main=2,font.main=1, axis.args=list(cex.axis=2.2)) ; map(add=T, interior=F);
mtext("d", side=3, adj=0, line=0.3, cex=1.5, font=2)
image.plot(lon - 180, lat[lowlat:highlat], (mean_corrmrsostranchange_esmFdbk1*mask_2x2_NAs)[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),
lowlat:highlat], zlim=c(-0.5,0.5), col=col_custom2(20), main=expression(paste(Delta,"cor(SM,Tran), RAD")), axis.args=list(cex.axis=2.2),cex.main=2,font.main=1,
 xlab="", ylab="",xaxt="n",yaxt="n", cex.axis=1.2) ; map(add=T, interior=F);
mtext("e", side=3, adj=0, line=0.3, cex=1.5, font=2)
image.plot(lon - 180, lat[lowlat:highlat], (mean_corrmrsostranchange_esmFixclim1*mask_2x2_NAs)[ c( (length(lon)/2+1):length(lon),
1:(length(lon)/2)),lowlat:highlat], zlim=c(-0.5,0.5), col=col_custom2(20),main= expression(paste(Delta,"cor(SM,Tran), PHYS")),cex.main=2,font.main=1,
axis.args=list(cex.axis=2.2), xlab="", ylab="",xaxt="n",yaxt="n", cex.axis=1.2) ;map(add=T, interior=F);
mtext("f", side=3, adj=0, line=0.3, cex=1.5, font=2)

dev.off()


