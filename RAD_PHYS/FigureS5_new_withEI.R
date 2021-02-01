#####################
###### Supplementary Figure 5: changes in E and P in PHYS, separately:
mean_pr_pres_esmFixclim1 <- mask_2x2_NAs*apply( mean_pr_2x2_allmodels_year_pres_esmFixclim1[,,], c(1, 2), mean, na.rm=T)
mean_pr_fut_esmFixclim1 <- mask_2x2_NAs*apply( mean_pr_2x2_allmodels_year_fut_esmFixclim1[,,], c(1, 2), mean, na.rm=T)
mean_PETow_pres_esmFixclim1 <- mask_2x2_NAs*apply( mean_PETow_2x2_allmodels_year_pres_esmFixclim1[,,], c(1, 2), mean, na.rm=T)
mean_PETow_fut_esmFixclim1 <- mask_2x2_NAs*apply( mean_PETow_2x2_allmodels_year_fut_esmFixclim1[,,], c(1, 2), mean, na.rm=T)

pdf("FigureS5_new_withEI.pdf", width = 10 , height = 10)
layout(matrix(1:2,2,1));par(mar=c(3,3,3,5))
## Pr
mean_prchange_esmFixclim1 <- (mean_pr_fut_esmFixclim1 - mean_pr_pres_esmFixclim1)*86400
mean_prchange_esmFixclim1[which(mean_prchange_esmFixclim1 > 1)] <-1
mean_prchange_esmFixclim1[which(mean_prchange_esmFixclim1 < -1)] <- -1;
image.plot(lon - 180, lat[lowlat:highlat], (mean_prchange_esmFixclim1*mask_2x2_NAs)[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],
  zlim=c(-1,1),  col=col_custom2(20)[20:1], main=expression(paste(Delta,"P (mm/d), PHYS")), cex.main=1.7,
axis.args = list(cex.axis = 1.5)) ; map(add=T, interior=F);
## PETow
mean_PETowchange_esmFixclim1 <- mean_PETow_fut_esmFixclim1 - mean_PETow_pres_esmFixclim1
mean_PETowchange_esmFixclim1[which(mean_PETowchange_esmFixclim1 > 1)] <-1
mean_PETowchange_esmFixclim1[which(mean_PETowchange_esmFixclim1 < -1)] <- -1;
image.plot(lon - 180, lat[lowlat:highlat], (mean_PETowchange_esmFixclim1*mask_2x2_NAs)[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],zlim=c(-1,1),  col=col_custom2(20), 
main=expression(paste(Delta,"PET (mm/d), PHYS")),cex.main=1.7, axis.args = list(cex.axis = 1.5)) ; map(add=T, interior=F);
dev.off()

