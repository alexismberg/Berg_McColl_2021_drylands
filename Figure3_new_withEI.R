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
mean_PrPET <- apply( mean_PrPET_year_pres_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1, 2), median, na.rm=T)
mean_lai <- apply(mean_lai_year_2x2_allmodels[,,which(list_models_lai %in% list_models_common_SMstress)], c(1, 2), median, na.rm=T)
mean_lai_change <- apply(mean_lai_year_change_2x2_allmodels[,,which(list_models_lai_fut %in% list_models_common_SMstress)], c(1, 2), median, na.rm=T)
mean_PrPET_fut <- apply( mean_PrPET_year_fut_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1, 2), median, na.rm=T)
mean_prchange <- 86400*apply(mean_pr_year_change_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1,2), median, na.rm=T)
mean_PETchange <- apply(mean_PET_year_change_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1,2), median, na.rm=T)
mean_PrPETchange <- apply(mean_PrPET_year_change_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1,2), median,
na.rm=T)
mean_lai_year_fut_2x2_allmodels <- mean_lai_year_change_2x2_allmodels + mean_lai_year_2x2_allmodels[,,which(list_models_lai %in%
list_models_lai_fut)]
mean_lai_fut <- apply( mean_lai_year_fut_2x2_allmodels[,,which(list_models_lai_fut %in% list_models_common_SMstress)], c(1, 2), median, na.rm=T)
mean_corr_mrsos_tran <- apply(corr_mrsos_tran_year_2x2_allmodels[,,which(list_models_tran_and_mrsos_PCMDI_fut %in% list_models_common_SMstress)],
c(1,2), median,na.rm=T)
mean_corr_mrsos_tran_fut <- apply(corr_mrsos_tran_year_rcp85_2x2_allmodels[,,
which(list_models_tran_and_mrsos_PCMDI_fut %in% list_models_common_SMstress)],c(1,2), median,na.rm=T)
mean_corr_mrsos_tran_change <- mean_corr_mrsos_tran_fut - mean_corr_mrsos_tran
pr_fut <- 86400*apply(mean_pr_year_fut_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)],c(1,2), median, na.rm=T)
PET_fut <- apply(mean_PET_year_fut_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)],c(1,2), median, na.rm=T)
pr_pres <- 86400*apply(mean_pr_year_pres_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)],c(1,2), median, na.rm=T)
PET_pres <- apply(mean_PET_year_pres_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)],c(1,2), median, na.rm=T)

mean_EI_pres <- mean_lai - ( a*mean_corr_mrsos_tran + b)
mean_EI_fut <- mean_lai_fut - ( a*mean_corr_mrsos_tran_fut + b)
mean_EIchange <- mean_EI_fut - mean_EI_pres

bin_PET <- seq(-0.125,1.775, by=0.05) #length.out=20)
bin_pr <- seq(-1.55,2.05, by=0.1)#length.out=20)
lat <- mask_2x2$y; lon <-  mask_2x2$x
lowlat <- min(which(lat > -60)); highlat <- min(which(lat > 80)); mask <- mask_2x2_NAs

################## Binning delta_LAI as a function of delta_Pr and delta_PET
laichange_year_binned_MMM <- array(NA, dim=c(length(bin_pr),length(bin_PET), 500))
w_acrossmodels <- array(0, dim=c(length(bin_pr),length(bin_PET)))
total_prchange_drylands <- NULL; total_PETchange_drylands <- NULL
####################
for (i in 1:length(lon)){                     print(i)
for (j in 1:length(lat)){
if (    (is.na( (pr_fut/PET_fut)[i,j])==F)){ 
total_prchange_drylands <- append (total_prchange_drylands, mean_prchange[i,j])
total_PETchange_drylands <- append (total_PETchange_drylands, mean_PETchange[i,j])
coord_x <- NULL; coord_y <- NULL
for (t in 1:(length(bin_pr)-1)) {
if ( (is.na(mean_prchange[i,j]*mask[i,j]) == F) && (bin_pr[t] <  mean_prchange[i,j]) && (mean_prchange[i,j] < bin_pr[t+1]))  {coord_x <- t} }
for (p in 1:(length(bin_PET)-1)) {
if ( (is.na(mask[i,j] * mean_PETchange[i,j] ) ==F) && (bin_PET[p] < mean_PETchange[i,j] ) && ( mean_PETchange[i,j] < bin_PET[p+1]))  {coord_y <- p} }
w_acrossmodels[coord_x, coord_y] <- w_acrossmodels[coord_x, coord_y]+1
laichange_year_binned_MMM[coord_x, coord_y, w_acrossmodels[coord_x, coord_y]] <- mean_lai_change[i,j]}}}
mean_laichange_year_binned_Pr_PET_MMM <- apply(laichange_year_binned_MMM, c(1,2), mean, na.rm=T)
median_laichange_year_binned_Pr_PET_MMM  <- apply(laichange_year_binned_MMM, c(1,2), median, na.rm=T)
w_laichange_year_binned_Pr_PET_MMM  <- w_acrossmodels

################## Binning delta_cor(SM,Tran) as a function of dPr and dPET
corrmrsostranchange_year_binned_MMM <- array(NA, dim=c(length(bin_pr),length(bin_PET), 500))
w_acrossmodels <- array(0, dim=c(length(bin_pr),length(bin_PET))) ### w is just to count the density in each bin
total_prchange_drylands <- NULL; total_PETchange_drylands <- NULL
####################
for (i in 1:length(lon)){                     print(i)
for (j in 1:length(lat)){
if ( (is.na(pr_fut[i,j])==F)){ 
total_prchange_drylands <- append (total_prchange_drylands, mean_prchange[i,j])
total_PETchange_drylands <- append (total_PETchange_drylands, mean_PETchange[i,j])
coord_x <- NULL; coord_y <- NULL
for (t in 1:(length(bin_pr)-1)) {
if ( (is.na(mean_prchange[i,j]*mask[i,j]) == F) && (bin_pr[t] <  mean_prchange[i,j]) && (mean_prchange[i,j] < bin_pr[t+1]))  {coord_x <- t} }
for (p in 1:(length(bin_PET)-1)) {
if ( (is.na(mask[i,j] * mean_PETchange[i,j] ) ==F) && (bin_PET[p] < mean_PETchange[i,j] ) && ( mean_PETchange[i,j] < bin_PET[p+1]))  {coord_y <- p} }
w_acrossmodels[coord_x, coord_y] <- w_acrossmodels[coord_x, coord_y]+1
corrmrsostranchange_year_binned_MMM[coord_x, coord_y, w_acrossmodels[coord_x, coord_y]] <- mean_corr_mrsos_tran_change[i,j]}}}
mean_corrmrsostranchange_year_binned_Pr_PET_MMM <- apply(corrmrsostranchange_year_binned_MMM, c(1,2), mean, na.rm=T)
w_corrmrsostranchange_year_binned_Pr_PET_MMM  <- w_acrossmodels

EIchange_year_binned_MMM <- array(NA, dim=c(length(bin_pr),length(bin_PET), 500))
w_acrossmodels <- array(0, dim=c(length(bin_pr),length(bin_PET)))
####################
for (i in 1:length(lon)){                     print(i)
for (j in 1:length(lat)){
if ( (is.na(pr_fut[i,j])==F)){ 
coord_x <- NULL; coord_y <- NULL
for (t in 1:(length(bin_pr)-1)) {
if ( (is.na(mean_prchange[i,j]*mask[i,j]) == F) && (bin_pr[t] <  mean_prchange[i,j]) && (mean_prchange[i,j] < bin_pr[t+1]))  {coord_x <- t} }
for (p in 1:(length(bin_PET)-1)) {
if ( (is.na(mask[i,j] * mean_PETchange[i,j] ) ==F) && (bin_PET[p] < mean_PETchange[i,j] ) && ( mean_PETchange[i,j] < bin_PET[p+1]))  {coord_y <- p} }
w_acrossmodels[coord_x, coord_y] <- w_acrossmodels[coord_x, coord_y]+1
EIchange_year_binned_MMM[coord_x, coord_y, w_acrossmodels[coord_x, coord_y]] <- mean_EIchange[i,j] }}}
mean_EIchange_year_binned_Pr_PET_MMM <- apply(EIchange_year_binned_MMM, c(1,2), mean, na.rm=T)
w_EIchange_year_binned_Pr_PET_MMM  <- w_acrossmodels

AIchange_year_binned_MMM <- array(NA, dim=c(length(bin_pr),length(bin_PET), 500))
w_acrossmodels <- array(0, dim=c(length(bin_pr),length(bin_PET)))
####################
for (i in 1:length(lon)){                     print(i)
for (j in 1:length(lat)){
if ( (is.na(pr_fut[i,j])==F)){ 
coord_x <- NULL; coord_y <- NULL
for (t in 1:(length(bin_pr)-1)) {
if ( (is.na(mean_prchange[i,j]*mask[i,j]) == F) && (bin_pr[t] <  mean_prchange[i,j]) && (mean_prchange[i,j] < bin_pr[t+1]))  {coord_x <- t} }
for (p in 1:(length(bin_PET)-1)) {
if ( (is.na(mask[i,j] * mean_PETchange[i,j] ) ==F) && (bin_PET[p] < mean_PETchange[i,j] ) && ( mean_PETchange[i,j] < bin_PET[p+1]))  {coord_y <- p} }
w_acrossmodels[coord_x, coord_y] <- w_acrossmodels[coord_x, coord_y]+1
AIchange_year_binned_MMM[coord_x, coord_y, w_acrossmodels[coord_x, coord_y]] <- mean_PrPETchange[i,j] }}}
mean_AIchange_year_binned_Pr_PET_MMM <- apply(AIchange_year_binned_MMM, c(1,2), mean, na.rm=T)
w_AIchange_year_binned_Pr_PET_MMM  <- w_acrossmodels



#col_custom3 =  colorRampPalette(c("purple", "darkblue","blue","cyan","beige", "orange","red","darkred", "black"))
col_custom3 =  colorRampPalette(c("darkblue","blue","cyan","beige", "orange","red","darkred"))

pdf("Figure3_new_withEI.pdf",  width=12, height=5)
layout(matrix(1:2,1,2)); par(mar=c(4,6,3,8))
### Binned AI
bill <-  mean_AIchange_year_binned_Pr_PET_MMM
bill[bill <= -0.5] <- -0.5; bill[bill >= 0.5] <- 0.5;
image.plot( bin_pr,bin_PET , bill, zlim=c(-0.5,0.5), breaks=seq(-0.5, 0.5, by=0.05), ylim=range(bin_PET), xlim=range(bin_pr),
main=expression(paste(Delta,"AI" )), col=col_custom3(20)[20:1], xlab=expression(paste(Delta,"P (mm/d)" )),
ylab=expression(paste(Delta,"PET (mm/d)" )), cex.axis=1.7, cex.lab=1.7,cex.main=2, axis.args=list( cex.axis=1.7));abline(h=0); abline(v=0);
mtext("a", side=3, at = -2, line=0.8, cex=1.7, font=2); #adj=0
### Binned EI
bill <-  mean_EIchange_year_binned_Pr_PET_MMM
bill[bill <= -2] <- -2; bill[bill >= 2] <- 2;
image.plot( bin_pr,bin_PET , bill, zlim=c(-2,2), breaks=seq(-2, 2, by=0.2), ylim=range(bin_PET), xlim=range(bin_pr),
main=expression(paste(Delta,"EI" )), col=col_custom3(20)[20:1], xlab=expression(paste(Delta,"P (mm/d)" )),
ylab=expression(paste(Delta,"PET (mm/d)" )), cex.axis=1.7, cex.lab=1.7,cex.main=2, axis.args=list( cex.axis=1.7));abline(h=0); abline(v=0);
mtext("b", side=3, at=-2 ,line=0.8, cex=1.7, font=2)   #adj=0
dev.off()
