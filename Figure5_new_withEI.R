##############################
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
mean_corr_mrsos_tran <- apply(corr_mrsos_tran_year_2x2_allmodels[,,which(list_models_tran_and_mrsos_PCMDI_fut %in% list_models_common_SMstress)],
c(1,2), mean,na.rm=T)
mean_corr_mrsos_tran_fut <- apply(corr_mrsos_tran_year_rcp85_2x2_allmodels[,,
which(list_models_tran_and_mrsos_PCMDI_fut %in% list_models_common_SMstress)],c(1,2), mean,na.rm=T)
mean_corr_mrsos_tran_change <- mean_corr_mrsos_tran_fut - mean_corr_mrsos_tran


### Here we calculate AI and EI for each model individually:
lat <- mask_2x2$y;lon <-  mask_2x2$x
lowlat <- min(which(lat > -60))
highlat <- min(which(lat > 80))
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
corr_mrsos_tran_year <- corr_mrsos_tran_year_2x2_allmodels[,,which(list_models_tran_and_mrsos_PCMDI_fut %in% list_models_common_SMstress)][,,m]
corr_mrsos_tran_year_fut <- corr_mrsos_tran_year_rcp85_2x2_allmodels[,,which(list_models_tran_and_mrsos_PCMDI_fut %in% list_models_common_SMstress)][,,m]
##### Fraction of drylands defined based on AI:
pres<- sum((mask_2x2_NAs*areacella_2x2)[which(mean_PrPET <= 0.65)], na.rm=T)
fut<- sum((mask_2x2_NAs*areacella_2x2)[which(mean_PrPET_fut <=  0.65 )], na.rm=T)
frac_drylands_AI_pres_allmodels[m] <- pres/tot*100
frac_drylands_AI_fut_allmodels[m]  <- fut/tot*100
##### Fraction of drylands defined based on Ecohydro:
a <- a_MMM
b <- b_MMM
mean_EI_pres <- mean_lai - (a*corr_mrsos_tran_year+b)
mean_EI_fut <- mean_lai_fut - (a*corr_mrsos_tran_year_fut+b)
mean_EI_pres[is.na(mean_PrPET)==T] <- NA
mean_EI_fut[is.na(mean_PrPET)==T] <- NA
## Fractions
pres<- sum((mask_2x2_NAs*areacella_2x2)[which(mean_EI_pres < 0 )], na.rm=T)
fut<- sum((mask_2x2_NAs*areacella_2x2)[which (mean_EI_fut < 0  )], na.rm=T)
frac_drylands_EI_pres_allmodels[m]  <- pres/tot*100
frac_drylands_EI_fut_allmodels[m]  <- fut/tot*100
rm(mean_PrPET); rm(mean_lai); rm(mean_PrPET_fut); rm(mean_lai_fut); rm(pres); rm(fut); rm(corr_mrsos_tran_year_fut); rm(corr_mrsos_tran_year) }

pdf("Figure5_new_withEI.pdf", width=8); par(mar=c(4,5,3,3))
#### AI-based
boxplot(frac_drylands_AI_pres_allmodels, col="orange", xlim=c(0,5), ylab="Fraction of global land area (%)", ylim=c(0, 70),
cex.lab=1.5, cex.axis=1.3, range=0, at=1.2)
#points(mean(frac_drylands_AI_pres_allmodels), pch=21,lwd=3, col="orange");
axis(1, labels=c("AI-based", "EI-based"), at=c(1.5, 3.5), tick=T, cex.axis=1.5)
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
dev.off()


print("frac_drylands_AI_pres_allmodels")
print(round(frac_drylands_AI_pres_allmodels,2))
print("frac_drylands_AI_fut_allmodels")
print(round(frac_drylands_AI_fut_allmodels,2))


print("frac_drylands_EI_pres_allmodels")
print(round(frac_drylands_EI_pres_allmodels,2))
print("frac_drylands_EI_fut_allmodels")
print(round(frac_drylands_EI_fut_allmodels,2))

print(paste("mean(frac_drylands_AI_pres_allmodels)=",round(mean(frac_drylands_AI_pres_allmodels),1)))
print(paste("mean(frac_drylands_AI_fut_allmodels)=",round(mean(frac_drylands_AI_fut_allmodels),1)))
print(round(mean(frac_drylands_AI_fut_allmodels),1) - round(mean(frac_drylands_AI_pres_allmodels),1))

print(paste("mean(frac_drylands_EI_pres_allmodels)=",round(mean(frac_drylands_EI_pres_allmodels),1)))
print(paste("mean(frac_drylands_EI_fut_allmodels)=",round(mean(frac_drylands_EI_fut_allmodels),1)))
print(round(mean(frac_drylands_EI_fut_allmodels),1) - round(mean(frac_drylands_EI_pres_allmodels),1))


print(paste("median(frac_drylands_AI_pres_allmodels)=",round(median(frac_drylands_AI_pres_allmodels),1)))
print(paste("median(frac_drylands_AI_fut_allmodels)=",round(median(frac_drylands_AI_fut_allmodels),1)))
print(round(median(frac_drylands_AI_fut_allmodels),1) - round(median(frac_drylands_AI_pres_allmodels),1))


print(paste("median(frac_drylands_EI_pres_allmodels)=",round(median(frac_drylands_EI_pres_allmodels),1)))
print(paste("median(frac_drylands_EI_fut_allmodels)=",round(median(frac_drylands_EI_fut_allmodels),1)))
print(round(median(frac_drylands_EI_fut_allmodels),1) - round(median(frac_drylands_EI_pres_allmodels),1))




