OB##############################
#.libPaths("/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
#library(ncdf, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(colorRamps, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(maps, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(ncdf4, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(fields, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(akima, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(LSD, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(RColorBrewer, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")


################################### Calculating the Fractions with the corrected PET, corrected for CO2 effects:
dim(mean_PrPETpmrc_co2_year_pres_2x2_allmodels)
dim(mean_PrPETpmrc_co2_year_change_2x2_allmodels)

## Calculating the multi-mean model mean quantities with the right models:
mean_PrPETpmrc_co2 <- apply( mean_PrPETpmrc_co2_year_pres_2x2_allmodels[,,which(list_models_common_fut %in% list_models_lai_and_common)], c(1, 2), mean,
na.rm=T)
mean_PrPETpmrc_co2_fut <- apply( mean_PrPETpmrc_co2_year_fut_2x2_allmodels[,,which(list_models_common_fut %in% list_models_lai_and_common)], c(1, 2), mean,
na.rm=T)
mean_PrPETpmrc_co2change <- apply(mean_PrPETpmrc_co2_year_change_2x2_allmodels[,,which(list_models_common_fut %in% list_models_lai_and_common)], c(1,2), mean,
na.rm=T)

## With the multimodel mean:
mean_PrPETpmrc_co2 <- apply( mean_PrPETpmrc_co2_year_pres_2x2_allmodels[,,which(list_models_common_fut %in% list_models_lai_and_common)], c(1, 2), mean,
na.rm=T)
mean_lai <- apply(mean_lai_year_2x2_allmodels[,,which(list_models_lai %in% list_models_lai_and_common)], c(1, 2), mean, na.rm=T)
mean_PrPETpmrc_co2_fut <- apply( mean_PrPETpmrc_co2_year_fut_2x2_allmodels[,,which(list_models_common_fut %in% list_models_lai_and_common)], c(1, 2), mean,
na.rm=T)
tot<- sum((mask_2x2_NAs*areacella_2x2), na.rm=T)
pres<- sum((mask_2x2_NAs*areacella_2x2)[which(mean_PrPETpmrc_co2 < 0.65)], na.rm=T)
fut<- sum((mask_2x2_NAs*areacella_2x2)[which(mean_PrPETpmrc_co2_fut <  0.65 )], na.rm=T)
frac_drylands_AI_pres <- pres/tot*100
frac_drylands_AI_fut <- fut/tot*100
## WIth all models individually:
frac_drylands_AIco2_pres_allmodels <- NULL
frac_drylands_AIco2_fut_allmodels <- NULL
for (m in 1:16) {
mean_PrPETpmrc_co2 <-  mean_PrPETpmrc_co2_year_pres_2x2_allmodels[,,which(list_models_common_fut %in% list_models_lai_and_common)][,,m]
mean_PrPETpmrc_co2_fut <-  mean_PrPETpmrc_co2_year_fut_2x2_allmodels[,,which(list_models_common_fut %in% list_models_lai_and_common)][,,m]
tot<- sum((mask_2x2_NAs*areacella_2x2), na.rm=T)
pres<- sum((mask_2x2_NAs*areacella_2x2)[which(mean_PrPETpmrc_co2 < 0.65)], na.rm=T)
fut<- sum((mask_2x2_NAs*areacella_2x2)[which(mean_PrPETpmrc_co2_fut <  0.65 )], na.rm=T)
frac_drylands_AIco2_pres_allmodels[m] <- pres/tot*100
frac_drylands_AIco2_fut_allmodels[m]  <- fut/tot*100
}

######################################################## Figure S6
## Now, use frac_drylands_AIco2_pres_allmodels
## This gives fractions for 16 models (list_models_lai_and_common)
## All we have to do here is to take the right models out of those:

bob <- which(list_models_lai_and_common %in% list_models_common_SMstress)
list_models_lai_and_common[bob] == list_models_common_SMstress

buff<-sum((mask_2x2_NAs*areacella_2x2)[which(mean_PrPETpmrc_co2 < 0.65)], na.rm=T)/ sum((mask_2x2_NAs*areacella_2x2), na.rm=T)* 100
print(paste("Frac drylands AI pres=",buff))
frac_drylands_AIco2_pres_MMM <- buff

buff<-sum((mask_2x2_NAs*areacella_2x2)[which(mean_PrPETpmrc_co2_fut < 0.65)], na.rm=T)/ sum((mask_2x2_NAs*areacella_2x2), na.rm=T)* 100
print(paste("Frac drylands AI fut=",buff))
frac_drylands_AIco2_fut_MMM <- buff


pdf("FigureS6_new_withEI.pdf", width=8); par(mar=c(4,5,3,3))
#### AI-based
boxplot(frac_drylands_AIco2_pres_allmodels[bob], col="orange", xlim=c(0,5), ylab="Fraction of global land area (%)", ylim=c(0, 60),
cex.lab=1.5, cex.axis=1.3, range=0, at=1.2)
#points(mean(frac_drylands_AIco2_pres_allmodels[bob]), pch=21,lwd=3, col="orange");
axis(1, labels=c("AI-based", "EI-based"), at=c(1.5, 3.5), tick=T, cex.axis=1.5)
legend("bottomleft", fill=c("orange", "red"),  c("Present", "Future"), bty="n", cex=1.5)
boxplot(frac_drylands_AIco2_fut_allmodels[bob], col="red", at=1.8, add=TRUE, yaxt="n", xaxt="n", range=0);
#points(mean(frac_drylands_AIco2_fut_allmodels[bob]), pch=21,lwd=3, col="red")
#### Ecohydro-based
boxplot(frac_drylands_EI_pres_allmodels, col="orange", at=3.2, add=TRUE,  yaxt="n", xaxt="n",range=0)
#points( mean(frac_drylands_EI_pres_allmodels), pch=21,lwd=3, col="orange")
boxplot(frac_drylands_EI_fut_allmodels, col="red", at=3.8, add=TRUE,yaxt="n", xaxt="n", range=0);
#points(mean(frac_drylands_EI_fut_allmodels), pch=21,lwd=3, col="red", )
points(cbind(c(1.2, 1.8, 3.2, 3.8), c(mean(frac_drylands_AIco2_pres_allmodels[bob]), mean(frac_drylands_AIco2_fut_allmodels[bob]),
mean(frac_drylands_EI_pres_allmodels), mean(frac_drylands_EI_fut_allmodels))), pch=21,lwd=3, col="black")
points(cbind(c(1.2, 1.8, 3.2, 3.8), c(mean(frac_drylands_AIco2_pres_MMM), mean(frac_drylands_AIco2_fut_MMM),
mean(frac_drylands_EI_pres_MMM), mean(frac_drylands_EI_fut_MMM))), pch=8,lwd=2, col="black")
dev.off()


### With conventional calculation, increase in drylands area:
mean(frac_drylands_AI_fut_allmodels - frac_drylands_AI_pres_allmodels)

### With Yang correction:
mean(frac_drylands_AIco2_fut_allmodels[bob] - frac_drylands_AIco2_pres_allmodels[bob])