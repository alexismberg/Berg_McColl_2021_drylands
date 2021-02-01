###############################
#.libPaths("/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
#library(ncdf, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(colorRamps, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(maps, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(ncdf4, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(fields, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(akima, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(LSD, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")
library(RColorBrewer, lib="/n/holystore01/LABS/mccoll_lab/Lab/aberg//R/x86_64-redhat-linux-gnu-library/3.2")

## Load a bunch of stuff in Lai_PrPET.R first!

## Then load those:


load("../MRLSL/mean_mrsos_year_2x2_allmodels.RData")
load("../MRLSL/mean_mrsos_year_rcp85_2x2_allmodels.RData")
load("../MRLSL/list_models_mrsos.RData")


mean_mrsos <- mask_2x2_NAs*apply(mean_mrsos_year_2x2_allmodels[,,which(list_models_mrsos %in% list_models_common_SMstress)],c(1,2), mean,na.rm=T)
mean_mrsos_fut <- mask_2x2_NAs*apply(mean_mrsos_year_rcp85_2x2_allmodels[,,which(list_models_mrsos %in% list_models_common_SMstress)],c(1,2),
mean,na.rm=T)
mean_mrsos_change <- mean_mrsos_fut - mean_mrsos

mean_mrsos_corr <- mean_mrsos
## Fixing mean soil moisture: removing problematic coastal pixels:
for (i in 1:length(lon)){# print(i)
for (j in 77:91){
if ((is.na(mean_mrsos[i,j])==F) && (mean_mrsos[i,j] <18)) {mean_mrsos_corr[i,j] <- NA }}}
for (i in 2:(length(lon)-1)){# print(i)
for (j in 2:(length(lat)-1)){
if (is.na(mean_mrsos[i,j])==F){
if ((mean_mrsos[i,j] <20) &&  (length(which(is.na(mean_mrsos[(i-1):(i+1),(j-1):(j+1)])==T)) ==8))  {mean_mrsos_corr[i,j] <- NA }}}}
for (i in 2:(length(lon)-1)){# print(i)
for (j in 2:(length(lat)-1)){
if (is.na(mean_mrsos[i,j])==F){
if ((mean_mrsos[i,j] <20) &&  (length(which(is.na( mean_mrsos[(i-1):(i+1),(j-1):(j+1)] )==T)) >= 3) &&
(length(which(is.na( mean_mrsos[(i-1):(i+1),(j-1):(j+1)] )==T)) < 8) &&
(mean( c(mean_mrsos[(i-1):(i+1),j+1], mean_mrsos[(i-1):(i+1),j-1],mean_mrsos[i-1,j],mean_mrsos[i+1,j]),na.rm=T)> mean_mrsos[i,j]+2))
{mean_mrsos_corr[i,j] <- NA }
}}}

bib <- mean_mrsos_corr[47:84,41:51]
bib[which(bib < 19)] <- NA
mean_mrsos_corr[47:84,41:51] <- bib
mean_mrsos<- mean_mrsos_corr
mean_mrsos_change <- mean_mrsos_fut - mean_mrsos

mean_pr <- 86400*apply( mean_pr_year_pres_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1, 2), mean, na.rm=T)
mean_pr_fut <- 86400*apply( mean_pr_year_fut_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1, 2), mean, na.rm=T)
mean_prchange <- 86400*apply(mean_pr_year_change_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1,2), mean,na.rm=T)


pdf("FigureS8.pdf", height=9)
layout(matrix(1:2, 2,1))
image.plot(lon-180, lat[lowlat:highlat], mean_mrsos_change[ c( (length(lon)/2+1):length(lon),
1:(length(lon)/2)),lowlat:highlat],  zlim=c(-5,5),
col=col_custom(20)[20:1], breaks=seq(-5,5,by=0.5), xlab="", ylab="",axis.args=list(cex.axis=1.5), cex.main=1.3,
main=expression(bold(paste(Delta,"MRSOS")))); map(add=T, interior=F)
mtext("a", side=3, adj=0, line=1, cex=1.5, font=2)

mean_corr_mrsos_tran <- apply(corr_mrsos_tran_year_2x2_allmodels[,,which(list_models_tran_and_mrsos_PCMDI_fut %in% list_models_common_SMstress)],
c(1,2), median,na.rm=T)
mean_corr_mrsos_tran_fut <- apply(corr_mrsos_tran_year_rcp85_2x2_allmodels[,,
which(list_models_tran_and_mrsos_PCMDI_fut %in% list_models_common_SMstress)],c(1,2), median,na.rm=T)
mean_corr_mrsos_tran_change <- mean_corr_mrsos_tran_fut - mean_corr_mrsos_tran

heatscatter(as.vector(mean_mrsos_change), as.vector(mean_corr_mrsos_tran_change), xlim=c(-5,5), ylim=c(-0.5, 0.5),
main="", xlab=expression(paste(Delta,"MRSOS")),ylab=expression(paste(Delta,"cor(MRSOS,TRAN)"))) 
abline(h=0, col="gray");abline(v=0, col="gray");

ul<- sum((areacella_2x2*mask_2x2_NAs)[which((mean_mrsos_change<0)&(mean_corr_mrsos_tran_change>0))],na.rm=T)/
 sum((areacella_2x2*mask_2x2_NAs)[which((is.na(mean_mrsos_change)==F)&( is.na(mean_corr_mrsos_tran_change)==F))],na.rm=T)*100
ll<-sum((areacella_2x2*mask_2x2_NAs)[which((mean_mrsos_change<0)&(mean_corr_mrsos_tran_change<0))],na.rm=T)/
sum((areacella_2x2*mask_2x2_NAs)[which((is.na(mean_mrsos_change)==F)&( is.na(mean_corr_mrsos_tran_change)==F))],na.rm=T)*100

ur <- sum((areacella_2x2*mask_2x2_NAs)[which((mean_mrsos_change>0)&(mean_corr_mrsos_tran_change>0))],na.rm=T)/
sum((areacella_2x2*mask_2x2_NAs)[which((is.na(mean_mrsos_change)==F)&( is.na(mean_corr_mrsos_tran_change)==F))],na.rm=T)*100
lr <- sum((areacella_2x2*mask_2x2_NAs)[which((mean_mrsos_change>0)&(mean_corr_mrsos_tran_change<0))],na.rm=T)/
sum((areacella_2x2*mask_2x2_NAs)[which((is.na(mean_mrsos_change)==F)&( is.na(mean_corr_mrsos_tran_change)==F))],na.rm=T)*100

text(-4, 0.4, paste(round(ul,2),"%", sep=""), cex=1.3)
text(4, 0.4, paste(round(ur,2),"%", sep=""), cex=1.3)
text(-4, -0.4, paste(round(ll,2),"%", sep=""), cex=1.3)
text(4, -0.4, paste(round(lr,2),"%", sep=""), cex=1.3)
mtext("b", side=3, adj=0, line=1, cex=1.5, font=2)
dev.off()


##Correlation:
cor(as.vector(mean_mrsos_change), as.vector(mean_corr_mrsos_tran_change), use="complete.obs")
