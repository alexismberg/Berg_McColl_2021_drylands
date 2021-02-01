

lat <- mask_2x2$y;lon <-  mask_2x2$x
lowlat <- min(which(lat > -60));highlat <- min(which(lat > 80)); mask <- mask_2x2_NAs
bin_corr_SM_Tran <- seq(-1,1, length.out=21)
bin_LAI <- seq(0,8, by=0.5)
PrPET_year_binned_acrossmodels <- array(NA, dim=c(length(bin_corr_SM_Tran),length(bin_LAI), 4000))
w <- array(0, dim=c(length(bin_corr_SM_Tran),length(bin_LAI)))
for (i in 1:length(lon)){ print(i)
for (j in 1:length(lat)){
coord_x <- NULL; coord_y <- NULL
for (t in 1:(length(bin_corr_SM_Tran)-1)) {
if ( (is.na(mean_corr_mrsos_tran[i,j]*mask[i,j]) == F) && (bin_corr_SM_Tran[t] <  mean_corr_mrsos_tran[i,j]) &&
(mean_corr_mrsos_tran[i,j] < bin_corr_SM_Tran[t+1]))  {coord_x <- t} }
for (p in 1:(length(bin_LAI)-1)) {
if ( (is.na(mask[i,j] * mean_lai[i,j] ) ==F) && (bin_LAI[p] < mean_lai[i,j] ) && ( mean_lai[i,j] < bin_LAI[p+1]))  {coord_y <- p} }
w[coord_x, coord_y] <- w[coord_x, coord_y]+1
PrPET_year_binned_acrossmodels[coord_x, coord_y, w[coord_x, coord_y]] <- mean_PrPET[i,j]}}
mean_PrPET_year_binned_acrossmodels <- apply(PrPET_year_binned_acrossmodels, c(1,2), mean, na.rm=T)
w_PrPET_year_binned_Pr_PET_acrossmodels  <- w
bill_pres <-  mean_PrPET_year_binned_acrossmodels
bill_pres[bill_pres <= -5] <- -5; bill_pres[bill_pres >= 5] <- 5;

lat <- mask_2x2$y;lon <-  mask_2x2$x
lowlat <- min(which(lat > -60));highlat <- min(which(lat > 80)); mask <- mask_2x2_NAs
bin_corr_SM_Tran <- seq(-1,1, length.out=21)
bin_LAI <- seq(0,8, by=0.5)
PrPET_year_binned_acrossmodels <- array(NA, dim=c(length(bin_corr_SM_Tran),length(bin_LAI), 4000))
w <- array(0, dim=c(length(bin_corr_SM_Tran),length(bin_LAI)))
for (i in 1:length(lon)){ print(i)
for (j in 1:length(lat)){
coord_x <- NULL; coord_y <- NULL
for (t in 1:(length(bin_corr_SM_Tran)-1)) {
if ( (is.na(mean_corr_mrsos_tran_fut[i,j]*mask[i,j]) == F) && (bin_corr_SM_Tran[t] <  mean_corr_mrsos_tran_fut[i,j]) &&
(mean_corr_mrsos_tran_fut[i,j] < bin_corr_SM_Tran[t+1]))  {coord_x <- t} }
for (p in 1:(length(bin_LAI)-1)) {
if ( (is.na(mask[i,j] * mean_lai_fut[i,j] ) ==F) && (bin_LAI[p] < mean_lai_fut[i,j] ) && ( mean_lai_fut[i,j] < bin_LAI[p+1]))  {coord_y <- p} }
w[coord_x, coord_y] <- w[coord_x, coord_y]+1
PrPET_year_binned_acrossmodels[coord_x, coord_y, w[coord_x, coord_y]] <- mean_PrPET_fut[i,j]}}
mean_PrPET_year_binned_acrossmodels <- apply(PrPET_year_binned_acrossmodels, c(1,2), mean, na.rm=T)
w_PrPET_year_binned_Pr_PET_acrossmodels  <- w
bill_fut <-  mean_PrPET_year_binned_acrossmodels
bill_fut[bill_fut <= -5] <- -5; bill_fut[bill_fut >= 5] <- 5;

pdf("FigureS2_new_withEI.pdf", width=8, height=6.5)
###
image.plot(bin_corr_SM_Tran, bin_LAI , bill_fut, zlim=c(0,5), breaks=seq(0, 5, by=0.1), ylim=range(bin_LAI), xlim=range(bin_corr_SM_Tran),
main="AI", col=col_custom3(50)[50:1], xlab="cor(SM,Tran)",
ylab="LAI", cex.main=1.5, cex.axis=1.3, cex.lab=1.3,axis.args=list( cex.axis=1.4)); abline(v=0)
contour( bin_corr_SM_Tran,bin_LAI, bill_pres,levels=c(0.65), col=c("black"), lwd=2, add=T, drawlabels=F)
contour( bin_corr_SM_Tran,bin_LAI, bill_fut,levels=c(0.65), col=c("black"), lty=2, lwd=2, add=T, drawlabels=F)
legend("topright", ncol=1, col=c("black", "black"), c("AI=0.65, pres", "AI=0.65, fut"), bty="n", lwd=1.2,lty=c(1,2), cex=1.5)
dev.off()






