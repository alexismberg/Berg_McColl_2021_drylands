
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


log_index <-  0 # make it 1 if we want log index...
print(paste("log_index=",log_index))


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

mean_pr <- 86400*apply( mean_pr_year_pres_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1, 2), mean, na.rm=T)
mean_pr_fut <- 86400*apply( mean_pr_year_fut_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1, 2), mean, na.rm=T)
mean_prchange <- 86400*apply(mean_pr_year_change_2x2_allmodels[,,which(list_models_common_fut %in% list_models_common_SMstress)], c(1,2), mean,na.rm=T)


####### Can we get a function for the drylands index: pr=f(corr(SM,Tran)
lat <- mask_2x2$y;lon <-  mask_2x2$x
lowlat <- min(which(lat > -60));highlat <- min(which(lat > 80)); mask <- mask_2x2_NAs
bin_corr_SM_Tran <- seq(-1,1, length.out=21)
bin_pr <- seq(0,12, by=0.5)
PrPET_year_binned_acrossmodels <- array(NA, dim=c(length(bin_corr_SM_Tran),length(bin_pr), 4000))
w <- array(0, dim=c(length(bin_corr_SM_Tran),length(bin_pr)))
for (i in 1:length(lon)){# print(i)
for (j in 1:length(lat)){
coord_x <- NULL; coord_y <- NULL
for (t in 1:(length(bin_corr_SM_Tran)-1)) {
if ( (is.na(mean_corr_mrsos_tran[i,j]*mask[i,j]) == F) && (bin_corr_SM_Tran[t] <  mean_corr_mrsos_tran[i,j]) &&
(mean_corr_mrsos_tran[i,j] < bin_corr_SM_Tran[t+1]))  {coord_x <- t} }
for (p in 1:(length(bin_pr)-1)) {
if ( (is.na(mask[i,j] * mean_pr[i,j] ) ==F) && (bin_pr[p] < mean_pr[i,j] ) && ( mean_pr[i,j] < bin_pr[p+1]))  {coord_y <- p} }
w[coord_x, coord_y] <- w[coord_x, coord_y]+1
PrPET_year_binned_acrossmodels[coord_x, coord_y, w[coord_x, coord_y]] <- mean_PrPET[i,j]}}
mean_PrPET_year_binned_acrossmodels <- apply(PrPET_year_binned_acrossmodels, c(1,2), mean, na.rm=T)
w_PrPET_year_binned_Pr_PET_acrossmodels  <- w
bill <-  mean_PrPET_year_binned_acrossmodels; bill[bill <= -5] <- -5; bill[bill >= 5] <- 5;
png("test.png")
image.plot(bin_corr_SM_Tran, bin_pr , bill, zlim=c(0,5), breaks=seq(0, 5, by=0.1), ylim=range(bin_pr), xlim=range(bin_corr_SM_Tran),
main="AI",cex.main=1.9, col=col_custom3(50)[50:1], xlab="cor(SM,Tran)",
ylab="pr", cex.axis=1.5, cex.lab=1.5,axis.args=list( cex.axis=1.5))
contour( bin_corr_SM_Tran,bin_pr, bill,levels=c(0.65), col=c("black"), lwd=2, add=T, drawlabels=F)
legend("topright", ncol=1, col=c("gold", "orange", "red", "black"), c("AI=0.05", "AI=0.2", "AI=0.5", "AI=0.65"), bty="n", lwd=1.2, cex=1.5)
#######
if (log_index==1) {
bob<- contourLines( bin_corr_SM_Tran,bin_pr, bill,levels=c(0.65))
x<- bob[[1]]$x ; y<- bob[[1]]$y
xx <- x[which(x>0)]; yy <- y[which(x>0)]
dat <- data.frame(x=xx, y=yy)
cost <- function( data, param) {
with(data, sum(  (  param[1]*log(x)+param[2]    -y)^2, na.rm=T)) }
result <- optim(par=c(1,1),cost, data=dat)
a <- result$par[1]; b <- result$par[2]
curve(  a*log(x) + b, add=T, col="black", lwd=2, lty=2) } else if (log_index ==0) {
bob<- contourLines( bin_corr_SM_Tran,bin_pr, bill,levels=c(0.65))
x<- bob[[1]]$x ; y<- bob[[1]]$y
dat <- data.frame(x=bob[[1]]$x, y=bob[[1]]$y)
cost <- function( data, param) {
with(data, sum(  (  param[1]*(x)+param[2]    -y)^2, na.rm=T)) }
result <- optim(par=c(1,1),cost, data=dat)
a <- result$par[1]; b <- result$par[2]
curve(  a*(x) + b, add=T, col="black", lwd=2, lty=2)}
######
dev.off()

a_MMM_withpr <- a
b_MMM_withpr <- b
print(paste("a_MMM_withpr=", a))
print(paste("b_MMM_withpr=", b))

if (log_index==1) {
mean_index_drylands_pres <- array(NA, dim=dim(mean_pr))
for (i in 1:length(lon)){
for (j in 1:length(lat)){
if ( (is.na(mean_corr_mrsos_tran[i,j])==F) && (mean_corr_mrsos_tran[i,j] < 0)){
mean_index_drylands_pres[i,j] <- mean_pr[i,j]  }  ##-  (a*log(mean_corr_mrsos_tran[i,j])+b) }
else if ((is.na(mean_corr_mrsos_tran[i,j])==F) && (mean_corr_mrsos_tran[i,j] > 0)) {
mean_index_drylands_pres[i,j]  <-  mean_pr[i,j] -  (a*log(mean_corr_mrsos_tran[i,j])+b) } }}
mean_index_drylands_fut <- array(NA, dim=dim(mean_pr))
for (i in 1:length(lon)){
for (j in 1:length(lat)){
if ( (is.na(mean_corr_mrsos_tran_fut[i,j])==F) && (mean_corr_mrsos_tran_fut[i,j] < 0)){
mean_index_drylands_fut[i,j] <- mean_pr_fut[i,j] } ##-  (a*log(mean_corr_mrsos_tran[i,j])+b)}
else if ((is.na(mean_corr_mrsos_tran_fut[i,j])==F) && (mean_corr_mrsos_tran_fut[i,j] > 0)) {
mean_index_drylands_fut[i,j]  <-  mean_pr_fut[i,j] -  (a*log(mean_corr_mrsos_tran_fut[i,j])+b) } }}
} else if (log_index==0) {
mean_index_drylands_pres <- array(NA, dim=dim(mean_pr))
for (i in 1:length(lon)){
for (j in 1:length(lat)){
if ( (is.na(mean_corr_mrsos_tran[i,j])==F) ){
mean_index_drylands_pres[i,j] <- mean_pr[i,j]   - (a*(mean_corr_mrsos_tran[i,j])+b) }}}
mean_index_drylands_fut <- array(NA, dim=dim(mean_pr))
for (i in 1:length(lon)){
for (j in 1:length(lat)){
if ( (is.na(mean_corr_mrsos_tran_fut[i,j])==F)  ) {
mean_index_drylands_fut[i,j] <- mean_pr_fut[i,j]  - (a*(mean_corr_mrsos_tran_fut[i,j])+b)} }}
}



buff<-sum((mask_2x2_NAs*areacella_2x2)[which(mean_PrPET < 0.65)], na.rm=T)/ sum((mask_2x2_NAs*areacella_2x2), na.rm=T)* 100
print(paste("Frac drylands AI pres=",buff))

buff<-sum((mask_2x2_NAs*areacella_2x2)[which(mean_PrPET_fut < 0.65)], na.rm=T)/ sum((mask_2x2_NAs*areacella_2x2), na.rm=T)* 100
print(paste("Frac drylands AI fut=",buff))

buff<-sum((mask_2x2_NAs*areacella_2x2)[which(mean_index_drylands_pres < 0)], na.rm=T)/ sum((mask_2x2_NAs*areacella_2x2), na.rm=T)* 100
print(paste("Frac drylands EI pres=",buff))
frac_drylands_EI_withpr_pres_MMM <- buff

buff<-sum((mask_2x2_NAs*areacella_2x2)[which(mean_index_drylands_fut < 0)], na.rm=T)/ sum((mask_2x2_NAs*areacella_2x2), na.rm=T)* 100
print(paste("Frac drylands EI fut=",buff))
frac_drylands_EI_withpr_fut_MMM <- buff





##################################################################################
##################################################################################
##################################################################################
##### Figure 1
lat <- mask_2x2$y; lon <-  mask_2x2$x
lowlat <- min(which(lat > -60)); highlat <- min(which(lat > 80))
pdf("FigureS12.pdf",  width=12, height=10)
layout(matrix(1:6, 3, 2, byrow=T));par(mar=c(2,3,3,4.5))
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
image.plot(lon-180, lat[lowlat:highlat], mean_pr[ c( (length(lon)/2+1):length(lon),
1:(length(lon)/2)),lowlat:highlat],  zlim=c(0,8),
col=brewer.pal(8, "YlGnBu"), breaks=seq(0,8,by=1), xlab="", ylab="",xaxt="n", yaxt="n",
axis.args=list(cex.axis=1.5), cex.main=1.9,main="Mean Pr and drylands")
bob <- mean_PrPET; bob[bob >4 ] <- 4;  bob[is.na(bob)==T] <- 1000
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
bob[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(0.65),
 col="darkred", lwd=1, add=T, drawlabels=F); map(add=T,interior=F); mtext("b", side=3, adj=0, line=1, cex=1.5, font=2)
legend(-50,-40, ncol=1, col=c("darkred"), c("Drylands (AI=0.65)"), bty="n", lwd=1.2, cex=1.7)
#### c
image.plot(lon-180, lat[lowlat:highlat], mean_corr_mrsos_tran[ c( (length(lon)/2+1):length(lon),
1:(length(lon)/2)),lowlat:highlat],  zlim=c(-1,1),
col=col_custom(20), breaks=seq(-1,1,by=0.1), xlab="", ylab="",xaxt="n", yaxt="n",
axis.args=list(cex.axis=1.5), cex.main=1.9,main="Mean cor(SM,Tran) and drylands")
bob <- mean_PrPET; bob[bob >4 ] <- 4;  bob[is.na(bob)==T] <- 1000
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
bob[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(0.65),
 col="darkred", lwd=1, add=T, drawlabels=F); map(add=T,interior=F); mtext("c", side=3, adj=0, line=1, cex=1.5, font=2)
legend(-50, -40, ncol=1, col=c("darkred"), c("Drylands, AI=0.65"), bty="n", lwd=1.2, cex=1.7)
#### d
par(mar=c(4,6.5,3,1))
plot(mean_PrPET, mean_pr, xlim=c(0,5), ylim=c(0,20),yaxt="n", cex.axis=1.5, xlab="", ylab="",cex.lab=1.5,cex.main=1.9,main="", pch=".");
mtext(side=1, line=2.2, cex=1.5, "AI", at=2.5)
mtext(side=2, line=3, cex=1.4, "Pr", at=6)
mtext(side=2, line=3, cex=1.4, "cor(SM,Tran)", at=16)
axis(side=2, at=c(1:20),   labels=F) #     cex.axis=1.3, labels=c(1:7, c(-1, -0.75,-0.5,-0.25, 0,0.25, 0.50, 0.75, 1)))
axis(side=2, at=c(1:12,14,16,18), cex.axis=1.4, labels=c(1:12, c(-0.5, 0,0.5)), las=1)
abline(h=12, lwd=2); abline(h=12, col="gray")
abline(v=0.05, col="gold");abline(v=0.2, col="orange");abline(v=0.5, col="red");
abline(v=0.65, col="darkred");mtext("d", side=3, adj=0, line=1, cex=1.5, font=2)
legend("topright", ncol=1, col=c("gold", "orange", "red", "black"), c("AI=0.05", "AI=0.2", "AI=0.5", "AI=0.65"), bty="n", lwd=1.2, cex=1.5)
lines(type="p", mean_PrPET, mean_corr_mrsos_tran*4+16, pch=".")
#### e: Binned plot, AI=f(pr,cor(SM,Tran))
par(mar=c(4,4.5,3,5))
lat <- mask_2x2$y;lon <-  mask_2x2$x
lowlat <- min(which(lat > -60));highlat <- min(which(lat > 80)); mask <- mask_2x2_NAs
#bin_corr_SM_Tran <- seq(-1,1, length.out=21)
#bin_pr <- seq(0,12, by=0.5)
PrPET_year_binned_acrossmodels <- array(NA, dim=c(length(bin_corr_SM_Tran),length(bin_pr), 4000))
w <- array(0, dim=c(length(bin_corr_SM_Tran),length(bin_pr)))
for (i in 1:length(lon)){ #print(i)
for (j in 1:length(lat)){
coord_x <- NULL; coord_y <- NULL
for (t in 1:(length(bin_corr_SM_Tran)-1)) {
if ( (is.na(mean_corr_mrsos_tran[i,j]*mask[i,j]) == F) && (bin_corr_SM_Tran[t] <  mean_corr_mrsos_tran[i,j]) &&
(mean_corr_mrsos_tran[i,j] < bin_corr_SM_Tran[t+1]))  {coord_x <- t} }
for (p in 1:(length(bin_pr)-1)) {
if ( (is.na(mask[i,j] * mean_pr[i,j] ) ==F) && (bin_pr[p] < mean_pr[i,j] ) && ( mean_pr[i,j] < bin_pr[p+1]))  {coord_y <- p} }
w[coord_x, coord_y] <- w[coord_x, coord_y]+1
PrPET_year_binned_acrossmodels[coord_x, coord_y, w[coord_x, coord_y]] <- mean_PrPET[i,j]}}
mean_PrPET_year_binned_acrossmodels <- apply(PrPET_year_binned_acrossmodels, c(1,2), mean, na.rm=T)
w_PrPET_year_binned_Pr_PET_acrossmodels  <- w
bill <-  mean_PrPET_year_binned_acrossmodels
bill[bill <= -5] <- -5; bill[bill >= 5] <- 5;
image.plot(bin_corr_SM_Tran, bin_pr , bill, zlim=c(0,5), breaks=seq(0, 5, by=0.1), ylim=range(bin_pr), xlim=range(bin_corr_SM_Tran), main="AI",cex.main=1.9, col=col_custom3(50)[50:1], xlab="cor(SM,Tran)", 
ylab="Pr (mm/d)", cex.axis=1.5, cex.lab=1.9,axis.args=list( cex.axis=1.5))
contour( bin_corr_SM_Tran,bin_pr, bill,levels=c(0.05, 0.2, 0.5, 0.65), col=c("gold", "orange", "red", "black"), lwd=2, add=T, drawlabels=F)
legend("topright", ncol=1, col=c("gold", "orange", "red", "black"), c("AI=0.05", "AI=0.2", "AI=0.5", "AI=0.65"), bty="n", lwd=1.2, cex=1.5)
abline(v=0)
mtext("e", side=3, adj=0, line=1, cex=1.5, font=2)
if (log_index==1) { curve(  a_MMM_withpr*log(x) + b_MMM_withpr, add=T, col="black", lwd=2, lty=2)} else 
if (log_index==0) { curve(  a*(x) + b, add=T, col="black", lwd=2, lty=2)}

#### f
par(mar=c(2,3,3,4.5))
image.plot(lon-180, lat[lowlat:highlat], mean_index_drylands_pres[ c( (length(lon)/2+1):length(lon),
1:(length(lon)/2)),lowlat:highlat],  zlim=c(-8,8),
col=col_custom(32)[32:1], breaks=seq(-8,8,by=0.5), xlab="", ylab="",xaxt="n", yaxt="n",
axis.args=list(cex.axis=1.5), cex.main=1.9,main="Mean EI' and drylands")
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
mean_index_drylands_pres[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(0),
 col="blue", lwd=1, add=T, drawlabels=F); map(add=T,interior=F)
bob <- mean_PrPET; bob[bob >4 ] <- 4;  bob[is.na(bob)==T] <- 1000
contour(seq(-180,180, length.out=180), seq(lat[lowlat],lat[highlat],length.out=length(lowlat:highlat)),
bob[ c( (length(lon)/2+1):length(lon), 1:(length(lon)/2)),lowlat:highlat],levels=c(0.65),
 col="red", lwd=1, add=T, drawlabels=F); map(add=T,interior=F)
legend(-50,-30, ncol=1, col=c("blue", "red"), c("Drylands (EI'=0)", "Drylands (AI=0.65)"), bty="n", lwd=1.2, cex=1.7)
mtext("f", side=3, adj=0, line=1, cex=1.5, font=2)

dev.off()

########################################################################

