pdf("FigureS3_new_withEI.pdf",  width=5, height=10)
layout(matrix(1:3,3,1)); par(mar=c(4,6,3,4))
### Binned lai
bill <-  mean_laichange_year_binned_Pr_PET_MMM
bill[bill <= -1.5] <- -1.5; bill[bill >= 1.5] <- 1.5;
image.plot( bin_pr,bin_PET , bill, zlim=c(-1.5,1.5), breaks=seq(-1.5, 1.5, by=0.05), ylim=range(bin_PET), xlim=range(bin_pr),
main=expression(paste(Delta,"LAI",sep="")), col=col_custom3(60)[60:1], cex.main=2,
xlab=expression(paste(Delta,"P (mm/d)")), ylab=expression(paste(Delta,"PET (mm/d)")), cex.axis=1.7, cex.lab=1.7,
axis.args=list( cex.axis=1.7)); abline(h=0); abline(v=0);
mtext("a", side=3, at=-2, line=1, cex=1.5, font=2)
### Binned cor(SM,Tran)
bill <-  mean_corrmrsostranchange_year_binned_Pr_PET_MMM
bill[bill <= -0.5] <- -0.5; bill[bill >= 0.5] <- 0.5;
image.plot( bin_pr,bin_PET , bill, zlim=c(-0.5,0.5), breaks=seq(-0.5, 0.5, by=0.05), ylim=range(bin_PET), xlim=range(bin_pr),
main=expression(paste(Delta,"cor(SM,Tran)" )), col=col_custom3(20), xlab=expression(paste(Delta,"P (mm/d)" )),
ylab=expression(paste(Delta,"PET (mm/d)" )), cex.axis=1.7, cex.lab=1.7,cex.main=2, axis.args=list( cex.axis=1.7));abline(h=0); abline(v=0);
mtext("b", side=3, at=-2, line=1, cex=1.5, font=2)
#### Density plot
bob <- w_laichange_year_binned_Pr_PET_MMM/sum(w_laichange_year_binned_Pr_PET_MMM)*100
bob[which(bob==0)] <- NA
image.plot( bin_pr,bin_PET ,bob, col=matlab.like(20),  ylim=range(bin_PET), xlim=range(bin_pr),
xlab=expression(paste(Delta,"P (mm/d)" )),ylab=expression(paste(Delta,"PET (mm/d)" )),
main="Density (% of land pixels)", font.main=1, cex.axis=1.7, cex.lab=1.7,cex.main=2, axis.args=list( cex.axis=1.7));abline(h=0); abline(v=0);
mtext("c", side=3, at=-2, line=1, cex=1.5, font=2)
dev.off()

