
a <- a_MMM
b <- b_MMM
### XY plot over new drylands - Figure S1:
mean_EI_pres <- mean_lai - ( a*mean_corr_mrsos_tran + b)
mean_EI_fut <- mean_lai_fut - ( a*mean_corr_mrsos_tran_fut + b)
mean_EIchange <- mean_EI_fut - mean_EI_pres
pdf("FigureS1_new_withEI.pdf", width=7, height=7); #layout(matrix(1:2, 2,1))
x<- mean_PrPETchange[which( (mean_PrPET_fut < 0.65)  & (mean_PrPET >= 0.65)   )]
y<- mean_EIchange[which( (mean_PrPET_fut < 0.65)  & (mean_PrPET >= 0.65)   )]
plot( x,y,col="gray40", xlim=c(-0.6,0.4), ylim=c(-4,4), xlab= expression(paste(Delta,"AI")),
ylab=expression(paste(Delta,"EI")),cex.lab=1.5,cex.axis=1.5,cex.main=1.5,
main=expression(paste(Delta,"EI and ",Delta,"AI in new drylands")), pch=3, cex=0.5); abline(v=0);abline(h=0)
# Just new dryland pixels:
#lines( mean_PrPETchange_1pctCO2[which( (mean_PrPET_fut_1pctCO2 < 0.65  )],
#mean_EIchange[which((mean_PrPET_fut < 0.65)  & (mean_PrPET >= 0.65) )], col="gray40", type="p",pch=3, cex=0.5)
#abline(v=mean( mean_PrPETchange[which( mean_PrPET_fut < 0.65  )], na.rm=T), col="gray40", lty=3)
#abline(h=mean( mean_EIchange[which( mean_PrPET_fut < 0.65  )],na.rm=T), col="gray40", lty=3)
buff <- x; buff[x>0.4]<- NA;  buff[x < -0.6]<- NA;
mean(buff, na.rm=T)
abline(v=mean( buff, na.rm=T), col="gray40", lty=3)
histx<- hist(buff, breaks=seq(-0.6,0.4,length.out=30),plot=F)
buff <- y;
mean(buff, na.rm=T)
abline(h=mean( buff, na.rm=T), col="gray40", lty=3)
histy<- hist(buff, breaks=seq(-4,4,  length.out=30),plot=F)
lines(histx$mids, (histx$density)/max( histx$density)-4, col="gray40")
lines(  (histy$density)/max(histy$density)*0.1-0.6, histy$mids, col="gray40")
dev.off()






