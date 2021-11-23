library(viridis)

thetam <- 1

Eks <- c(25,50,100,150,200)

Is <- seq(0,500,0.1)


cols <- turbo(6)

pdf('plots/relative_variation_Eks.pdf',height=4,width=5)
plot(-999,xlim=c(0,500),ylim=c(0,1),xlab='',ylab='')
  mtext(side=1,expression('Mixed Layer-Averaged Irradiance ['*mu*'mol Photons/m'^2*'/s]'),line=2.5)
  mtext(side=2,expression('Chl:C/(Chl:C)'['max']),line=2.5)
for(i in 1:length(Eks)){
  Istar <- Is/Eks[i]
  lines(Is, (thetam/Istar)*(1-exp(-Istar)),col=cols[i])
}
legend('topright',legend=c(expression(italic('E'['k'])~'=  25'),
                           expression(italic('E'['k'])~'=  50'),
                           expression(italic('E'['k'])~'= 100'),
                           expression(italic('E'['k'])~'= 150'),
                           expression(italic('E'['k'])~'= 200')),bty='n',lty=1,cex=0.9,col=cols[1:5])
dev.off()


