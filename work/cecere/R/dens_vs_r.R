library('snapshot')

# library("scatterplot3d")
# scatterplot3d(xsel, ysel, zsel,pch=".")

base='/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_'
N0=0           #tiempo inicial
N1=370         #tiempo final
step=1        #cada cuanto
NN=N1-N0+1
rr=paste0(rep(base,(NN-1)/step+1),sprintf("%03d",seq(N0,N1,step)))

pdf("dens_vs_r_time3.pdf")

for (ifile in 1:length(rr)){
  cada=rr[ifile]

  pos=snap.read.2(cada,'POS ')
  xsel=pos$x[which(45<pos$z & pos$z<55 & -200<pos$x & pos$x<200 & -200<pos$y & pos$y<200)]
  ysel=pos$y[which(45<pos$z & pos$z<55 & -200<pos$x & pos$x<200 & -200<pos$y & pos$y<200)]
  zsel=pos$z[which(45<pos$z & pos$z<55 & -200<pos$x & pos$x<200 & -200<pos$y & pos$y<200)]
  r = sqrt(xsel*xsel+ysel*ysel)
  hr=hist(r,plot=FALSE)
  
  plot(hr$mids, hr$counts,log="y",type="b",pch=21,col="red",lty=3,xlab="r", ylab="N", xlim=c(0,200),ylim=c(10,1000))

  texto <- c("t=", toString(ifile))

  legend("topright", toString(texto))

  title("Histograma z=50")
}



dev.off()
