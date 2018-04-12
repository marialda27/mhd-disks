library('snapshot')

# library("scatterplot3d")
# scatterplot3d(xsel, ysel, zsel,pch=".")
# plot(xsel,zsel,xlim=c(-200,200),ylim=c(0,400))


base='/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_'
N0=0           #tiempo inicial
N1=370         #tiempo final
step=5        #cada cuanto
NN=N1-N0+1
rr=paste0(rep(base,(NN-1)/step+1),sprintf("%03d",seq(N0,N1,step)))

pdf("dens_vs_r_theta30_zoom.pdf")

for (ifile in 1:length(rr)){
  cada=rr[ifile]

  pos=snap.read.2(cada,'POS ')
#   xsel=pos$x[which(sqrt(pos$x*pos$x+pos$y*pos$y)/tan(30*pi/180) < pos$z & 0<pos$z & pos$z < 40)]
#   ysel=pos$y[which(sqrt(pos$x*pos$x+pos$y*pos$y)/tan(30*pi/180) < pos$z & 5<pos$z & pos$z < 400)]
  zsel=pos$z[which(sqrt(pos$x*pos$x+pos$y*pos$y)/tan(30*pi/180) < pos$z & 0<pos$z & pos$z < 40)]
  
#   plot(xsel,zsel,xlim=c(-20,20),ylim=c(0,40),pch=".")
#   hz=hist(zsel,plot=FALSE)
  hz=hist(zsel,breaks=20,plot=FALSE)
  
  plot(hz$mids, hz$counts,log="y",type="b",pch=21,col="red",lty=3,xlab="z", ylab="N", xlim=c(0,40),ylim=c(5,500))
# 
  texto <- c("t=", toString((ifile-1)*5))

  legend("topright", toString(texto))

#   title("Histograma cono theta=15")
}



dev.off()
