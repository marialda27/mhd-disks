library('snapshot')

# library("scatterplot3d")
# scatterplot3d(xsel, ysel, zsel,pch=".")
# plot(xsel,zsel,xlim=c(-200,200),ylim=c(0,400))


base='/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_'
N0=0           #tiempo inicial
N1=350         #tiempo final
step=5        #cada cuanto
NN=N1-N0+1
rr=paste0(rep(base,(NN-1)/step+1),sprintf("%03d",seq(N0,N1,step)))

pdf("n_vs_t_z30_theta15.pdf")

pos=snap.read.2('/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_000','POS ')
zsel=pos$z[which(sqrt(pos$x*pos$x+pos$y*pos$y)/tan(15*pi/180)<pos$z & 20<pos$z & pos$z<40)]
nzsel = length(zsel)

for (ifile in 2:length(rr)){
  cada=rr[ifile]

  pos=snap.read.2(cada,'POS ')
# xsel=pos$x[which(sqrt(pos$x*pos$x+pos$y*pos$y)/tan(30*pi/180) < pos$z & 20<pos$z & pos$z < 40)]
# ysel=pos$y[which(sqrt(pos$x*pos$x+pos$y*pos$y)/tan(30*pi/180) < pos$z & 5<pos$z & pos$z < 400)]
  zsel=pos$z[which(sqrt(pos$x*pos$x+pos$y*pos$y)/tan(15*pi/180)<pos$z & 20<pos$z & pos$z<40)]

  nzsel = c(nzsel,length(zsel))
# plot(xsel,zsel,xlim=c(-20,20),ylim=c(0,40),pch=".")
# hz=hist(zsel,plot=FALSE)
# hz=hist(zsel,breaks=20,plot=FALSE)
  
#   plot(hz$mids, hz$counts,log="y",type="b",pch=21,col="red",lty=3,xlab="z", ylab="N", xlim=c(20,40),ylim=c(100,500))
# 
#   texto <- c("t=", toString((ifile-1)*5))

#   legend("topright", toString(texto))

#   title("Histograma cono theta=15")
}

plot(seq(0,350,5),nzsel,log="y",xlab="time",
     ylab=expression(N),yaxt="n",type="b",pch=21,col="red",
     ylim=c(min(nzsel),max(nzsel)))
axis(2, col.axis="black", las=2, cex.axis=0.9, tck=-.01)     

dev.off()
