# este programa lo que hace es graficar la energía cinética del disco,
# definido entre 2<r<7 y |z|<1

library('snapshot')

pdf("dens_vs_t_disco.pdf")

rho0 = 4.03e-12

rmin = 2.0
rmax = 7.0
zlim = 1.0

pose=snap.read.2('etacs/snapshot_000','POS ',debug=1,gas=1) 
rhoe=snap.read.2('etacs/snapshot_000','RHO ',debug=1,gas=1)

rhoe = rhoe*rho0

re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

rhoe = rhoe[which(rmin<re & re<rmax & ze<zlim)]

rhoemediana = median(rhoe)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # d

posc=snap.read.2('controlrun/snapshot_000','POS ',debug=1,gas=1) 
rhoc=snap.read.2('controlrun/snapshot_000','RHO ',debug=1,gas=1)

rhoc = rhoc*rho0

rc=sqrt(posc$x*posc$x+posc$y*posc$y)
zc = sqrt(posc$z*posc$z)

rhoc = rhoc[which(rmin<rc & rc<rmax & zc<zlim)]

rhocmediana = median(rhoc)
# --------------------------------------------------------------------------------
for (i in seq(10,90,10))
{
chartime=as.character(i)
namefilee=paste0('etacs/snapshot_0',chartime)

pose=snap.read.2(namefilee,'POS ',debug=1,gas=1)
rhoe=snap.read.2(namefilee,'RHO ',debug=1,gas=1)

rhoe = rhoe*rho0

re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

rhoe = rhoe[which(rmin<re & re<rmax & ze<zlim)]

rhoemediana = c(rhoemediana,median(rhoe))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
namefilec=paste0('controlrun/snapshot_0',chartime)
 
posc=snap.read.2(namefilec,'POS ',debug=1,gas=1)
rhoc=snap.read.2(namefilec,'RHO ',debug=1,gas=1)

rhoc = rhoc*rho0

rc=sqrt(posc$x*posc$x+posc$y*posc$y)
zc = sqrt(posc$z*posc$z)

rhoc = rhoc[which(rmin<rc & rc<rmax & zc<zlim)]

rhocmediana = c(rhocmediana,median(rhoc))
}
# ----------------------------------------------------------------------------
for (i in seq(100,350,10))
{
chartime=as.character(i)
namefilee=paste0('etacs/snapshot_',chartime)
pose=snap.read.2(namefilee,'POS ',debug=1,gas=1)
rhoe=snap.read.2(namefilee,'RHO ',debug=1,gas=1)

rhoe = rhoe*rho0

re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

rhoe = rhoe[which(rmin<re & re<rmax & ze<zlim)]

rhoemediana = c(rhoemediana,median(rhoe))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

namefilec=paste0('controlrun/snapshot_',chartime)
posc=snap.read.2(namefilec,'POS ',debug=1,gas=1)
rhoc=snap.read.2(namefilec,'RHO ',debug=1,gas=1)

rhoc = rhoc*rho0

rc=sqrt(posc$x*posc$x+posc$y*posc$y)
zc = sqrt(posc$z*posc$z)

rhoc = rhoc[which(rmin<rc & rc<rmax & zc<zlim)]

rhocmediana = c(rhocmediana,median(rhoc))
}

# para plotear directamente en R
# plot(seq(0,350,10),tempemediana,log="y",xlab="time", ylab="T",yaxt="n")
# plot(seq(0,350,10),tempcmediana,type="b", pch=22, col="blue", lty=2)


par(mar=c(5, 5, 3, 3))
plot(seq(0,350,10),rhocmediana,log="y",xlab="time",
     ylab=expression(paste(rho," ","[",g/cm^3,"]")),yaxt="n",type="b",pch=21,col="red",
     ylim=c(min(rhocmediana,rhoemediana),max(rhocmediana,rhoemediana)))
axis(2, col.axis="black", las=2, cex.axis=0.9, tck=-.01)

par(new=T)
plot(seq(0,350,10),rhoemediana,log="y",type="b", pch=22, col="blue",   
     lty=2,yaxt="n",ylab="",xlab="",axes=F,
     ylim=c(min(rhocmediana,rhoemediana),max(rhocmediana,rhoemediana)))
#axis(4, col.axis="blue", las=2, cex.axis=0.7, tck=-.01)

par(new=F)
mtext("control", side=4, line=-3, cex.lab=1,las=2, col="red" ,padj=-20)
mtext("etacs",   side=4, line=-3,           las=2, col="blue",padj = -19)

# # opcion 1
# par(mar=c(5, 4, 4, 8) + 0.1)
# plot(seq(0,90,10),tempemediana,log="y",xlab="time", ylab="T",yaxt="n",type="b",pch=21,col="red")
# axis(2, at=tempemediana,labels=format(tempemediana, scientific=TRUE, digits=3),col.axis="red", las=2, cex.axis=0.7, tck=-.01)
# par(new=T)
# plot(seq(0,90,10),tempcmediana,type="b", pch=22, col="blue", lty=2,yaxt="n",ylab="")
# axis(4, at=tempemediana,labels=format(tempemediana, scientific=TRUE, digits=3),col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
# par(new=F)
# 
# # opcion 2
# par(mar=c(5, 4, 4, 8) + 0.1)
# plot(seq(0,90,10),tempemediana,log="y",xlab="time", ylab="T",yaxt="n",type="b",pch=21,col="red")
# axis(2, col.axis="red", las=2, cex.axis=0.7, tck=-.01)
# par(new=T)
# plot(seq(0,90,10),tempcmediana,type="b", pch=22, col="blue", lty=2,yaxt="n",ylab="")
# axis(4, col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
# par(new=F)

title("Density inside the disk")
dev.off()
