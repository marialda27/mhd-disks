library('snapshot')

pdf("enkin_vs_t_disco.pdf")

rho0 = 4.03e-12
v0 = 6.23e7

pose=snap.read.2('etacs/snapshot_000','POS ',debug=1,gas=1) 
rhoe=snap.read.2('etacs/snapshot_000','RHO ',debug=1,gas=1)
ve  =snap.read.2('etacs/snapshot_000','VEL ',debug=1,gas=1) 

rhoe = rhoe*rho0
ve = ve*v0

re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

vex=ve$x[which(2<re & re<7 & ze<1)]
vey=ve$y[which(2<re & re<7 & ze<1)]
vez=ve$z[which(2<re & re<7 & ze<1)]
modve2=vex*vex + vey*vey + vez*vez

rhoe = rhoe[which(2<re & re<7 & ze<1)]

entote = 0.5*rhoe*modve2

entotemediana = median(entote)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # d

posc=snap.read.2('controlrun/snapshot_000','POS ',debug=1,gas=1) 
rhoc=snap.read.2('controlrun/snapshot_000','RHO ',debug=1,gas=1)
vc  =snap.read.2('controlrun/snapshot_000','VEL ',debug=1,gas=1) 

rhoc = rhoc*rho0
vc = vc*v0

rc=sqrt(posc$x*posc$x+posc$y*posc$y)
zc = sqrt(posc$z*posc$z)

vcx=vc$x[which(2<rc & rc<7 & zc<1)]
vcy=vc$y[which(2<rc & rc<7 & zc<1)]
vcz=vc$z[which(2<rc & rc<7 & zc<1)]
modvc2=vcx*vcx + vcy*vcy + vcz*vcz

rhoc = rhoc[which(2<rc & rc<7 & zc<1)]

entotc = 0.5*rhoc*modvc2

entotcmediana = median(entotc)
# --------------------------------------------------------------------------------
for (i in seq(10,90,10))
{
chartime=as.character(i)
namefilee=paste0('etacs/snapshot_0',chartime)

pose=snap.read.2(namefilee,'POS ',debug=1,gas=1)
rhoe=snap.read.2(namefilee,'RHO ',debug=1,gas=1)
ve  =snap.read.2(namefilee,'VEL ',debug=1,gas=1)

rhoe = rhoe*rho0
ve = ve*v0

re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

vex=ve$x[which(2<re & re<7 & ze<1)]
vey=ve$y[which(2<re & re<7 & ze<1)]
vez=ve$z[which(2<re & re<7 & ze<1)]
modve2=vex*vex + vey*vey + vez*vez

rhoe = rhoe[which(2<re & re<7 & ze<1)]

entote = 0.5*rhoe*modve2

entotemediana = c(entotemediana,median(entote))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
namefilec=paste0('controlrun/snapshot_0',chartime)
 
posc=snap.read.2(namefilec,'POS ',debug=1,gas=1)
rhoc=snap.read.2(namefilec,'RHO ',debug=1,gas=1)
vc  =snap.read.2(namefilec,'VEL ',debug=1,gas=1)

rhoc = rhoc*rho0
vc = vc*v0

rc=sqrt(posc$x*posc$x+posc$y*posc$y)
zc = sqrt(posc$z*posc$z)

vcx=vc$x[which(2<rc & rc<7 & zc<1)]
vcy=vc$y[which(2<rc & rc<7 & zc<1)]
vcz=vc$z[which(2<rc & rc<7 & zc<1)]
modvc2=vcx*vcx + vcy*vcy + vcz*vcz

rhoc = rhoc[which(2<rc & rc<7 & zc<1)]
entotc = 0.5*rhoc*modvc2

entotcmediana = c(entotcmediana,median(entotc))
}
# ----------------------------------------------------------------------------
for (i in seq(100,350,10))
{
chartime=as.character(i)
namefilee=paste0('etacs/snapshot_',chartime)
pose=snap.read.2(namefilee,'POS ',debug=1,gas=1)
rhoe=snap.read.2(namefilee,'RHO ',debug=1,gas=1)
ve  =snap.read.2(namefilee,'VEL ',debug=1,gas=1)

rhoe = rhoe*rho0
ve = ve*v0

re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

vex=ve$x[which(2<re & re<7 & ze<1)]
vey=ve$y[which(2<re & re<7 & ze<1)]
vez=ve$z[which(2<re & re<7 & ze<1)]
modve2=vex*vex + vey*vey + vez*vez

rhoe = rhoe[which(2<re & re<7 & ze<1)]

entote = 0.5*rhoe*modve2

entotemediana = c(entotemediana,median(entote))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

namefilec=paste0('controlrun/snapshot_',chartime)
posc=snap.read.2(namefilec,'POS ',debug=1,gas=1)
rhoc=snap.read.2(namefilec,'RHO ',debug=1,gas=1)
vc  =snap.read.2(namefilec,'VEL ',debug=1,gas=1)

rhoc = rhoc*rho0
vc = vc*v0

rc=sqrt(posc$x*posc$x+posc$y*posc$y)
zc = sqrt(posc$z*posc$z)

vcx=vc$x[which(2<rc & rc<7 & zc<1)]
vcy=vc$y[which(2<rc & rc<7 & zc<1)]
vcz=vc$z[which(2<rc & rc<7 & zc<1)]
modvc2=vcx*vcx + vcy*vcy + vcz*vcz

rhoc = rhoc[which(2<rc & rc<7 & zc<1)]
entotc = 0.5*rhoc*modvc2

entotcmediana = c(entotcmediana,median(entotc))
}

# plot(seq(0,350,10),tempemediana,log="y",xlab="time", ylab="T",yaxt="n")
# plot(seq(0,350,10),tempcmediana,type="b", pch=22, col="blue", lty=2)

par(mar=c(5, 4, 4, 8) + 0.1)
plot(seq(0,350,10),entotcmediana,log="y",xlab="time", ylab=expression(E[kin]),yaxt="n",type="b",pch=21,col="red",ylim=c(min(entotcmediana,entotemediana),max(entotcmediana,entotemediana)))
axis(2, col.axis="red", las=2, cex.axis=0.7, tck=-.01)
par(new=T)
plot(seq(0,350,10),entotemediana,log="y",type="b", pch=22, col="blue", lty=2,yaxt="n",ylab="",xlab="",axes=F,ylim=c(min(entotcmediana,entotemediana),max(entotcmediana,entotemediana)))
#axis(4, col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
par(new=F)
mtext("control", side=4, line=3, cex.lab=1,las=2, col="red")
mtext("etacs",   side=4, line=3,           las=2, col="blue",at=c(6.5))

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

title("Kinetic energy inside the disk")
dev.off()
