library('snapshot')

pdf("entot_t_vs_t_outside.pdf")

rho0 = 4.03e-12
v0 = 6.23e7
u0 = v0*v0
m0 = 6.97e39


be    =snap.read.2('etacs/snapshot_000','BFLD',debug=1,gas=1)
ue    =snap.read.2('etacs/snapshot_000','U   ',debug=1,gas=1) 
pose  =snap.read.2('etacs/snapshot_000','POS ',debug=1,gas=1) 
rhoe  =snap.read.2('etacs/snapshot_000','RHO ',debug=1,gas=1)
ve    =snap.read.2('etacs/snapshot_000','VEL ',debug=1,gas=1)
masse =snap.read.2('etacs/snapshot_000','MASS',debug=1,gas=1)

rhoe  = rhoe*rho0
ue    = ue*u0
ve    = ve*v0
masse = masse*m0

re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

bex=be$x[which(2<re & ze>1)]
bey=be$y[which(2<re & ze>1)]
bez=be$z[which(2<re & ze>1)]

modbe2=bex*bex + bey*bey + bez*bez
enmage=0.125*modbe2/pi

vex=ve$x[which(2<re & ze>1)]
vey=ve$y[which(2<re & ze>1)]
vez=ve$z[which(2<re & ze>1)]
modve2=vex*vex + vey*vey + vez*vez

rhoe = rhoe[which(2<re & ze>1)]
ue   = rhoe*ue[which(2<re & ze>1)]
masse= masse[which(2<re & ze>1)]

entote = (enmage + ue + 0.5*rhoe*modve2)/rhoe*masse

entotemediana = median(entote)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # d

uc    =snap.read.2('controlrun/snapshot_000','U   ',debug=1,gas=1) 
posc  =snap.read.2('controlrun/snapshot_000','POS ',debug=1,gas=1) 
vc    =snap.read.2('controlrun/snapshot_000','VEL ',debug=1,gas=1) 
massc =snap.read.2('controlrun/snapshot_000','MASS',debug=1,gas=1)

uc    = uc*u0
vc    = vc*v0
massc = massc*m0

rc=sqrt(posc$x*posc$x+posc$y*posc$y)
zc = sqrt(posc$z*posc$z)

vcx=vc$x[which(2<rc & zc>1)]
vcy=vc$y[which(2<rc & zc>1)]
vcz=vc$z[which(2<rc & zc>1)]
modvc2=vcx*vcx + vcy*vcy + vcz*vcz

uc    = uc[which(2<rc & zc>1)]
massc = massc[which(2<rc & zc>1)]

entotc = (uc + 0.5*modvc2)*massc

entotcmediana = median(entotc)
# --------------------------------------------------------------------------------
for (i in seq(10,90,10))
{
chartime=as.character(i)
namefilee=paste0('etacs/snapshot_0',chartime)
ue    =snap.read.2(namefilee,'U   ',debug=1,gas=1) 
pose  =snap.read.2(namefilee,'POS ',debug=1,gas=1)
rhoe  =snap.read.2(namefilee,'RHO ',debug=1,gas=1)
be    =snap.read.2(namefilee,'BFLD',debug=1,gas=1)
ve    =snap.read.2(namefilee,'VEL ',debug=1,gas=1)
masse =snap.read.2(namefilee,'MASS',debug=1,gas=1)

rhoe = rhoe*rho0
ue = ue*u0
ve = ve*v0
masse = masse*m0

re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

bex=be$x[which(2<re & ze>1)]
bey=be$y[which(2<re & ze>1)]
bez=be$z[which(2<re & ze>1)]

modbe2=bex*bex + bey*bey + bez*bez
enmage=0.125*modbe2/pi

vex=ve$x[which(2<re & ze>1)]
vey=ve$y[which(2<re & ze>1)]
vez=ve$z[which(2<re & ze>1)]
modve2=vex*vex + vey*vey + vez*vez

rhoe = rhoe[which(2<re & ze>1)]
ue   = rhoe*ue[which(2<re & ze>1)]
masse= masse[which(2<re & ze>1)]

entote = (enmage + ue + 0.5*rhoe*modve2)/rhoe*masse

entotemediana = c(entotemediana,median(entote))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
namefilec=paste0('controlrun/snapshot_0',chartime)
uc    =snap.read.2(namefilec,'U   ',debug=1,gas=1) 
posc  =snap.read.2(namefilec,'POS ',debug=1,gas=1)
vc    =snap.read.2(namefilec,'VEL ',debug=1,gas=1)
massc =snap.read.2(namefilec,'MASS',debug=1,gas=1)

uc = uc*u0
vc = vc*v0
massc = massc*m0

rc=sqrt(posc$x*posc$x+posc$y*posc$y)
zc = sqrt(posc$z*posc$z)

vcx=vc$x[which(2<rc & zc>1)]
vcy=vc$y[which(2<rc & zc>1)]
vcz=vc$z[which(2<rc & zc>1)]
modvc2=vcx*vcx + vcy*vcy + vcz*vcz

uc   = uc[which(2<rc & zc>1)]
massc= massc[which(2<rc & zc>1)]

entotc = (uc + 0.5*modvc2)*massc

entotcmediana = c(entotcmediana,median(entotc))
}
# ----------------------------------------------------------------------------
for (i in seq(100,350,10))
{
chartime=as.character(i)
namefilee=paste0('etacs/snapshot_',chartime)
ue    =snap.read.2(namefilee,'U   ',debug=1,gas=1) 
pose  =snap.read.2(namefilee,'POS ',debug=1,gas=1)
rhoe  =snap.read.2(namefilee,'RHO ',debug=1,gas=1)
be    =snap.read.2(namefilee,'BFLD',debug=1,gas=1)
ve    =snap.read.2(namefilee,'VEL ',debug=1,gas=1)
masse =snap.read.2(namefilee,'MASS',debug=1,gas=1)

rhoe = rhoe*rho0
ue = ue*u0
ve = ve*v0
masse = masse*m0

re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

bex=be$x[which(2<re & ze>1)]
bey=be$y[which(2<re & ze>1)]
bez=be$z[which(2<re & ze>1)]

modbe2=bex*bex + bey*bey + bez*bez
enmage=0.125*modbe2/pi

vex=ve$x[which(2<re & ze>1)]
vey=ve$y[which(2<re & ze>1)]
vez=ve$z[which(2<re & ze>1)]
modve2=vex*vex + vey*vey + vez*vez

rhoe = rhoe[which(2<re & ze>1)]
ue   = rhoe*ue[which(2<re & ze>1)]
masse= masse[which(2<re & ze>1)]

entote = (enmage + ue + 0.5*rhoe*modve2)/rhoe*masse

entotemediana = c(entotemediana,median(entote))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

namefilec=paste0('controlrun/snapshot_',chartime)
uc    =snap.read.2(namefilec,'U   ',debug=1,gas=1) 
posc  =snap.read.2(namefilec,'POS ',debug=1,gas=1)
vc    =snap.read.2(namefilec,'VEL ',debug=1,gas=1)
massc =snap.read.2(namefilec,'MASS',debug=1,gas=1)

uc = uc*u0
vc = vc*v0
massc = massc*m0

rc=sqrt(posc$x*posc$x+posc$y*posc$y)
zc = sqrt(posc$z*posc$z)

vcx=vc$x[which(2<rc & zc>1)]
vcy=vc$y[which(2<rc & zc>1)]
vcz=vc$z[which(2<rc & zc>1)]
modvc2=vcx*vcx + vcy*vcy + vcz*vcz

uc   = uc[which(2<rc & zc>1)]
massc= massc[which(2<rc & zc>1)]

entotc = (uc + 0.5*modvc2)*massc

entotcmediana = c(entotcmediana,median(entotc))
}

# plot(seq(0,350,10),tempemediana,log="y",xlab="time", ylab="T",yaxt="n")
# plot(seq(0,350,10),tempcmediana,type="b", pch=22, col="blue", lty=2)

par(mar=c(5, 5, 3, 3))
plot(seq(0,350,10),entotcmediana,log="y",xlab="time", ylab=expression(paste(E[tot]," ","[",erg,"]" )),
     yaxt="n",type="b",pch=21,col="red",ylim=c(min(entotcmediana,entotemediana),max(entotcmediana,entotemediana)))
axis(2, col.axis="black", las=2, cex.axis=0.9, tck=-.01)
par(new=T)
plot(seq(0,350,10),entotemediana,log="y",type="b", pch=22, col="blue", lty=2,yaxt="n",ylab="",xlab="",axes=F,ylim=c(min(entotcmediana,entotemediana),max(entotcmediana,entotemediana)))
#axis(4, col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
par(new=F)
mtext("control", side=4, line=-3, cex.lab=1,las=2, col="red",padj=-20)
mtext("etacs",   side=4, line=-3,           las=2, col="blue",padj=-19)

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

title("Total energy outside the disk")
dev.off()
