library('snapshot')

pdf("enint_vs_t_disco.pdf")

rho0 = 4.03e-12
v0 = 6.23e7
u0 = v0*v0

ue  =snap.read.2('etacs/snapshot_000','U   ',debug=1,gas=1) 
pose=snap.read.2('etacs/snapshot_000','POS ',debug=1,gas=1) 
rhoe=snap.read.2('etacs/snapshot_000','RHO ',debug=1,gas=1)


rhoe = rhoe*rho0
ue = ue*u0

re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

rhoe = rhoe[which(2<re & re<7 & ze<1)]
ue   = rhoe*ue[which(2<re & re<7 & ze<1)]

entote = ue

entotemediana = median(entote)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # d

uc  =snap.read.2('controlrun/snapshot_000','U   ',debug=1,gas=1) 
posc=snap.read.2('controlrun/snapshot_000','POS ',debug=1,gas=1) 
rhoc=snap.read.2('controlrun/snapshot_000','RHO ',debug=1,gas=1)

rhoc = rhoc*rho0
uc = uc*u0

rc=sqrt(posc$x*posc$x+posc$y*posc$y)
zc = sqrt(posc$z*posc$z)

rhoc = rhoc[which(2<rc & rc<7 & zc<1)]
uc= rhoc*uc[which(2<rc & rc<7 & zc<1)]

entotc = uc

entotcmediana = median(entotc)
# --------------------------------------------------------------------------------
for (i in seq(10,90,10))
{
chartime=as.character(i)
namefilee=paste0('etacs/snapshot_0',chartime)
ue  =snap.read.2(namefilee,'U   ',debug=1,gas=1) 
pose=snap.read.2(namefilee,'POS ',debug=1,gas=1)
rhoe=snap.read.2(namefilee,'RHO ',debug=1,gas=1)

rhoe = rhoe*rho0
ue = ue*u0

re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

rhoe = rhoe[which(2<re & re<7 & ze<1)]
ue   = rhoe*ue[which(2<re & re<7 & ze<1)]
entote = ue 

entotemediana = c(entotemediana,median(entote))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
namefilec=paste0('controlrun/snapshot_0',chartime)
uc  =snap.read.2(namefilec,'U   ',debug=1,gas=1) 
posc=snap.read.2(namefilec,'POS ',debug=1,gas=1)
rhoc=snap.read.2(namefilec,'RHO ',debug=1,gas=1)

rhoc = rhoc*rho0
uc = uc*u0

rc=sqrt(posc$x*posc$x+posc$y*posc$y)
zc = sqrt(posc$z*posc$z)

rhoc = rhoc[which(2<rc & rc<7 & zc<1)]
uc= rhoc*uc[which(2<rc & rc<7 & zc<1)]
entotc = uc 

entotcmediana = c(entotcmediana,median(entotc))
}
# ----------------------------------------------------------------------------
for (i in seq(100,350,10))
{
chartime=as.character(i)
namefilee=paste0('etacs/snapshot_',chartime)
ue  =snap.read.2(namefilee,'U   ',debug=1,gas=1) 
pose=snap.read.2(namefilee,'POS ',debug=1,gas=1)
rhoe=snap.read.2(namefilee,'RHO ',debug=1,gas=1)

rhoe = rhoe*rho0
ue = ue*u0

re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

rhoe = rhoe[which(2<re & re<7 & ze<1)]
ue   = rhoe*ue[which(2<re & re<7 & ze<1)]
entote = ue

entotemediana = c(entotemediana,median(entote))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

namefilec=paste0('controlrun/snapshot_',chartime)
uc  =snap.read.2(namefilec,'U   ',debug=1,gas=1) 
posc=snap.read.2(namefilec,'POS ',debug=1,gas=1)
rhoc=snap.read.2(namefilec,'RHO ',debug=1,gas=1)

rhoc = rhoc*rho0
uc = uc*u0

rc=sqrt(posc$x*posc$x+posc$y*posc$y)
zc = sqrt(posc$z*posc$z)

rhoc = rhoc[which(2<rc & rc<7 & zc<1)]
uc= rhoc*uc[which(2<rc & rc<7 & zc<1)]
entotc = uc 

entotcmediana = c(entotcmediana,median(entotc))
}

# plot(seq(0,350,10),tempemediana,log="y",xlab="time", ylab="T",yaxt="n")
# plot(seq(0,350,10),tempcmediana,type="b", pch=22, col="blue", lty=2)

par(mar=c(5, 5, 3, 3))
plot(seq(0,350,10),entotcmediana,log="y",xlab="time", ylab=expression(paste(E[int]," ","[",erg/cm^3,"]" )),
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

title("Internal energy inside the disk")
dev.off()
