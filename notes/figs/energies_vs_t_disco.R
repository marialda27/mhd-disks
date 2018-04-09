library('snapshot')

pdf("energies_vs_t_disco.pdf")

rho0 = 4.03e-12
v0 = 6.23e7
u0 = v0*v0


be  =snap.read.2('etacs/snapshot_000','BFLD',debug=1,gas=1)
ue  =snap.read.2('etacs/snapshot_000','U   ',debug=1,gas=1) 
pose=snap.read.2('etacs/snapshot_000','POS ',debug=1,gas=1) 
rhoe=snap.read.2('etacs/snapshot_000','RHO ',debug=1,gas=1)
ve  =snap.read.2('etacs/snapshot_000','VEL ',debug=1,gas=1) 

rhoe = rhoe*rho0
ue = ue*u0
ve = ve*v0

re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

bex=be$x[which(2<re & re<7 & ze<1)]
bey=be$y[which(2<re & re<7 & ze<1)]
bez=be$z[which(2<re & re<7 & ze<1)]

modbe2=bex*bex + bey*bey + bez*bez
enmage=0.125*modbe2/pi

vex=ve$x[which(2<re & re<7 & ze<1)]
vey=ve$y[which(2<re & re<7 & ze<1)]
vez=ve$z[which(2<re & re<7 & ze<1)]
modve2=vex*vex + vey*vey + vez*vez

rhoe    = rhoe[which(2<re & re<7 & ze<1)]
ue      = rhoe*ue[which(2<re & re<7 & ze<1)]
encine  = 0.5*rhoe*modve2

entote = enmage + ue + encine

enmagemediana = median(enmage)
encinemediana = median(encine)
enintemediana = median(ue)
entotemediana = median(entote)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # d

# --------------------------------------------------------------------------------
for (i in seq(10,90,10))
{
chartime=as.character(i)
namefilee=paste0('etacs/snapshot_0',chartime)
ue  =snap.read.2(namefilee,'U   ',debug=1,gas=1) 
pose=snap.read.2(namefilee,'POS ',debug=1,gas=1)
rhoe=snap.read.2(namefilee,'RHO ',debug=1,gas=1)
be  =snap.read.2(namefilee,'BFLD',debug=1,gas=1)
ve  =snap.read.2(namefilee,'VEL ',debug=1,gas=1)

rhoe = rhoe*rho0
ue = ue*u0
ve = ve*v0

re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

bex=be$x[which(2<re & re<7 & ze<1)]
bey=be$y[which(2<re & re<7 & ze<1)]
bez=be$z[which(2<re & re<7 & ze<1)]

modbe2=bex*bex + bey*bey + bez*bez
enmage=0.125*modbe2/pi

vex=ve$x[which(2<re & re<7 & ze<1)]
vey=ve$y[which(2<re & re<7 & ze<1)]
vez=ve$z[which(2<re & re<7 & ze<1)]
modve2=vex*vex + vey*vey + vez*vez

rhoe    = rhoe[which(2<re & re<7 & ze<1)]
ue      = rhoe*ue[which(2<re & re<7 & ze<1)]
encine  = 0.5*rhoe*modve2

entote = enmage + ue + encine

enmagemediana = c(enmagemediana,median(enmage))
encinemediana = c(encinemediana,median(encine))
enintemediana = c(enintemediana,median(ue))
entotemediana = c(entotemediana,median(entote))
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ----------------------------------------------------------------------------
for (i in seq(100,350,10))
{
chartime=as.character(i)
namefilee=paste0('etacs/snapshot_',chartime)
ue  =snap.read.2(namefilee,'U   ',debug=1,gas=1) 
pose=snap.read.2(namefilee,'POS ',debug=1,gas=1)
rhoe=snap.read.2(namefilee,'RHO ',debug=1,gas=1)
be  =snap.read.2(namefilee,'BFLD',debug=1,gas=1)
ve  =snap.read.2(namefilee,'VEL ',debug=1,gas=1)

rhoe = rhoe*rho0
ue = ue*u0
ve = ve*v0

re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

bex=be$x[which(2<re & re<7 & ze<1)]
bey=be$y[which(2<re & re<7 & ze<1)]
bez=be$z[which(2<re & re<7 & ze<1)]

modbe2=bex*bex + bey*bey + bez*bez
enmage=0.125*modbe2/pi

vex=ve$x[which(2<re & re<7 & ze<1)]
vey=ve$y[which(2<re & re<7 & ze<1)]
vez=ve$z[which(2<re & re<7 & ze<1)]
modve2=vex*vex + vey*vey + vez*vez

rhoe    = rhoe[which(2<re & re<7 & ze<1)]
ue      = rhoe*ue[which(2<re & re<7 & ze<1)]
encine  = 0.5*rhoe*modve2

entote = enmage + ue + encine

enmagemediana = c(enmagemediana,median(enmage))
encinemediana = c(encinemediana,median(encine))
enintemediana = c(enintemediana,median(ue))
entotemediana = c(entotemediana,median(entote))
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

par(mar=c(5, 5, 3, 3))
plot(seq(0,350,10),enmagemediana,log="y",xlab="time", ylab=expression(paste(E," ","[",erg/cm^3,"]" )),
     yaxt="n",type="b",pch=21,col="red",ylim=c(min(enmagemediana,encinemediana,enintemediana,entotemediana),max(enmagemediana,encinemediana,enintemediana,entotemediana)))
axis(2, col.axis="black", las=2, cex.axis=0.9, tck=-.01)

par(new=T)
plot(seq(0,350,10),encinemediana,log="y",type="b", pch=22, col="blue", lty=2,yaxt="n",ylab="",xlab="",axes=F,ylim=c(min(enmagemediana,encinemediana,enintemediana,entotemediana),max(enmagemediana,encinemediana,enintemediana,entotemediana)))

par(new=T)
plot(seq(0,350,10),enintemediana,log="y",type="b", pch=23, col="green", lty=2,yaxt="n",ylab="",xlab="",axes=F,ylim=c(min(enmagemediana,encinemediana,enintemediana,entotemediana),max(enmagemediana,encinemediana,enintemediana,entotemediana)))

par(new=T)
plot(seq(0,350,10),entotemediana,log="y",type="b", pch=24, col="magenta", lty=2,yaxt="n",ylab="",xlab="",axes=F,ylim=c(min(enmagemediana,encinemediana,enintemediana,entotemediana),max(enmagemediana,encinemediana,enintemediana,entotemediana)))

par(new=F)
mtext(expression(E[mag]), side=4, line=-3, cex.lab=1,las=2, col="red",padj=14)
mtext(expression(E[kin]),   side=4, line=-3,           las=2, col="blue",padj=13)
mtext(expression(E[int]),   side=4, line=-3,           las=2, col="green",padj=12)
mtext(expression(E[tot]),   side=4, line=-3,           las=2, col="magenta",padj=11)



title("Energy densities inside the disk")
dev.off()
