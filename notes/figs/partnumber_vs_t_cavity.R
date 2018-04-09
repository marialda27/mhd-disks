# este programa lo que hace es graficar la energía cinética del disco,
# definido entre 2<r<7 y |z|<1

library('snapshot')

pdf("partnumber_vs_t_cavity.pdf")

rho0 = 4.03e-12

rmin = 2.0
rmax = 7.0
zlim = 1.0

pose=snap.read.2('etacs/snapshot_000','POS ',debug=1,gas=1) 
rhoe=snap.read.2('etacs/snapshot_000','RHO ',debug=1,gas=1)
posc=snap.read.2('controlrun/snapshot_000','POS ',debug=1,gas=1) 
rhoc=snap.read.2('controlrun/snapshot_000','RHO ',debug=1,gas=1)

rhoe = rhoe*rho0
rhoeinitial=length(rhoe)
rhoc = rhoc*rho0
rhocinitial=length(rhoc)

re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)
rc=sqrt(posc$x*posc$x+posc$y*posc$y)
zc = sqrt(posc$z*posc$z)

rhoes = rhoe[which(rmin>re)]
rhoedisco=length(rhoes)
rhocs = rhoc[which(rmin>rc)]
rhocdisco=length(rhocs)

rhoemedianai = rhoedisco/rhoeinitial
rhoemedianac = rhoedisco/rhoeinitial
rhocmedianai = rhocdisco/rhocinitial
rhocmedianac = rhocdisco/rhocinitial
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # d

# --------------------------------------------------------------------------------
for (i in seq(10,90,10))
{
chartime=as.character(i)
namefilee=paste0('etacs/snapshot_0',chartime)
namefilec=paste0('controlrun/snapshot_0',chartime)

pose=snap.read.2(namefilee,'POS ',debug=1,gas=1)
rhoe=snap.read.2(namefilee,'RHO ',debug=1,gas=1)
posc=snap.read.2(namefilec,'POS ',debug=1,gas=1)
rhoc=snap.read.2(namefilec,'RHO ',debug=1,gas=1)

rhoe = rhoe*rho0
rhoeactual=length(rhoe)
rhoc = rhoc*rho0
rhocactual=length(rhoc)

re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)
rc=sqrt(posc$x*posc$x+posc$y*posc$y)
zc = sqrt(posc$z*posc$z)

rhoes = rhoe[which(rmin>re)]
rhoedisco=length(rhoes) 
rhocs = rhoc[which(rmin>rc)]
rhocdisco=length(rhocs) 

rhoemedianai = c(rhoemedianai,rhoedisco/rhoeinitial)
rhoemedianac = c(rhoemedianac,rhoedisco/rhoeactual)
rhocmedianai = c(rhocmedianai,rhocdisco/rhocinitial)
rhocmedianac = c(rhocmedianac,rhocdisco/rhocactual)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ----------------------------------------------------------------------------
for (i in seq(100,350,10))
{
chartime=as.character(i)
namefilee=paste0('etacs/snapshot_',chartime)
namefilec=paste0('controlrun/snapshot_',chartime)

pose=snap.read.2(namefilee,'POS ',debug=1,gas=1)
rhoe=snap.read.2(namefilee,'RHO ',debug=1,gas=1)
posc=snap.read.2(namefilec,'POS ',debug=1,gas=1)
rhoc=snap.read.2(namefilec,'RHO ',debug=1,gas=1)

rhoe = rhoe*rho0
rhoeactual=length(rhoe)
rhoc = rhoc*rho0
rhocactual=length(rhoc)

re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)
rc=sqrt(posc$x*posc$x+posc$y*posc$y)
zc = sqrt(posc$z*posc$z)

rhoes = rhoe[which(rmin>re)]
rhoedisco=length(rhoes) 
rhocs = rhoc[which(rmin>rc)]
rhocdisco=length(rhocs) 

rhoemedianai = c(rhoemedianai,rhoedisco/rhoeinitial)
rhoemedianac = c(rhoemedianac,rhoedisco/rhoeactual)
rhocmedianai = c(rhocmedianai,rhocdisco/rhocinitial)
rhocmedianac = c(rhocmedianac,rhocdisco/rhocactual)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# para plotear directamente en R
# plot(seq(0,350,10),tempemediana,log="y",xlab="time", ylab="T",yaxt="n")
# plot(seq(0,350,10),tempcmediana,type="b", pch=22, col="blue", lty=2)


par(mar=c(5, 5, 3, 3))
plot(seq(0,350,10),rhoemedianai,log="y",xlab="time",
     ylab=expression(n[c]/n[x]),yaxt="n",type="b",pch=21,col="blue",
     ylim=c(min(rhoemedianai,rhoemedianac,rhocmedianai,rhocmedianac),max(rhoemedianai,rhoemedianac,rhocmedianai,rhocmedianac)))
axis(2, col.axis="black", las=2, cex.axis=0.9, tck=-.01)

par(new=T)
plot(seq(0,350,10),rhoemedianac,log="y",type="b", pch=22, col="green",   
     lty=2,yaxt="n",ylab="",xlab="",axes=F,
     ylim=c(min(rhoemedianai,rhoemedianac,rhocmedianai,rhocmedianac),max(rhoemedianai,rhoemedianac,rhocmedianai,rhocmedianac)))
#axis(4, col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
par(new=T)
plot(seq(0,350,10),rhocmedianai,log="y",type="b", pch=23, col="red",   
     lty=2,yaxt="n",ylab="",xlab="",axes=F,
     ylim=c(min(rhoemedianai,rhoemedianac,rhocmedianai,rhocmedianac),max(rhoemedianai,rhoemedianac,rhocmedianai,rhocmedianac)))

par(new=T)
plot(seq(0,350,10),rhocmedianac,log="y",type="b", pch=24, col="magenta",   
     lty=2,yaxt="n",ylab="",xlab="",axes=F,
     ylim=c(min(rhoemedianai,rhoemedianac,rhocmedianai,rhocmedianac),max(rhoemedianai,rhoemedianac,rhocmedianai,rhocmedianac)))

par(new=F)
mtext("Initial etacs ", side=4, line=-7, cex.lab=1,las=2, col="blue" ,padj=-20)
mtext("Current etacs",   side=4, line=-7,           las=2, col="green",padj = -19)
mtext("Initial control", side=4, line=-7, cex.lab=1,las=2, col="red" ,padj=-18)
mtext("Current control",   side=4, line=-7,           las=2, col="magenta",padj = -17)


title("Number of particles in cavity/Initial or Current total number")
dev.off()
