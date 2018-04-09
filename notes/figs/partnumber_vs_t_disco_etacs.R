# este programa lo que hace es graficar la energía cinética del disco,
# definido entre 2<r<7 y |z|<1

library('snapshot')

pdf("partnumber_vs_t_disco.pdf")

rho0 = 4.03e-12

rmin = 2.0
rmax = 7.0
zlim = 1.0

pose=snap.read.2('etacs/snapshot_000','POS ',debug=1,gas=1) 
rhoe=snap.read.2('etacs/snapshot_000','RHO ',debug=1,gas=1)

rhoe = rhoe*rho0
rhoeinitial=length(rhoe)

re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

rhoes = rhoe[which(rmin<re & re<rmax & ze<zlim)]
rhoedisco=length(rhoes) 

rhoemediana = rhoedisco/rhoeinitial
rhocmediana = rhoedisco/rhoeinitial
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # d

# --------------------------------------------------------------------------------
for (i in seq(10,90,10))
{
chartime=as.character(i)
namefilee=paste0('etacs/snapshot_0',chartime)

pose=snap.read.2(namefilee,'POS ',debug=1,gas=1)
rhoe=snap.read.2(namefilee,'RHO ',debug=1,gas=1)

rhoe = rhoe*rho0
rhoeactual=length(rhoe)

re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

rhoes = rhoe[which(rmin<re & re<rmax & ze<zlim)]
rhoedisco=length(rhoes) 

rhoemediana = c(rhoemediana,rhoedisco/rhoeinitial)
rhocmediana = c(rhocmediana,rhoedisco/rhoeactual)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ----------------------------------------------------------------------------
for (i in seq(100,350,10))
{
chartime=as.character(i)
namefilee=paste0('etacs/snapshot_',chartime)
pose=snap.read.2(namefilee,'POS ',debug=1,gas=1)
rhoe=snap.read.2(namefilee,'RHO ',debug=1,gas=1)

rhoe = rhoe*rho0
rhoeactual=length(rhoe)

re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

rhoes = rhoe[which(rmin<re & re<rmax & ze<zlim)]
rhoedisco=length(rhoes)

rhoemediana = c(rhoemediana,rhoedisco/rhoeinitial)
rhocmediana = c(rhocmediana,rhoedisco/rhoeactual)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# para plotear directamente en R
# plot(seq(0,350,10),tempemediana,log="y",xlab="time", ylab="T",yaxt="n")
# plot(seq(0,350,10),tempcmediana,type="b", pch=22, col="blue", lty=2)


par(mar=c(5, 5, 3, 3))
plot(seq(0,350,10),rhocmediana,log="y",xlab="time",
     ylab=expression(n[d]/n[x]),yaxt="n",type="b",pch=21,col="red",
     ylim=c(min(rhocmediana,rhoemediana),max(rhocmediana,rhoemediana)))
axis(2, col.axis="black", las=2, cex.axis=0.9, tck=-.01)

par(new=T)
plot(seq(0,350,10),rhoemediana,log="y",type="b", pch=22, col="blue",   
     lty=2,yaxt="n",ylab="",xlab="",axes=F,
     ylim=c(min(rhocmediana,rhoemediana),max(rhocmediana,rhoemediana)))
#axis(4, col.axis="blue", las=2, cex.axis=0.7, tck=-.01)

par(new=F)
mtext("Current", side=4, line=-3, cex.lab=1,las=2, col="red" ,padj=-20)
mtext("Initial",   side=4, line=-3,           las=2, col="blue",padj = -19)


title("Number of particles inside/Initial or Current total number")
dev.off()
