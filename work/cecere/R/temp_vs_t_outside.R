library('snapshot')

pdf("temp_vs_t_outside.pdf")

ue=snap.read.2('etacs/snapshot_000','U   ',debug=1,gas=1) 
pose=snap.read.2('etacs/snapshot_000','POS ',debug=1,gas=1) 
re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

tempe=(5./3.-1.)*ue[which(re>7 & ze>1)]

tempemediana = median(tempe)

uc=snap.read.2('controlrun/snapshot_000','U   ',debug=1,gas=1) 
posc=snap.read.2('controlrun/snapshot_000','POS ',debug=1,gas=1) 
rc=sqrt(posc$x*posc$x+posc$y*posc$y)
zc = sqrt(posc$z*posc$z)

tempc=(5./3.-1.)*uc[which(rc>7 & zc>1)]

tempcmediana = median(tempc)

for (i in seq(10,90,10))
{
chartime=as.character(i)
namefilee=paste0('etacs/snapshot_0',chartime)
ue=snap.read.2(namefilee,'U   ',debug=1,gas=1) 
pose=snap.read.2(namefilee,'POS ',debug=1,gas=1) 
re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

tempe=(5./3.-1.)*ue[which(re>7 & ze>1)]

tempemediana = c(tempemediana,median(tempe))

namefilec=paste0('controlrun/snapshot_0',chartime)
uc=snap.read.2(namefilec,'U   ',debug=1,gas=1) 
posc=snap.read.2(namefilec,'POS ',debug=1,gas=1) 
rc=sqrt(posc$x*posc$x+posc$y*posc$y)
zc = sqrt(posc$z*posc$z)

tempc=(5./3.-1.)*uc[which(rc>7 & zc>1)]

tempcmediana = c(tempcmediana,median(tempc))
}

for (i in seq(100,130,10))
{
chartime=as.character(i)
namefilee=paste0('etacs/snapshot_',chartime)
ue=snap.read.2(namefilee,'U   ',debug=1,gas=1) 
pose=snap.read.2(namefilee,'POS ',debug=1,gas=1) 
re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

tempe=(5./3.-1.)*ue[which(re>7 & ze>1)]

tempemediana = c(tempemediana,median(tempe))

namefilec=paste0('controlrun/snapshot_',chartime)
uc=snap.read.2(namefilec,'U   ',debug=1,gas=1) 
posc=snap.read.2(namefilec,'POS ',debug=1,gas=1) 
rc=sqrt(posc$x*posc$x+posc$y*posc$y)
zc = sqrt(posc$z*posc$z)

tempc=(5./3.-1.)*uc[which(rc>7 & zc>1)]

tempcmediana = c(tempcmediana,median(tempc))
}

tempemediana = c(tempemediana,5.e-3)
tempcmediana = c(tempcmediana,5.e-3)

for (i in seq(150,320,10))
{
chartime=as.character(i)
namefilee=paste0('etacs/snapshot_',chartime)
ue=snap.read.2(namefilee,'U   ',debug=1,gas=1) 
pose=snap.read.2(namefilee,'POS ',debug=1,gas=1) 
re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

tempe=(5./3.-1.)*ue[which(re>7 & ze>1)]

tempemediana = c(tempemediana,median(tempe))

namefilec=paste0('controlrun/snapshot_',chartime)
uc=snap.read.2(namefilec,'U   ',debug=1,gas=1) 
posc=snap.read.2(namefilec,'POS ',debug=1,gas=1) 
rc=sqrt(posc$x*posc$x+posc$y*posc$y)
zc = sqrt(posc$z*posc$z)

tempc=(5./3.-1.)*uc[which(rc>7 & zc>1)]

tempcmediana = c(tempcmediana,median(tempc))
}

tempemediana = c(tempemediana,5.e-3)
tempcmediana = c(tempcmediana,5.e-3)

for (i in seq(340,350,10))
{
chartime=as.character(i)
namefilee=paste0('etacs/snapshot_',chartime)
ue=snap.read.2(namefilee,'U   ',debug=1,gas=1) 
pose=snap.read.2(namefilee,'POS ',debug=1,gas=1) 
re=sqrt(pose$x*pose$x+pose$y*pose$y)
ze = sqrt(pose$z*pose$z)

tempe=(5./3.-1.)*ue[which(re>7 & ze>1)]

tempemediana = c(tempemediana,median(tempe))

namefilec=paste0('controlrun/snapshot_',chartime)
uc=snap.read.2(namefilec,'U   ',debug=1,gas=1) 
posc=snap.read.2(namefilec,'POS ',debug=1,gas=1) 
rc=sqrt(posc$x*posc$x+posc$y*posc$y)
zc = sqrt(posc$z*posc$z)

tempc=(5./3.-1.)*uc[which(rc>7 & zc>1)]

tempcmediana = c(tempcmediana,median(tempc))
}

# plot(seq(0,350,10),tempemediana,log="y",xlab="time", ylab="T",yaxt="n")
# plot(seq(0,350,10),tempcmediana,type="b", pch=22, col="blue", lty=2)

par(mar=c(5, 4, 4, 8) + 0.1)
plot(seq(0,350,10),tempcmediana,log="y",xlab="time", ylab="T",yaxt="n",type="b",pch=21,col="red")
axis(2, col.axis="red", las=2, cex.axis=0.7, tck=-.01)
par(new=T)
plot(seq(0,350,10),tempemediana,type="b", pch=22, col="blue", lty=2,yaxt="n",ylab="",xlab="",log="y")
axis(4, col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
par(new=F)
mtext("control", side=4, line=3, cex.lab=1,las=2, col="red")
mtext("etacs",   side=4, line=3,           las=2, col="blue",at=c(0.0018))

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

title("Temperature outside the disk")
dev.off()
