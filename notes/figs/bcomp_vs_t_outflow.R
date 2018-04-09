library('snapshot')

pdf("comp_em_vs_t_outflow.pdf")

pi=4.*atan(1)

be=snap.read.2('snapshot_000','BFLD',debug=1,gas=1) 
pos=snap.read.2('snapshot_000','POS ',debug=1,gas=1) 
r=sqrt(pos$x*pos$x+pos$y*pos$y)
z = sqrt(pos$z*pos$z)

br  = be$x*cos(atan2(pos$y,pos$x)) + be$y*sin(atan2(pos$y,pos$x))
bphi=-be$x*sin(atan2(pos$y,pos$x)) + be$y*cos(atan2(pos$y,pos$x))
bz  = be$z

br   =   br[which(r<2)]
bphi = bphi[which(r<2)]
bz   =   bz[which(r<2)]

br2 = 0.125*br*br/pi
bphi2 = 0.125*bphi*bphi/pi
bz2 = 0.125*bz*bz/pi

br2prom   = median(br2)
bphi2prom = median(bphi2)
bz2prom   = median(bz2)

for (i in seq(10,90,10))
{
chartime=as.character(i)
namefile=paste0('snapshot_0',chartime)
be=snap.read.2(namefile,'BFLD',debug=1,gas=1) 
pos=snap.read.2(namefile,'POS ',debug=1,gas=1) 
r=sqrt(pos$x*pos$x+pos$y*pos$y)
z = sqrt(pos$z*pos$z)

br  = be$x*cos(atan2(pos$y,pos$x)) + be$y*sin(atan2(pos$y,pos$x))
bphi=-be$x*sin(atan2(pos$y,pos$x)) + be$y*cos(atan2(pos$y,pos$x))
bz  = be$z

br   =   br[which(r<2)]
bphi = bphi[which(r<2)]
bz   =   bz[which(r<2)]

br2 = 0.125*br*br/pi
bphi2 = 0.125*bphi*bphi/pi
bz2 = 0.125*bz*bz/pi

br2prom   = c(br2prom,median(br2))
bphi2prom = c(bphi2prom,median(bphi2))
bz2prom   = c(bz2prom,median(bz2))
}

for (i in seq(100,350,10))
{
chartime=as.character(i)
namefile=paste0('snapshot_',chartime)
be=snap.read.2(namefile,'BFLD',debug=1,gas=1) 
pos=snap.read.2(namefile,'POS ',debug=1,gas=1) 
r=sqrt(pos$x*pos$x+pos$y*pos$y)
z = sqrt(pos$z*pos$z)

br  = be$x*cos(atan2(pos$y,pos$x)) + be$y*sin(atan2(pos$y,pos$x))
bphi=-be$x*sin(atan2(pos$y,pos$x)) + be$y*cos(atan2(pos$y,pos$x))
bz  = be$z

br   =   br[which(r<2)]
bphi = bphi[which(r<2)]
bz   =   bz[which(r<2)]

br2 = 0.125*br*br/pi
bphi2 = 0.125*bphi*bphi/pi
bz2 = 0.125*bz*bz/pi

br2prom   = c(br2prom,median(br2))
bphi2prom = c(bphi2prom,median(bphi2))
bz2prom   = c(bz2prom,median(bz2))
}

par(mar=c(5, 5, 3, 3))
plot(seq(0,350,10),br2prom,log="y",xlab="time", ylab=expression(paste(E[mag]," ","[",erg/cm^3,"]" )),yaxt="n",type="b",pch=21,col="red",
 ylim=c(min(min(br2prom, bphi2prom), bz2prom),max(max(br2prom, bphi2prom), bz2prom)))
axis(2, las=2, cex.axis=0.7, tck=-.01)
par(new=T)
plot(seq(0,350,10),bphi2prom,log="y",type="b", pch=22, col="blue", lty=2,yaxt="n",ylab="",xlab="",axes=F,
ylim=c(min(min(br2prom, bphi2prom), bz2prom),max(max(br2prom, bphi2prom), bz2prom)))
par(new=F)
par(new=T)
# axis(4, col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
plot(seq(0,350,10),bz2prom,log="y",type="b", pch=22, col="green", lty=2,yaxt="n",ylab="",xlab="",axes=F,
ylim=c(min(min(br2prom, bphi2prom), bz2prom),max(max(br2prom, bphi2prom), bz2prom)))
par(new=F)
mtext(expression(B[r]),   side=4, line=-3, cex.lab=1,las=2, col="red",padj=15)
mtext(expression(B[phi]), side=4, line=-3,           las=2, col="blue",padj=14)
mtext(expression(B[z]),   side=4, line=-3,           las=2, col="green",padj=13)


title("Magnetic energy by components at r<2")
dev.off()
