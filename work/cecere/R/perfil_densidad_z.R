library('snapshot')

pos=snap.read.2('/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_100','POS ',debug=1,gas=1)
# pos=snap.read.2('/home/cecere/sshfs/is2/fstasys/MHD_Disc/controlrun/snapshot_200','POS ',debug=1,gas=1)
xp = pos$x
yp = pos$y
zp = pos$z

pdf("particulas_todo.pdf")
plot(xp,yp,pch=".",xlim=c(-3,3),ylim=c(-5,5))
plot(xp,zp,pch=".",xlim=c(-3,3),ylim=c(-5,5))
abline(h=2)
abline(h=1.5,col="blue")
abline(h=1.,col="yellow")
abline(h=0.5,col="red")
dev.off()

hz=hist(log(abs(zp)),plot=FALSE)
b=sort(abs(zp))  
b[95/100*length(zp)]

pdf("perfil_z_todo.pdf")
# , xlim=c(-5,5),ylim=c(1,1e7)
plot(10^hz$mids, hz$counts,log="yx",type="b",pch=21,col="red",lty=3,xlab="z", ylab="N")

abline(v=b[95/100*length(zp)] )
dev.off()

pdf("x_vs_z.pdf")
plot(xp,zp,pch=".",xlim=c(-3,3),ylim=c(-10,10))
abline(h=2)
abline(h=1.5,col="blue")
abline(h=1.,col="yellow")
abline(h=0.5,col="red")
dev.off()

radius = sqrt(xp*xp + yp*yp)
possel <- which(2 < radius & radius < 3) 

Lo que hice fue hacer un histograma del log(abs(z)) para las particulas que estan en 2<r<3. El grafico muestra: 10^hz$mids, hz$counts,log="yx". Luego hice un sort de 

plot(xp[possel],yp[possel],pch=".")
plot(xp[possel],zp[possel],pch=".",xlim=c(-3,3),ylim=c(-5,5))

# ,breaks=1000
hz=hist(log(abs(zp)),plot=FALSE)

b=sort(abs(zp))  
b[95/100*length(zp)] 

pdf("perfil_z.pdf")
# , xlim=c(-5,5),ylim=c(1,1e7)
plot(10^hz$mids, hz$counts,log="yx",type="b",pch=21,col="red",lty=3,xlab="z", ylab="N")

abline(v=b[95/100*length(zp)] )
dev.off()

# b[65/100*length(zp)]
# abline(v=b[65/100*length(zp)] )
