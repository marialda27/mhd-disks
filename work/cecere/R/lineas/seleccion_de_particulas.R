


library('snapshot')

pos=snap.read.2('/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_370','POS ',debug=1,gas=1)
xp = pos$x
yp = pos$y
zp = pos$z

id=snap.read.2('/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_370','ID  ',debug=1,gas=1)

data <- data.frame(id,xp,yp,zp)

# me da el orden del array donde pasa esto
# possel <- which(-50.0 < xp & xp < 50. & -50.0 < yp & yp < 50.0 & 45.0 < zp & zp < 55.0)
# possel <- which(-100.0 < xp & xp < 100. & -100.0 < yp & yp < 100.0 & 145.0 < zp & zp < 155.0) 
possel <- which(-200.0 < xp & xp < 200. & -200.0 < yp & yp < 200.0 & 395.0 < zp & zp < 405.0) 

datapos=c(NULL,possel[1])
for (i in 2:length(possel)){
 datapos=c(datapos,possel[i])
}

# esto me da un muestreo de las posiciones del array
possam <- sample(datapos, 2, replace=FALSE)

# data[datapos,1] me devuelve los id de todo
# data[possam,1] me devuelve los id del sampleo

xpc = data[possam,2]  
ypc = data[possam,3]
zpc = data[possam,4]

x = xpc
y = ypc
z = zpc

# plot(x,y,xlim=c(-50,50),ylim=c(-50,50),type = "o", col = "red")
# plot(x,z,xlim=c(-50,50),ylim=c(0,60),type = "o", col = "red")

# library("scatterplot3d")
# scatterplot3d(x, y, z,pch=".",xlim=c(-50,50),ylim=c(-50,50),zlim=c(0,60))

base='/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_'
N0=370       #tiempo inicial
N1=0         #tiempo final
step=10      #cada cuanto
NN=N0-N1+1
rr=paste0(rep(base,(NN-1)/step+1),sprintf("%03d",rev(seq(N1,N0,step))))

num=length(possam)*((NN-1)/step+1)
cx = matrix( c(1:num), nrow=length(possam), ncol=(NN-1)/step+1,   byrow = TRUE)
cy = matrix( c(1:num), nrow=length(possam), ncol=(NN-1)/step+1,   byrow = TRUE)
cz = matrix( c(1:num), nrow=length(possam), ncol=(NN-1)/step+1,   byrow = TRUE)

for(ip in 1:length(possam)){
iii=0
 for (ifile in 1:length(rr)){
      cada=rr[ifile]
#   for (cada in rr){
     iii=iii+1
     print(cada)
     iid=snap.read.2(cada,'ID  ')
     po=snap.read.2(cada,'POS ')
     cx[ip,iii]=po$x[which(iid[] == data[possam,1,][ip])]
     cy[ip,iii]=po$y[which(iid[] == data[possam,1,][ip])]
     cz[ip,iii]=po$z[which(iid[] == data[possam,1,][ip])] 
  }
}

library("rgl")

cl <- rainbow(length(possam))
axes3d()
# axes3d(col='red', box=TRUE, labels=TRUE, ticks=TRUE) 
for (ip in 1:length(possam)){ 
  lines3d(cx[ip,], cy[ip,], cz[ip,], col = cl[ip], lwd = 2)
}
title3d('','','xlab','ylab','zlab')

