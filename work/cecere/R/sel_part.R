load("z30t125dt1T200")
print(load("z30t125dt1T200"))

# esto me da un array de las posiciones del array de possam donde ocurre esto para t=0
arraypos <- which(-5 < cx[,1] & cx[,1] < 5 & -5 < cy[,1] & cy[,1] < 5 & 0 < cz[,1] & cz[,1] < 5)

# deberia ser lo mismo
cx[,1][arraypos]
cy[,1][arraypos]
cz[,1][arraypos]
posxsel2 <- cx[,1][which(-5 < cx[,1] & cx[,1] < 5 & -5 < cy[,1] & cy[,1] < 5 & 0 < cz[,1] & cz[,1] < 5)]
posysel2 <- cy[,1][which(-5 < cx[,1] & cx[,1] < 5 & -5 < cy[,1] & cy[,1] < 5 & 0 < cz[,1] & cz[,1] < 5)]
poszsel2 <- cz[,1][which(-5 < cx[,1] & cx[,1] < 5 & -5 < cy[,1] & cy[,1] < 5 & 0 < cz[,1] & cz[,1] < 5)]
# # # # # # # # # # # # # 

library('snapshot')

pos=snap.read.2('/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_125','POS ',debug=1,gas=1)
xp = pos$x
yp = pos$y
zp = pos$z

id=snap.read.2('/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_125','ID  ',debug=1,gas=1)

data <- data.frame(id,xp,yp,zp)

# # # # # # # # # # # # # # # # # # # # # # # # 

cxsel = NULL #c(1:length(arraypos))
cysel = NULL #c(1:length(arraypos))
czsel = NULL #c(1:length(arraypos))
for(ip in 1:length(arraypos)){
     cada="/home/cecere/sshfs/is2/fstasys/MHD_Disc/controlrun/snapshot_000"
     print(cada)
     iid=snap.read.2(cada,'ID  ')
     po=snap.read.2(cada,'POS ')
     ind=which(iid[] == data[possam[arraypos],1,][ip])
     if(length(ind) > 0){
      cxsel=c(cxsel,po$x[ind])
      cysel=c(cysel,po$y[ind])
      czsel=c(czsel,po$z[ind])
     }
  
}


# cxsel = c(1:length(arraypos))
# cysel = c(1:length(arraypos))
# czsel = c(1:length(arraypos))
# for(ip in 1:length(arraypos)){
#      cada="/home/cecere/sshfs/is2/fstasys/MHD_Disc/controlrun/snapshot_000"
#      print(cada)
#      iid=snap.read.2(cada,'ID  ')
#      po=snap.read.2(cada,'POS ')
#      ind=which(iid[] == data[possam[arraypos],1,][ip])
#      cxsel[ip]=po$x[ind]
#      cysel[ip]=po$y[ind]
#      czsel[ip]=po$z[ind]
#   
# }

pdf("1.pdf")
  plot(cxsel,cysel)
  plot(cxsel,czsel)
dev.off()

base='/home/cecere/sshfs/is2/fstasys/MHD_Disc/controlrun/snapshot_'
N0=0       #tiempo inicial
N1=200         #tiempo final
step=10      #cada cuanto
NN=N1-N0+1
rr=paste0(rep(base,(NN-1)/step+1),sprintf("%03d",seq(N0,N1,step)))
num=length(arraypos)*((NN-1)/step+1)
cxn = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)*0-10
cyn = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)*0-10
czn = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)*0-10

iii=0
for (ifile in 1:length(rr)){
     iii=iii+1
     cada=rr[ifile]
     print(cada)
     iid=snap.read.2(cada,'ID  ')
     po=snap.read.2(cada,'POS ')
  for(ip in 1:length(arraypos)){
     ind=which(iid[] == data[possam[arraypos],1,][ip])
     if(length(ind) > 0){
      cxn[ip,iii]=po$x[ind]
      cyn[ip,iii]=po$y[ind]
      czn[ip,iii]=po$z[ind]
     }
  }
}

# iii=0
# for (ifile in 1:length(rr)){
#      iii=iii+1
#      cada=rr[ifile]
#      print(cada)
#      iid=snap.read.2(cada,'ID  ')
#      po=snap.read.2(cada,'POS ')
#   for(ip in 1:length(arraypos)){
#      cxn[ip,iii]=po$x[which(iid[] == data[possam[arraypos],1,][ip])]
#      cyn[ip,iii]=po$y[which(iid[] == data[possam[arraypos],1,][ip])]
#      czn[ip,iii]=po$z[which(iid[] == data[possam[arraypos],1,][ip])] 
#   }
# }

library("rgl")

cl <- rainbow(length(arraypos))
axes3d()
for (ip in 1:length(arraypos)){ 
  lines3d(cxn[ip,], cyn[ip,], czn[ip,], col = cl[ip], lwd = 2)
}
title3d('','','xlab','ylab','zlab')

pdf("2.pdf")
cl <- rainbow(length(arraypos))
xmin=min(sqrt(cxn*cxn+cyn*cyn))
xmax=max(sqrt(cxn*cxn+cyn*cyn))
ymin=min(czn)
ymax=max(czn)
  plot(sqrt(cxn[1,]*cxn[1,]+cyn[1,]*cyn[1,]),czn[1,],type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
for (ip in 2:length(arraypos)){
  par(new=T)
  plot(sqrt(cxn[ip,]*cxn[ip,]+cyn[ip,]*cyn[ip,]),czn[ip,],type="l", axes = FALSE, , xlab = "", ylab = "", col = cl[ip],xlim=c(xmin,xmax),ylim=c(ymin,ymax))
}
dev.off()

#   plot(sqrt(cxn[ip,]*cxn[ip,]+cyn[ip,]*cyn[ip,]),czn[ip,],type="l")


base='/home/cecere/sshfs/is2/fstasys/MHD_Disc/controlrun/snapshot_'
N0=0       #tiempo inicial
N1=200         #tiempo final
step=10      #cada cuanto
NN=N1-N0+1
rr=paste0(rep(base,(NN-1)/step+1),sprintf("%03d",seq(N0,N1,step)))
num=length(arraypos)*((NN-1)/step+1)
eint = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)
B2   = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)
v2   = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)
K    = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)
xx   = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)
yy   = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)
zz   = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)






iii=0
for (ifile in 1:length(rr)){
     iii=iii+1
     cada=rr[ifile]
     print(cada)

     poss=snap.read.2(cada,'POS ',gas=1)
     u=snap.read.2(cada,'U   ',gas=1)
     b=snap.read.2(cada,'BFLD',gas=1)
     v=snap.read.2(cada,'VEL ',gas=1)     
     rho=snap.read.2(cada,'RHO ',gas=1)
     iid=snap.read.2(cada,'ID  ',gas=1)
     
  for(ip in 1:length(arraypos)){     
     xx[ip,iii]=poss$x[which(iid[] == data[possam[arraypos],1,][ip])]
     yy[ip,iii]=poss$y[which(iid[] == data[possam[arraypos],1,][ip])]
     zz[ip,iii]=poss$z[which(iid[] == data[possam[arraypos],1,][ip])]
     eint[ip,iii]=u[which(iid[] == data[possam[arraypos],1,][ip])]
     bx=b$x[which(iid[] == data[possam[arraypos],1,][ip])]
     by=b$y[which(iid[] == data[possam[arraypos],1,][ip])]
     bz=b$z[which(iid[] == data[possam[arraypos],1,][ip])]
     B2[ip,iii]=0.5*(bx*bx + by*by + bz*bz)
     rhoo=rho[which(iid[] == data[possam[arraypos],1,][ip])]
     vx=v$x[which(iid[] == data[possam[arraypos],1,][ip])]
     vy=v$y[which(iid[] == data[possam[arraypos],1,][ip])]
     vz=v$z[which(iid[] == data[possam[arraypos],1,][ip])]
     K[ip,iii]=0.5*rhoo*(vx*vx + vy*vy + vz*vz)
     v2[ip,iii]=0.5*(vx*vx + vy*vy + vz*vz)
     
  }
}

pdf("3.pdf")
cl <- rainbow(length(arraypos))
  ymin=min(eint)
  ymax=max(eint)
  plot(0:(length(rr)-1)*step,eint[1,],type="l",log="y",ylim=c(ymin,ymax))
for (ip in 2:length(arraypos)){
  par(new=T)
  plot(0:(length(rr)-1)*step,eint[ip,],type="l",log="y", axes = FALSE, , xlab = "", ylab = "", col = cl[ip],ylim=c(ymin,ymax))
}
dev.off()

cl <- rainbow(length(arraypos))
  plot(0:(length(rr)-1)*step,eint[1,],type="l",log="y",ylim=c(ymin,ymax))
for (ip in 2:length(arraypos)){
  par(new=T)
  plot(0:(length(rr)-1)*step,eint[ip,],type="l",log="y", axes = FALSE, , xlab = "", ylab = "", col = cl[ip],ylim=c(ymin,ymax))
}

pdf("4.pdf")
cl <- rainbow(length(arraypos))
  ymin=min(B2)
  ymax=max(B2)
  plot(0:(length(rr)-1)*step,B2[1,],type="l",log="y",ylim=c(ymin,ymax))
for (ip in 2:length(arraypos)){
  par(new=T)
  plot(0:(length(rr)-1)*step,B2[ip,],type="l",log="y", axes = FALSE, , xlab = "", ylab = "", col = cl[ip],ylim=c(ymin,ymax))
}
dev.off()

pdf("5.pdf")
cl <- rainbow(length(arraypos))
  ymin=min(K)
  ymax=max(K)
  plot(0:(length(rr)-1)*step,K[1,],type="l",log="y",ylim=c(ymin,ymax))
for (ip in 2:length(arraypos)){
  par(new=T)
  plot(0:(length(rr)-1)*step,K[ip,],type="l",log="y", axes = FALSE, , xlab = "", ylab = "", col = cl[ip],ylim=c(ymin,ymax))
}
dev.off()

pdf("6.pdf")
cl <- rainbow(length(arraypos))
  ymin=min(v2)
  ymax=max(v2)
  plot(0:(length(rr)-1)*step,v2[1,],type="l",log="y")
for (ip in 2:length(arraypos)){
  par(new=T)
  plot(0:(length(rr)-1)*step,v2[ip,],type="l",log="y", axes = FALSE, , xlab = "", ylab = "", col = cl[ip])
}
dev.off()


pdf("7.pdf")
mediaeint=NULL
for (it in 1:length(rr)){
mediaeint=c(mediaeint,median(eint[,it]))
}
plot(0:(length(rr)-1)*step,mediaeint,type="l",log="y")
dev.off()

pdf("8.pdf")
mediaB2=NULL
for (it in 1:length(rr)){
mediaB2=c(mediaB2,median(B2[,it]))
}
plot(0:(length(rr)-1)*step,mediaB2,type="l",log="y")
dev.off()

pdf("9.pdf")
mediaK=NULL
for (it in 1:length(rr)){
mediaK=c(mediaK,median(K[,it]))
}
plot(0:(length(rr)-1)*step,mediaK,type="l",log="y")
dev.off()

pdf("10.pdf")
mediav2=NULL
for (it in 1:length(rr)){
mediav2=c(mediav2,median(v2[,it]))
}
plot(0:(length(rr)-1)*step,mediav2,type="l",log="y")
dev.off()

