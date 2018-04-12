# reconociendo las que se fueron de etacs
# EJEMPLO SIMPLE
a = c(10,3,5,1,9,8,6,2,4,7)   # dato inicial
b = c(1,5,6,7,8,3)            # no esta el 2,4,9,10
where <- match( a, b )
where[ is.na( where ) ] <- 0   #pone ceros donde no hay coincidencia
# me devuelve un array con las posiciones donde a se hace 2,4,9,10
aca <- which(where == 0)
a[aca]
# # # # # # # # # # # # # # # # # # # # # # # 

library('snapshot')
idetacst0=snap.read.2('/home/cecere/sshfs/is2/fstasys/MHD_Disc/controlrun/snapshot_000','ID  ',debug=1,gas=1)
idetacs=  snap.read.2('/home/cecere/sshfs/is2/fstasys/MHD_Disc/controlrun/snapshot_350','ID  ',debug=1,gas=1)

# idetacst0_ord <- idetacst0[order(idetacst0)]
# idetacs_ord <- idetacs[order(idetacs)]

where <- match( idetacst0, idetacs )
# where <- match( idetacst0_ord, idetacs_ord )
where[ is.na( where ) ] <- 0

aca <- which(where == 0)
idetacst0[aca]  #aca tengo los ids en t=0 que en 350 se fueron

# voy tomar algunas particulas, pq son muchas
idsam <- sample(idetacst0[aca], 1000, replace=FALSE)

# npartsel=length(aca)
npartsel=length(idsam)

base='/home/cecere/sshfs/is2/fstasys/MHD_Disc/controlrun/snapshot_'
N0=0       #tiempo inicial
N1=350         #tiempo final
step=5      #cada cuanto
NN=N1-N0+1
rr=paste0(rep(base,(NN-1)/step+1),sprintf("%03d",seq(N0,N1,step)))
num=length(npartsel)*((NN-1)/step+1)
eint = matrix( c(1:num), nrow=npartsel, ncol=(NN-1)/step+1,   byrow = TRUE)*0.
# B2   = matrix( c(1:num), nrow=npartsel, ncol=(NN-1)/step+1,   byrow = TRUE)*0.
v2   = matrix( c(1:num), nrow=npartsel, ncol=(NN-1)/step+1,   byrow = TRUE)*0.
K    = matrix( c(1:num), nrow=npartsel, ncol=(NN-1)/step+1,   byrow = TRUE)*0.
xx   = matrix( c(1:num), nrow=npartsel, ncol=(NN-1)/step+1,   byrow = TRUE)*0.
yy   = matrix( c(1:num), nrow=npartsel, ncol=(NN-1)/step+1,   byrow = TRUE)*0.
zz   = matrix( c(1:num), nrow=npartsel, ncol=(NN-1)/step+1,   byrow = TRUE)*0.


iii=0
for (ifile in 1:length(rr)){
     iii=iii+1
     cada=rr[ifile]
     print(cada)

     poss=snap.read.2(cada,'POS ',gas=1)
     u=snap.read.2(cada,'U   ',gas=1)
#      b=snap.read.2(cada,'BFLD',gas=1)
     v=snap.read.2(cada,'VEL ',gas=1)     
     rho=snap.read.2(cada,'RHO ',gas=1)
     iid=snap.read.2(cada,'ID  ',gas=1)
     
  for(ip in 1:npartsel){   
     ind= which(iid[] == idsam[ip])
     if(length(ind) > 0){
       xx[ip,iii]=poss$x[ind]
       yy[ip,iii]=poss$y[ind]
       zz[ip,iii]=poss$z[ind]
       eint[ip,iii]=u[ind]
#        bx=b$x[ind]
#        by=b$y[ind]
#        bz=b$z[ind]
#        B2[ip,iii]=0.5*(bx*bx + by*by + bz*bz)
       rhoo=rho[ind]
       vx=v$x[ind]
       vy=v$y[ind]
       vz=v$z[ind]
       K[ip,iii]=0.5*rhoo*(vx*vx + vy*vy + vz*vz)
       v2[ip,iii]=0.5*(vx*vx + vy*vy + vz*vz)
     }
#      else{
#        xx[ip,iii]=xx[ip,iii-1]
#        yy[ip,iii]=yy[ip,iii-1]
#        zz[ip,iii]=zz[ip,iii-1]
#        eint[ip,iii]=eint[ip,iii-1]
#        B2[ip,iii]=B2[ip,iii-1]
#        K[ip,iii]=K[ip,iii-1]
#        v2[ip,iii]=v2[ip,iii-1]
#      
#      }
      
  }
}


library("rgl")

cl <- rainbow(npartsel)
axes3d()
for (ip in 1:npartsel){ 
  lines3d(xx[ip,], yy[ip,], zz[ip,], col = cl[ip], lwd = 2)
}
title3d('','','xlab','ylab','zlab')

pdf("3.pdf")
cl <- rainbow(npartsel)
  ymin=min(eint)+5e-4
  ymax=max(eint)
  plot(0:(length(rr)-1)*step,eint[1,],type="l",log="y",ylim=c(ymin,ymax))
for (ip in 2:npartsel){
  par(new=T)
  plot(0:(length(rr)-1)*step,eint[ip,],type="l",log="y", axes = FALSE, , xlab = "", ylab = "", col = cl[ip],ylim=c(ymin,ymax))
}
dev.off()

pdf("5.pdf")
cl <- rainbow(npartsel)
  ymin=min(K)+1e-7
  ymax=max(K)
  plot(0:(length(rr)-1)*step,K[1,],type="l",log="y",ylim=c(ymin,ymax))
for (ip in 2:npartsel){
  par(new=T)
  plot(0:(length(rr)-1)*step,K[ip,],type="l",log="y", axes = FALSE, , xlab = "", ylab = "", col = cl[ip],ylim=c(ymin,ymax))
}
dev.off()

pdf("6.pdf")
cl <- rainbow(npartsel)
  ymin=min(v2)+1e-2
  ymax=max(v2)
  plot(0:(length(rr)-1)*step,v2[1,],type="l",log="y",ylim=c(ymin,ymax))
for (ip in 2:npartsel){
  par(new=T)
  plot(0:(length(rr)-1)*step,v2[ip,],type="l",log="y", axes = FALSE, , xlab = "", ylab = "", col = cl[ip],ylim=c(ymin,ymax))
}
dev.off()


pdf("7.pdf")
mediaeint=NULL
for (it in 1:length(rr)){
mediaeint=c(mediaeint,median(eint[,it]))
}
plot(0:(length(rr)-1)*step,mediaeint,type="l",log="y")
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





