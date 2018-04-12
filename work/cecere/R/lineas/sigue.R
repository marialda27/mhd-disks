# sigue <-
# function(N0,N1,step)
# {

# pdf("particulas_time_xyz.pdf")
# source("sigue.R")

library('snapshot')
library("rgl")
N0=300       #tiempo inicial
N1=370     #tiempo final
step=1    #cada cuanto
NN=N1-N0+1
ID=c(416837, 416897, 423349) 
# ID=c(416837, 416897, 423349, 423353, 423951, 423985, 423990) 
# ID=423985
base='/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_'


num=length(ID)*((NN-1)/step+1)
# cx=c(1:NN)
cx = matrix( c(1:num), nrow=length(ID), ncol=(NN-1)/step+1,   byrow = TRUE)
# cy=c(1:NN)
cy = matrix( c(1:num), nrow=length(ID), ncol=(NN-1)/step+1,   byrow = TRUE)
# cz=c(1:NN)
cz = matrix( c(1:num), nrow=length(ID), ncol=(NN-1)/step+1,   byrow = TRUE)
rr=paste0(rep(base,(NN-1)/step+1),sprintf("%03d",seq(N0,N1,step)))
# rr=paste0(rep(base,NN),sprintf("%03d",c(N0:N1)*step))

for(ip in 1:length(ID)){
iii=0
 for (ifile in 1:length(rr)){
      cada=rr[ifile]
#   for (cada in rr){
     iii=iii+1
     print(cada)
     iid=snap.read.2(cada,'ID  ')
     po=snap.read.2(cada,'POS ')
     cx[ip,iii]=po$x[which(iid[] == ID[ip])]
     cy[ip,iii]=po$y[which(iid[] == ID[ip])]
     cz[ip,iii]=po$z[which(iid[] == ID[ip])] 
  }
}

# }

# para una particula
# axes3d()
# lines3d(cx, cy, cz, col = 'red', lwd = 2)

# para varias
cl <- rainbow(length(ID))
axes3d()
for (ip in 1:length(ID)){ 
#  lines3d(cx[ip,], cy[ip,], cz[ip,], col = 'red', lwd = 2)
# lines3d(cx[ip,], cy[ip,], cz[ip,], col=1:3, lwd = 2)
lines3d(cx[ip,], cy[ip,], cz[ip,], col = cl[ip], lwd = 2)
}



# dev.off()
