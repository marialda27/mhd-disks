library('snapshot')

pos=snap.read.2('/home/cecere/sshfs/is2/fstasys/MHD_Disc/controlrun/snapshot_000','POS ',debug=1,gas=1)
xp = pos$x
yp = pos$y
zp = pos$z

id=snap.read.2('/home/cecere/sshfs/is2/fstasys/MHD_Disc/controlrun/snapshot_000','ID  ',debug=1,gas=1)

data <- data.frame(id,xp,yp,zp)

# me da el orden del array donde pasa esto
radius = sqrt(pos$x*pos$x + pos$y*pos$y)
h=0.5
# possel <- which(-2 < pos$z & pos$z < 2 & 2 < radius & radius < 3) 
possel <- which(-h < pos$z & pos$z < h & 2 < radius & radius < 3)
print(str(possel))

# plot(pos$x[possel],pos$y[possel],pch=".")
plot(pos$x[possel],pos$z[possel],pch=".")

# esto me da un muestreo de las posiciones del array
possam <- sample(possel, 1000, replace=FALSE)

arraypos=possam

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# propiedades: u, B2, K
base='/home/cecere/sshfs/is2/fstasys/MHD_Disc/controlrun/snapshot_'
N0=0       #tiempo inicial
N1=350         #tiempo final
step=10      #cada cuanto
NN=N1-N0+1
rr=paste0(rep(base,(NN-1)/step+1),sprintf("%03d",seq(N0,N1,step)))
num=length(arraypos)*((NN-1)/step+1)
eint = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)*0
v2   = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)*0
K    = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)*0
xx   = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)*0
yy   = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)*0
zz   = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)*0

# # # # # # # # # # # # # # # # # # # # # # # # 

iii=0
for (ifile in 1:length(rr)){
     iii=iii+1
     cada=rr[ifile]
     print(cada)

     poss=snap.read.2(cada,'POS ',gas=1)
     u=snap.read.2(cada,'U   ',gas=1)
     v=snap.read.2(cada,'VEL ',gas=1)     
     rho=snap.read.2(cada,'RHO ',gas=1)
     iid=snap.read.2(cada,'ID  ',gas=1)
     
  for(ip in 1:length(arraypos)){  
     ind=which(iid[] == data[possam,1,][ip])
     if(length(ind) > 0){
       xx[ip,iii]=poss$x[ind]
       yy[ip,iii]=poss$y[ind]
       zz[ip,iii]=poss$z[ind]
       eint[ip,iii]=u[ind]
       rhoo=rho[ind]
       vx=v$x[ind]
       vy=v$y[ind]
       vz=v$z[ind]
       K[ip,iii]=0.5*rhoo*(vx*vx + vy*vy + vz*vz)
       v2[ip,iii]=0.5*(vx*vx + vy*vy + vz*vz)
     }
     
  }
}

save(possam,xx,yy,zz,K,v2,file="disco_2r3_0.5_control_N1000")
print(load("/home/cecere/Dropbox/R/controlrun/perfiles/disco_2r3_z2_control_N1000"))
print(load("/home/cecere/Dropbox/R/controlrun/perfiles/disco_2r3_z1.5_control_N1000"))


# pdf("1.pdf")
#   plot(xx,yy,pch=".")
#   plot(xx,zz,pch=".")
# dev.off()
# 
# pdf("2.pdf")
# for (it in 1:length(rr)){
#   plot(sqrt(xx[,it]*xx[,it]+yy[,it]*yy[,it]), zz[,it],xlim=c(0,50),ylim=c(-35,35),pch=".")
#   lines(c(0,15),c(2,2))
#   lines(c(2,2),c(-30,30))
#   lines(c(0,15),c(-2,-2))
#   texto <- c("t=", toString((it-1)*step))
#   legend("topright", toString(texto))
# }
# dev.off()


# library("rgl")
# cl <- rainbow(length(arraypos))
# axes3d()
# for (ip in 1:length(arraypos)){ 
#   lines3d(xx[ip,], yy[ip,], zz[ip,], col = cl[ip], lwd = 2)
# }
# title3d('','','xlab','ylab','zlab')


# arraypos2 <- sample(arraypos, 1000, replace=FALSE)
# library("rgl")
# cl <- rainbow(length(arraypos2))
# axes3d()
# for (ip in 1:length(arraypos2)){ 
#   lines3d(xx[ip,], yy[ip,], zz[ip,], col = cl[ip], lwd = 2)
# }
# title3d('','','xlab','ylab','zlab')
# 
# part <- sample(1:length(arraypos), 1)
# plot(xx[part,],zz[part,],xlim=c(-6,6),ylim=c(-5,5),type="l")


count=0
zout = list(NULL)
radio = list(NULL)
for (ip in 1:length(arraypos))
{ 
    for(it in 1:length(rr))
   {
           if(zz[ip,it] > h | zz[ip,it] < -h)
           { 
#                   print(zz[ip,it])
                  count=count+1
                  zout[[count]] =zz[ip,it:length(rr)]
                  radio[[count]] = sqrt(xx[ip,it:length(rr)]*xx[ip,it:length(rr)]+yy[ip,it:length(rr)]*yy[ip,it:length(rr)])
                  break
            } 
    }
#     if(count > 0){
#     zout[[count]] =zz[ip,1:length(rr)]
#     }
}
print(count)

# pdf("3.pdf")
# cl <- rainbow(count)
# xmin=0
# xmax=10
# ymin=-5
# ymax=5
# plot(radio[[1]],zout[[1]],type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
# for (t in 2:count){
#   par(new=T)
#  plot(radio[[t]],zout[[t]],type="l", axes = FALSE, , xlab = "", ylab = "", col = cl[t],xlim=c(xmin,xmax),ylim=c(ymin,ymax))
# }
# dev.off()

# part <- sample(1:908, 1)
# plot(radio[[part]],zout[[part]],type="l")
# 
# 
# plot(radio[[part]],zout[[part]],type="l",xlim=c(0,10),ylim=c(-3,3))

# # # # # # # # # # # # # # # # # # # # # # # # # 

count2=0
a=NULL
for(c in 1:count){
  if(zout[[c]][1]>h){
    if(length(which(zout[[c]]<h))>0){
      count2=count2+1
      a=c(a,c)
    }
  }
  else if(zout[[c]][1]<(-h)){
    if(length(which(zout[[c]]>-h))>0){
      count2=count2+1
      a=c(a,c)
    }
  }
}
print(count2)

# pdf("4.pdf")
# cl <- rainbow(count2)
# xmin=0
# xmax=10
# ymin=-5
# ymax=5
# plot(radio[[a[1]]],zout[[a[1]]],type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
# for (t in 2:count2){
#   par(new=T)
#  plot(radio[[a[t]]],zout[[a[t]]],type="l", axes = FALSE, , xlab = "", ylab = "", col = cl[t],xlim=c(xmin,xmax),ylim=c(ymin,ymax))
# }
# dev.off()



# pdf("5.pdf")
# cl <- rainbow(count2)
# xxx=NULL
# yyy=NULL
# for(j in 1:count2) {
#   xxx =c(xxx,radio[[j]])
#   yyy =c(yyy,zout[[j]])
# }
# xmin=min(xxx)
# xmax=max(xxx)
# ymin=min(yyy)
# ymax=max(yyy)
# plot(radio[[a[1]]][length(radio[[a[1]]])],zout[[a[1]]][length(zout[[a[1]]])],xlim=c(xmin,xmax),ylim=c(ymin,ymax))
# for (t in 2:count2){
#   par(new=T)
#  plot(radio[[a[t]]][length(radio[[a[t]]])],zout[[a[t]]][length(zout[[a[t]]])], axes = FALSE, , xlab = "", ylab = "", col = cl[t],xlim=c(xmin,xmax),ylim=c(ymin,ymax))
# }
# dev.off()
# 
# pdf("6.pdf")
# cl <- rainbow(count2)
# xmin=0
# xmax=10
# ymin=-3
# ymax=3
# plot(radio[[a[1]]][length(radio[[a[1]]])],zout[[a[1]]][length(zout[[a[1]]])],xlim=c(xmin,xmax),ylim=c(ymin,ymax))
# for (t in 2:count2){
#   par(new=T)
#  plot(radio[[a[t]]][length(radio[[a[t]]])],zout[[a[t]]][length(zout[[a[t]]])], axes = FALSE, , xlab = "", ylab = "", col = cl[t],xlim=c(xmin,xmax),ylim=c(ymin,ymax))
# }
# dev.off()

count3=0
for (t in 1:count2){
  if(-h < zout[[a[t]]][length(zout[[a[t]]])] & zout[[a[t]]][length(zout[[a[t]]])] < h ){
     count3=count3+1
  }
}
print(count3)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

pdf("5.pdf")
cl <- rainbow(length(arraypos))
ymin=min(eint)+1e-4
ymax=max(eint)
plot(0:(length(rr)-1)*step,eint[1,],type="l",log="y",ylim=c(ymin,ymax))
for (ip in 2:length(arraypos)){
  par(new=T)
  plot(0:(length(rr)-1)*step,eint[ip,],type="l",log="y", axes = FALSE, , xlab = "", ylab = "", col = cl[ip],ylim=c(ymin,ymax))
}
dev.off()


pdf("6.pdf")
cl <- rainbow(length(arraypos))
ymin=min(B2)+1e-18
ymax=max(B2)
plot(0:(length(rr)-1)*step,B2[1,],type="l",log="y",ylim=c(ymin,ymax))
for (ip in 2:length(arraypos)){
  par(new=T)
  plot(0:(length(rr)-1)*step,B2[ip,],type="l",log="y", axes = FALSE, , xlab = "", ylab = "", col = cl[ip],ylim=c(ymin,ymax))
}
dev.off()

pdf("7.pdf")
cl <- rainbow(length(arraypos))
ymin=min(K)+1e-7
ymax=max(K)
plot(0:(length(rr)-1)*step,K[1,],type="l",log="y",ylim=c(ymin,ymax))
for (ip in 2:length(arraypos)){
  par(new=T)
  plot(0:(length(rr)-1)*step,K[ip,],type="l",log="y", axes = FALSE, , xlab = "", ylab = "", col = cl[ip],ylim=c(ymin,ymax))
}
dev.off()

pdf("8.pdf")
cl <- rainbow(length(arraypos))
ymin=min(v2)+1e-2
ymax=max(v2)
plot(0:(length(rr)-1)*step,v2[1,],type="l",log="y",ylim=c(ymin,ymax))
for (ip in 2:length(arraypos)){
  par(new=T)
  plot(0:(length(rr)-1)*step,v2[ip,],type="l",log="y", axes = FALSE, , xlab = "", ylab = "", col = cl[ip],ylim=c(ymin,ymax))
}
dev.off()




pdf("9.pdf")
mediaeint=NULL
for (it in 1:length(rr)){
mediaeint=c(mediaeint,median(eint[,it]))
}
plot(0:(length(rr)-1)*step,mediaeint,type="l",log="y")
dev.off()

pdf("10.pdf")
mediaB2=NULL
for (it in 1:length(rr)){
mediaB2=c(mediaB2,median(B2[,it]))
}
plot(0:(length(rr)-1)*step,mediaB2,type="l",log="y")
dev.off()

pdf("11.pdf")
mediaK=NULL
for (it in 1:length(rr)){
mediaK=c(mediaK,median(K[,it]))
}
plot(0:(length(rr)-1)*step,mediaK,type="l",log="y")
dev.off()

pdf("12.pdf")
mediav2=NULL
for (it in 1:length(rr)){
mediav2=c(mediav2,median(v2[,it]))
}
plot(0:(length(rr)-1)*step,mediav2,type="l",log="y")
dev.off()
