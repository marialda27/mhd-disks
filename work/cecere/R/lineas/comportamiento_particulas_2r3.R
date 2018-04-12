load("disco_2r3")
print(load("disco_2r3"))

arraypos=possam

base='/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_'
N0=0       #tiempo inicial
N1=350         #tiempo final
step=10      #cada cuanto
NN=N1-N0+1
rr=paste0(rep(base,(NN-1)/step+1),sprintf("%03d",seq(N0,N1,step)))


pdf("1.pdf")
  plot(xx,yy,pch=".")
  plot(xx,zz,pch=".")
dev.off()

pdf("2.pdf")
for (it in 1:length(rr)){
  plot(sqrt(xx[,it]*xx[,it]+yy[,it]*yy[,it]), zz[,it],xlim=c(0,50),ylim=c(-35,35),pch=".")
  lines(c(0,15),c(2,2))
  lines(c(2,2),c(-30,30))
  lines(c(0,15),c(-2,-2))
  texto <- c("t=", toString((it-1)*step))
  legend("topright", toString(texto))
}
dev.off()


library("rgl")
cl <- rainbow(length(arraypos))
axes3d()
for (ip in 1:length(arraypos)){ 
  lines3d(xx[ip,], yy[ip,], zz[ip,], col = cl[ip], lwd = 2)
}
title3d('','','xlab','ylab','zlab')


arraypos2 <- sample(arraypos, 100, replace=FALSE)
library("rgl")
cl <- rainbow(length(arraypos2))
axes3d()
for (ip in 1:length(arraypos2)){ 
  lines3d(xx[ip,1:10], yy[ip,1:10], zz[ip,1:10], col = cl[ip], lwd = 2)
}
title3d('','','xlab','ylab','zlab')

part <- sample(1:length(arraypos), 1)
plot(xx[part,],zz[part,],xlim=c(-6,6),ylim=c(-5,5),type="l")


count=0
zout = list(NULL)
radio = list(NULL)
for (ip in 1:length(arraypos))
{ 
    for(it in 1:length(rr))
   {
           if(zz[ip,it] > 2 | zz[ip,it] < -2)
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

pdf("3.pdf")
cl <- rainbow(908)
xmin=0
xmax=10
ymin=-5
ymax=5
plot(radio[[1]],zout[[1]],type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
for (t in 2:908){
  par(new=T)
 plot(radio[[t]],zout[[t]],type="l", axes = FALSE, , xlab = "", ylab = "", col = cl[t],xlim=c(xmin,xmax),ylim=c(ymin,ymax))
}
dev.off()




part <- sample(1:908, 1)
plot(radio[[part]],zout[[part]],type="l")


plot(radio[[part]],zout[[part]],type="l",xlim=c(0,10),ylim=c(-3,3))

# # # # # # # # # # # # # # # # # # # # # # # # # 

count=0
a=NULL
for(c in 1:908){
  if(zout[[c]][1]>2){
    if(length(which(zout[[c]]<2))>0){
      count=count+1
      a=c(a,c)
    }
  }
  else if(zout[[c]][1]<(-2)){
    if(length(which(zout[[c]]>-2))>0){
      count=count+1
      a=c(a,c)
    }
  }
}

pdf("4.pdf")
cl <- rainbow(243)
xmin=0
xmax=10
ymin=-5
ymax=5
plot(radio[[a[1]]],zout[[a[1]]],type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
for (t in 2:243){
  par(new=T)
 plot(radio[[a[t]]],zout[[a[t]]],type="l", axes = FALSE, , xlab = "", ylab = "", col = cl[t],xlim=c(xmin,xmax),ylim=c(ymin,ymax))
}
dev.off()



pdf("5.pdf")
cl <- rainbow(243)
xxx=NULL
yyy=NULL
for(j in 1:243) {
  xxx =c(xxx,radio[[j]])
  yyy =c(yyy,zout[[j]])
}
xmin=min(xxx)
xmax=max(xxx)
ymin=min(yyy)
ymax=max(yyy)
plot(radio[[a[1]]][length(radio[[a[1]]])],zout[[a[1]]][length(zout[[a[1]]])],xlim=c(xmin,xmax),ylim=c(ymin,ymax))
for (t in 2:243){
  par(new=T)
 plot(radio[[a[t]]][length(radio[[a[t]]])],zout[[a[t]]][length(zout[[a[t]]])], axes = FALSE, , xlab = "", ylab = "", col = cl[t],xlim=c(xmin,xmax),ylim=c(ymin,ymax))
}
dev.off()

pdf("6.pdf")
cl <- rainbow(243)
xmin=0
xmax=10
ymin=-3
ymax=3
plot(radio[[a[1]]][length(radio[[a[1]]])],zout[[a[1]]][length(zout[[a[1]]])],xlim=c(xmin,xmax),ylim=c(ymin,ymax))
for (t in 2:243){
  par(new=T)
 plot(radio[[a[t]]][length(radio[[a[t]]])],zout[[a[t]]][length(zout[[a[t]]])], axes = FALSE, , xlab = "", ylab = "", col = cl[t],xlim=c(xmin,xmax),ylim=c(ymin,ymax))
}
dev.off()

count=0
for (t in 1:243){
  if(-2 < zout[[a[t]]][length(zout[[a[t]]])] & zout[[a[t]]][length(zout[[a[t]]])] < 2 ){
     count=count+1
  }
}




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
