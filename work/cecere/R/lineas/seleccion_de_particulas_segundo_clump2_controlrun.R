


library('snapshot')

pos=snap.read.2('/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_180','POS ',debug=1,gas=1)
xp = pos$x
yp = pos$y
zp = pos$z

id=snap.read.2('/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_180','ID  ',debug=1,gas=1)

data <- data.frame(id,xp,yp,zp)

# me da el orden del array donde pasa esto
# possel <- which(-50.0 < xp & xp < 50. & -50.0 < yp & yp < 50.0 & 45.0 < zp & zp < 55.0)
# possel <- which(-100.0 < xp & xp < 100. & -100.0 < yp & yp < 100.0 & 145.0 < zp & zp < 155.0) 
possel <- which(-20.0 < xp & xp < 20. & -20.0 < yp & yp < 20.0 & 29.0 < zp & zp < 31.0) 

datapos=c(NULL,possel[1])
for (i in 2:length(possel)){
 datapos=c(datapos,possel[i])
}

# esto me da un muestreo de las posiciones del array
possam <- sample(datapos, 109, replace=FALSE)

# data[datapos,1] #me devuelve los id de todo
# data[possam,1] #me devuelve los id del sampleo

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

base='/home/cecere/sshfs/is2/fstasys/MHD_Disc/controlrun/snapshot_'
N0=0       #tiempo inicial
N1=350         #tiempo final
step=10      #cada cuanto
# NN=N0-N1+1
# rr=paste0(rep(base,(NN-1)/step+1),sprintf("%03d",rev(seq(N1,N0,step))))
NN=N1-N0+1
rr=paste0(rep(base,(NN-1)/step+1),sprintf("%03d",seq(N0,N1,step)))
num=length(possam)*((NN-1)/step+1)
cx = matrix( c(1:num), nrow=length(possam), ncol=(NN-1)/step+1,   byrow = TRUE)*0
cy = matrix( c(1:num), nrow=length(possam), ncol=(NN-1)/step+1,   byrow = TRUE)*0
cz = matrix( c(1:num), nrow=length(possam), ncol=(NN-1)/step+1,   byrow = TRUE)*0

# num=2*((NN-1)/step+1)
# cx = matrix( c(1:num), nrow=2, ncol=(NN-1)/step+1,   byrow = TRUE)
# cy = matrix( c(1:num), nrow=2, ncol=(NN-1)/step+1,   byrow = TRUE)
# cz = matrix( c(1:num), nrow=2, ncol=(NN-1)/step+1,   byrow = TRUE)

# for(ip in 1:length(possam)){
# # for(ip in 1:2){
# iii=0
# print(ip)
# #  for (ifile in 4:5){
#  for (ifile in 1:length(rr)){
#      cada=rr[ifile]
# #   for (cada in rr){
#      iii=iii+1
#      print(cada)
#      iid=snap.read.2(cada,'ID  ')
#      po=snap.read.2(cada,'POS ')
#      cx[ip,iii]=po$x[which(iid[] == data[possam,1,][ip])]
#      cy[ip,iii]=po$y[which(iid[] == data[possam,1,][ip])]
#      cz[ip,iii]=po$z[which(iid[] == data[possam,1,][ip])] 
#   }
# }

iii=0
for (ifile in 1:length(rr)){
     iii=iii+1
     cada=rr[ifile]
     print(cada)
     iid=snap.read.2(cada,'ID  ')
     po=snap.read.2(cada,'POS ')
  for(ip in 1:length(possam)){
     ind=which(iid[] == data[possam,1,][ip])
#      print(ind)
     if(length(ind) > 0){
      cx[ip,iii]=po$x[ind]
      cy[ip,iii]=po$y[ind]
      cz[ip,iii]=po$z[ind] 
     }
  }
}

pdf("1.pdf")
  plot(cx,cy)
  plot(cx,cz)
dev.off()

library("rgl")


cl <- rainbow(length(possam))
# cl <- rainbow(2)
axes3d()
# axes3d(col='red', box=TRUE, labels=TRUE, ticks=TRUE) 
for (ip in 1:length(possam)){ 
# for (ip in 1:2){ 
  lines3d(cx[ip,], cy[ip,], cz[ip,], col = cl[ip], lwd = 2)
}
title3d('','','xlab','ylab','zlab')


for (ip in 1:length(possam)){ 
cxzoom <- subset(cx[ip,], -20 < cx[ip,], cx[ip,] < 20, -20 < cy[ip,], cy[ip,] < 20, 0 < cz[ip,], cz[ip,] < 50)
cyzoom <- subset(cy[ip,], -20 < cx[ip,], cx[ip,] < 20, -20 < cy[ip,], cy[ip,] < 20, 0 < cz[ip,], cz[ip,] < 50)
czzoom <- subset(cz[ip,], -20 < cx[ip,], cx[ip,] < 20, -20 < cy[ip,], cy[ip,] < 20, 0 < cz[ip,], cz[ip,] < 50)
}


# cl <- rainbow(54)
# axes3d()
# # axes3d(col='red', box=TRUE, labels=TRUE, ticks=TRUE) 
# for (ip in 1:54){ 
#   lines3d(cxzoom[ip,], cyzoom[ip,], czzoom[ip,], col = cl[ip], lwd = 2)
# }

# bin<-hexbin(sqrt(cx[,1]*cx[,1]+cy[,1]*cy[,1]), cz[,1], xbins=5) 
# plot(bin, main="Hexagonal Binning")
pdf("2.pdf")
for (it in 1:length(rr)){
  plot(sqrt(cx[,it]*cx[,it]+cy[,it]*cy[,it]), cz[,it],xlim=c(0,20),ylim=c(0,35))
  lines(c(0,15),c(2,2))
  lines(c(2,2),c(0,30))
  texto <- c("t=", toString((it-1)*step))
  legend("topright", toString(texto))
}
dev.off()

pdf("2b.pdf")
cl <- rainbow(length(possam))
xmin=min(sqrt(cx*cx+cy*cy))
xmax=max(sqrt(cx*cx+cy*cy))
ymin=min(cz)
ymax=max(cz)
plot(sqrt(cx[1,]*cx[1,]+cy[1,]*cy[1,]),cz[1,],type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
for (ip in 2:length(possam)){
  par(new=T)
  plot(sqrt(cx[ip,]*cx[ip,]+cy[ip,]*cy[ip,]),cz[ip,],type="l", axes = FALSE, , xlab = "", ylab = "", col = cl[ip],xlim=c(xmin,xmax),ylim=c(ymin,ymax))
}
dev.off()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

library(psych)
pdf("z_vs_r_hist.pdf")
for (it in 1:length(rr)){
scatter.hist(sqrt(cx[,it]*cx[,it]+cy[,it]*cy[,it]),cz[,it])
}
dev.off()
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# library(ggplot2)
# pdf("z_vs_r_histograma.pdf")
# 
# for (it in 1:length(rr)){
# # create dataset with 1000 normally distributed points
# df <- data.frame(x = sqrt(cx[,it]*cx[,it]+cy[,it]*cy[,it]), y = cz[,it])
# # create a ggplot2 scatterplot
# p <- ggplot(df, aes(x, y)) + geom_point() + theme_classic()
# # add marginal histograms
# ggExtra::ggMarginal(p, type = "histogram")
# 
# }
# dev.off()
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # d

# library(rgl) 
# library(car) 
# library(ggplot2)
# library(gridExtra)
# library(devtools)
# library(proto)


# for (it in 1:length(rr)){
# df <- data.frame(x = sqrt(cx[,it]*cx[,it]+cy[,it]*cy[,it]), y = cz[,it])
# 
# htop <- ggplot(data=df, aes(x=sqrt(cx[,it]*cx[,it]+cy[,it]*cy[,it]))) + 
# geom_histogram(aes(y=..density..), fill = "white", color = "black", binwidth = 2) + 
# stat_density(colour = "blue", geom="line", size = 1.5, position="identity") + 
# # scale_x_continuous("V1", limits = c(0,15),breaks = c(-40,-20,0,20,40)) + 
# # scale_y_continuous("Count",breaks=c(0.0,0.01,0.02,0.03,0.04),labels=c(0,100,200,300,400)) + 
# theme_bw() + 
# theme(axis.title.x = element_blank())
#   
# blank <- ggplot() + geom_point(aes(1,1), colour="white") + theme(axis.ticks=element_blank(), panel.background=element_blank(), panel.grid=element_blank(),
#         axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())
# 
# scatter <- ggplot(data=df, aes(x=sqrt(cx[,it]*cx[,it]+cy[,it]*cy[,it]), y=cz[,it])) + 
#   geom_point(size = 0.6) + stat_ellipse(level = 0.95, size = 1, color="green") +
# #   scale_x_continuous("label V1", limits = c(-40,40), breaks = c(-40,-20,0,20,40)) + 
# #   scale_y_continuous("label V2", limits = c(-20,20), breaks = c(-20,-10,0,10,20)) + 
#   theme_bw()
# 
# hright <- ggplot(data=df, aes(x=cz[,it])) + 
#   geom_histogram(aes(y=..density..), fill = "white", color = "black", binwidth = 1) + 
#   stat_density(colour = "red", geom="line", size = 1, position="identity") +
# #   scale_x_continuous("V2", limits = c(-20,20), breaks = c(-20,-10,0,10,20)) + 
# #   scale_y_continuous("Count", breaks=c(0.0,0.02,0.04,0.06,0.08), labels=c(0,200,400,600,800)) + 
#   coord_flip() + theme_bw() + theme(axis.title.y = element_blank())
# 
#   
#   grid.arrange(htop, blank, scatter, hright, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
#   }
#   dev.off()
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

save(possam,cx,cy,cz,file="z30t125dt10T350")
print(load("z30t125dt10T350"))

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

# library('snapshot')
# 
# pos=snap.read.2('/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_125','POS ',debug=1,gas=1)
# xp = pos$x
# yp = pos$y
# zp = pos$z
# 
# id=snap.read.2('/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_125','ID  ',debug=1,gas=1)
# data <- data.frame(id,xp,yp,zp)

# seleccion de las particulas que seleccione arriba, para t=0
cxsel = c(1:length(arraypos))
cysel = c(1:length(arraypos))
czsel = c(1:length(arraypos))
for(ip in 1:length(arraypos)){
     cada="/home/cecere/sshfs/is2/fstasys/MHD_Disc/controlrun/snapshot_000"
     print(cada)
     iid=snap.read.2(cada,'ID  ')
     po=snap.read.2(cada,'POS ')
     ind=which(iid[] == data[possam[arraypos],1,][ip])
     if(length(ind) > 0){
       cxsel[ip]=po$x[ind]
       cysel[ip]=po$y[ind]
       czsel[ip]=po$z[ind] 
     }
  
}

pdf("3.pdf")
  plot(cxsel,cysel)
  plot(cxsel,czsel)
dev.off()


# # # # # # # # # # # # # # # # # # # # # 
# para graficar
base='/home/cecere/sshfs/is2/fstasys/MHD_Disc/controlrun/snapshot_'
N0=0       #tiempo inicial
N1=350         #tiempo final
step=10      #cada cuanto
NN=N1-N0+1
rr=paste0(rep(base,(NN-1)/step+1),sprintf("%03d",seq(N0,N1,step)))
num=length(arraypos)*((NN-1)/step+1)
cxn = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)*0
cyn = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)*0
czn = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)*0

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

library("rgl")

cl <- rainbow(length(arraypos))
axes3d()
for (ip in 1:length(arraypos)){ 
  lines3d(cxn[ip,], cyn[ip,], czn[ip,], col = cl[ip], lwd = 2)
}
title3d('','','xlab','ylab','zlab')


axes3d()
lines3d(cxn[ip,], cyn[ip,], czn[ip,], col = cl[ip], lwd = 2)

# sirve para plot
lnames <- as.character(seq(1,length(arraypos),1))
legend('topright', lnames, col = 1:length(arraypos), lty = 1)

# # # # # # # # # # # # # # # # # # # # # # # d

pdf("4.pdf")
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
# B2   = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)*0
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
#      b=snap.read.2(cada,'BFLD',gas=1)
     v=snap.read.2(cada,'VEL ',gas=1)     
     rho=snap.read.2(cada,'RHO ',gas=1)
     iid=snap.read.2(cada,'ID  ',gas=1)
     
  for(ip in 1:length(arraypos)){   
     ind=which(iid[] == data[possam[arraypos],1,][ip])
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
  }
}

cl <- rainbow(length(arraypos))
axes3d()
for (ip in 1:length(arraypos)){ 
  lines3d(xx[ip,], yy[ip,], zz[ip,], col = cl[ip], lwd = 2)
}
title3d('','','xlab','ylab','zlab')

# # # # # # # # # # # # # # # # # # # # # # # # # 



ip=2
plot(0:(length(rr)-1)*step,eint[ip,],type="l",log="y")
plot(0:(length(rr)-1)*step,K[ip,],type="l",log="y")
plot(0:(length(rr)-1)*step,v2[ip,],type="l",log="y")
plot(0:(length(rr)-1)*step,B2[ip,],type="l",log="y")
axes3d()
lines3d(cxn[ip,], cyn[ip,], czn[ip,], col = cl[ip], lwd = 2, pch=2)

plot(xx[ip,],zz[ip,])

plot(0:(length(rr)-1)*step,xx[ip,])
plot(0:(length(rr)-1)*step,yy[ip,])
plot(0:(length(rr)-1)*step,zz[ip,])
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

pdf("5.pdf")
cl <- rainbow(length(arraypos))
ymin=min(eint)+1e-3
ymax=max(eint)
plot(0:(length(rr)-1)*step,eint[1,],type="l",log="y",ylim=c(ymin,ymax))
for (ip in 2:length(arraypos)){
  par(new=T)
  plot(0:(length(rr)-1)*step,eint[ip,],type="l",log="y", axes = FALSE, , xlab = "", ylab = "", col = cl[ip],ylim=c(ymin,ymax))
}

dev.off()


pdf("7.pdf")
cl <- rainbow(length(arraypos))
ymin=min(K)+1e-8
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
