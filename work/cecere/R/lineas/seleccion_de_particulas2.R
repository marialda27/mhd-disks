


library('snapshot')

pos=snap.read.2('/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_125','POS ',debug=1,gas=1)
xp = pos$x
yp = pos$y
zp = pos$z

id=snap.read.2('/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_125','ID  ',debug=1,gas=1)

data <- data.frame(id,xp,yp,zp)

# me da el orden del array donde pasa esto
# possel <- which(-50.0 < xp & xp < 50. & -50.0 < yp & yp < 50.0 & 45.0 < zp & zp < 55.0)
# possel <- which(-100.0 < xp & xp < 100. & -100.0 < yp & yp < 100.0 & 145.0 < zp & zp < 155.0) 
possel <- which(-10.0 < xp & xp < 10. & -10.0 < yp & yp < 10.0 & 29.0 < zp & zp < 31.0) 

datapos=c(NULL,possel[1])
for (i in 2:length(possel)){
 datapos=c(datapos,possel[i])
}

# esto me da un muestreo de las posiciones del array
possam <- sample(datapos, 66, replace=FALSE)

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

base='/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_'
N0=0       #tiempo inicial
N1=200         #tiempo final
step=1      #cada cuanto
# NN=N0-N1+1
# rr=paste0(rep(base,(NN-1)/step+1),sprintf("%03d",rev(seq(N1,N0,step))))
NN=N1-N0+1
rr=paste0(rep(base,(NN-1)/step+1),sprintf("%03d",seq(N0,N1,step)))
num=length(possam)*((NN-1)/step+1)
cx = matrix( c(1:num), nrow=length(possam), ncol=(NN-1)/step+1,   byrow = TRUE)
cy = matrix( c(1:num), nrow=length(possam), ncol=(NN-1)/step+1,   byrow = TRUE)
cz = matrix( c(1:num), nrow=length(possam), ncol=(NN-1)/step+1,   byrow = TRUE)

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
     cx[ip,iii]=po$x[which(iid[] == data[possam,1,][ip])]
     cy[ip,iii]=po$y[which(iid[] == data[possam,1,][ip])]
     cz[ip,iii]=po$z[which(iid[] == data[possam,1,][ip])] 
  }
}


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
pdf("z_vs_r.pdf")
for (it in 1:length(rr)){
  plot(sqrt(cx[,it]*cx[,it]+cy[,it]*cy[,it]), cz[,it],xlim=c(0,15),ylim=c(0,30))
  lines(c(0,15),c(2,2))
  lines(c(2,2),c(0,30))
  texto <- c("t=", toString((it-1)*1))
  legend("topright", toString(texto))
}
dev.off()

pdf("z_vs_r2.pdf")
for (it in 1:length(rr)){
  plot(sqrt(cx[,it]*cx[,it]+cy[,it]*cy[,it]), cz[,it],xlim=c(0,15),ylim=c(0,30))
  lines(c(0,15),c(2,2))
  lines(c(2,2),c(0,30))
  texto <- c("t=", toString((it-1)*1))
  legend("topright", toString(texto))
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

library(ggplot2)
pdf("z_vs_r_histograma.pdf")

for (it in 1:length(rr)){
# create dataset with 1000 normally distributed points
df <- data.frame(x = sqrt(cx[,it]*cx[,it]+cy[,it]*cy[,it]), y = cz[,it])
# create a ggplot2 scatterplot
p <- ggplot(df, aes(x, y)) + geom_point() + theme_classic()
# add marginal histograms
ggExtra::ggMarginal(p, type = "histogram")

}
dev.off()
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # d

library(rgl) 
library(car) 
library(ggplot2)
library(gridExtra)
library(devtools)
library(proto)


for (it in 1:length(rr)){
df <- data.frame(x = sqrt(cx[,it]*cx[,it]+cy[,it]*cy[,it]), y = cz[,it])

htop <- ggplot(data=df, aes(x=sqrt(cx[,it]*cx[,it]+cy[,it]*cy[,it]))) + 
geom_histogram(aes(y=..density..), fill = "white", color = "black", binwidth = 2) + 
stat_density(colour = "blue", geom="line", size = 1.5, position="identity") + 
# scale_x_continuous("V1", limits = c(0,15),breaks = c(-40,-20,0,20,40)) + 
# scale_y_continuous("Count",breaks=c(0.0,0.01,0.02,0.03,0.04),labels=c(0,100,200,300,400)) + 
theme_bw() + 
theme(axis.title.x = element_blank())
  
blank <- ggplot() + geom_point(aes(1,1), colour="white") + theme(axis.ticks=element_blank(), panel.background=element_blank(), panel.grid=element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

scatter <- ggplot(data=df, aes(x=sqrt(cx[,it]*cx[,it]+cy[,it]*cy[,it]), y=cz[,it])) + 
  geom_point(size = 0.6) + stat_ellipse(level = 0.95, size = 1, color="green") +
#   scale_x_continuous("label V1", limits = c(-40,40), breaks = c(-40,-20,0,20,40)) + 
#   scale_y_continuous("label V2", limits = c(-20,20), breaks = c(-20,-10,0,10,20)) + 
  theme_bw()

hright <- ggplot(data=df, aes(x=cz[,it])) + 
  geom_histogram(aes(y=..density..), fill = "white", color = "black", binwidth = 1) + 
  stat_density(colour = "red", geom="line", size = 1, position="identity") +
#   scale_x_continuous("V2", limits = c(-20,20), breaks = c(-20,-10,0,10,20)) + 
#   scale_y_continuous("Count", breaks=c(0.0,0.02,0.04,0.06,0.08), labels=c(0,200,400,600,800)) + 
  coord_flip() + theme_bw() + theme(axis.title.y = element_blank())

  
  grid.arrange(htop, blank, scatter, hright, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
  }
  dev.off()
  
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
load("z30t125_dt1_T110")

# esto me da un array de las posiciones del array de possam donde ocurre esto
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

cxsel = c(1:length(arraypos))
cysel = c(1:length(arraypos))
czsel = c(1:length(arraypos))
for(ip in 1:length(arraypos)){
     cada="/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_000"
     print(cada)
     iid=snap.read.2(cada,'ID  ')
     po=snap.read.2(cada,'POS ')
     cxsel[ip]=po$x[which(iid[] == data[possam[arraypos],1,][ip])]
     cysel[ip]=po$y[which(iid[] == data[possam[arraypos],1,][ip])]
     czsel[ip]=po$z[which(iid[] == data[possam[arraypos],1,][ip])] 
  
}

# # # # # # # # # # # # # # # # # # # # # 
# para graficar
base='/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_'
N0=100       #tiempo inicial
N1=150         #tiempo final
step=1      #cada cuanto
NN=N1-N0+1
rr=paste0(rep(base,(NN-1)/step+1),sprintf("%03d",seq(N0,N1,step)))
num=length(arraypos)*((NN-1)/step+1)
cxn = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)
cyn = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)
czn = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)


for(ip in 1:length(arraypos)){
iii=0
print(ip)
 for (ifile in 1:length(rr)){
     cada=rr[ifile]
     iii=iii+1
     print(cada)
     iid=snap.read.2(cada,'ID  ')
     po=snap.read.2(cada,'POS ')
     cxn[ip,iii]=po$x[which(iid[] == data[possam[arraypos],1,][ip])]
     cyn[ip,iii]=po$y[which(iid[] == data[possam[arraypos],1,][ip])]
     czn[ip,iii]=po$z[which(iid[] == data[possam[arraypos],1,][ip])] 
  }
}

library("rgl")

cl <- rainbow(length(arraypos))
axes3d()
for (ip in 1:length(arraypos)){ 
  lines3d(cxn[ip,], cyn[ip,], czn[ip,], col = cl[ip], lwd = 2)
}
title3d('','','xlab','ylab','zlab')

# # # # # # # # # # # # # # # # # # # # # # # d

cl <- rainbow(length(arraypos))
  plot(sqrt(cxn[1,]*cxn[1,]+cyn[1,]*cyn[1,]),czn[1,],type="l")
for (ip in 2:length(arraypos)){
  par(new=T)
  plot(sqrt(cxn[ip,]*cxn[ip,]+cyn[ip,]*cyn[ip,]),czn[ip,],type="l", axes = FALSE, , xlab = "", ylab = "", col = cl[ip])
}





# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# propiedades: u, B2, K
base='/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_'
N0=0       #tiempo inicial
N1=200         #tiempo final
step=10      #cada cuanto
NN=N1-N0+1
rr=paste0(rep(base,(NN-1)/step+1),sprintf("%03d",seq(N0,N1,step)))
num=length(arraypos)*((NN-1)/step+1)
eint = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)
B2   = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)
K    = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)
xx   = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)
yy   = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)
zz   = matrix( c(1:num), nrow=length(arraypos), ncol=(NN-1)/step+1,   byrow = TRUE)
for(ip in 1:length(arraypos)){
iii=0
print(ip)
 for (ifile in 1:length(rr)){
     cada=rr[ifile]
     iii=iii+1
     print(cada)
     poss=snap.read.2(cada,'POS ',gas=1)
     xx[ip,iii]=poss$x[which(iid[] == data[possam[arraypos],1,][ip])]
     yy[ip,iii]=poss$y[which(iid[] == data[possam[arraypos],1,][ip])]
     zz[ip,iii]=poss$z[which(iid[] == data[possam[arraypos],1,][ip])]
#      u=snap.read.2(cada,'U   ',gas=1)
#      b=snap.read.2(cada,'BFLD',gas=1)
#      v=snap.read.2(cada,'VEL ',gas=1)
#      rho=snap.read.2(cada,'RHO ',gas=1)
#      iid=snap.read.2(cada,'ID  ',gas=1)
#      eint[ip,iii]=u[which(iid[] == data[possam[arraypos],1,][ip])]
#      bx=b$x[which(iid[] == data[possam[arraypos],1,][ip])]
#      by=b$y[which(iid[] == data[possam[arraypos],1,][ip])]
#      bz=b$z[which(iid[] == data[possam[arraypos],1,][ip])]
#      B2[ip,iii]=0.5*(bx*bx + by*by + bz*bz)
#      rho=rho[which(iid[] == data[possam[arraypos],1,][ip])]
#      vx=v$x[which(iid[] == data[possam[arraypos],1,][ip])]
#      vy=v$y[which(iid[] == data[possam[arraypos],1,][ip])]
#      vz=v$z[which(iid[] == data[possam[arraypos],1,][ip])]
#      K[ip,iii]=0.5*rho*(vx*vx + vy*vy + vz*vz)
#      v2[ip,iii]=0.5*(vx*vx + vy*vy + vz*vz)
     
  }
}

ip=2
plot(0:(length(rr)-1)*step,eint[ip,],type="l",log="y")
plot(0:(length(rr)-1)*step,K[ip,],type="l",log="y")
plot(0:(length(rr)-1)*step,v2[ip,],type="l",log="y")
plot(0:(length(rr)-1)*step,B2[ip,],type="l",log="y")
axes3d()
lines3d(cxn[ip,], cyn[ip,], czn[ip,], col = cl[ip], lwd = 2, pch=2)

plot(0:(length(rr)-1)*step,xx[2,])
plot(0:(length(rr)-1)*step,yy[2,])
plot(0:(length(rr)-1)*step,zz[2,])


pdf("eint_t200.pdf")
cl <- rainbow(length(arraypos))
  plot(0:(length(rr)-1)*step,eint[1,],type="l",log="y")
for (ip in 2:length(arraypos)){
  par(new=T)
  plot(0:(length(rr)-1)*step,eint[ip,],type="l",log="y", axes = FALSE, , xlab = "", ylab = "", col = cl[ip])
}
dev.off()


pdf("B2_t200.pdf")
cl <- rainbow(length(arraypos))
  plot(0:(length(rr)-1)*step,B2[1,],type="l",log="y")
for (ip in 2:length(arraypos)){
  par(new=T)
  plot(0:(length(rr)-1)*step,B2[ip,],type="l",log="y", axes = FALSE, , xlab = "", ylab = "", col = cl[ip])
}
dev.off()

pdf("K_t200.pdf")
cl <- rainbow(length(arraypos))
  plot(0:(length(rr)-1)*step,K[1,],type="l",log="y")
for (ip in 2:length(arraypos)){
  par(new=T)
  plot(0:(length(rr)-1)*step,K[ip,],type="l",log="y", axes = FALSE, , xlab = "", ylab = "", col = cl[ip])
}
dev.off()

pdf("v2_t200.pdf")
cl <- rainbow(length(arraypos))
  plot(0:(length(rr)-1)*step,v2[1,],type="l",log="y")
for (ip in 2:length(arraypos)){
  par(new=T)
  plot(0:(length(rr)-1)*step,v2[ip,],type="l",log="y", axes = FALSE, , xlab = "", ylab = "", col = cl[ip])
}
dev.off()




pdf("eint_t200_media.pdf")
mediaeint=NULL
for (it in 1:length(rr)){
mediaeint=c(mediaeint,median(eint[,it]))
}
plot(0:(length(rr)-1)*step,mediaeint,type="l",log="y")
dev.off()

pdf("B2_t200_media.pdf")
mediaB2=NULL
for (it in 1:length(rr)){
mediaB2=c(mediaB2,median(B2[,it]))
}
plot(0:(length(rr)-1)*step,mediaB2,type="l",log="y")
dev.off()

pdf("K_t200_media.pdf")
mediaK=NULL
for (it in 1:length(rr)){
mediaK=c(mediaK,median(K[,it]))
}
plot(0:(length(rr)-1)*step,mediaK,type="l",log="y")
dev.off()

pdf("v2_t200_media.pdf")
mediav2=NULL
for (it in 1:length(rr)){
mediav2=c(mediav2,median(v2[,it]))
}
plot(0:(length(rr)-1)*step,mediav2,type="l",log="y")
dev.off()
