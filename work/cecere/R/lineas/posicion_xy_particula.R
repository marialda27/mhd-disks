
pdf("part_time_t10.pdf")

library('snapshot')
library("plot3D")

pos=snap.read.2('../../../R/etacs/snapshot_000','POS ',debug=1,gas=1)
xp = pos$x
yp = pos$y

id=snap.read.2('../../../R/etacs/snapshot_000','ID  ',debug=1,gas=1)

data <- data.frame(id,xp,yp)
dataord <- data[order(data[,1]),]  #ordenalo por id

# esto ya me da los id 
idsel <- which(2. < dataord[2] & dataord[2] < 2.01 & 0. < dataord[3] & dataord[3] < 0.01) 
# dataord[id,]  me devuelve los valores de dataord ya con la restriccion en x-y

dataind <- data.frame(idsel)
idsam <- sample(nrow(dataind), 1, replace=FALSE)
# idsel[idsam]

xpc = dataord[idsel[idsam],2,]  
ypc = dataord[idsel[idsam],3,]

x = xpc
y = ypc

xc=c(NULL,x)
yc=c(NULL,y)

         
for (i in seq(1,9,1))
{

chartime=as.character(i)
namefile=paste0('../../../R/etacs/snapshot_00',chartime)

pos=snap.read.2(namefile,'POS ',debug=1,gas=1)
xp = pos$x
yp = pos$y

id=snap.read.2(namefile,'ID  ',debug=1,gas=1)

data <- data.frame(id,xp,yp)
dataord <- data[order(data[,1]),]  #ordenalo por id

xpc = dataord[idsel[idsam],2,]  
ypc = dataord[idsel[idsam],3,]

x = xpc
y = ypc

xc=c(xc,x)
yc=c(yc,y)

}

# for (i in seq(10,90,10))
# {
# 
# chartime=as.character(i)
# namefile=paste0('../../../R/etacs/snapshot_0',chartime)
# 
# pos=snap.read.2(namefile,'POS ',debug=1,gas=1)
# xp = pos$x
# yp = pos$y
# 
# id=snap.read.2(namefile,'ID  ',debug=1,gas=1)
# 
# data <- data.frame(id,xp,yp)
# dataord <- data[order(data[,1]),]  #ordenalo por id
# 
# xpc = dataord[idsel[idsam],2,]  
# ypc = dataord[idsel[idsam],3,]
# 
# x = xpc
# y = ypc
# 
# xc=c(xc,x)
# yc=c(yc,y)
# 
# }

# for (i in seq(100,370,10))
# {
# 
# chartime=as.character(i)
# namefile=paste0('../../../R/etacs/snapshot_',chartime)
# 
# pos=snap.read.2(namefile,'POS ',debug=1,gas=1)
# xp = pos$x
# yp = pos$y
# 
# id=snap.read.2(namefile,'ID  ',debug=1,gas=1)
# 
# data <- data.frame(id,xp,yp)
# dataord <- data[order(data[,1]),]  #ordenalo por id
# 
# xpc = dataord[idsel[idsam],2,]  
# ypc = dataord[idsel[idsam],3,]
# 
# x = xpc
# y = ypc
# 
# xc=c(xc,x)
# yc=c(yc,y)
# 
# }


plot(xc,yc,type="l",xlim=c(-5,5),ylim=c(-5,5))

dev.off()

