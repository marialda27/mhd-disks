
pdf("part_izq_time.pdf")

library('snapshot')
library("plot3D")

pos=snap.read.2('../../../R/etacs/snapshot_000','POS ',debug=1,gas=1)
xp = pos$x
yp = pos$y

b=snap.read.2('../../../R/etacs/snapshot_000','BFLD',debug=1,gas=1)
bxp = b$x
byp = b$y

id=snap.read.2('../../../R/etacs/snapshot_000','ID  ',debug=1,gas=1)

data <- data.frame(id,xp,yp,bxp,byp)
dataord <- data[order(data[,1]),]  #ordenalo por id

# esto ya me da los id 
idsel <- which(2. < dataord[2] & dataord[2] < 2.01 & 0. < dataord[3] & dataord[3] < 0.01) 
# dataord[id,]  me devuelve los valores de dataord ya con la restriccion en x-y

dataind <- data.frame(idsel)
idsam <- sample(nrow(dataind), 1, replace=FALSE)
# idsel[idsam]

xpc = dataord[idsel[idsam],2,]  
ypc = dataord[idsel[idsam],3,]
bxpc= dataord[idsel[idsam],4,]
bypc= dataord[idsel[idsam],5,]

x = xpc
y = ypc
bx = bxpc
by = bypc

xc=c(NULL,x)
yc=c(NULL,y)

# minlog = floor(log10(min(sqrt(bxp*bxp+byp*byp))))
# maxlog = floor(log10(max(sqrt(bxp*bxp+byp*byp))))
# corr = maxlog-minlog
# 
# x1 = x+(log10(sqrt(bx*bx+by*by))+corr)*bx/sqrt(bx*bx+by*by)/corr
# y1 = y+(log10(sqrt(bx*bx+by*by))+corr)*by/sqrt(bx*bx+by*by)/corr 
# 
# bphi = -bx*sin(atan2(y,x))+by*cos(atan2(y,x))

# x=x[which(bphi > 0.)]
# y=y[which(bphi > 0.)]
# x1=x1[which(bphi > 0.)]
# y1=y1[which(bphi > 0.)]


# arrows2D(x, y,  x1, y1,  colvar = sqrt((x1-x)^2+(y1-y)^2), col = NULL,
#          lwd = 3, clab = c("B"), 
#           bty ="n", xlim = c(-5, 5), ylim = c(-5, 5),type = "triangle",length=0.05)

          
for (i in seq(1,9,1))
{

chartime=as.character(i)
namefile=paste0('../../../R/etacs/snapshot_00',chartime)

pos=snap.read.2(namefile,'POS ',debug=1,gas=1)
xp = pos$x
yp = pos$y

b=snap.read.2(namefile,'BFLD',debug=1,gas=1)
bxp = b$x
byp = b$y

id=snap.read.2(namefile,'ID  ',debug=1,gas=1)

data <- data.frame(id,xp,yp,bxp,byp)
dataord <- data[order(data[,1]),]  #ordenalo por id

xpc = dataord[idsel[idsam],2,]  
ypc = dataord[idsel[idsam],3,]
bxpc= dataord[idsel[idsam],4,]
bypc= dataord[idsel[idsam],5,]

x = xpc
y = ypc
bx = bxpc
by = bypc

xc=c(xc,x)
yc=c(yc,x)

# minlog = floor(log10(min(sqrt(bxp*bxp+byp*byp))))
# maxlog = floor(log10(max(sqrt(bxp*bxp+byp*byp))))
# corr = maxlog-minlog
# 
# x1 = x+(log10(sqrt(bx*bx+by*by))+corr)*bx/sqrt(bx*bx+by*by)/corr
# y1 = y+(log10(sqrt(bx*bx+by*by))+corr)*by/sqrt(bx*bx+by*by)/corr 
# 
# bphi = -bx*sin(atan2(y,x))+by*cos(atan2(y,x))

# x=x[which(bphi > 0.)]
# y=y[which(bphi > 0.)]
# x1=x1[which(bphi > 0.)]
# y1=y1[which(bphi > 0.)]


# arrows2D(x, y,  x1, y1,  colvar = sqrt((x1-x)^2+(y1-y)^2), col = NULL,
#          lwd = 3, clab = c("B"), 
#           bty ="n", xlim = c(-5, 5), ylim = c(-5, 5),type = "triangle",length=0.05)

}

for (i in seq(10,90,10))
{

chartime=as.character(i)
namefile=paste0('../../../R/etacs/snapshot_0',chartime)

pos=snap.read.2(namefile,'POS ',debug=1,gas=1)
xp = pos$x
yp = pos$y

b=snap.read.2(namefile,'BFLD',debug=1,gas=1)
bxp = b$x
byp = b$y

id=snap.read.2(namefile,'ID  ',debug=1,gas=1)

data <- data.frame(id,xp,yp,bxp,byp)
dataord <- data[order(data[,1]),]  #ordenalo por id

xpc = dataord[idsel[idsam],2,]  
ypc = dataord[idsel[idsam],3,]
bxpc= dataord[idsel[idsam],4,]
bypc= dataord[idsel[idsam],5,]

x = xpc
y = ypc
bx = bxpc
by = bypc

xc=c(xc,x)
yc=c(yc,x)

# minlog = floor(log10(min(sqrt(bxp*bxp+byp*byp))))
# maxlog = floor(log10(max(sqrt(bxp*bxp+byp*byp))))
# corr = maxlog-minlog
# 
# x1 = x+(log10(sqrt(bx*bx+by*by))+corr)*bx/sqrt(bx*bx+by*by)/corr
# y1 = y+(log10(sqrt(bx*bx+by*by))+corr)*by/sqrt(bx*bx+by*by)/corr 
# 
# bphi = -bx*sin(atan2(y,x))+by*cos(atan2(y,x))

# x=x[which(bphi > 0.)]
# y=y[which(bphi > 0.)]
# x1=x1[which(bphi > 0.)]
# y1=y1[which(bphi > 0.)]


# arrows2D(x, y,  x1, y1,  colvar = sqrt((x1-x)^2+(y1-y)^2), col = NULL,
#          lwd = 3, clab = c("B"), 
#           bty ="n", xlim = c(-5, 5), ylim = c(-5, 5),type = "triangle",length=0.05)

}

for (i in seq(100,370,10))
{

chartime=as.character(i)
namefile=paste0('../../../R/etacs/snapshot_',chartime)

pos=snap.read.2(namefile,'POS ',debug=1,gas=1)
xp = pos$x
yp = pos$y

b=snap.read.2(namefile,'BFLD',debug=1,gas=1)
bxp = b$x
byp = b$y

id=snap.read.2(namefile,'ID  ',debug=1,gas=1)

data <- data.frame(id,xp,yp,bxp,byp)
dataord <- data[order(data[,1]),]  #ordenalo por id

xpc = dataord[idsel[idsam],2,]  
ypc = dataord[idsel[idsam],3,]
bxpc= dataord[idsel[idsam],4,]
bypc= dataord[idsel[idsam],5,]

x = xpc
y = ypc
bx = bxpc
by = bypc

xc=c(xc,x)
yc=c(yc,x)

# minlog = floor(log10(min(sqrt(bxp*bxp+byp*byp))))
# maxlog = floor(log10(max(sqrt(bxp*bxp+byp*byp))))
# corr = maxlog-minlog
# 
# x1 = x+(log10(sqrt(bx*bx+by*by))+corr)*bx/sqrt(bx*bx+by*by)/corr
# y1 = y+(log10(sqrt(bx*bx+by*by))+corr)*by/sqrt(bx*bx+by*by)/corr 
# 
# bphi = -bx*sin(atan2(y,x))+by*cos(atan2(y,x))

# x=x[which(bphi > 0.)]
# y=y[which(bphi > 0.)]
# x1=x1[which(bphi > 0.)]
# y1=y1[which(bphi > 0.)]


# arrows2D(x, y,  x1, y1,  colvar = sqrt((x1-x)^2+(y1-y)^2), col = NULL,
#          lwd = 3, clab = c("B"), 
#           bty ="n", xlim = c(-5, 5), ylim = c(-5, 5),type = "triangle",length=0.05)

}


plot(xc,yc,xlim=c(-5,5),ylim=c(-5,5))

dev.off()

