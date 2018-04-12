
pdf("disco_xyz.pdf")

library('snapshot')
library("plot3D")

pos=snap.read.2('../../../R/etacs/snapshot_000','POS ',debug=1,gas=1)
xp = pos$x
yp = pos$y
zp = pos$z
id=snap.read.2('../../../R/etacs/snapshot_000','ID  ',debug=1,gas=1)
idc = id[which(-5 < xp & xp < 5 & -5 < yp & yp < 5 & 2 < zp & zp < 10)]

data <- data.frame(idc)
ind <- sample(nrow(data), 1000, replace=FALSE)

b=snap.read.2('../../../R/etacs/snapshot_000','BFLD',debug=1,gas=1)
bxp = b$x
byp = b$y
bzp = b$z

xpc = xp[ind]
ypc = yp[ind]
zpc = zp[ind]
bxpc=bxp[ind]
bypc=byp[ind]
bzpc=bzp[ind]

x = xpc
y = ypc
z = zpc
bx = bxpc
by = bypc
bz = bzpc

minlog = floor(log10(min(sqrt(bxp*bxp+byp*byp+bzp*bzp))))
maxlog = floor(log10(max(sqrt(bxp*bxp+byp*byp+bzp*bzp))))
corr = maxlog-minlog

x1 = x+(log10(sqrt(bx*bx+by*by+bz*bz))+corr)*bx/sqrt(bx*bx+by*by+bz*bz)/corr
y1 = y+(log10(sqrt(bx*bx+by*by+bz*bz))+corr)*by/sqrt(bx*bx+by*by+bz*bz)/corr
z1 = z+(log10(sqrt(bx*bx+by*by+bz*bz))+corr)*bz/sqrt(bx*bx+by*by+bz*bz)/corr 
arrows3D(x, y, z, x1, y1, z1,   colvar = sqrt((x1-x)^2+(y1-y)^2+(z1-z)^2), col = NULL,
         lwd = 2, d = 3, clab = c("B"), 
         main = "Arrows 3D t=260", bty ="g", ticktype = "detailed", phi = 90) 

for (i in seq(10,90,10))
{

chartime=as.character(i)
namefile=paste0('../../../R/etacs/snapshot_0',chartime)

pos=snap.read.2(namefile,'POS ',debug=1,gas=1)
xp = pos$x
yp = pos$y
zp = pos$z

b=snap.read.2(namefile,'BFLD',debug=1,gas=1)
bxp = b$x
byp = b$y
bzp = b$z

xpc = xp[ind]
ypc = yp[ind]
zpc = zp[ind]
bxpc=bxp[ind]
bypc=byp[ind]
bzpc=bzp[ind]

x = xpc
y = ypc
z = zpc
bx = bxpc
by = bypc
bz = bzpc

minlog = floor(log10(min(sqrt(bxp*bxp+byp*byp+bzp*bzp))))
maxlog = floor(log10(max(sqrt(bxp*bxp+byp*byp+bzp*bzp))))
corr = maxlog-minlog

x1 = x+(log10(sqrt(bx*bx+by*by+bz*bz))+corr)*bx/sqrt(bx*bx+by*by+bz*bz)/corr
y1 = y+(log10(sqrt(bx*bx+by*by+bz*bz))+corr)*by/sqrt(bx*bx+by*by+bz*bz)/corr
z1 = z+(log10(sqrt(bx*bx+by*by+bz*bz))+corr)*bz/sqrt(bx*bx+by*by+bz*bz)/corr 

# arrows3D(x, y, z,  x1, y1, z1,  colvar = sqrt((x1-x)^2+(y1-y)^2+(z1-z)^2), col = NULL,
#          lwd = 2, d = 3, clab = c("B"), 
#           bty ="g", xlim = c(-10, 10), ylim = c(-5, 5), zlim = c(0,10),type = "triangle",length=0.05, phi = 20)

arrows3D(x, y, z, x1, y1, z1,   colvar = sqrt((x1-x)^2+(y1-y)^2+(z1-z)^2), col = NULL,
         lwd = 2, d = 3, clab = c("B"), 
         main = "Arrows 3D t=260", bty ="g", ticktype = "detailed", phi = 0)            
          
          
}


dev.off()

