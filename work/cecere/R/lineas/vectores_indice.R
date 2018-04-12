
library('snapshot')
library("plot3D")

pdf("disco_xz.pdf")

pos=snap.read.2('../../../R/etacs/snapshot_000','POS ',debug=1,gas=1)
xp = pos$x
zp = pos$z
id=snap.read.2('../../../R/etacs/snapshot_000','ID  ',debug=1,gas=1)
idc = id[which(-5 < xp & xp < 5 & 1 < zp & zp < 10)]

data <- data.frame(idc)
ind <- sample(nrow(data), 1000, replace=FALSE)

for (i in seq(10,90,10))
{

chartime=as.character(i)
namefile=paste0('../../../R/etacs/snapshot_0',chartime)

pos=snap.read.2(namefile,'POS ',debug=1,gas=1)
xp = pos$x
zp = pos$z

b=snap.read.2(namefile,'BFLD',debug=1,gas=1)
bxp = b$x
bzp = b$z

xpc = xp[ind]
zpc = zp[ind]
bxpc=bxp[ind]
bzpc=bzp[ind]

x = xpc
z = zpc
bx = bxpc
bz = bzpc

minlog = floor(log10(min(sqrt(bxp*bxp+bzp*bzp))))
maxlog = floor(log10(max(sqrt(bxp*bxp+bzp*bzp))))
corr = maxlog-minlog

x1 = x+(log10(sqrt(bx*bx+bz*bz))+corr)*bx/sqrt(bx*bx+bz*bz)/corr
z1 = z+(log10(sqrt(bx*bx+bz*bz))+corr)*bz/sqrt(bx*bx+bz*bz)/corr 

arrows2D(x, z,  x1, z1,  colvar = sqrt((x1-x)^2+(z1-z)^2), col = NULL,
         lwd = 3, clab = c("B"), 
          bty ="n", xlim = c(-5, 5), ylim = c(0, 10),type = "triangle",length=0.05)

}


dev.off()

