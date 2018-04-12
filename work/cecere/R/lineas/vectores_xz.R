
library('snapshot')
library("plot3D")

# pdf("disco_xz.pdf")

pos=snap.read.2('../../../R/etacs/snapshot_200','POS ',debug=1,gas=1)

id=snap.read.2('../../../R/etacs/snapshot_200','ID  ',debug=1,gas=1)

xp = pos$x
zp = pos$z

b=snap.read.2('../../../R/etacs/snapshot_200','BFLD',debug=1,gas=1)

bxp = b$x
bzp = b$z


xpc = xp[which(-5 < xp & xp < 5 & 1 < zp & zp < 10)]
zpc = zp[which(-5 < xp & xp < 5 & 1 < zp & zp < 10)]
bxpc=bxp[which(-5 < xp & xp < 5 & 1 < zp & zp < 10)]
bzpc=bzp[which(-5 < xp & xp < 5 & 1 < zp & zp < 10)]
idc = id[which(-5 < xp & xp < 5 & 1 < zp & zp < 10)]

data <- data.frame(xpc,zpc,bxpc,bzpc)
ind <- sample(nrow(data), 1000, replace=FALSE)

x = data[ind[],1]
z = data[ind[],2]
bx = data[ind[],3]
bz = data[ind[],4]

minlog = floor(log10(min(sqrt(bxp*bxp+bzp*bzp))))
maxlog = floor(log10(max(sqrt(bxp*bxp+bzp*bzp))))
corr = maxlog-minlog


x1 = x+(log10(sqrt(bx*bx+bz*bz))+corr)*bx/sqrt(bx*bx+bz*bz)/corr
z1 = z+(log10(sqrt(bx*bx+bz*bz))+corr)*bz/sqrt(bx*bx+bz*bz)/corr 

# phi = sign(atan2(y1,x1))
# 
# which(phi < 0.)
# 
# x=x[which(phi < 0.)]
# y=y[which(phi < 0.)]
# x1=x1[which(phi < 0.)]
# y1=y1[which(phi < 0.)]

arrows2D(x, z,  x1, z1,  colvar = sqrt((x1-x)^2+(z1-z)^2), col = NULL,
         lwd = 3, clab = c("B"), 
          bty ="n", xlim = c(-5, 5), ylim = c(0, 10),type = "triangle",length=0.05)



# dev.off()



# hist(log10(abs(bx)),breaks=100)
# 
# 
# xp = runif(100) - 0.5
# yp = runif(100) - 0.5
# 
# bxp = -yp
# byp =  xp
# 
# plot(NA, xlim=c(-1,1), ylim=c(-1,1), xlab="X", ylab="Y")
# 
# arrows(xp,yp,xp+bxp,yp+byp,length=0.1)
# 
# 
# x=c(-1,0.1,0.1,2,-0.1,-0.2)
# y=c(2,0.1,3,-1,-0.1,0.3)
# data <- data.frame(x,y)
# ind <- sample(nrow(data), 4, replace=FALSE)
# data[ind[2],]
