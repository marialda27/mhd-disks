
library('snapshot')
library("plot3D")

jpeg("disco3d_260.jpeg")

pos=snap.read.2('../../../R/etacs/snapshot_260','POS ',debug=1,gas=1)

xp = pos$x
yp = pos$y
zp = pos$z

b=snap.read.2('../../../R/etacs/snapshot_260','BFLD',debug=1,gas=1)

bxp = b$x
byp = b$y
bzp = b$z

xpc=xp[which(-5 < xp & xp < 5 & -5 < yp & yp < 5 & -10 < zp & zp < 10)]
ypc=yp[which(-5 < xp & xp < 5 & -5 < yp & yp < 5 & -10 < zp & zp < 10)]
zpc=zp[which(-5 < xp & xp < 5 & -5 < yp & yp < 5 & -10 < zp & zp < 10)]
bxpc=bxp[which(-5 < xp & xp < 5 & -5 < yp & yp < 5 & -10 < zp & zp < 10)]
bypc=byp[which(-5 < xp & xp < 5 & -5 < yp & yp < 5 & -10 < zp & zp < 10)]
bzpc=bzp[which(-5 < xp & xp < 5 & -5 < yp & yp < 5 & -10 < zp & zp < 10)]

data <- data.frame(xpc,ypc,zpc,bxpc,bypc,bzpc)
ind <- sample(nrow(data), 1000, replace=FALSE)

x = data[ind[],1]
y = data[ind[],2]
z = data[ind[],3]
bx = data[ind[],4]
by = data[ind[],5]
bz = data[ind[],6]

minlog = floor(log10(min(sqrt(bxp*bxp+byp*byp+bzp*bzp))))
maxlog = floor(log10(max(sqrt(bxp*bxp+byp*byp+bzp*bzp))))
corr = maxlog-minlog

# plot(NA, xlim=c(-5,5), ylim=c(-5,5), xlab="X", ylab="Y")
# 
# plot(NA, xlim=c(-3.2,-1.5), ylim=c(-2.6,2), xlab="X", ylab="Y")
# arrows(x,y,x+(log10(sqrt(bx*bx+by*by))+corr)*bx/sqrt(bx*bx+by*by)/corr,y+(log10(sqrt(bx*bx+by*by))+corr)*by/sqrt(bx*bx+by*by)/corr,length=0.05)
# 
# arrows(x,y,x+(log10(sqrt(bx*bx+by*by))+corr)*bx/sqrt(bx*bx+by*by)/6.,y+(log10(sqrt(bx*bx+by*by))+corr)*by/sqrt(bx*bx+by*by)/6.,length=0.05)

x1 = x+(log10(sqrt(bx*bx+by*by+bz*bz))+corr)*bx/sqrt(bx*bx+by*by+bz*bz)/corr
y1 = y+(log10(sqrt(bx*bx+by*by+bz*bz))+corr)*by/sqrt(bx*bx+by*by+bz*bz)/corr 
z1 = z+(log10(sqrt(bx*bx+by*by+bz*bz))+corr)*bz/sqrt(bx*bx+by*by+bz*bz)/corr 
# phi = sign(atan2(y1,x1))
# 
# which(phi < 0.)
# 
# x=x[which(phi < 0.)]
# y=y[which(phi < 0.)]
# x1=x1[which(phi < 0.)]
# y1=y1[which(phi < 0.)]

# arrows2D(x, y,  x1, y1,  colvar = sqrt((x1-x)^2+(y1-y)^2), col = NULL,
#          lwd = 3, clab = c("B"), 
#           bty ="n", xlim = c(-5, 5), ylim = c(-5, 5),type = "triangle",length=0.05)

arrows3D(x, y, z, x1, y1, z1, colvar = sqrt((x1-x)^2+(y1-y)^2+(z1-z)^2), col = NULL,
         lwd = 2, d = 3, clab = c("B"), 
         main = "Arrows 3D t=260", bty ="g", ticktype = "detailed", phi = 20)          
          
# arrows2D(x, y,  x1, y1,  colvar = sqrt((x1-x)^2+(y1-y)^2), col = NULL,
#          lwd = 3, clab = c("B"), 
#           bty ="n", xlim = c(-5, 5), ylim = c(-5, 5),type = "triangle",length=0.05)


          
          
# plot(NA, xlim=c(-3.2,-1.5), ylim=c(-2.6,2), xlab="X", ylab="Y")
# arrows(x,y,x+2000*bx,y+2000*by,length=0.05)


dev.off()

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
