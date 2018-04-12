
library('snapshot')
library("plot3D")

pdf("disco_izq.pdf")

pos=snap.read.2('../../../R/etacs/snapshot_080','POS ',debug=1,gas=1)

xp = pos$x
yp = pos$y

b=snap.read.2('../../../R/etacs/snapshot_080','BFLD',debug=1,gas=1)

bxp = b$x
byp = b$y


xpc=xp[which(-5 < xp & xp < 5 & -5 < yp & yp < 5)]
ypc=yp[which(-5 < xp & xp < 5 & -5 < yp & yp < 5)]
bxpc=bxp[which(-5 < xp & xp < 5 & -5 < yp & yp < 5)]
bypc=byp[which(-5 < xp & xp < 5 & -5 < yp & yp < 5)]

data <- data.frame(xpc,ypc,bxpc,bypc)
ind <- sample(nrow(data), 1000, replace=FALSE)

x = data[ind[],1]
y = data[ind[],2]
bx = data[ind[],3]
by = data[ind[],4]

minlog = floor(log10(min(sqrt(bxp*bxp+byp*byp))))
maxlog = floor(log10(max(sqrt(bxp*bxp+byp*byp))))
corr = maxlog-minlog

# plot(NA, xlim=c(-5,5), ylim=c(-5,5), xlab="X", ylab="Y")
# 
# plot(NA, xlim=c(-3.2,-1.5), ylim=c(-2.6,2), xlab="X", ylab="Y")
# arrows(x,y,x+(log10(sqrt(bx*bx+by*by))+corr)*bx/sqrt(bx*bx+by*by)/corr,y+(log10(sqrt(bx*bx+by*by))+corr)*by/sqrt(bx*bx+by*by)/corr,length=0.05)
# 
# arrows(x,y,x+(log10(sqrt(bx*bx+by*by))+corr)*bx/sqrt(bx*bx+by*by)/6.,y+(log10(sqrt(bx*bx+by*by))+corr)*by/sqrt(bx*bx+by*by)/6.,length=0.05)

x1 = x+(log10(sqrt(bx*bx+by*by))+corr)*bx/sqrt(bx*bx+by*by)/corr
y1 = y+(log10(sqrt(bx*bx+by*by))+corr)*by/sqrt(bx*bx+by*by)/corr 

bphi = -bx*sin(atan2(y,x))+by*cos(atan2(y,x))
# x=x[which(bphi < 0.)]
# y=y[which(bphi < 0.)]
# x1=x1[which(bphi < 0.)]
# y1=y1[which(bphi < 0.)]

x=x[which(bphi > 0.)]
y=y[which(bphi > 0.)]
x1=x1[which(bphi > 0.)]
y1=y1[which(bphi > 0.)]


# phi = sign(atan2(y1,x1))
# 
# which(phi < 0.)
# 
# x=x[which(phi < 0.)]
# y=y[which(phi < 0.)]
# x1=x1[which(phi < 0.)]
# y1=y1[which(phi < 0.)]

arrows2D(x, y,  x1, y1,  colvar = sqrt((x1-x)^2+(y1-y)^2), col = NULL,
         lwd = 3, clab = c("B"), 
          bty ="n", xlim = c(-5, 5), ylim = c(-5, 5),type = "triangle",length=0.05)

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
