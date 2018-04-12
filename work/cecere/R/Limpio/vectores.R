
library('snapshot')

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
ind <- sample(nrow(data), 5000, replace=FALSE)

x = data[ind[],1]
y = data[ind[],2]
bx = data[ind[],3]
by = data[ind[],4]

plot(NA, xlim=c(-5,5), ylim=c(-5,5), xlab="X", ylab="Y")
arrows(x,y,x+log10(sqrt(bx*bx+by*by))*bx/sqrt(bx*bx+by*by)/10,y+log10(sqrt(bx*bx+by*by))*by/sqrt(bx*bx+by*by)/10,length=0.05)


# arrows(x,y,x+200*bx,y+200*by,length=0.1)



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
