data=read.table("lineas.dat")
plot(data$V1,data$V2,pch=".")

scatterplot3d(xlv, ylv, zlv,pch=".")

scatterplot3d(xp, yp, zp,pch=".")
plot(xp, yp,pch=".",xlim=c(-5,5),ylim=c(-5,5))

plot(xlv, ylv,xlim=c(-5,5),ylim=c(-5,5))

xmax = NULL
for(i in 1:nlines)
{
maxi = max(tt[i][[1]]$x)
xmax=max(c(xmax,maxi))
}

xmin = NULL
for(i in 1:nlines)
{
mini = min(tt[i][[1]]$x)
xmin=min(c(xmin,mini))
}

ymax = NULL
for(i in 1:nlines)
{
maxi = max(tt[i][[1]]$y)
ymax=max(c(ymax,maxi))
}

ymin = NULL
for(i in 1:nlines)
{
mini = min(tt[i][[1]]$y)
ymin=min(c(ymin,mini))
}

xmin = -5.
xmax =  5.
ymin = -5.
ymax =  5.

plot(tt[1][[1]]$x,tt[1][[1]]$y,yaxt="n",xaxt="n",type="b",pch=21,col="red",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
axis(2, col.axis="red", las=2, cex.axis=0.7, tck=-.01)
axis(1, col.axis="red", las=2, cex.axis=0.7, tck=-.01)
par(new=T)
lines(tt[2][[1]]$x,tt[2][[1]]$y,type="b", pch=22, col="blue", lty=2,yaxt="n",xaxt="n")
par(new=T)
lines(tt[3][[1]]$x,tt[3][[1]]$y,type="b", pch=22, col="green", lty=2,yaxt="n",xaxt="n")
par(new=T)
lines(tt[4][[1]]$x,tt[4][[1]]$y,type="b", pch=22, col="black", lty=2,yaxt="n",xaxt="n")
par(new=T)
lines(tt[5][[1]]$x,tt[5][[1]]$y,type="b", pch=22, col="cyan", lty=2,yaxt="n",xaxt="n")
par(new=T)
lines(tt[6][[1]]$x,tt[6][[1]]$y,type="b", pch=22, col="orange", lty=2,yaxt="n",xaxt="n")
par(new=T)
lines(tt[7][[1]]$x,tt[7][[1]]$y,type="b", pch=22, col="magenta", lty=2,yaxt="n",xaxt="n")
par(new=F)
