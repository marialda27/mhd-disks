# install.packages("scatterplot3d") # Install
library("scatterplot3d") # load
plot(x, y)
plot(x,y,pch=".")
scatterplot3d(x, y, z,pch=".")
ls('package:snapshot')


library('snapshot')
h=snap.read.2('snapshot_356_controlrun','POS ',debug=1) 
plot(h$x, h$y,pch=".")

g=snap.read.2('snapshot_356_controlrun','RHO ',debug=1) 
gg=hist(g)
plot(gg$mids,gg$counts,log="y")



library('snapshot')

g=snap.read.2('snapshot_356_controlrun','RHO ',debug=1) 
gg=hist(g)
j=snap.read.2('snapshot_356_etacs','RHO ',debug=1) 
jj=hist(j)

# create extra margin room on the right for an axis
par(mar=c(5, 4, 4, 8) + 0.1)
# plot hist de snapshot_356_controlrun
#plot(jj$mids, jj$counts,log="y",type="b",yaxt="n",pch=21,col="red",lty=3,xlab="dens", ylab="prob", xlim=c(0,0.11))
plot(jj$mids, jj$counts,log="y",type="b",pch=21,col="red",lty=3,xlab="dens", ylab="dist", xlim=c(0,0.11))
# add hist de snapshot_356_etacs
lines(gg$mids, gg$counts,type="b", pch=22, col="blue", lty=2)
# draw an axis on the left
#axis(2, at=jj$counts,labels=jj$counts, col.axis="red", las=2)
# draw an axis on the right, with smaller text and ticks
axis(4, at=gg$counts,labels=round(gg$counts,digits=2),
  col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
# add a title for the right axis
mtext("etacs", side=4, line=3, cex.lab=1,las=2, col="blue")
mtext("controlrun", side=4, line=3, cex.lab=1,las=2, col="red", at = 1800)  
# add a main title and bottom and left axis labels
title("Comparacion distribucion de densidad t=356")

library('snapshot')
uj=snap.read.2('snapshot_356_controlrun','U   ',debug=1) 
ug=snap.read.2('snapshot_356_etacs','U   ',debug=1) 
tempj=(5/3-1)*uj
tempg=(5/3-1)*ug
tempj_hist=hist(tempj)
tempg_hist=hist(tempg)
jj=tempj_hist
gg=tempg_hist


# create extra margin room on the right for an axis
par(mar=c(5, 4, 4, 8) + 0.1)
# plot hist de snapshot_356_controlrun
#plot(jj$mids, jj$counts,log="y",type="b",yaxt="n",pch=21,col="red",lty=3,xlab="dens", ylab="prob", xlim=c(0,0.11))
plot(jj$mids, jj$counts,log="y",type="b",pch=21,col="red",lty=3,xlab="temp", ylab="dist", xlim=c(0,max(tempj,tempg)))
# add hist de snapshot_356_etacs
lines(gg$mids, gg$counts,type="b", pch=22, col="blue", lty=2)
# draw an axis on the left
#axis(2, at=jj$counts,labels=jj$counts, col.axis="red", las=2)
# draw an axis on the right, with smaller text and ticks
axis(4, at=gg$counts,labels=round(gg$counts,digits=2),
  col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
# add a title for the right axis
mtext("etacs", side=4, line=3, cex.lab=1,las=2, col="blue")
mtext("controlrun", side=4, line=3, cex.lab=1,las=2, col="red", at = 1800)  
# add a main title and bottom and left axis labels
title("Comparacion distribucion de densidad t=356")


library("scatterplot3d")
scatterplot3d(h$x, h$y, h$z, pch=".")

plot(h$x, h$z,pch=".",xlim=c(-200,200))
plot(h$x, h$y,pch=".",xlim=c(-5,5),ylim=c(-5,5))






library('snapshot')
h=snap.read.2('snapshot_356_etacs','POS ',debug=1) 
plot(h$x, h$y,pch=".")

g=snap.read.2('snapshot_356_etacs','RHO ',debug=1) 
gg=hist(g)
plot(gg$mids,gg$counts,log="y")


gg=hist(h)
plot(gg$mids,gg$counts)
plot(gg$mids,gg$counts,log="y")
