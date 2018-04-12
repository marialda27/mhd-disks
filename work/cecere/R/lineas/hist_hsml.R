library('snapshot')

hs=snap.read.2('snapshot_000','HSML',debug=1,gas=1)
hc=hist(hs)
par(mar=c(5, 4, 4, 8) + 0.1)     

plot(hc$mids, hc$counts,type="b",pch=21,col="red",lty=3,xlab="hsml", 
ylab="dist", xlim=c(min(hs),100),yaxt="n",ylim=c(min(hc$counts),max(hc$counts)))

axis(2,col.axis="red", las=2, cex.axis=0.7, tck=-.01)
  
