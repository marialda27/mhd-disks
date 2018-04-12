library('snapshot')


rhoc=snap.read.2('snapshot_000_controlrun','RHO ',debug=1) 
hc=hist(rhoc)
rhoe=snap.read.2('snapshot_000_etacs','RHO ',debug=1) 
he=hist(rhoe)



pdf("dens_t0.pdf")
# create extra margin room on the right for an axis
par(mar=c(5, 4, 4, 8) + 0.1)
# plot hist de controlrun
plot(hc$mids, hc$counts,log="y",type="b",pch=21,col="red",lty=3,xlab="dens", ylab="dist", xlim=c(0,max(rhoc,rhoe)),yaxt="n",ylim=c(min(hc$counts,he$counts),max(hc$counts,he$counts)))
# add hist de etacs
lines(he$mids, he$counts,type="b", pch=22, col="blue", lty=2)
# draw an axis on the left
# axis(2, at=cc$counts,labels=cc$counts, col.axis="red", las=2)
axis(2, at=hc$counts,labels=round(hc$counts,digits=2),
  col.axis="red", las=2, cex.axis=0.7, tck=-.01)
# draw an axis on the right, with smaller text and ticks
axis(4, at=he$counts,labels=round(he$counts,digits=2),
  col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
# add a title for the right axis
mtext("etacs", side=4, line=3, cex.lab=1,las=2, col="blue")
mtext("controlrun", side=4, line=3, cex.lab=1,las=2, col="red", at = 1100)  
# add a main title and bottom and left axis labels
title("Distribucion de densidad t=0")
dev.off()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# library('snapshot')
uc=snap.read.2('snapshot_000_controlrun','U   ',debug=1) 
ue=snap.read.2('snapshot_000_etacs','U   ',debug=1) 

tempc=(5./3.-1.)*uc
tempe=(5./3.-1.)*ue
hc=hist(tempc)
he=hist(tempe)


pdf("temp_t0.pdf")
# create extra margin room on the right for an axis
par(mar=c(5, 4, 4, 8) + 0.1)
# plot hist de controlrun
plot(hc$mids, hc$counts,log="y",type="b",pch=21,col="red",lty=3,xlab="temp", ylab="dist", xlim=c(0,max(tempc,tempe)),yaxt="n",ylim=c(min(hc$counts,he$counts)+0.1,max(hc$counts,he$counts)))
# add hist de etacs
lines(he$mids, he$counts,type="b", pch=22, col="blue", lty=2)
# draw an axis on the left
# axis(2, at=cc$counts,labels=cc$counts, col.axis="red", las=2)
axis(2, at=hc$counts,labels=round(hc$counts,digits=2),col.axis="red", las=2, cex.axis=0.7, tck=-.01)
# draw an axis on the right, with smaller text and ticks
axis(4, at=he$counts,labels=round(he$counts,digits=2),col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
# add a title for the right axis
mtext("etacs", side=4, line=3, cex.lab=1,las=2, col="blue")
mtext("controlrun", side=4, line=3, cex.lab=1,las=2, col="red", at = 1100)  
# add a main title and bottom and left axis labels
title("Distribucion de energia interna t=0")

dev.off()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# library('snapshot')
# rhoc=snap.read.2('snapshot_000_controlrun','RHO ',debug=1) 
# rhoe=snap.read.2('snapshot_000_etacs','RHO ',debug=1) 
# uc=snap.read.2('snapshot_000_controlrun','U   ',debug=1) 
# ue=snap.read.2('snapshot_000_etacs','U   ',debug=1)


# bc NO HAY INFO SOBRE EL CAMPO 
be=snap.read.2('snapshot_000_etacs',     'BFLD',debug=1) 
vc=snap.read.2('snapshot_000_controlrun','VEL ',debug=1) 
ve=snap.read.2('snapshot_000_etacs',     'VEL ',debug=1) 

modbe=be$x*be$x+be$y*be$y+be$z*be$z
modbe=modbe[1:1880577]
enmage=0.5*modbe*modbe
modve=ve$x*ve$x+ve$y*ve$y+ve$z*ve$z
modve=modve[1:1880577]
entote=enmage + ue + 0.5*rhoe*modve*modve
he=hist(entote)


enmagc=0.*modbe
modvc=vc$x*vc$x+vc$y*vc$y+vc$z*vc$z
modvc=modvc[1:1880577]
entotc=enmagc+uc+0.5*rhoc*modvc*modvc
hc=hist(entotc)

pdf("entot_t0.pdf")
# create extra margin room on the right for an axis
par(mar=c(5, 4, 4, 8) + 0.1)
# plot hist de controlrun
plot(hc$mids, hc$counts,log="y",type="b",pch=21,col="red",lty=3,xlab="en total", ylab="dist", xlim=c(0,max(entotc,entote)),yaxt="n",ylim=c(min(hc$counts,he$counts)+0.1,max(hc$counts,he$counts)))
# add hist de etacs
lines(he$mids, he$counts,type="b", pch=22, col="blue", lty=2)
# draw an axis on the left
# axis(2, at=cc$counts,labels=cc$counts, col.axis="red", las=2)
axis(2, at=hc$counts,labels=round(hc$counts,digits=2),col.axis="red", las=2, cex.axis=0.7, tck=-.01)
# draw an axis on the right, with smaller text and ticks
axis(4, at=he$counts,labels=round(he$counts,digits=2),col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
# add a title for the right axis
mtext("etacs", side=4, line=3, cex.lab=1,las=2, col="blue")
mtext("controlrun", side=4, line=3, cex.lab=1,las=2, col="red", at = 1100)  
# add a main title and bottom and left axis labels
title("Distribucion de energia interna t=0")

dev.off()



# ,xaxp  = c(0, max(c,e), 12)
