library('snapshot')

pdf("em_vs_t_outside.pdf")

be=snap.read.2('snapshot_000','BFLD',debug=1,gas=1) 
pos=snap.read.2('snapshot_000','POS ',debug=1,gas=1) 
r=sqrt(pos$x*pos$x+pos$y*pos$y)
z = sqrt(pos$z*pos$z)

# pdf("disco_xy_corte.pdf")
# plot(pos$x[which(r<7)],pos$y[which(r<7)],pch=".")
# dev.off()

# pdf("disco_xyz_corte.pdf")
# z = sqrt(pos$z*pos$z)
# scatterplot3d(pos$x[which(r<7 & z<1)],pos$y[which(r<7 & z<1)],pos$z[which(r<7 & z<1)],pch=".")
# dev.off()

bex=be$x[which(2<r & z>1)]
bey=be$y[which(2<r & z>1)]
bez=be$z[which(2<r & z>1)]

modbe=bex*bex + bey*bey + bez*bez
enmage=0.125*modbe/pi

enmageprom = median(enmage)

for (i in seq(10,90,10))
{
chartime=as.character(i)
namefile=paste0('snapshot_0',chartime)
be=snap.read.2(namefile,'BFLD',debug=1,gas=1) 
pos=snap.read.2(namefile,'POS ',debug=1,gas=1) 
r=sqrt(pos$x*pos$x+pos$y*pos$y)
z = sqrt(pos$z*pos$z)

bex=be$x[which(2<r & z>1)]
bey=be$y[which(2<r & z>1)]
bez=be$z[which(2<r & z>1)]

modbe=bex*bex + bey*bey + bez*bez
enmage=0.125*modbe/pi
enmageprom = c(enmageprom,median(enmage))
}

for (i in seq(100,350,10))
{
chartime=as.character(i)
namefile=paste0('snapshot_',chartime)
be=snap.read.2(namefile,'BFLD',debug=1,gas=1) 
pos=snap.read.2(namefile,'POS ',debug=1,gas=1) 
r=sqrt(pos$x*pos$x+pos$y*pos$y)
z = sqrt(pos$z*pos$z)

bex=be$x[which(2<r & z>1)]
bey=be$y[which(2<r & z>1)]
bez=be$z[which(2<r & z>1)]

modbe=bex*bex + bey*bey + bez*bez
enmage=0.125*modbe/pi
enmageprom = c(enmageprom,median(enmage))
}

par(mar=c(5, 5, 3, 3))
plot(seq(0,350,10),enmageprom,log="y",xlab="time", ylab=expression(paste(E[mag]," ","[",erg/cm^3,"]" )))

title("Magnetic energy outside the disk")
dev.off()
