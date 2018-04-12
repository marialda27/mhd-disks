library("scatterplot3d") # load
library('snapshot')


pos=snap.read.2('/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_370','POS ',debug=1,gas=1)
x = pos$x
y = pos$y
z = pos$z
scatterplot3d(x, y, z,pch=".",xlim=c(-5,5),ylim=c(-5,5),zlim=c(-5,5))

plot(x, z,pch=".",xlim=c(-5,5),ylim=c(-5,5))
