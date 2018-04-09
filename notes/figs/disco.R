library('snapshot')
h=snap.read.2('snapshot_000','POS ')

# plot(h$x, h$y,pch=".")
# plot(h$x, h$z,pch=".")

pdf("disk_xz.pdf")
plot(h$x, h$z,pch=".",xlim=c(-8,8),ylim=c(-8,8),at=-8:8)
title("Disk xz")

# pdf("disk_xy.pdf")
# plot(h$x, h$y,pch=".",xlim=c(-8,8),ylim=c(-8,8),at=-8:8)
# title("Disk xy")

dev.off()

