dat <- matrix(runif(40,1,20),ncol=4) # make data
matplot(dat, type = c("b"),pch=1,col = 1:4) #plot
legend("topleft", legend = 1:4, col=1:4, pch=1) # optional legend

library("rgl")
f <- function(x, y) sin(10 * x * y) * cos(4 * y^3) + x

nx <- 30
ny <- 100
x <- seq(0, 1, length = nx)
y <- seq(0, 1, length = ny)
z <- outer(x, y, FUN = f)
axes3d()
for (i in 1:nx) lines3d(x[i], y, z[i, ], col = 'red', lwd = 2)
