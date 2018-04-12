
library("plot3D")

x0 <- c(0, 0, 0, 0)
y0 <- c(0, 0, 0, 0)
z0 <- c(0, 0, 0, 0)
x1 <- c(0.89, -0.46, 0.59, 0.96)
y1 <- c(0.36,  0.88, 0.02, 0.5)
z1 <- c(-0.28, 0.09, 0.05, 0.24)
cols <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")



plot(NA, xlim=c(-1,1), ylim=c(-1,1), xlab="X", ylab="Y")


arrows2D(x0, y0,  x1, y1,  colvar = sqrt((x1-x0)^2+(y1-y0)^2), col = cols,
         lwd = 2, clab = c("Quality", "score"), 
          bty ="n", xlim = c(-1, 1), ylim = c(-1, 1))

          
          
arrows(x0, y0,  x1, y1,  col = cols, lwd = 2, bty ="n")

