install.packages("FNN")
install.packages("RANN")

library("scatterplot3d")
library("FNN")
library("RANN")


data<- query<- cbind(1:10, 1:10)
scatterplot3d(data$x, data$y, data$z,pch=".")

get.knn(data, k=5)

knn.index(data, k=5)


# Arguments
# data 	
# 
# An input-output dataset. FOR nn THE OUTPUT MUST BE IN THE RIGHT MOST COLUMN OF A DATA FRAME OR MATRIX. nn2 uses ALL columns.
# query 	
# 
# nn2: A set of points that will be queried against data - must have same number of columns.
# mask 	
# 
# nn: A vector of 1's and 0's representing input inclusion/exclusion. The default mask is all 1's (i.e. include all inputs in the test).
# p 	
# 
# nn:The maximum number of near neighbours to compute. The default value is set to 10.
# k 	
# 
# nn2:The maximum number of near neighbours to compute. The default value is set to 10.
# treetype 	
# 
# nn2: Either the standard kd tree or a bd (box-decomposition, AMNSW98) tree which may perform better for larger point sets
# searchtype 	
# 
# nn2: See details
# radius 	
# 
# nn2: radius of search for searchtype='radius'
# eps 	
# 
# nn2: error bound: default of 0.0 implies exact nearest neighbour search



# A noisy sine wave example
x1 <- runif(100, 0, 2*pi)
x2 <- runif(100, 0,3)
e  <- rnorm(100, sd=sqrt(0.075)) 
y <- sin(x1) + 2*cos(x2) + e
DATA <- data.frame(x1, x2, y)
nearest <- nn2(DATA)


library("RANN")
x=c(-2,-1,0,1,2)
y=c(1,1,1,1,1)
z=c(0,0,3,0,0)
data <- data.frame(x,y,z)


for(i in 1:5)
{
kc <- nn2(data, data[i,],k=4)
if(i==1)hsml = max(kc$nn.dist)
if(i>=2)hsml = c(hsml,max(kc$nn.dist))
}


scatterplot3d(data$x, data$y, data$z)
nearest <- nn2(data)
k <- nn2(data, data[2,],k=3)
hsml = max(k$nn.dist)





nn2(DATA, DATA[1,1,1],k=3)



scatterplot3d(DATA$x1, DATA$x2, DATA$y)

nn(data, mask=rep.int(1, times=ncol(data)-1), p=min(10,nrow(data)))

nn2(data, query, k=min(10,nrow(data)),treetype=c("kd","bd"),
	searchtype=c("standard","priority","radius"),radius=0.0,eps=0.0)

nn2(DATA, DATA[1,1,1],k=3)

# del punto DATA[1,1,1] me tira las posicion x, y, z de los primeros dos vecinos
# > nn2(DATA, DATA[1,1,1],k=2)  del punto DATA[1,1,1] me tira las posicion x, y, z de los primeros dos vecinos
# $nn.idx
#      [,1] [,2]
# [1,]   79   10
# [2,]    7   43
# [3,]    7   43
# 
# $nn.dists
#          [,1]     [,2]
# [1,] 4.415623 4.463121
# [2,] 1.520040 1.545870
# [3,] 1.520040 1.545870

scatterplot3d(data$x, data$y, data$z)