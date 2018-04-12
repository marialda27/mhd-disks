library('foreach')
library('doMC')
registerDoMC(4)

tt <- foreach(il=1:nlines) %dopar%
{
runif(1)
}

