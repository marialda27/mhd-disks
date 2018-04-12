sigue <-
function(N0,N1,step)
{
library('snapshot')
NN=N1-N0
ID=1022019
base='./etacs/snapshot_'
cx=c(1:NN)
cy=c(1:NN)
rr=paste0(rep(base,NN),sprintf("%03d",c(N0:N1)*step))
#lwht=c('POS ','VEL ','ID  ','MASS','U   ','RHO ','HSML')
iii=0
for (cada in rr){
   iii=iii+1
   print(cada)
  # h=snap.read.2(cada,'HEAD',debug=1)
   iid=snap.read.2(cada,'ID  ')
   po=snap.read.2(cada,'POS ')
   cx[iii]=po$x[which(iid[] == ID)]
   cy[iii]=po$y[which(iid[] == ID)]
}
return(data.frame(cx=cx,cy=cy))
}
