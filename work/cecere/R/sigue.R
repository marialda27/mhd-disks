sigue <-
function(N0,N1,step)
{

library('snapshot')
N0=300
N1=370
step=1
NN=N1-N0
# ID=1022019  416837 416897 423349 423353 423951 423985 423990
ID=423985
base='/home/cecere/sshfs/is2/fstasys/MHD_Disc/etacs/snapshot_'
cx=c(1:NN)
cy=c(1:NN)
rr=paste0(rep(base,NN),sprintf("%03d",c(N0:N1)*step))
#lwht=c('POS ','VEL ','ID  ','MASS','U   ','RHO ','HSML')
iii=0
for (cada in rr){
#for (i=0, i < lenght(rr))
#    cada=rr[i]
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
plot(cx,cy,xlim=c(-30,30),ylim=c(-30,30),type = "o", col = "red")
