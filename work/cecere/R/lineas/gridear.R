gridear <- function(dd,scales)
{

    xmin=min(dd$x)
    xmax=max(dd$x)
    ymin=min(dd$y)
    ymax=max(dd$y)
    nx=floor((xmax-xmin)/scales[1])
    ny=floor((ymax-ymin)/scales[2])

    lirst=matrix(rep(-1,nx*ny),c(nx,ny))
    hm=matrix(rep(0,nx*ny),c(nx,ny))
    ll=dd$x*0-1

    for(i in 1:length(dd$x))
    {
        ix=floor(((dd$x[i]-xmin)/(xmax-xmin))*nx)+1
        if(ix>nx)ix=nx
        iy=floor(((dd$y[i]-ymin)/(ymax-ymin))*ny)+1
        if(iy>ny)iy=ny
        lirst[ix,iy]=i
    }

    for(i in 1:length(dd$x))
    {
        ix=floor(((dd$x[i]-xmin)/(xmax-xmin))*nx)+1
        if(ix>nx)ix=nx
        iy=floor(((dd$y[i]-ymin)/(ymax-ymin))*ny)+1
        if(iy>ny)iy=ny
        ll[lirst[ix,iy]]=i
        lirst[ix,iy]=i
        hm[ix,iy]=hm[ix,iy]+1
    }

    return(list(ll=ll,lirst=lirst,hm=hm,nx=nx,ny=ny))

}
