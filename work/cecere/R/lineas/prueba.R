library('RANN')
library("scatterplot3d")


KernelGadget <- function(dist,hsml)
{
  COEF_1 =  2.54647908947
  COEF_2 = 15.278874536822
  COEF_5 =  5.092958178941
  NORM   = 8.0/3.1415

  if(dist>hsml)
  {
    fac=0.0
  }
  else
  {
    hinv=1./hsml
    hinv3=(1./hsml)**3.
    u=dist*hinv
    if(u<0.5)
    { 
      fac=hinv3*(1.0 + 6.0 * ( u - 1.0)*u*u)*NORM 
    }
    else
    {
      fac=hinv3 * 2.0* (1.-u) * (1.-u) * (1.-u) *NORM
    }
  }
  return(fac)
}
library('RANN')
library("scatterplot3d")
Npart = 1000                     # sph particles

# x = seq(0,1,by=0.1)#runif(Npart)
# y = seq(0,1,by=0.1)#runif(Npart)
# z = seq(0,1,by=0.1)#runif(Npart)

# posiciones de las particulas
xp = runif(Npart) -0.5
yp = runif(Npart) -0.5
zp = runif(Npart) -0.5

data <- data.frame(xp,yp,zp)

hsml = NULL # esto lo hago para que al principio concatene con nada, y luego concatene con lo que sigue
for(i in 1:Npart)
{
  kc <- nn2(data, data[i,],k=6)  # data[i,] es el xp, yp, zp de la particula i
# if(i==1)hsml = max(kc$nn.dist) # k = 6 son los cinco vecinos (ella misma es la 6)
# if(i>=2)
  hsml = c(hsml,max(kc$nn.dist))
}

hsmlmax = max(hsml)

xmin=min(xp)
xmax=max(xp)
xlong=abs(xmax-xmin)
ymin=min(yp)
ymax=max(yp)
ylong=abs(ymax-ymin)
zmin=min(zp)
zmax=max(zp)
zlong=abs(zmax-zmin)

# defino nx, ny, nz, los numeros de celdas que voy a tener por lado
nx = floor(xlong/hsmlmax)
ny = floor(ylong/hsmlmax)
nz = floor(zlong/hsmlmax)
# nx = 4 
# ny = 4
# nz = 4 


# construyendo el ancho de cada celda
deltax = xlong/nx
deltay = ylong/ny
deltaz = zlong/nz

ll=xp*0-1

lirst=array(0,dim=c(nx,ny,nz))

for(i in 1:length(xp))   #lazo sobre todas las particulas size(ll)=Npart
{
# para cada posicion del la particula i (pos$x[i],pos$y[i],pos$z[i])
# 0 < x <longx ; calculo en que celda cae:
  ix=floor((xp[i]-xmin)/deltax)+1    #1<=ix<=nx
  if(ix>nx)ix=nx
  iy=floor((yp[i]-ymin)/deltay)+1    #1<=iy<=ny
  if(iy>ny)iy=ny
  iz=floor((zp[i]-zmin)/deltaz)+1    #1<=iz<=nz
  if(iz>nz)iz=nz
  lirst[ix,iy,iz]=i
}

for(i in 1:length(xp))
{
  ix=floor((xp[i]-xmin)/deltax)+1 
  if(ix>nx)ix=nx
  iy=floor((yp[i]-ymin)/deltay)+1
  if(iy>ny)iy=ny
  iz=floor((zp[i]-zmin)/deltaz)+1    
  if(iz>nz)iz=nz
# aca va ordenando las particulas
  ll[lirst[ix,iy,iz]]=i
# aca reconstruyo el lirst
  lirst[ix,iy,iz]=i
}

# campo analitico
bxp = -yp
byp =  xp
bzp = xp*0.

# delta de integracion de la linea
dl = 0.001

# numero de lineas
nlines = 1

xlv = NULL; ylv = NULL; zlv = NULL
bxl = NULL; byl = NULL; bzl = NULL
for(il in 1:nlines)
{
  xl = -0.2#runif(1)*xlong  #donde quiero calcular el campo, ahora un numero random
  yl = 0.001#runif(1)*ylong
  zl = 0.001#runif(1)*zlong
  
  
  for(itime in 1:8000)
  {
  
    xlv=c(xlv,xl)
    ylv=c(ylv,yl)
    zlv=c(zlv,zl)
  
#   en que celda está el lugar a estudiar?
    ix = floor((xl-xmin)/deltax) + 1 
    iy = floor((yl-ymin)/deltay) + 1
    iz = floor((zl-zmin)/deltaz) + 1
  
# guardo los valores de bxl, byl, bzl, por si alguna posicion no tiene vecinos
# y por lo tanto no voy a poder calcular el campo. Me quedo con el campo anterior
# y lo avanzo un dl de nuevo, hasta que encuentre vecinos la posicion
    bxold=bxl
    byold=byl
    bzold=bzl
    
    bxl = 0.
    byl = 0.
    bzl = 0.
    borrar = 0.
    
#   los tres lazos son para pasearse por todos las celdas vecinas
#   incluida la celda en la que estoy  
    for(iix in ix-1:ix+1)
    {
      if(iix==0 || iix==nx+1)next
    
      for(iiy in iy-1:iy+1)
      {
        if(iiy==0 || iiy==ny+1)next
     
        for(iiz in iz-1:iz+1)
        {
          if(iiz==0 || iiz==nz+1)next
        

        
#         me fijo cual es la última partícula de la celda -> it
          it = lirst[iix,iiy,iiz]
          if(it != 0)    # o sea que la celda no esta vacía
          {
            lgt=TRUE
            while(lgt)        #recorro todas las particulas de esa celda
            {
          
              it = ll[it]     #tomo la siguiente particula (si es la primera vez es la primer particula de la celda)
#             print(c("it",it))
              x_p = xp[it]  #tomo la posicion de la particula it
              y_p = yp[it]
              z_p = zp[it]
           
#             calculo de distancia respecto al punto donde quiero saber el campo
              dc = (xl-x_p)**2 + (yl-y_p)**2 + (zl-z_p)**2
              dist = sqrt(dc)

#             toca el pto campo con su Hsml?
              if(dist < hsml[it])
              {
#               print("es menor")
                #fac = KernelGadget(dist, hsml[it])   #me devuelve el factor que necesito
#               calculo el valor del campo en el punto deseado
                borrar = borrar + 1.
#               print(c("borrar",borrar))
                bxl = bxl + bxp[it] #*fac
                byl = byl + byp[it] #*fac
                bzl = bzl + bzp[it] #*fac
#               print(c(it,bxp,byp,bzp))
#               bxp[il] = bxp[il] + bx[it]*fac
#               byp[il] = byp[il] + by[it]*fac
#               bzp[il] = bzp[il] + bz[it]*fac
              }
              if(it == lirst[iix,iiy,iiz])lgt=FALSE
              
            }
          }
        }
      }
    }
  
    if(borrar != 0)
    {
      bxl = bxl/borrar
      byl = byl/borrar
      bzl = bzl/borrar

    } else
    {
       bxl = bxold
       byl = byold
       bzl = bzold
    }
  
    xl = xl + bxl*dl
    yl = yl + byl*dl
    zl = zl + bzl*dl 
  

  }
# YA ESTA DADO EL CAMPO EN EL PUNTO  
# ACA DEBERIA IR LA PARTE DEL AVANCE TEMPORAL PARA CONSTRUIR LA LINEA DE CAMPO

} #nlines



fieldline <- function(xl,yl,zl,bxl,byl,bzl)
{
}
