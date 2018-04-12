library('RANN')
library("scatterplot3d")
library('snapshot')
library('foreach')
library('doMC')
registerDoMC(4)


interp_field <- function(xl,yl,bold)
{

    bl=data.frame(bx=0.0,by=0.0)
#     borrar = 0.
    normfac = 0.
    
#   los tres lazos son para pasearse por todos las celdas vecinas
#   incluida la celda en la que estoy  
    for(iix in ix-1:ix+1)
    {
      if(iix==0 || iix==nx+1)next
    
      for(iiy in iy-1:iy+1)
      {
        if(iiy==0 || iiy==ny+1)next
     
              
#         me fijo cual es la última partícula de la celda -> it
          it = lirst[iix,iiy]
          if(it != 0)    # o sea que la celda no esta vacía
          {
            lgt=TRUE
            while(lgt)        #recorro todas las particulas de esa celda
            {
          
              it = ll[it]     #tomo la siguiente particula (si es la primera vez es la primer particula de la celda)
#             print(c("it",it))
              x_p = xp[it]  #tomo la posicion de la particula it
              y_p = yp[it]
              
           
#             calculo de distancia respecto al punto donde quiero saber el campo
              dc = (xl-x_p)**2 + (yl-y_p)**2 
              dist = sqrt(dc)

#             toca el pto campo con su Hsml?
              if(dist < hsml[it])
              {
#               print(c("es menor",dist))
#               print(c("hsml[it]",it,hsml[it]))
#               print(c("x_p, y_p",x_p,y_p))
#               print(c("xl, yl",xl,yl))
                fac = KernelGadget(dist, hsml[it])   #me devuelve el factor que necesito
#                 print(c("fac",fac))
#               calculo el valor del campo en el punto deseado
#                 borrar = borrar + 1.
                normfac = fac + normfac
#                 print(c("normfac",normfac))
#                 line <- readline() 
#               print(c("borrar",borrar))
                bl$bx = bl$bx + bxp[it]*fac
                bl$by = bl$by + byp[it]*fac
           
#               print(c(it,bxp,byp,bzp))
#               bxp[il] = bxp[il] + bx[it]*fac
#               byp[il] = byp[il] + by[it]*fac
#               bzp[il] = bzp[il] + bz[it]*fac
              }
              if(it == lirst[iix,iiy])lgt=FALSE
              
            }
          }
        
      }
    }
  
#     if(borrar != 0)
    if(normfac != 0)
    {
#       bl = bl/borrar
      bl = bl/normfac
#       print(bl)
    } else
    {
      bl = bold
    }
    return(bl)
}


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

# pos=snap.read.2('/home/cecere/Descargas/snapshot_000','POS ',debug=1,gas=1)
pos=snap.read.2('../../../R/etacs/snapshot_080','POS ',debug=1,gas=1)

posx=pos$x[which(-5 < pos$x & pos$x < 5 & -5 < pos$y & pos$y < 5)]
posy=pos$y[which(-5 < pos$x & pos$x < 5 & -5 < pos$y & pos$y < 5)]

Npart = length(posx)
# Npart = nrow(pos)                     # sph particles
# Npart = 1000

# x = seq(0,1,by=0.1)#runif(Npart)
# y = seq(0,1,by=0.1)#runif(Npart)
# z = seq(0,1,by=0.1)#runif(Npart)

# posiciones de las particulas
# xp = runif(Npart) - 0.5
# yp = runif(Npart) - 0.5
# zp = runif(Npart) - 0.5

# xp = pos$x
# yp = pos$y

xp = posx
yp = posy

assign("xp",xp, envir = .GlobalEnv)
assign("yp",yp, envir = .GlobalEnv)


# data <- data.frame(xp,yp,zp)
# 
# hsml = NULL # esto lo hago para que al principio concatene con nada, y luego concatene con lo que sigue
# # construyo el hsml de los datos, usando la libreria RANN
# for(i in 1:Npart)
# {
#   kc <- nn2(data, data[i,],k=9)  # data[i,] es el xp, yp, zp de la particula i
#   hsml = c(hsml,max(kc$nn.dist))
# }


hsml=snap.read.2('../../../R/etacs/snapshot_080','HSML',debug=1,gas=1)
# pido el maximo de los hsml
# hsmlmax = max(hsml)

hsml=hsml[which(-5 < pos$x & pos$x < 5 & -5 < pos$y & pos$y < 5)]
hsmlmax = median(hsml)

xmin=min(xp)
xmax=max(xp)
xlong=abs(xmax-xmin)
ymin=min(yp)
ymax=max(yp)
ylong=abs(ymax-ymin)


# defino nx, ny, nz: los numeros de celdas que voy a tener por lado
# nx = floor(xlong/hsmlmax)
# ny = floor(ylong/hsmlmax)

nx = 50
ny = 50


# construyendo el ancho de cada celda
deltax = xlong/nx
deltay = ylong/ny


ll=xp*0-1

lirst=array(0,dim=c(nx,ny))

for(i in 1:length(xp))   #lazo sobre todas las particulas size(ll)=Npart
{
# para cada posicion del la particula i (pos$x[i],pos$y[i],pos$z[i])
# 0 < x <longx ; calculo en que celda cae:
  ix=floor((xp[i]-xmin)/deltax)+1    #1<=ix<=nx
  if(ix>nx)ix=nx
  iy=floor((yp[i]-ymin)/deltay)+1    #1<=iy<=ny
  if(iy>ny)iy=ny

  lirst[ix,iy]=i
}

for(i in 1:length(xp))
{
  ix=floor((xp[i]-xmin)/deltax)+1 
  if(ix>nx)ix=nx
  iy=floor((yp[i]-ymin)/deltay)+1
  if(iy>ny)iy=ny

# aca va ordenando las particulas
  ll[lirst[ix,iy]]=i
# aca reconstruyo el lirst
  lirst[ix,iy]=i
}

assign("ll",ll, envir = .GlobalEnv)
assign("lirst",lirst, envir = .GlobalEnv)

# campo analitico
# bxp = -yp
# byp =  xp
# bzp =  0.*xp+0.01
b=snap.read.2('../../../R/etacs/snapshot_080','BFLD',debug=1,gas=1)

bxp = b$x[which(-5 < pos$x & pos$x < 5 & -5 < pos$y & pos$y < 5)]
byp = b$y[which(-5 < pos$x & pos$x < 5 & -5 < pos$y & pos$y < 5)]


assign("bxp",bxp, envir = .GlobalEnv)
assign("byp",byp, envir = .GlobalEnv)


# delta de integracion de la linea
dl = deltax/median(sqrt(bxp*bxp+byp*byp))

# numero de lineas
nlines = 7

# numero de iteraciones para hacer las lineas
niter = 1000

# xlv = NULL; ylv = NULL; zlv = NULL
# bl=data.frame(bx=0.0,by=0.0,bz=0.0)

xinit = seq(-3.,3.,by=1.)
yinit = seq(-3.,3.,by=1.)*0.-2.


# xinit =  -2.9
# yinit =   2.


tt <- foreach(il=1:nlines) %dopar% 
{
  xlv = NULL; ylv = NULL
  bl=data.frame(bx=0.0,by=0.0)
  blv = NULL
  
#   xl = runif(1)*xlong*0.25  #donde quiero calcular el campo, ahora un numero random
#   yl = runif(1)*ylong*0.25
#   zl = runif(1)*zlong*0.25
  
  xl = xinit[il]
  yl = yinit[il]
  
#   xl = xinit
#   yl = yinit

  
#   print(c(il,xl,yl))
  
  for(itime in 1:niter)
  {
  
    xlv = c(xlv,xl)
    ylv = c(ylv,yl)
  
#     blv = c(blv,bl)
  
#   en que celda está el lugar a estudiar?
    ix = floor((xl-xmin)/deltax) + 1 
    iy = floor((yl-ymin)/deltay) + 1
 
    
# guardo los valores de bxl, byl, bzl, por si alguna posicion no tiene vecinos
# y por lo tanto no voy a poder calcular el campo. Me quedo con el campo anterior
# y lo avanzo un dl de nuevo, hasta que encuentre vecinos la posicion
    bold = bl
  
    bl=interp_field(xl,yl,bold)  
 
    xl = xl + bl$bx*dl
    yl = yl + bl$by*dl
     print(itime)
      
  } #niter

  list(x=xlv,y=ylv)
#   list(x=xlv,y=ylv,z=zlv,b=blv)  
} #nlines



