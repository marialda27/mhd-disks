library('snapshot')

B=snap.read.2('snapshot_000','BFLD',debug=1,gas=1) 
pos=snap.read.2('snapshot_000','POS ',debug=1,gas=1)
hsml=snap.read.2('snapshot_000','HSML',debug=1,gas=1)

# corte
posx=pos$x[which(20 < pos$x & pos$x < 100)]
posy=pos$y[which(20 < pos$y & pos$y < 100)]
posz=pos$z[which(20 < pos$z & pos$z < 100)]

xmin=min(posx)
xmax=max(posx)
xlong=abs(xmax-xmin)
ymin=min(posy)
ymax=max(posy)
ylong=abs(ymax-ymin)
zmin=min(posz)
zmax=max(posz)
zlong=abs(zmax-zmin)

# tomar el maximo hsml
hsmlmax = 10.
# max(hsml[which(20 < pos$x & pos$x < 100 & 20 < pos$y & pos$y < 100 & 20 < pos$z & pos$z < 100)])

# defino nx, ny, nz
nx = floor(xlong/hsmlmax)
ny = floor(ylong/hsmlmax)
nz = floor(zlong/hsmlmax)

# construyendo el grid
deltax = xlong/nx
deltay = ylong/ny
deltaz = zlong/nz

# defino ll. Es un array entero de longitud igual al numero de particulas
# cada partícula tiene su ll, la i tiene el ll[i]
ll=posx*0-1

# construyo el lirst
lirst=array(-1,dim=c(nx,ny,nz))
# lirst es matriz de enteros, cuya longitud es el numero de grides dim=(nx,ny,nz)
# osea es un entero por cada celda
for(i in 1:length(posx))   #lazo sobre todas las particulas size(ll)=Npart
{
# para cada posicion del la particula i (pos$x[i],pos$y[i],pos$z[i])
# 0 < x <longx ; calculo en que celda cae:
  ix=floor(posx[i]/deltax)+1    #1<=ix<=nx
  if(ix>nx)ix=nx
  iy=floor(posy[i]/deltay)+1    #1<=iy<=ny
  if(iy>ny)iy=ny
  iz=floor(posz[i]/deltaz)+1    #1<=iz<=nz
  if(iz>nz)iz=nz
  lirst[ix,iy,iz]=i
}
# acá el lirst contiene el índice de la última partícula que se encontró en la celda ix,iy,iz

# aca voy a construir el ll ya ordenado, para luego leer las particulas de cada celda con mas eficiencia
for(i in 1:length(posx))
{
  ix=floor(posx[i]/deltax)+1 
  if(ix>nx)ix=nx
  iy=floor(posy[i]/deltay)+1
  if(iy>ny)iy=ny
  iz=floor(posz[i]/deltaz)+1    
  if(iz>nz)iz=nz
# aca va ordenando las particulas
  ll[lirst[ix,iy,iz]]=i
# aca reconstruyo el lirst
  lirst[ix,iy,iz]=i
}

# Hasta aca, ya tenemos ll para todo el box, para todas las partículas
# Ahora voy a dar una posición arbitraria, y voy a interpolar el campo
# magnético en esa posición, usando el Kernel de Gadget.

# defino el numero de lineas que quiero graficar:
nlines = 10

for(il in 1:nlines)
{
  xl = runif(1)*xlong  #donde quiero calcular el campo, ahora un numero random
  yl = runif(1)*ylong
  zl = runif(1)*zlong
  
# en que celda está el lugar a estudiar?
  ix = floor(xl/deltax) + 1
  iy = floor(yl/deltay) + 1
  iz = floor(zl/deltaz) + 1
  
# los tres lazos son para pasearse por todos las celdas vecinas
# incluida la celda en la que estoy
  for(iix in ix-1:ix+1)
  {
    if(iix==0 || iix==nx+1)next
    
    for(iiy in iy-1:iy+1)
    {
      if(iiy==0 || iiy==ny+1)next
     
      for(iiz in iz-1:iz+1)
      {
        if(iiz==0 || iiz==nz+1)next
     
#       me fijo cual es la última partícula de la celda -> it
        it = lirst[iix,iiy,iiz]
        if(it != 0)    # o sea que la celda no esta vacía
        {
          lgt=TRUE
          while(lgt)        #recorro todas las particulas de esa celda
          {
            it = ll[it]     #tomo la siguiente particula (si es la primera vez es la primer particula de la celda)
            xp = pos$x[it]  #tomo la posicion de la particula it
            yp = pos$y[it]
            zp = pos$z[it]
           
#           calculo de distancia respecto al punto donde quiero saber el campo
            dc = (xl-xp)**2 + (yl-yp)**2 + (zl-zp)**2
            dist = sqrt(dc)
           
#           toca el pto campo con su Hsml?
            if(dist < hsml[it])
            {
              KernelGadget(dist, hsml[it])   #me devuelve el factor que necesito
#             calculo el valor del campo en el punto deseado
              bx[il] = bx[il] + B$x[it]*fac
              by[il] = by[il] + B$y[it]*fac
              bz[il] = bz[il] + B$z[it]*fac
            }
          if(it == lirst[iix,iiy,iiz])lgt=FALSE
          }
        }
      }
    }
  }
# YA ESTA DADO EL CAMPO EN EL PUNTO  
# ACA DEBERIA IR LA PARTE DEL AVANCE TEMPORAL PARA CONSTRUIR LA LINEA DE CAMPO
  
}






