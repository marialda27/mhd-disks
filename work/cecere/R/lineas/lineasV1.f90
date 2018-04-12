program lineas
! programa que calcula lineas de campo magnetico, 
! dado el campo vectorial
implicit none
real         :: bx,by,bz
real         :: x, y, z, dl
integer      :: N, i

real         :: xkl,ykl,zkl
real         :: bxkl,bykl,bzkl
real         :: xk,yk,zk
real         :: xk1m,yk1m,zk1m
real         :: bxk1m,byk1m,bzk1m
real         :: xkm,ykm,zkm
real         :: bxkm,bykm,bzkm
real         :: xskl,yskl,zskl
real         :: bxsk1,bysk1,bzsk1
real         :: xsl,ysl,zsl

open(10,file='lineas.dat',status='replace')

! definiendo el delta de avance

dl = 0.001

! punto inicial
x = 0.
y = 1.
z = 0.

! numero de paso de integracion
N = 70000

do i=1,N
! definiendo el campo vectorial
bx = y
by = -x
bz = 0.

! primer orden
! x = x + bx*dl
! y = y + by*dl
! z = z + bz*dl

! segundo orden
! primer paso
xkl = x + bx*dl
ykl = y + by*dl
zkl = z + bz*dl

bxkl = ykl
bykl = -xkl
bzkl = 0.

xk = x + 0.5*(bx+bxkl)*dl
yk = y + 0.5*(by+bykl)*dl
zk = z + 0.5*(bz+bzkl)*dl

! segundo paso
xk1m = x + 0.5*bx*dl
yk1m = y + 0.5*by*dl
zk1m = z + 0.5*bz*dl

bxk1m = yk1m
byk1m = -xk1m
bzk1m = 0.

xkm = x + 0.25*(bx+bxk1m)*dl
ykm = y + 0.25*(by+byk1m)*dl
zkm = z + 0.25*(bz+bzk1m)*dl

! ultimo paso
bxkm = ykm
bykm = -xkm
bzkm = 0.

xskl = xkm + 0.5*bxkm*dl
yskl = ykm + 0.5*bykm*dl
zskl = zkm + 0.5*bzkm*dl

bxsk1 = yskl
bysk1 = -xskl
bzsk1 = 0.

xsl = xkm + 0.25*(bxkm+bxsk1)*dl
ysl = ykm + 0.25*(bykm+bysk1)*dl
zsl = zkm + 0.25*(bzkm+bzsk1)*dl

x = xsl
y = ysl
z = zsl
write(10,'(E23.14,x,E23.14,x,E23.14)') x,y,z

end do

close(10)

end program lineas


