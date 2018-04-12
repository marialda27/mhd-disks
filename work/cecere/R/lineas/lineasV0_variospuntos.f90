program lineas
! programa que calcula lineas de campo magnetico, 
! dado el campo vectorial
implicit none
real         :: bx,by,bz
real         :: x, y, z, dl
integer      :: N, i
integer      :: puntos, Np

open(10,file='lineas.dat',status='replace')

! definiendo el delta de avance
dl = 0.001
! numero de paso de integracion
N = 7000
! numero de puntos a graficar las lineas
Np = 10

do puntos = 1, Np
  ! puntos iniciales
  x = rand()
  y = rand()
  z = rand()

  do i=1,N
    ! definiendo el campo vectorial
    bx = y
    by = -x
    bz = 0.

    ! primer orden
    x = x + bx*dl
    y = y + by*dl
    z = z + bz*dl

    write(10,'(E23.14,x,E23.14,x,E23.14)') x,y,z
  end do !i
end do !puntos

close(10)

end program lineas


