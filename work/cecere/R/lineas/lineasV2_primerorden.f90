program lineas
! programa que calcula lineas de campo magnetico, 
! dado el campo vectorial
implicit none
real         :: bx,by,bz
real         :: x, y, z, dl
integer      :: Nt, i
integer      :: puntos, Np

open(10,file='lineas.dat',status='replace')

! definiendo el delta de avance
dl = 0.001
! numero de paso de integracion
Nt = 70000
! numero de puntos a graficar las lineas
Np = 10

do puntos = 1, Np
  ! puntos iniciales
  x = rand()
  y = rand()
  z = rand()

  do i=1,Nt
  ! definiendo el campo vectorial
    call campo(x,y,z,bx,by,bz)

    ! primer orden
    x = x + bx*dl
    y = y + by*dl  
    z = z + bz*dl

    write(10,'(E23.14,x,E23.14,x,E23.14)') x,y,z

  end do
end do
close(10)

end program lineas


subroutine campo(x,y,z,bx,by,bz)

real, intent(in)  :: x, y, z
real, intent(out) :: bx, by, bz

bx = -y
by = x
bz = 0.

end subroutine
