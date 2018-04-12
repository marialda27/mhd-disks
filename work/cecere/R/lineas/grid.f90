   subroutine grid(x,y,z,lirst,ll)
     implicit none
     integer                                            :: i,ind,ix,iy,iz
     integer,dimension(nside,nside,nside),intent(inout) :: lirst
     integer,dimension(:),intent(out)                   :: ll
     real(kind=DP),dimension(:),intent(in)              :: x,y,z
    
     lirst = 0
     !!matriz de enteros, cuya longitud es el numero de grides dim=(ngrid,ngrid,ngrid) (ngrid=nside)
     !!osea es un entero por cada celda
     ll    = 0
     !!array ll es un entero de longitud igual al numero de particulas
     !! cada partícula tiene su ll, la i tiene el ll(i)
     print*,facbin,'--------------------------'
     if(facbin < 0.0) stop 'definir facbin antes de llamar grid'

     !ngrid es el numero de celdas por lado
     dx=lbox/nside

     write(*,*) 'grid: start'
     do i = 1,size(ll) !lazo sobre todas las particulas size(ll)=npart
       !para cada posicion del la particula i (x(i),y(i),z(i))
       ! 0 < x <lbox ;
       !calculo en que celda cae:
       ix = int(x(i)/dx)+1
       iy = int(y(i)/dx)+1
       iz = int(z(i)/dx)+1
       !1<=ix<=ngrid
       
       !en el lirst de la celda en cuestión guardo el índice de la partícula
       lirst(ix,iy,iz) = i
     end do
     !! acá el lirst contiene el índice de la última particula que se encontró en la celda ix,iy,iz


     do i = 1,size(ll)
       ix = int(x(i)/dx)+1
       iy = int(y(i)/dx)+1
       iz = int(z(i)/dx)+1
       ll(lirst(ix,iy,iz)) = i
       lirst(ix,iy,iz)   = i
     end do
     write(*,*) 'grid: end'


end subroutine grid
