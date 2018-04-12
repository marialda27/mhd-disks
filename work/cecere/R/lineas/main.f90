punto_campo: do ic=1,ncentro
                  xc = xtest(ic) !! donde quiero calcular el campo
                  yc =
                  zc =
 
                  !en que grid esta el lugar a estudiar?
                  ix = int(xc/dx) + 1
                  iy = int(yc/dx) + 1
                  iz = int(zc/dx) + 1
 
                  !los lazos pri seg y ter es para pasearse por todos los grides vecinos
                  !incluido el grid en el que estoy
                  pri: do iix = ix - 1,ix + 1
                    !! si me salí del borde no hago nada
                    if(iix==0 .or. iix==(nside+1) )cycle
                    seg: do iiy = iy - 1,iy + 1
                      if(iiy<=0 || iiy>nside )cycle
                      ter: do iiz = iz - 1,iz + 1
                        if(iiz<=0 || iiz>nside )cycle
                        !! me fijo cual es la última particula del grid, es it
                        it = lirst(iix,iiy,iiz)
                        if(it /= 0) then !osea que el grid no esta vacío
                          vecino: do
                            it = ll(it) !tomo la siguiente particula (si es la primera vez es la primer particula de la celda)
                            xp =x(it) ; yp=y(it) ; zp=z(it)
                            !!calculo de distancia
                            dc = (xc-xp)**2+(yc-yp)**2+(zc-zp)**2
                            dist = sqrt(dc)

                            !! toca el pto campo con su Hsml?
                            if(dist < Hsml(it))then
                               call KernelGadget(dist, hsml(it),fac)!me devuelve el facto que necesito
                               Campo_en_el_pto(ic) = Campo_en_el_pto(ic) + Campo_de_la_part(it)*fac
                            endif
                            if(it == lirst(iix,iiy,iiz))exit vecino
                          end do vecino
                        endif
 
                      end do ter
                    end do seg
                  end do pri
               enddo punto_campo
