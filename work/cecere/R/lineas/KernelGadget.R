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
