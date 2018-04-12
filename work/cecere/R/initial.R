# filename = "q3_r2_m.-3_hr.dat" ; output file

q = 3.                           # mass ratio
Mbbh = 1.                         # central BBH mass

m2 = Mbbh / (1. + q)             # masses individual bhs
m1 = q * m2
Mbh=c(m1,m2)

a1 = 1. / (1. + q)               # initial positions (a1 + a2 = 1)
a2 = q * a1

v1 = sqrt(m2*a1)/(a1 + a2)       # initial velocities
v2 = sqrt(m1*a2)/(a1 + a2)


Ngas = 2**21                        # sph particles
Md = 0.001 * Mbbh                # disc mass

Rin = 2.                         # disc inner radius
Rout = 5.                        # disc outer radius

hr = 2. * Md / Mbbh              # disc h/r

# gas coordinates

# first produce only one eigth of the particles
N8 = as.integer(Ngas/8.)
rgas = Rin + (Rout-Rin)*0:(N8-1)/N8
phi =  pi/2. * runif(N8)
xgas = rgas * cos(phi)
ygas = rgas * sin(phi)

# now create the z-coordinate
zgas = runif(N8) * hr * rgas
 
# now replicate to have the eight octants
xgas = c(xgas, xgas, xgas, xgas, -xgas, -xgas, -xgas, -xgas)
ygas = c(ygas, ygas, -ygas, -ygas, ygas, ygas, -ygas, -ygas)
zgas = c(zgas, -zgas, zgas, -zgas, zgas, -zgas, zgas, -zgas)
 
# now recalculate r and phi for better accuracy
phi = atan2(ygas,xgas)
rgas = sqrt(xgas^2 + ygas^2)
 
# gas velocity
vk = sqrt(Mbbh/rgas)             # assumes that G = 1
vxgas = -vk * sin(phi)
vygas =  vk * cos(phi)
vzgas = (2.*runif(Ngas)-1.) * hr * vk
 
# gas mass per particle
mgas = rep(0,Ngas) + Md/Ngas

# ; gas thermal energy
cs = hr * vk
gamma = 5./3.
u = cs^2 / (gamma*(gamma-1.))


# black hole
Nbh = as.integer(2)
xbh = c(-a1, a2)
ybh = c(0,0)
zbh = c(0,0)
vxbh = c(0,0)
vybh = c(-v1, v2)
vzbh = c(0,0)


# other particles

Ndisk  = as.integer(0)
Nbulge = as.integer(0)
Nstars = as.integer(0)
Nhalo  = as.integer(0)

# concatenate arrays

m = c(mgas, Mbh)
x = c(xgas, xbh)
y = c(ygas, ybh)
z = c(zgas, zbh)
vx = c(vxgas, vxbh)
vy = c(vygas, vybh)
vz = c(vzgas, vzbh)

N=Ndisk+Nbulge+Ngas+Nhalo+Nstars+Nbh

pos=data.frame(x,y,z)
vel=data.frame(vx,vy,vz)

