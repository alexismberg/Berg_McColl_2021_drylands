
### Script to calculate grid cell area:

nlon <- 720
nlat <- 360
r <- 6371.0 # Earth radius              

dlon <- 360.0/nlon 
dlat <- 180.0/nlat

lon<- seq(dlon/2, 360, by=dlon) #np.arange(dlon/2,360.0,dlon)
lat <- seq(-90+dlat/2,90, by=dlat) # np.arange(-90.0+dlat/2,90.0,dlat)
                                 
cell_area <- array(NA, dim=c(nlon, nlat))
lat2 <- pi/180.0 # convert to radians                                                                                                                                              

dlon = 2*pi/nlon ; dlat = pi/nlat

area[:] = r**2 * cos(lat2) * dlon * 2 * sin(dlat/2)
                                                                                                            
