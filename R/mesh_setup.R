
## prepare shapefile
prepare_shapefile <- function(shapefile){
  # get rid of internal boundaries
  no_boundary_shapefile <- rgeos::gUnaryUnion(shapefile) 
  
  # crop to emilia romagna and surrounding region
  shapefile_ER <- sf::st_as_sf(shapefile) |>    filter(NAME_1  == "Emilia-Romagna"|NAME_1  == "Veneto"|NAME_1  == "Lombardia" ) 
  
  ext<- extent(9.2,13.1,44.2,45.4) # manually pick where to crop shapefile based on extent from ER line above
  terra::crop(no_boundary_shapefile,ext)->new_shapefile
  
  return(new_shapefile)
}

## calculate distance between traps for mesh parameters
trap_distances <- function(coords){
  mydata_sf<-st_as_sf(coords, coords = c("y", "x"), crs = 2100) # turns coordinates into suitable format to calculate distances betweeen traps
  dist_df<-as.data.frame(st_distance(mydata_sf))
  return(dist_df)
}

## plot the mesh
plot_mesh <- function(new_shapefile, mesh_n1, xy){
  ggplot() +
  geom_sf(data=st_as_sf(new_shapefile),color='lightblue',fill='lightblue')+  
  gg(mesh_n1) +
  geom_point(aes(x=xy[,1],y=xy[,2]),col='black',size=1.7,alpha=0.5) +
  theme_classic()+
  labs(x="Longitude", y="Latitude")
}