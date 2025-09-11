
## prepare shapefile
prepare_shapefile <- function(shapefile){
  
  no_boundary_shapefile <- readRDS("data/Emilia_Romagna_outline.RDS")
  
  # crop to emilia romagna and surrounding region
  shapefile_ER <- sf::st_as_sf(shapefile) |>    filter(NAME_1  == "Emilia-Romagna"|NAME_1  == "Veneto"|NAME_1  == "Lombardia" ) 
  
  ext <- extent(9.2,13.1,44.2,45.4) # manually pick where to crop shapefile based on extent from ER
  terra::crop(no_boundary_shapefile,ext) -> new_shapefile
  
  # project the new CRS 
  new_shapefile <- st_transform(st_as_sf(new_shapefile), crs="+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")
  
  return(new_shapefile)
}


## plot the mesh
plot_mesh <- function(new_shapefile, mesh_n1){
  ggplot() +
  geom_sf(data=st_as_sf(new_shapefile),color='lightblue',fill='lightblue')+  
  gg(mesh_n1) +
  theme_classic()+
  labs(x="Longitude", y="Latitude")
}