source("R/mesh_setup.R")

# prepare shapefile (no internal boundaries but keeping the coastal boundary)
new_shapefile <- prepare_shapefile(shapefile)

# make boundary for mesh using the new shapefile 
country.bdry <-  new_shapefile %>% inla.sp2segment()

# calculate distances between traps to help choose mesh parameters
r<- trap_distances(coords)

# mesh parameters
max.edge = diff(range(r))/(10) 
bound.outer = diff(range(r))/5

mesh_n1 <- inla.mesh.2d(boundary = country.bdry,
                        loc=coords[c("x","y")],
                        min.angle = c(25,25), # changing triangle size in the inner and outer segment
                        max.edge = c(1,2)*max.edge, # changing triangle size in the inner and outer segment
                        offset=c(max.edge,bound.outer), # changing how wide the inner and outer domain go from the locations (overall size)
                        cutoff = max.edge/10) # make triangles near boundary smaller to reduce boundary effects

## set up number of unique years and trap IDs
n<-length(unique(data$trap))
m = length(unique(data$year_no))

## separate presence and prevalence
data %>%  mutate(variable = IR, variable2 = IR) ->new
z <- (new$IR > 0)
y <- ifelse(z == 1, unlist(new$IR), NA)


## prioirs for SPDE
min_dist<-min(r[lower.tri(r)])

# pc prior on the spatial correlation range
prange <- c(min_dist, 0.05) # less than 50% chance that the correlation is less than the minimum distance

# pc prior on the range variance
psigma <- c(min_dist, 0.5) # less than 50% likelihood that the value is >0.5. 

spde <- inla.spde2.pcmatern(alpha=2, # smoothing parameter. 
                            mesh_n1, #  mesh
                            prior.range = prange, # prior on the range (distance where spatial correlation reaches)
                            prior.sigma = psigma) # prior on the variance of the range

## SPDE

## make A
coords_all <-(cbind(new[,"x"],new[,"y"]))
A <- inla.spde.make.A(mesh = mesh_n1, 
                      loc = coords_all, 
                      group = new$year_no)

## specify the GMRFs

field.z.idx <- inla.spde.make.index(name = 'x', 
                                    n.spde = spde$n.spde,n.group = m) # hurdle z
field.zc.idx <- inla.spde.make.index(name = 'xc', 
                                     n.spde = spde$n.spde,n.group = m) # shared z and y 
field.y.idx <- inla.spde.make.index(name = 'u', 
                                    n.spde = spde$n.spde,n.group = m) # amount y

