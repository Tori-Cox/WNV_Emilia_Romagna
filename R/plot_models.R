visualise_field_means <- function(res, stk.yz, mesh, m=10, summary="median"){
  
  if(summary %in% c("median", "ciU", "ciL")==F){
    print("summary must be one of median, ciU, or ciL")
  }else{
  
  idx.z <- inla.stack.index(stk.yz, 'est.z')$data
  idx.y <- inla.stack.index(stk.yz, 'est.y')$data
  
  nxy<-c(200,200)
  projgrid <- inla.mesh.projector(
    mesh_n1, 
    xlim = range(mesh_n1$loc[,1]),
    ylim = range(mesh_n1$loc[,2]),
    dims = nxy)
  
  cp = colorRampPalette(c("blue","cyan","yellow","red"))
  
  zmean <-ymean <-zsd <-ysd<-zlo <-ylo<-zupp <-yupp <-list()
  for (j in 1:m){
    zmean[[j]] <- inla.mesh.project(
      projgrid, res$summary.random$x$mean[field.z.idx$x.group == j])
    ymean[[j]] <- inla.mesh.project(
      projgrid, res$summary.random$u$mean[field.y.idx$u.group == j])
    zsd[[j]] <- inla.mesh.project(
      projgrid, res$summary.random$x$sd[field.z.idx$x.group == j])
    ysd[[j]] <- inla.mesh.project(
      projgrid, res$summary.random$u$sd[field.y.idx$u.group == j])
    
    zlo[[j]] <- inla.mesh.project(
      projgrid, res$summary.random$x$`0.025quant`[field.z.idx$x.group == j])
    ylo[[j]] <- inla.mesh.project(
      projgrid, res$summary.random$u$`0.025quant`[field.y.idx$u.group == j])
    zupp[[j]] <- inla.mesh.project(
      projgrid, res$summary.random$x$`0.975quant`[field.z.idx$x.group == j])
    yupp[[j]] <- inla.mesh.project(
      projgrid, res$summary.random$u$`0.975quant`[field.y.idx$u.group == j])
  }
  zstore<-ystore<-z2store<-y2store<-z3store<-y3store<-list()
  for(i in 1:m){
    zstore[[i]]<- levelplot(row.values=projgrid$x,
                            column.values=projgrid$y,
                            x=zmean[[i]],
                            col.regions=cp,
                            at = c(-Inf, seq(-3, 3, by = 0.1), Inf),
                            contour=F,labels=F,#pretty=T,
                            #main = as.expression("mu"),#paste0(i+2012),
                            xlab=NULL,ylab=NULL,scales=list(draw=FALSE))+ 
      latticeExtra::layer(sp.polygons(new_shapefile, lwd=0.9, col='black'))#+
    
    ystore[[i]]<- levelplot(row.values=projgrid$x,
                            column.values=projgrid$y,
                            x=ymean[[i]],
                            col.regions=cp,
                            at = c(-Inf, seq(-0.6, 1, by = 0.1), Inf),
                            contour=F,labels=F,#pretty=T,
                            #main = "\U2208",#paste0(i+2012),
                            xlab=NULL,ylab=NULL,scales=list(draw=FALSE))+ 
      latticeExtra::layer(sp.polygons(new_shapefile, lwd=0.9, col='black'))#+
    
    z2store[[i]]<- levelplot(row.values=projgrid$x,
                             column.values=projgrid$y,
                             x=zlo[[i]],
                             col.regions=cp,
                             at = c(-Inf, seq(-6,1, by = 0.05), Inf),
                             contour=F,labels=F,#pretty=T,
                             #main = "z sd",#paste0(i+2012),
                             xlab=NULL,ylab=NULL,scales=list(draw=FALSE))+ 
      latticeExtra::layer(sp.polygons(new_shapefile, lwd=0.9, col='black'))#+
    
    y2store[[i]]<- levelplot(row.values=projgrid$x,
                             column.values=projgrid$y,
                             x=ylo[[i]],
                             col.regions=cp,
                             at = c(-Inf, seq(-1.5, 0.5, by = 0.05), Inf),
                             contour=F,labels=F,#pretty=T,
                             #main = "y sd",#paste0(i+2012),
                             xlab=NULL,ylab=NULL,scales=list(draw=FALSE))+ 
      latticeExtra::layer(sp.polygons(new_shapefile, lwd=0.9, col='black'))#+
    
    z3store[[i]]<- levelplot(row.values=projgrid$x,
                             column.values=projgrid$y,
                             x=zupp[[i]],
                             col.regions=cp,
                             at = c(-Inf, seq(-1,5, by = 0.05), Inf),
                             contour=F,labels=F,#pretty=T,
                             #main = "z sd",#paste0(i+2012),
                             xlab=NULL,ylab=NULL,scales=list(draw=FALSE))+ 
      latticeExtra::layer(sp.polygons(new_shapefile, lwd=0.9, col='black'))#+
    
    y3store[[i]]<- levelplot(row.values=projgrid$x,
                             column.values=projgrid$y,
                             x=yupp[[i]],
                             col.regions=cp,
                             at = c(-Inf, seq(0,1.4, by = 0.05), Inf),
                             contour=F,labels=F,#pretty=T,
                             #main = "y sd",#paste0(i+2012),
                             xlab=NULL,ylab=NULL,scales=list(draw=FALSE))+ 
      latticeExtra::layer(sp.polygons(new_shapefile, lwd=0.9, col='black'))
  }
  
  a<-plot_grid(zstore[[1]], 
               zstore[[2]],  
               zstore[[3]],  
               zstore[[4]],  
               zstore[[5]],  
               zstore[[6]],  
               zstore[[7]],  
               zstore[[8]], 
               zstore[[9]], 
               zstore[[10]], 
               ncol=4, labels = c("         2013", "         2014", "         2015", "         2016", "         2017", "         2018",
                                  "         2019", "         2020", "         2021", "         2022"))
  
  b<-plot_grid(z2store[[1]], 
               z2store[[2]],  
               z2store[[3]],  
               z2store[[4]],  
               z2store[[5]],  
               z2store[[6]],  
               z2store[[7]],  
               z2store[[8]], 
               z2store[[9]], 
               z2store[[10]], 
               ncol=4, labels = c("         2013", "         2014", "         2015", "         2016", "         2017", "         2018",
                                  "         2019", "         2020", "         2021", "         2022"))
  
  #plot_grid(a,b,ncol=1,labels="AUTO")
  
  
  C<-plot_grid(ystore[[1]], 
               ystore[[2]],  
               ystore[[3]],  
               ystore[[4]],  
               ystore[[5]],  
               ystore[[6]],  
               ystore[[7]],  
               ystore[[8]], 
               ystore[[9]], 
               ystore[[10]], 
               ncol=4, labels = c("         2013", "         2014", "         2015", "         2016", "         2017", "         2018",
                                  "         2019", "         2020", "         2021", "         2022"))
  
  D<-plot_grid(y2store[[1]], 
               y2store[[2]],  
               y2store[[3]],  
               y2store[[4]],  
               y2store[[5]],  
               y2store[[6]],  
               y2store[[7]],  
               y2store[[8]], 
               y2store[[9]], 
               y2store[[10]], 
               ncol=4, labels = c("         2013", "         2014", "         2015", "         2016", "         2017", "         2018",
                                  "         2019", "         2020", "         2021", "         2022"))
  
  e<-plot_grid(y3store[[1]], 
               y3store[[2]],  
               y3store[[3]],  
               y3store[[4]],  
               y3store[[5]],  
               y3store[[6]],  
               y3store[[7]],  
               y3store[[8]], 
               y3store[[9]], 
               y3store[[10]], 
               ncol=4, labels = c("         2013", "         2014", "         2015", "         2016", "         2017", "         2018",
                                  "         2019", "         2020", "         2021", "         2022"))
  g<-plot_grid(z3store[[1]], 
               z3store[[2]],  
               z3store[[3]],  
               z3store[[4]],  
               z3store[[5]],  
               z3store[[6]],  
               z3store[[7]],  
               z3store[[8]], 
               z3store[[9]], 
               z3store[[10]], 
               ncol=4, labels = c("         2013", "         2014", "         2015", "         2016", "         2017", "         2018",
                                  "         2019", "         2020", "         2021", "         2022"))
  
  if(summary=="median"){plot_grid(a,C,ncol=1, labels=c("A", "B"))->field}
  if(summary=="ciL"){plot_grid(b,D,ncol=1,labels=c("A", "B"))->field}
  if(summary=="ciU"){plot_grid(g,e,ncol=1,labels=c("A", "B"))->field}
  
  return(field)
}


plot_field_parameters <- function(res){
  
  rho = inla.tmarginal(function(x) x,
                       res$marginals.hyperpar$`GroupRho for x`)
  z_range = inla.tmarginal(function(x) x,
                           res$marginals.hyperpar$`Range for x`)
  y_range = inla.tmarginal(function(x) x,
                           res$marginals.hyperpar$`Range for u`)
  z_sd = inla.tmarginal(function(x) x,
                        res$marginals.hyperpar$`Stdev for x`)
  y_sd = inla.tmarginal(function(x) x,
                        res$marginals.hyperpar$`Stdev for u`)
  pres = inla.tmarginal(function(x) x,
                        res$marginals.hyperpar$`Precision-parameter for the Gamma observations`)
  
  ggplot()+geom_point(aes(x=rho[,1],y=rho[,2]))+
    theme_bw()+
    theme(text=element_text(size=15))+
    labs(y="Density", x= expression(paste("GroupRho for ", mu[z])))->A
  ggplot()+geom_point(aes(x=z_range[,1],y=z_range[,2]))+
    theme_bw()+
    theme(text=element_text(size=15))+
    labs(y="Density", x= expression(paste('Practical range for ', mu[z])))->B
  ggplot()+geom_point(aes(x=y_range[,1],y=y_range[,2]))+
    theme_bw()+
    theme(text=element_text(size=15))+
    labs(y="Density", x= expression(paste('Practical range for ', mu[y])))->C
  ggplot()+geom_point(aes(x=z_sd[,1],y=z_sd[,2]))+
    theme_bw()+
    theme(text=element_text(size=15))+
    labs(y="Density", x= expression(paste('Standard deviation for ', mu[z])))->D
  ggplot()+geom_point(aes(x=y_sd[,1],y=y_sd[,2]))+
    theme_bw()+
    theme(text=element_text(size=15))+
    labs(y="Density", x= expression(paste('Standard deviation for ', mu[y])))->E
  ggplot()+geom_point(aes(x=pres[,1],y=pres[,2]))+
    theme_bw()+
    theme(text=element_text(size=15))+
    labs(y="Density", x= "Precision for Gaussian observations")->G
  
  cowplot::plot_grid(A,G,B,C,D,E,ncol=2, labels="AUTO")->plot_grid
  
  return(plot_grid)
}
}
