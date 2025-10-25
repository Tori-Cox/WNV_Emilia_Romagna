dir.create("scripts/sensitivity_without_spatial/output")
dir.create("scripts/sensitivity_without_spatial/output/univar")

data_stack <- stk.yz
temp_data <- inla.stack.data(data_stack)

hyper.rw2.clim <- list(theta = list(prior="pc.prec", param=c(1, 0.1))) 
pcgprior <- list(prior = 'pc.gamma', param = 1)

### run linear univar ----------------------------------------------------------

res <- list()
dataframe <- list()

variable_z_tot <- names(temp_data)[c(56:90)]

for(i in 1:length(variable_z_tot)){
  
  formula <- Y ~ -1 + z.intercept + y.intercept + 
    temp_data[[variable_z_tot[i]]]
  
  
  res[[i]] <- inla(formula, 
                           family = c("gamma", "binomial"),
                           data = inla.stack.data(data_stack), 
                           control.predictor = list(A = inla.stack.A(data_stack)), 
                           control.family =list(list(link="log", hyper=list(prec=pcgprior)), list(link="logit")),
                           control.compute = list(waic = TRUE, config = TRUE),
                           control.inla = list(int.strategy = "eb", stupid.search=T, strategy = 'adaptive'),
                           verbose = F)
  
  waic <- res[[i]]$waic$waic
  
  beta<-res[[i]]$summary.fixed[3,]
  beta2<-res[[i]]$summary.fixed[4,]
  beta$group <-"y"
  beta2$group<-"z"
  
  beta<-cbind(variable_z_tot[i], beta)
  beta2<-cbind(variable_z_tot[i], beta2)
  colnames(beta)[1]<-colnames(beta2)[1]<-"variable"
  beta$waic<-waic
  beta2$waic<-waic
  
  dataframe[[i]] <- rbind(beta,beta2)
  saveRDS(dataframe,"scripts/sensitivity_without_spatial/output/univar/univar_linear.RDS")
  res[[i]]<-NA
  print(paste0("linear univariable model #", i, " out of ", length(variable_z_tot)))
  
}

### run non linear univar ------------------------------------------------------

res <- list()
dataframe <- list()

variable_z_tot <- names(temp_data)[c(7:55)]
variable_y_tot <- names(temp_data)[c(98:146)]

for(i in 1:length(variable_z_tot)){
  
  formula <- Y ~ -1 + z.intercept + y.intercept + 
    
    f(temp_data[[variable_z_tot[i]]], model= "rw2", 
      scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
    f(temp_data[[variable_y_tot[i]]], model= "rw2", 
      scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)
  
  
  res[[i]] <- inla(formula, 
                           family = c("gamma", "binomial"),
                           data = inla.stack.data(data_stack), 
                           control.predictor = list(A = inla.stack.A(data_stack)), 
                           control.family =list(list(link="log", hyper=list(prec=pcgprior)), list(link="logit")),
                           control.compute = list(waic = TRUE, config = TRUE),
                           control.inla = list(int.strategy = "eb", stupid.search=T, strategy = 'adaptive'),
                           verbose = F)
  
  waic <- res[[i]]$waic$waic
  
  
  beta<-res[[i]]$summary.random$`temp_data[[variable_y_tot[i]]]`
  beta2<-res[[i]]$summary.random$`temp_data[[variable_z_tot[i]]]`
  beta$group <-"y"
  beta2$group<-"z"
  
  beta<-cbind(variable_y_tot[i], beta)
  beta2<-cbind(variable_z_tot[i], beta2)
  colnames(beta)[1]<-colnames(beta2)[1]<-"variable"
  beta$waic<-waic
  beta2$waic<-waic
  
  dataframe[[i]] <- rbind(beta,beta2)
  
  res[[i]]<-NA
  print(paste0("non linear univariable model #", i, " out of ", length(variable_z_tot)))
  saveRDS(dataframe,"scripts/sensitivity_without_spatial/output/univar/univar_nonlinear.RDS")
}
