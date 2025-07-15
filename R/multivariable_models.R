
## step 1 ----------------------------------------------------------------------
run_multivar1_nl <- function(temp_data,
                             data_stack, 
                             spde,
                             variable_z, 
                             variable_y,
                             best_previous){
  cg <- list(model = 'iid')
  cg2 <- list(model = 'ar1')
  pcgprior <- list(prior = 'pc.gamma', param = 1)
  
  res<-list()
  dataframe <-list()
  hyper.rw2.clim <- list(theta = list(prior="pc.prec", param=c(1, 0.1))) 
  
  if(length(best_previous)>2){choice<-"nonlinear"}
  if(length(best_previous)==2){choice<-"linear"}
  
  if(choice=="nonlinear"){best_previous<-best_previous[c(1:2)]}else{best_previous<-best_previous[1]}
  
  
  for(i in 1:length(variable_z)){
    
    if(choice=="nonlinear"){
      formula <- Y ~ -1 + z.intercept + y.intercept + 
        f(x, group = x.group, model = spde,control.group = cg2) + 
        f(u, group = u.group, model = spde,control.group = cg)+
        f(temp_data[[best_previous[1]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
        f(temp_data[[best_previous[2]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
        
        f(temp_data[[variable_z[i]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
        f(temp_data[[variable_y[i]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)
    }else{
      formula <- Y ~ -1 + z.intercept + y.intercept + 
        f(x, group = x.group, model = spde,control.group = cg2) + 
        f(u, group = u.group, model = spde,control.group = cg)+
        temp_data[[best_previous]]+
        f(temp_data[[variable_z[i]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
        f(temp_data[[variable_y[i]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)
    }
    
    res[[i]] <- inla(formula, 
                             family = c("gamma", "binomial"),
                             data = inla.stack.data(data_stack), 
                             control.predictor = list(A = inla.stack.A(data_stack)), 
                             control.family =list(list(link="log", hyper=list(prec=pcgprior)), list(link="logit")),
                             control.compute = list(waic = TRUE, config = TRUE),
                             control.inla = list(int.strategy = "eb", stupid.search=T, strategy = 'adaptive'),
                             verbose = F)
    
    waic <- res[[i]]$waic$waic
    
    beta <- res[[i]]$summary.random$`temp_data[[variable_z[i]]]`
    beta2 <- res[[i]]$summary.random$`temp_data[[variable_y[i]]]`
    beta$group <- "z"
    beta2$group <- "y"
    
    beta <- cbind(variable_z[i], beta)
    beta2 <- cbind(variable_y[i], beta2)
    colnames(beta)[1] <- colnames(beta2)[1] <- "variable"
    beta$waic <- beta2$waic <- waic
    
    dataframe[[i]] <- rbind(beta,beta2)
    saveRDS(dataframe,"output/multivar/step1_nonlinear.RDS")
    
    res[[i]]<-NA
    
    print(paste0("non linear step1 model ", i, " out of ", length(variable_z)))
    
  }
  
  return(dataframe)
}

run_multivar1_l <- function(temp_data,
                             data_stack, 
                             spde,
                             variable_z, 
                             best_previous){
  
  cg <- list(model = 'iid')
  cg2 <- list(model = 'ar1')
  pcgprior <- list(prior = 'pc.gamma', param = 1)
  
  res<-list()
  dataframe <-list()
  hyper.rw2.clim <- list(theta = list(prior="pc.prec", param=c(1, 0.1))) 
  
  if(length(best_previous)>2){choice<-"nonlinear"}
  if(length(best_previous)==2){choice<-"linear"}
  
  if(choice=="nonlinear"){best_previous<-best_previous[c(1:2)]}else{best_previous<-best_previous[1]}
  
  
  for(i in 1:length(variable_z)){
    
    if(choice=="nonlinear"){
    formula <- Y ~ -1 + z.intercept + y.intercept + 
      f(x, group = x.group, model = spde,control.group = cg2) + 
      f(u, group = u.group, model = spde,control.group = cg)+
      f(temp_data[[best_previous[1]]], model= "rw2", 
        scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
      f(temp_data[[best_previous[2]]], model= "rw2", 
        scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
    temp_data[[variable_z[i]]]
    }else{
      formula <- Y ~ -1 + z.intercept + y.intercept + 
        f(x, group = x.group, model = spde,control.group = cg2) + 
        f(u, group = u.group, model = spde,control.group = cg)+
        temp_data[[best_previous]]+
        temp_data[[variable_z[i]]]
    }
    
  res[[i]] <- inla(formula, 
                           family = c("gamma", "binomial"),
                           data = inla.stack.data(data_stack), 
                           control.predictor = list(A = inla.stack.A(data_stack)), 
                           control.family =list(list(link="log", hyper=list(prec=pcgprior)), list(link="logit")),
                           control.compute = list(waic = TRUE, config = TRUE),
                           control.inla = list(int.strategy = "eb", stupid.search=T, strategy = 'adaptive'),
                           verbose = F)
  
  waic <- res[[i]]$waic$waic
  
  beta<-res[[i]]$summary.fixed[rownames(res[[i]]$summary.fixed)=="temp_data[[variable_z[i]]]1",]
  beta2<-res[[i]]$summary.fixed[rownames(res[[i]]$summary.fixed)=="temp_data[[variable_z[i]]]2",]
  beta$group <-"y"
  beta2$group<-"z"
  
  beta<-cbind(variable_z[i], beta)
  beta2<-cbind(variable_z[i], beta2)
  colnames(beta)[1]<-colnames(beta2)[1]<-"variable"
  beta$waic<-waic
  beta2$waic<-waic
  
  dataframe[[i]] <- rbind(beta,beta2)
  saveRDS(dataframe,"output/multivar/step1_linear.RDS")
  res[[i]]<-NA
  print(paste0("linear step1 model ", i, " out of ", length(variable_z)))
  
  }
  
  return(dataframe)
}

## step 2 ----------------------------------------------------------------------
run_multivar2_nl <- function(temp_data,
                             data_stack, 
                             spde,
                             variable_z, 
                             variable_y,
                             best_previous,
                             best_previous2){
  cg <- list(model = 'iid')
  cg2 <- list(model = 'ar1')
  pcgprior <- list(prior = 'pc.gamma', param = 1)
  
  res<-list()
  dataframe <-list()
  hyper.rw2.clim <- list(theta = list(prior="pc.prec", param=c(1, 0.1))) 
  
  if(length(best_previous)>2){choice<-"nonlinear"}
  if(length(best_previous)==2){choice<-"linear"}
  if(choice=="nonlinear"){best_previous<-best_previous[c(1:2)]}else{best_previous<-best_previous[1]}
  
  if(length(best_previous2)>2){choice2<-"nonlinear"}
  if(length(best_previous2)==2){choice2<-"linear"}
  if(choice2=="nonlinear"){best_previous2<-best_previous2[c(1:2)]}else{best_previous2<-best_previous2[1]}
  
  
  for(i in 1:length(variable_z)){
    
    if(choice=="nonlinear"){
      if(choice2=="nonlinear"){
      formula <- Y ~ -1 + z.intercept + y.intercept + 
        f(x, group = x.group, model = spde,control.group = cg2) + 
        f(u, group = u.group, model = spde,control.group = cg)+
        f(temp_data[[best_previous[1]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
        f(temp_data[[best_previous[2]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
        f(temp_data[[best_previous2[1]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
        f(temp_data[[best_previous2[2]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
        f(temp_data[[variable_z[i]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
        f(temp_data[[variable_y[i]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)
      }else{
        formula <- Y ~ -1 + z.intercept + y.intercept + 
          f(x, group = x.group, model = spde,control.group = cg2) + 
          f(u, group = u.group, model = spde,control.group = cg)+
          f(temp_data[[best_previous[1]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[best_previous[2]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          temp_data[[best_previous2]]+
          f(temp_data[[variable_z[i]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[variable_y[i]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)
      }
    }else{
      if(choice2=="nonlinear"){
      formula <- Y ~ -1 + z.intercept + y.intercept + 
        f(x, group = x.group, model = spde,control.group = cg2) + 
        f(u, group = u.group, model = spde,control.group = cg)+
        temp_data[[best_previous]]+
        f(temp_data[[best_previous2[1]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
        f(temp_data[[best_previous2[2]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
        f(temp_data[[variable_z[i]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
        f(temp_data[[variable_y[i]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)
      }else{
        formula <- Y ~ -1 + z.intercept + y.intercept + 
          f(x, group = x.group, model = spde,control.group = cg2) + 
          f(u, group = u.group, model = spde,control.group = cg)+
          temp_data[[best_previous]]+
          temp_data[[best_previous2]]+
          f(temp_data[[variable_z[i]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[variable_y[i]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)
      }
    }
    
    res[[i]] <- inla(formula, 
                     family = c("gamma", "binomial"),
                     data = inla.stack.data(data_stack), 
                     control.predictor = list(A = inla.stack.A(data_stack)), 
                     control.family =list(list(link="log", hyper=list(prec=pcgprior)), list(link="logit")),
                     control.compute = list(waic = TRUE, config = TRUE),
                     control.inla = list(int.strategy = "eb", stupid.search=T, strategy = 'adaptive'),
                     verbose = F)
    
    waic <- res[[i]]$waic$waic
    
    beta<-res[[i]]$summary.random$`temp_data[[variable_z[i]]]`
    beta2<-res[[i]]$summary.random$`temp_data[[variable_y[i]]]`
    beta$group <-"z"
    beta2$group<-"y"
    
    beta<-cbind(variable_z[i], beta)
    beta2<-cbind(variable_y[i], beta2)
    colnames(beta)[1]<-colnames(beta2)[1]<-"variable"
    beta$waic<-waic
    beta2$waic<-waic
    
    dataframe[[i]] <- rbind(beta,beta2)
    saveRDS(dataframe,"output/multivar/step2_nonlinear.RDS")
    
    res[[i]]<-NA
    
    print(paste0("non linear step2 model ", i, " out of ", length(variable_z)))
    
  }
  
  return(dataframe)
}

run_multivar2_l <- function(temp_data,
                            data_stack, 
                            spde,
                            variable_z, 
                            best_previous,
                            best_previous2){
  
  cg <- list(model = 'iid')
  cg2 <- list(model = 'ar1')
  pcgprior <- list(prior = 'pc.gamma', param = 1)
  
  res<-list()
  dataframe <-list()
  hyper.rw2.clim <- list(theta = list(prior="pc.prec", param=c(1, 0.1))) 
  
  if(length(best_previous)>2){choice<-"nonlinear"}
  if(length(best_previous)==2){choice<-"linear"}
  if(choice=="nonlinear"){best_previous<-best_previous[c(1:2)]}else{best_previous<-best_previous[1]}
  
  if(length(best_previous2)>2){choice2<-"nonlinear"}
  if(length(best_previous2)==2){choice2<-"linear"}
  if(choice2=="nonlinear"){best_previous2<-best_previous2[c(1:2)]}else{best_previous2<-best_previous2[1]}
  
  
  for(i in 1:length(variable_z)){
    
    if(choice=="nonlinear"){
      if(choice2=="nonlinear"){
      formula <- Y ~ -1 + z.intercept + y.intercept + 
        f(x, group = x.group, model = spde,control.group = cg2) + 
        f(u, group = u.group, model = spde,control.group = cg)+
        f(temp_data[[best_previous[1]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
        f(temp_data[[best_previous[2]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
        f(temp_data[[best_previous2[1]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
        f(temp_data[[best_previous2[2]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
        temp_data[[variable_z[i]]]
      }else{
        formula <- Y ~ -1 + z.intercept + y.intercept + 
          f(x, group = x.group, model = spde,control.group = cg2) + 
          f(u, group = u.group, model = spde,control.group = cg)+
          f(temp_data[[best_previous[1]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[best_previous[2]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          temp_data[[best_previous2]] +
          temp_data[[variable_z[i]]] 
      }
    }else{
      if(choice2=="nonlinear"){
      formula <- Y ~ -1 + z.intercept + y.intercept + 
        f(x, group = x.group, model = spde,control.group = cg2) + 
        f(u, group = u.group, model = spde,control.group = cg)+
        temp_data[[best_previous]]+
        f(temp_data[[best_previous2[1]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
        f(temp_data[[best_previous2[2]]], model= "rw2", 
          scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
        temp_data[[variable_z[i]]]
      }else{
        formula <- Y ~ -1 + z.intercept + y.intercept + 
          f(x, group = x.group, model = spde,control.group = cg2) + 
          f(u, group = u.group, model = spde,control.group = cg)+
          temp_data[[best_previous]] +
          temp_data[[best_previous2]] +
          temp_data[[variable_z[i]]] 
      }
    }
    
    res[[i]] <- inla(formula, 
                     family = c("gamma", "binomial"),
                     data = inla.stack.data(data_stack), 
                     control.predictor = list(A = inla.stack.A(data_stack)), 
                     control.family =list(list(link="log", hyper=list(prec=pcgprior)), list(link="logit")),
                     control.compute = list(waic = TRUE, config = TRUE),
                     control.inla = list(int.strategy = "eb", stupid.search=T, strategy = 'adaptive'),
                     verbose = F)
    
    waic <- res[[i]]$waic$waic
    
    beta<-res[[i]]$summary.fixed[rownames(res[[i]]$summary.fixed)=="temp_data[[variable_z[i]]]1",]
    beta2<-res[[i]]$summary.fixed[rownames(res[[i]]$summary.fixed)=="temp_data[[variable_z[i]]]2",]
    beta$group <- "y"
    beta2$group <- "z"
    
    beta<-cbind(variable_z[i], beta)
    beta2<-cbind(variable_z[i], beta2)
    colnames(beta)[1]<-colnames(beta2)[1]<-"variable"
    beta$waic<-waic
    beta2$waic<-waic
    
    dataframe[[i]] <- rbind(beta,beta2)
    saveRDS(dataframe,"output/multivar/step2_linear.RDS")
    res[[i]]<-NA
    print(paste0("linear step2 model ", i, " out of ", length(variable_z)))
    
  }
  
  return(dataframe)
}

## step 3 ----------------------------------------------------------------------
run_multivar3_nl <- function(temp_data,
                             data_stack, 
                             spde,
                             variable_z, 
                             variable_y,
                             best_previous,
                             best_previous2,
                             best_previous3){
  cg <- list(model = 'iid')
  cg2 <- list(model = 'ar1')
  pcgprior <- list(prior = 'pc.gamma', param = 1)
  
  res<-list()
  dataframe <-list()
  hyper.rw2.clim <- list(theta = list(prior="pc.prec", param=c(1, 0.1))) 
  
  if(length(best_previous)>2){choice<-"nonlinear"}
  if(length(best_previous)==2){choice<-"linear"}
  if(choice=="nonlinear"){best_previous<-best_previous[c(1:2)]}else{best_previous<-best_previous[1]}
  
  if(length(best_previous2)>2){choice2<-"nonlinear"}
  if(length(best_previous2)==2){choice2<-"linear"}
  if(choice2=="nonlinear"){best_previous2<-best_previous2[c(1:2)]}else{best_previous2<-best_previous2[1]}
  
  if(length(best_previous3)>2){choice3<-"nonlinear"}
  if(length(best_previous3)==2){choice3<-"linear"}
  if(choice3=="nonlinear"){best_previous3<-best_previous3[c(1:2)]}else{best_previous3<-best_previous3[1]}
  
  
  for(i in 1:length(variable_z)){
    
    if(choice=="nonlinear"){
      if(choice2=="nonlinear"){
        if(choice3=="nonlinear"){
        formula <- Y ~ -1 + z.intercept + y.intercept + 
          f(x, group = x.group, model = spde,control.group = cg2) + 
          f(u, group = u.group, model = spde,control.group = cg)+
          f(temp_data[[best_previous[1]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[best_previous[2]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[best_previous2[1]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[best_previous2[2]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[best_previous3[1]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[best_previous3[2]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[variable_z[i]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[variable_y[i]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)
        }else{
          formula <- Y ~ -1 + z.intercept + y.intercept + 
            f(x, group = x.group, model = spde,control.group = cg2) + 
            f(u, group = u.group, model = spde,control.group = cg)+
            f(temp_data[[best_previous[1]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[best_previous[2]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[best_previous2[1]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[best_previous2[2]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            temp_data[[best_previous3]]+
            f(temp_data[[variable_z[i]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[variable_y[i]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)
        }
      }else{
        if(choice3=="nonlinear"){
        formula <- Y ~ -1 + z.intercept + y.intercept + 
          f(x, group = x.group, model = spde,control.group = cg2) + 
          f(u, group = u.group, model = spde,control.group = cg)+
          f(temp_data[[best_previous[1]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[best_previous[2]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          temp_data[[best_previous2]]+
          f(temp_data[[best_previous3[1]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[best_previous3[2]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[variable_z[i]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[variable_y[i]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)
        }else{
          formula <- Y ~ -1 + z.intercept + y.intercept + 
            f(x, group = x.group, model = spde,control.group = cg2) + 
            f(u, group = u.group, model = spde,control.group = cg)+
            f(temp_data[[best_previous[1]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[best_previous[2]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            temp_data[[best_previous2]]+
            temp_data[[best_previous3]]+
            f(temp_data[[variable_z[i]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[variable_y[i]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)
        }
      }
    }else{
      if(choice2=="nonlinear"){
        if(choice3=="nonlinear"){
        formula <- Y ~ -1 + z.intercept + y.intercept + 
          f(x, group = x.group, model = spde,control.group = cg2) + 
          f(u, group = u.group, model = spde,control.group = cg)+
          temp_data[[best_previous]]+
          f(temp_data[[best_previous2[1]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[best_previous2[2]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[best_previous3[1]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[best_previous3[2]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[variable_z[i]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[variable_y[i]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)
        }else{
          formula <- Y ~ -1 + z.intercept + y.intercept + 
            f(x, group = x.group, model = spde,control.group = cg2) + 
            f(u, group = u.group, model = spde,control.group = cg)+
            temp_data[[best_previous]]+
            f(temp_data[[best_previous2[1]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[best_previous2[2]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            temp_data[[best_previous3]]+
            f(temp_data[[variable_z[i]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[variable_y[i]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)
        }
      }else{
        if(choice3=="nonlinear"){
        formula <- Y ~ -1 + z.intercept + y.intercept + 
          f(x, group = x.group, model = spde,control.group = cg2) + 
          f(u, group = u.group, model = spde,control.group = cg)+
          temp_data[[best_previous]]+
          temp_data[[best_previous2]]+
          f(temp_data[[best_previous3[1]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[best_previous3[2]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[variable_z[i]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
          f(temp_data[[variable_y[i]]], model= "rw2", 
            scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)
        }else{
          formula <- Y ~ -1 + z.intercept + y.intercept + 
            f(x, group = x.group, model = spde,control.group = cg2) + 
            f(u, group = u.group, model = spde,control.group = cg)+
            temp_data[[best_previous]]+
            temp_data[[best_previous2]]+
            temp_data[[best_previous3]]+
            f(temp_data[[variable_z[i]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[variable_y[i]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)
        }
      }
    }
    
    res[[i]] <- inla(formula, 
                     family = c("gamma", "binomial"),
                     data = inla.stack.data(data_stack), 
                     control.predictor = list(A = inla.stack.A(data_stack)), 
                     control.family =list(list(link="log", hyper=list(prec=pcgprior)), list(link="logit")),
                     control.compute = list(waic = TRUE, config = TRUE),
                     control.inla = list(int.strategy = "eb", stupid.search=T, strategy = 'adaptive'),
                     verbose = F)
    
    waic <- res[[i]]$waic$waic
    
    beta<-res[[i]]$summary.random$`temp_data[[variable_z[i]]]`
    beta2<-res[[i]]$summary.random$`temp_data[[variable_y[i]]]`
    beta$group <-"z"
    beta2$group<-"y"
    
    beta<-cbind(variable_z[i], beta)
    beta2<-cbind(variable_y[i], beta2)
    colnames(beta)[1]<-colnames(beta2)[1]<-"variable"
    beta$waic<-waic
    beta2$waic<-waic
    
    dataframe[[i]] <- rbind(beta,beta2)
    saveRDS(dataframe,"output/multivar/step3_nonlinear.RDS")
    
    res[[i]]<-NA
    
    print(paste0("non linear step3 model ", i, " out of ", length(variable_z)))
    
  }
  
  return(dataframe)
}

run_multivar3_l <- function(temp_data,
                            data_stack, 
                            spde,
                            variable_z, 
                            best_previous,
                            best_previous2,
                            best_previous3){
  
  cg <- list(model = 'iid')
  cg2 <- list(model = 'ar1')
  pcgprior <- list(prior = 'pc.gamma', param = 1)
  
  res<-list()
  dataframe <-list()
  hyper.rw2.clim <- list(theta = list(prior="pc.prec", param=c(1, 0.1))) 
  
  if(length(best_previous)>2){choice<-"nonlinear"}
  if(length(best_previous)==2){choice<-"linear"}
  if(choice=="nonlinear"){best_previous<-best_previous[c(1:2)]}else{best_previous<-best_previous[1]}
  
  if(length(best_previous2)>2){choice2<-"nonlinear"}
  if(length(best_previous2)==2){choice2<-"linear"}
  if(choice2=="nonlinear"){best_previous2<-best_previous2[c(1:2)]}else{best_previous2<-best_previous2[1]}
  
  if(length(best_previous3)>2){choice3<-"nonlinear"}
  if(length(best_previous3)==2){choice3<-"linear"}
  if(choice3=="nonlinear"){best_previous3<-best_previous3[c(1:2)]}else{best_previous3<-best_previous3[1]}

  
  for(i in 1:length(variable_z)){
    
    if(choice=="nonlinear"){
      if(choice2=="nonlinear"){
        if(choice3=="nonlinear"){
          formula <- Y ~ -1 + z.intercept + y.intercept + 
            f(x, group = x.group, model = spde,control.group = cg2) + 
            f(u, group = u.group, model = spde,control.group = cg)+
            f(temp_data[[best_previous[1]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[best_previous[2]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[best_previous2[1]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[best_previous2[2]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[best_previous3[1]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[best_previous3[2]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            temp_data[[variable_z[i]]]
          
        }else{
          formula <- Y ~ -1 + z.intercept + y.intercept + 
            f(x, group = x.group, model = spde,control.group = cg2) + 
            f(u, group = u.group, model = spde,control.group = cg)+
            f(temp_data[[best_previous[1]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[best_previous[2]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[best_previous2[1]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[best_previous2[2]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            temp_data[[best_previous3]]+
            temp_data[[variable_z[i]]]
          
        }
      }else{
        if(choice3=="nonlinear"){
          formula <- Y ~ -1 + z.intercept + y.intercept + 
            f(x, group = x.group, model = spde,control.group = cg2) + 
            f(u, group = u.group, model = spde,control.group = cg)+
            f(temp_data[[best_previous[1]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[best_previous[2]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            temp_data[[best_previous2]]+
            f(temp_data[[best_previous3[1]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[best_previous3[2]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            temp_data[[variable_z[i]]]
          
        }else{
          formula <- Y ~ -1 + z.intercept + y.intercept + 
            f(x, group = x.group, model = spde,control.group = cg2) + 
            f(u, group = u.group, model = spde,control.group = cg)+
            f(temp_data[[best_previous[1]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[best_previous[2]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            temp_data[[best_previous2]]+
            temp_data[[best_previous3]]+
            temp_data[[variable_z[i]]]
          
        }
      }
    }else{
      if(choice2=="nonlinear"){
        if(choice3=="nonlinear"){
          formula <- Y ~ -1 + z.intercept + y.intercept + 
            f(x, group = x.group, model = spde,control.group = cg2) + 
            f(u, group = u.group, model = spde,control.group = cg)+
            temp_data[[best_previous]]+
            f(temp_data[[best_previous2[1]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[best_previous2[2]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[best_previous3[1]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[best_previous3[2]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            temp_data[[variable_z[i]]]
          
        }else{
          formula <- Y ~ -1 + z.intercept + y.intercept + 
            f(x, group = x.group, model = spde,control.group = cg2) + 
            f(u, group = u.group, model = spde,control.group = cg)+
            temp_data[[best_previous]]+
            f(temp_data[[best_previous2[1]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[best_previous2[2]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            temp_data[[best_previous3]]+
            temp_data[[variable_z[i]]]
          
        }
      }else{
        if(choice3=="nonlinear"){
          formula <- Y ~ -1 + z.intercept + y.intercept + 
            f(x, group = x.group, model = spde,control.group = cg2) + 
            f(u, group = u.group, model = spde,control.group = cg)+
            temp_data[[best_previous]]+
            temp_data[[best_previous2]]+
            f(temp_data[[best_previous3[1]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            f(temp_data[[best_previous3[2]]], model= "rw2", 
              scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
            temp_data[[variable_z[i]]]
          
        }else{
          formula <- Y ~ -1 + z.intercept + y.intercept + 
            f(x, group = x.group, model = spde,control.group = cg2) + 
            f(u, group = u.group, model = spde,control.group = cg)+
            temp_data[[best_previous]]+
            temp_data[[best_previous2]]+
            temp_data[[best_previous3]]+
            temp_data[[variable_z[i]]]
          
        }
      }
    }
    
    res[[i]] <- inla(formula, 
                     family = c("gamma", "binomial"),
                     data = inla.stack.data(data_stack), 
                     control.predictor = list(A = inla.stack.A(data_stack)), 
                     control.family =list(list(link="log", hyper=list(prec=pcgprior)), list(link="logit")),
                     control.compute = list(waic = TRUE, config = TRUE),
                     control.inla = list(int.strategy = "eb", stupid.search=T, strategy = 'adaptive'),
                     verbose = F)
    
    waic <- res[[i]]$waic$waic
    
    beta<-res[[i]]$summary.fixed[rownames(res[[i]]$summary.fixed)=="temp_data[[variable_z[i]]]1",]
    beta2<-res[[i]]$summary.fixed[rownames(res[[i]]$summary.fixed)=="temp_data[[variable_z[i]]]2",]
    beta$group <- "y"
    beta2$group <- "z"
    
    beta<-cbind(variable_z[i], beta)
    beta2<-cbind(variable_z[i], beta2)
    colnames(beta)[1]<-colnames(beta2)[1]<-"variable"
    beta$waic<-waic
    beta2$waic<-waic
    
    dataframe[[i]] <- rbind(beta,beta2)
    saveRDS(dataframe,"output/multivar/step3_linear.RDS")
    res[[i]]<-NA
    print(paste0("linear step3 model ", i, " out of ", length(variable_z)))
    
  }
  
  return(dataframe)
}
