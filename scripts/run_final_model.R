source("R/plot_models.R")

data_stack<-stk.yz

### field models 
cg <- list(model = 'iid')
cg2 <- list(model = 'ar1')
pcgprior <- list(prior = 'pc.gamma', param = 1)
hyper.rw2.clim <- list(theta = list(prior="pc.prec", param=c(1, 0.1))) 


formula <- Y ~ -1 + z.intercept + y.intercept + 
  f(x, group = x.group, model = spde,control.group = cg2) + 
  f(u, group = u.group, model = spde,control.group = cg)+
  
  f(WEEKLY_AV_TMIN_03_z, model= "rw2", 
    scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
  f(WEEKLY_AV_TMIN_03_y, model= "rw2", 
    scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
  
  f(WEEKLY_AV_RAD_01_z, model= "rw2", 
    scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
  f(WEEKLY_AV_RAD_01_y, model= "rw2", 
    scale.model = TRUE, constr = TRUE, hyper = hyper.rw2.clim)+
  
  avian_WNV_bin +
  CLC2 

final_model <- inla(formula, 
                       family = c("gamma", "binomial"), 
                       data = inla.stack.data(data_stack), 
                       control.predictor = list(A = inla.stack.A(data_stack), 
                                                compute=TRUE), 
                       control.family =list(list(link="log", hyper=list(prec=pcgprior)), list(link="logit")),
                       control.compute = list(waic = TRUE, cpo=T,
                                               config=TRUE, return.marginals.predictor=TRUE),
                       control.inla = list(int.strategy = "eb"),verbose = F)

final<-final_model$summary.random$WEEKLY_AV_TMIN_03_z
ggplot(final)+
  geom_line(aes(ID,y=mean))+
  geom_ribbon(aes(x=ID,ymin=`0.025quant`,ymax=`0.975quant`), alpha=0.2)+
  geom_hline(yintercept=0,linetype="dashed")+
  theme_bw()+
  labs(x=paste0("Average minimum temperature (","\u00B0","C)"), y="Coefficients (95% CrI)")->a

final<-final_model$summary.random$WEEKLY_AV_TMIN_03_y
ggplot(final)+
  geom_line(aes(ID,y=mean))+
  geom_ribbon(aes(x=ID,ymin=`0.025quant`,ymax=`0.975quant`), alpha=0.2)+
  geom_hline(yintercept=0,linetype="dashed")+
  theme_bw()+
  labs(x=paste0("Average minimum temperature (","\u00B0","C)"), y="Coefficients (95% CrI)")->b

final<-final_model$summary.random$WEEKLY_AV_RAD_01_z
ggplot(final)+
  geom_line(aes(ID,y=mean))+
  geom_ribbon(aes(x=ID,ymin=`0.025quant`,ymax=`0.975quant`), alpha=0.2)+
  geom_hline(yintercept=0,linetype="dashed")+
  theme_bw()+
  labs(x=paste0("Average radiation (MJ m-2)"), y="Coefficients (95% CrI)")->c

final<-final_model$summary.random$WEEKLY_AV_RAD_01_y
ggplot(final)+
  geom_line(aes(ID,y=mean))+
  geom_ribbon(aes(x=ID,ymin=`0.025quant`,ymax=`0.975quant`), alpha=0.2)+
  geom_hline(yintercept=0,linetype="dashed")+
  theme_bw()+
  labs(x=paste0("Average radiation (MJ m-2)"), y="Coefficients (95% CrI)")->d

plot_grid(a,b,c,d, labels="AUTO") -> final_model_plots

