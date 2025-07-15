## quantile method model to determine if a variable should be linear or non-linear

source("R/quantiles.R")

q_data <- quantiles_variables(new)


## organise the data into stacks

stk.z.q <- inla.stack(
  data = list(Y = cbind(NA,z), link=2),
  A = list(A, 1,1),
  effects = list(field.z.idx, 
                 z.intercept = rep(1, length(z)),
                 list(testvar1 = q_data$WEEKLY_AV_TAVG_0)
  ), 
  tag = 'est.z') 

stk.y.q <- inla.stack(
  data = list(Y = cbind(y,NA), link=1),
  A = list(A, 1,1),
  effects = list(c(field.zc.idx, field.y.idx), y.intercept = rep(1, length(z)),
                 
                 list(testvar2= q_data$WEEKLY_AV_TAVG_0)
  ), 
  tag = 'est.y') 


## join all the data stacks:
stk.yz_q <- inla.stack(stk.z.q, stk.y.q)
data_stack_q <- stk.yz_q
temp_data_q <- inla.stack.data(data_stack_q)


cg <- list(model = 'iid')
cg2 <- list(model = 'ar1')

TV <- "WEEKLY_AV_TAVG_0"

formula <- Y ~ -1 + z.intercept + y.intercept + 
  f(x, group = x.group, model = spde,control.group = cg2) + 
  f(u, group = u.group, model = spde,control.group = cg) +
  f(testvar1)+f(testvar2)

res <- inla(formula, 
                  family = c("gamma", "binomial"),
                  data = inla.stack.data(data_stack_q), 
                  control.predictor = list(A = inla.stack.A(data_stack_q), link=link), 
                  control.family = list(list(link="log", hyper=list(prec=pcgprior)), list(link="logit")),
                  control.compute = list(waic = TRUE, config = TRUE),
                  control.inla = list(int.strategy = "eb", stupid.search=T, strategy = 'adaptive'),
                  control.fixed = list(expand.factor.strategy = 'inla'), 
                  verbose = F)

store <- res$summary.random$testvar1
store2 <- res$summary.random$testvar2
store$variable <- TV
store2$variable <- TV
store$part <-"Presence"
store2$part <-"Prevalence"
store <- rbind(store,store2)
saveRDS(store, paste0("output/base/", TV, "_quantile_betas.RDS"))

store$ID2 <- c("0-10%","91-100%","11-20%","21-30%","31-40%","41-50%","51-60%","61-70%","71-80%","81-90%")
store$ID2 <- factor(store$ID2,c("0-10%","11-20%","21-30%","31-40%","41-50%","51-60%","61-70%","71-80%","81-90%","91-100%"))

p <- ggplot(store)+facet_grid("part", scales="free")+
  geom_point(aes(ID2,y=mean))+geom_errorbar(aes(x=ID2,ymin=`0.025quant`,ymax=`0.975quant`))+
  theme_bw()+labs( x="Quantiles",y="Coefficient", title = TV)+ 
  theme(axis.text=element_text(size=10),axis.title=element_text(size=12))
print(p)


