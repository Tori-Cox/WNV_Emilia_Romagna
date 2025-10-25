data_stack <- stk.yz

formula <- Y ~ -1 + z.intercept + y.intercept 
pcgprior <- list(prior = 'pc.gamma', param = 1)

baseline_model <- inla(formula, 
                       family = c("gamma", "binomial"), 
                       data = inla.stack.data(data_stack), 
                       control.predictor = list(A = inla.stack.A(data_stack), 
                                                compute=TRUE), 
                       control.family =list(list(link="log", hyper=list(prec=pcgprior)), list(link="logit")),
                       control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
                       control.inla = list(int.strategy = "eb"),verbose = F)
