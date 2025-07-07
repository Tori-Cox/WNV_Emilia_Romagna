best_univar <- function(univar_nl, univar_l){
  
  nl<- bind_rows(univar_nl)
  l <- bind_rows(univar_l)
  
  univar_results_table <-rbind(nl,l%>%mutate(ID=NA))
  
  univar_results_table |> filter(`0.025quant` < 0 & `0.975quant` <0 | `0.025quant` > 0 & `0.975quant` >0)->sig
  unique(sig$var[sig$waic==min(sig$waic)]) ->var
  waic<-min(sig$waic)
  return(c(var,waic))
}

colinarity_check <- function(data=new, all_variables, variable_chosen){
  
  m.cor = abs(cor(data[,all_variables],use="complete.obs", method="pearson"))
  var<-variable_chosen
  sub1 <- m.cor[,colnames(m.cor)==var] #subset just the chosen variable
  test_df1 <- data.frame(names = all_variables,
                         values = sub1)
  remove <- test_df1[which(test_df1$values>0.6 | test_df1$values < -0.6),1] #names of the variables co-linear not to be included in next round
  
  return(remove)
}

narrow_variables <- function(stk.yz, data=new, best_previous, run="short"){
  
  data_stack <- stk.yz
  temp_data <- inla.stack.data(data_stack)
  
  variable_z_l_tot <- names(temp_data)[c(56:89)]
  if(run=="short"){
    variable_z_l_tot<-variable_z_l_tot[4]
  }
  
  variable_z_tot <- names(temp_data)[c(7:55)]
  variable_y_tot <- names(temp_data)[c(98:146)]
  if(run=="short"){
    variable_z_tot<-variable_z_tot[1]
    variable_y_tot<-variable_y_tot[1]
  }
  variable_z_tot_n <- sub("\\_[^_]*$", "", variable_z_tot)
  
  if(length(best_previous)>2){choice<-"nonlinear"}
  if(length(best_previous)==2){choice<-"linear"}
  
  if(choice=="nonlinear"){best_previous<-sub("\\_[^_]*$", "", best_previous[1])}else{best_previous<-best_previous[1]}
  
  all_variables<-c(paste0(variable_z_tot_n), variable_z_l_tot)
  remove<-colinarity_check(data, all_variables, variable_chosen=best_previous)
  
  return(remove)
}
