source("R/multivariable_models.R")

## run step 1 ------------------------------------------------------------------

remove <- narrow_variables(stk.yz, data, best_previous=best_univariable, run=run)

# nonlinear
variable_z_tot <- names(temp_data)[c(7:55)]
variable_y_tot <- names(temp_data)[c(98:146)]
if(run=="short"){
  variable_z_tot<-variable_z_tot[1]
  variable_y_tot<-variable_y_tot[1]
}

variable_z_1 <- variable_z_tot[!variable_z_tot%in% paste0(remove,"_z")]
variable_y_1 <- variable_y_tot[!variable_y_tot%in% paste0(remove,"_y")]

if(length(variable_z_1)>0){
  
model_nonlinear <- run_multivar1_nl(temp_data, data_stack, spde,
                   variable_z=variable_z_1, 
                   variable_y=variable_y_1,
                   best_previous=best_univariable)

t<-bind_rows(model_nonlinear)
saveRDS(t,"output/multivar/multivar1_nonlinear.RDS")

  
}else{
  print("no more non linear variables left to try in the model building steps")
}

# linear
variable_z_tot <- names(temp_data)[c(56:90)]
if(run=="short"){
  variable_z_tot<-variable_z_tot[4]
}

variable_z_1 <- variable_z_tot[!variable_z_tot %in% paste0(remove,"_z")]
variable_y_1 <- variable_y_tot[!variable_y_tot %in% paste0(remove,"_y")]

if(length(variable_z_1)>0){
  
  model_linear <- run_multivar1_l(temp_data, data_stack, spde,
                            variable_z=variable_z_1,
                            best_previous=best_univariable)
  
  t<-bind_rows(model_linear)
  saveRDS(t,"output/multivar/multivar1_linear.RDS")
  

}else{
  print("no more linear variables left to try in the model building steps")
}

# which variable is chosen after step 1
if(file.exists("output/multivar/multivar1_nonlinear.RDS")){
  nl_step1_result<-readRDS("output/multivar/multivar1_nonlinear.RDS")
}else{
  nl_step1_result<-NULL
}
if(file.exists("output/multivar/multivar1_linear.RDS")){
  l_step1_result<-readRDS("output/multivar/multivar1_linear.RDS")
}else{
  l_step1_result<-NULL
}
best_step1 <- best_univar(nl_step1_result,l_step1_result)
print(paste0("best step 1 model = ", best_step1[1]))

if(run!="short"){
  
## run step 2 ------------------------------------------------------------------

  remove_2 <- narrow_variables(stk.yz, data, best_previous=best_step1[1], run="all")
  remove <- c(remove,remove_2)
  
  # nonlinear
  variable_z_tot <- names(temp_data)[c(7:55)]
  variable_y_tot <- names(temp_data)[c(98:146)]
  
  variable_z_1 <- variable_z_tot[!variable_z_tot %in% paste0(remove,"_z")]
  variable_y_1 <- variable_y_tot[!variable_y_tot %in% paste0(remove,"_y")]
  
  if(length(variable_z_1)>0){
    
    model_nonlinear <- run_multivar2_nl(temp_data, data_stack, spde,
                                        variable_z=variable_z_1, 
                                        variable_y=variable_y_1,
                                        best_previous=best_univariable,
                                        best_previous2=best_step1)
    
    t<-bind_rows(model_nonlinear)
    saveRDS(t,"output/multivar/multivar2_nonlinear.RDS")
    
    
  }else{
    print("no more non linear variables left to try in the model building steps")
  }
  
  # linear
  variable_z_tot <- names(temp_data)[c(56:90)]
  variable_z_1 <- variable_z_tot[!variable_z_tot %in% paste0(remove,"_z")]
  variable_y_1 <- variable_y_tot[!variable_y_tot %in% paste0(remove,"_y")]
  
  if(length(variable_z_1)>0){
    
    model_linear <- run_multivar2_l(temp_data, data_stack, spde,
                                    variable_z=variable_z_1,
                                    best_previous=best_univariable,
                                    best_previous2=best_step1
                                    )
    
    t<-bind_rows(model_linear)
    saveRDS(t,"output/multivar/multivar2_linear.RDS")
    
    
  }else{
    print("no more linear variables left to try in the model building steps")
  }
  
  # which variable is chosen after step 2
  if(file.exists("output/multivar/multivar2_nonlinear.RDS")){
    nl_step2_result<-readRDS("output/multivar/multivar2_nonlinear.RDS")
  }else{
    nl_step2_result<-NULL
  }
  if(file.exists("output/multivar/multivar2_linear.RDS")){
    l_step2_result<-readRDS("output/multivar/multivar2_linear.RDS")
  }else{
    l_step2_result<-NULL
  }
  best_step2 <- best_univar(nl_step2_result,l_step2_result)
  print(paste0("best step 2 model = ", best_step2[1]))
  
  
## run step 3 ------------------------------------------------------------------
  remove_2 <- narrow_variables(stk.yz, data, best_previous=best_step2[1], run="all")
  remove <- c(remove,remove_2)
  
  # nonlinear
  variable_z_tot <- names(temp_data)[c(7:55)]
  variable_y_tot <- names(temp_data)[c(98:146)]
  
  variable_z_1 <- variable_z_tot[!variable_z_tot %in% paste0(remove,"_z")]
  variable_y_1 <- variable_y_tot[!variable_y_tot %in% paste0(remove,"_y")]
  
  if(length(variable_z_1)>0){
    
    model_nonlinear <- run_multivar2_nl(temp_data, data_stack, spde,
                                        variable_z=variable_z_1, 
                                        variable_y=variable_y_1,
                                        best_previous=best_univariable,
                                        best_previous2=best_step1,
                                        best_previous3=best_step2)
    
    t<-bind_rows(model_nonlinear)
    saveRDS(t,"output/multivar/multivar3_nonlinear.RDS")
    
    
  }else{
    print("no more non linear variables left to try in the model building steps")
  }
  
  # linear
  variable_z_tot <- names(temp_data)[c(56:90)]
  variable_z_1 <- variable_z_tot[!variable_z_tot %in% paste0(remove,"_z")]
  variable_y_1 <- variable_y_tot[!variable_y_tot %in% paste0(remove,"_y")]
  
  if(length(variable_z_1)>0){
    
    model_linear <- run_multivar3_l(temp_data, data_stack, spde,
                                    variable_z=variable_z_1,
                                    best_previous=best_univariable,
                                    best_previous2=best_step1,
                                    best_previous3=best_step2
    )
    
    t<-bind_rows(model_linear)
    saveRDS(t,"output/multivar/multivar3_linear.RDS")
    
    
  }else{
    print("no more linear variables left to try in the model building steps")
  }
  
  # which variable is chosen after step 2
  if(file.exists("output/multivar/multivar3_nonlinear.RDS")){
    nl_step3_result<-readRDS("output/multivar/multivar3_nonlinear.RDS")
  }else{
    nl_step3_result<-NULL
  }
  if(file.exists("output/multivar/multivar3_linear.RDS")){
    l_step3_result<-readRDS("output/multivar/multivar3_linear.RDS")
  }else{
    l_step3_result<-NULL
  }
  best_step3 <- best_univar(nl_step3_result,l_step3_result)
  print(paste0("best step 3 model = ", best_step3[1]))
  
}
