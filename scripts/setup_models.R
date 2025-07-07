source("R/mesh_setup.R")

## set up number of unique years and trap IDs
n <- length(unique(data$trap))
m <- length(unique(data$year_no))

## separate presence and prevalence
data %>%  mutate(variable = IR, variable2 = IR) ->new
z <- (new$IR > 0)
y <- ifelse(z == 1, unlist(new$IR), NA)

new_shapefile <- prepare_shapefile(shapefile)

## specify the GMRFs

field.z.idx <- inla.spde.make.index(name = 'x', 
                                    n.spde = spde$n.spde,n.group = m) # hurdle z
field.zc.idx <- inla.spde.make.index(name = 'xc', 
                                     n.spde = spde$n.spde,n.group = m) # shared z and y 
field.y.idx <- inla.spde.make.index(name = 'u', 
                                    n.spde = spde$n.spde,n.group = m) # amount y


## set up new stacks for SPDE models (after initial analysis deciding non linear vs linear for each variable)
stk.z <- inla.stack(
  data = list(Y = cbind(NA,z), link=2),
  A = list(A, 1,1),
  effects = list(field.z.idx, 
                 z.intercept = rep(1, length(z)),
                 
                 list(
                   WEEKLY_AV_TAVG_0_z =inla.group(new$WEEKLY_AV_TAVG_0, n = 30, idx.only = FALSE),
                   WEEKLY_AV_TAVG_1_z =inla.group(new$WEEKLY_AV_TAVG_1, n = 30, idx.only = FALSE),
                   WEEKLY_AV_TAVG_2_z =inla.group(new$WEEKLY_AV_TAVG_2, n = 30, idx.only = FALSE),
                   WEEKLY_AV_TAVG_3_z =inla.group(new$WEEKLY_AV_TAVG_3, n = 30, idx.only = FALSE),
                   WEEKLY_AV_TAVG_01_z =inla.group(new$WEEKLY_AV_TAVG_01, n = 30, idx.only = FALSE),
                   WEEKLY_AV_TAVG_02_z =inla.group(new$WEEKLY_AV_TAVG_02, n = 30, idx.only = FALSE),
                   WEEKLY_AV_TAVG_03_z =inla.group(new$WEEKLY_AV_TAVG_03, n = 30, idx.only = FALSE),
                   
                   
                   WEEKLY_AV_TMIN_0_z =inla.group(new$WEEKLY_AV_TMIN_0, n = 30, idx.only = FALSE),
                   WEEKLY_AV_TMIN_1_z =inla.group(new$WEEKLY_AV_TMIN_1, n = 30, idx.only = FALSE),
                   WEEKLY_AV_TMIN_2_z =inla.group(new$WEEKLY_AV_TMIN_2, n = 30, idx.only = FALSE),
                   WEEKLY_AV_TMIN_3_z =inla.group(new$WEEKLY_AV_TMIN_3, n = 30, idx.only = FALSE),
                   WEEKLY_AV_TMIN_01_z =inla.group(new$WEEKLY_AV_TMIN_01, n = 30, idx.only = FALSE),
                   WEEKLY_AV_TMIN_02_z =inla.group(new$WEEKLY_AV_TMIN_02, n = 30, idx.only = FALSE),
                   WEEKLY_AV_TMIN_03_z =inla.group(new$WEEKLY_AV_TMIN_03, n = 30, idx.only = FALSE),
                   
                   WEEKLY_AV_TMAX_0_z =inla.group(new$WEEKLY_AV_TMAX_0, n = 30, idx.only = FALSE),
                   WEEKLY_AV_TMAX_1_z =inla.group(new$WEEKLY_AV_TMAX_1, n = 30, idx.only = FALSE),
                   WEEKLY_AV_TMAX_2_z =inla.group(new$WEEKLY_AV_TMAX_2, n = 30, idx.only = FALSE),
                   WEEKLY_AV_TMAX_3_z =inla.group(new$WEEKLY_AV_TMAX_3, n = 30, idx.only = FALSE),
                   WEEKLY_AV_TMAX_01_z =inla.group(new$WEEKLY_AV_TMAX_01, n = 30, idx.only = FALSE),
                   WEEKLY_AV_TMAX_02_z =inla.group(new$WEEKLY_AV_TMAX_02, n = 30, idx.only = FALSE),
                   WEEKLY_AV_TMAX_03_z =inla.group(new$WEEKLY_AV_TMAX_03, n = 30, idx.only = FALSE),
                   
                   
                   WEEKLY_AV_ETOPM_0_z =inla.group(new$WEEKLY_AV_ETOPM_0, n = 30, idx.only = FALSE),
                   WEEKLY_AV_ETOPM_1_z =inla.group(new$WEEKLY_AV_ETOPM_1, n = 30, idx.only = FALSE),
                   WEEKLY_AV_ETOPM_2_z =inla.group(new$WEEKLY_AV_ETOPM_2, n = 30, idx.only = FALSE),
                   WEEKLY_AV_ETOPM_3_z =inla.group(new$WEEKLY_AV_ETOPM_3, n = 30, idx.only = FALSE),
                   WEEKLY_AV_ETOPM_01_z =inla.group(new$WEEKLY_AV_ETOPM_01, n = 30, idx.only = FALSE),
                   WEEKLY_AV_ETOPM_02_z =inla.group(new$WEEKLY_AV_ETOPM_02, n = 30, idx.only = FALSE),
                   WEEKLY_AV_ETOPM_03_z =inla.group(new$WEEKLY_AV_ETOPM_03, n = 30, idx.only = FALSE),
                   
                   WEEKLY_AV_RAD_0_z =inla.group(new$WEEKLY_AV_RAD_0, n = 30, idx.only = FALSE),
                   WEEKLY_AV_RAD_1_z =inla.group(new$WEEKLY_AV_RAD_1, n = 30, idx.only = FALSE),
                   WEEKLY_AV_RAD_2_z =inla.group(new$WEEKLY_AV_RAD_2, n = 30, idx.only = FALSE),
                   WEEKLY_AV_RAD_3_z =inla.group(new$WEEKLY_AV_RAD_3, n = 30, idx.only = FALSE),
                   WEEKLY_AV_RAD_01_z =inla.group(new$WEEKLY_AV_RAD_01, n = 30, idx.only = FALSE),
                   WEEKLY_AV_RAD_02_z =inla.group(new$WEEKLY_AV_RAD_02, n = 30, idx.only = FALSE),
                   WEEKLY_AV_RAD_03_z =inla.group(new$WEEKLY_AV_RAD_03, n = 30, idx.only = FALSE),
                   
                   WEEKLY_MIN_TMIN_0_z =inla.group(new$WEEKLY_MIN_TMIN_0, n = 30, idx.only = FALSE),
                   WEEKLY_MIN_TMIN_1_z =inla.group(new$WEEKLY_MIN_TMIN_1, n = 30, idx.only = FALSE),
                   WEEKLY_MIN_TMIN_2_z =inla.group(new$WEEKLY_MIN_TMIN_2, n = 30, idx.only = FALSE),
                   WEEKLY_MIN_TMIN_3_z =inla.group(new$WEEKLY_MIN_TMIN_3, n = 30, idx.only = FALSE),
                   WEEKLY_MIN_TMIN_01_z =inla.group(new$WEEKLY_MIN_TMIN_01, n = 30, idx.only = FALSE),
                   WEEKLY_MIN_TMIN_02_z =inla.group(new$WEEKLY_MIN_TMIN_02, n = 30, idx.only = FALSE),
                   WEEKLY_MIN_TMIN_03_z =inla.group(new$WEEKLY_MIN_TMIN_03, n = 30, idx.only = FALSE),
                   
                   WEEKLY_MAX_TMAX_0_z =inla.group(new$WEEKLY_MAX_TMAX_0, n = 30, idx.only = FALSE),
                   WEEKLY_MAX_TMAX_1_z =inla.group(new$WEEKLY_MAX_TMAX_1, n = 30, idx.only = FALSE),
                   WEEKLY_MAX_TMAX_2_z =inla.group(new$WEEKLY_MAX_TMAX_2, n = 30, idx.only = FALSE),
                   WEEKLY_MAX_TMAX_3_z =inla.group(new$WEEKLY_MAX_TMAX_3, n = 30, idx.only = FALSE),
                   WEEKLY_MAX_TMAX_01_z =inla.group(new$WEEKLY_MAX_TMAX_01, n = 30, idx.only = FALSE),
                   WEEKLY_MAX_TMAX_02_z =inla.group(new$WEEKLY_MAX_TMAX_02, n = 30, idx.only = FALSE),
                   WEEKLY_MAX_TMAX_03_z =inla.group(new$WEEKLY_MAX_TMAX_03, n = 30, idx.only = FALSE),
                   
                   avian_WNV_bin    =cbind(rep(NA, length(z)), ( new$avian_WNV_bin))   , 
                   
                   alt    =cbind(rep(NA, length(z)), ( new$z))   , 
                   
                   CLC1    =cbind(rep(NA, length(z)), ( new$CLC1))   , 
                   CLC2    =cbind(rep(NA, length(z)), ( new$CLC2))   , 
                   CLC3    =cbind(rep(NA, length(z)), ( new$CLC3))   , 
                   CLC4    =cbind(rep(NA, length(z)), ( new$CLC4))   , 
                   CLC5    =cbind(rep(NA, length(z)), ( new$CLC5))   ,
                   
                   WEEKLY_AV_RHAVG_0    =cbind(rep(NA, length(z)), ( new$WEEKLY_AV_RHAVG_0))   ,
                   WEEKLY_AV_RHAVG_1    =cbind(rep(NA, length(z)), ( new$WEEKLY_AV_RHAVG_1))   ,
                   WEEKLY_AV_RHAVG_2    =cbind(rep(NA, length(z)), ( new$WEEKLY_AV_RHAVG_2))   ,
                   WEEKLY_AV_RHAVG_3    =cbind(rep(NA, length(z)), ( new$WEEKLY_AV_RHAVG_3))   ,
                   WEEKLY_AV_RHAVG_01    =cbind(rep(NA, length(z)), ( new$WEEKLY_AV_RHAVG_01))   ,
                   WEEKLY_AV_RHAVG_02    =cbind(rep(NA, length(z)), ( new$WEEKLY_AV_RHAVG_02))   ,
                   WEEKLY_AV_RHAVG_03    =cbind(rep(NA, length(z)), ( new$WEEKLY_AV_RHAVG_03))   ,
                   
                   WEEKLY_CUM_PREC_0    =cbind(rep(NA, length(z)), ( new$WEEKLY_CUM_PREC_0))   ,
                   WEEKLY_CUM_PREC_1    =cbind(rep(NA, length(z)), ( new$WEEKLY_CUM_PREC_1))   ,
                   WEEKLY_CUM_PREC_2    =cbind(rep(NA, length(z)), ( new$WEEKLY_CUM_PREC_2))   ,
                   WEEKLY_CUM_PREC_3    =cbind(rep(NA, length(z)), ( new$WEEKLY_CUM_PREC_3))   ,
                   WEEKLY_CUM_PREC_01    =cbind(rep(NA, length(z)), ( new$WEEKLY_CUM_PREC_01))   ,
                   WEEKLY_CUM_PREC_02    =cbind(rep(NA, length(z)), ( new$WEEKLY_CUM_PREC_02))   ,
                   WEEKLY_CUM_PREC_03    =cbind(rep(NA, length(z)), ( new$WEEKLY_CUM_PREC_03))   ,
                   
                   
                   WEEKLY_CUM_LEAFW_0    =cbind(rep(NA, length(z)), ( new$WEEKLY_CUM_LEAFW_0))   ,
                   WEEKLY_CUM_LEAFW_1    =cbind(rep(NA, length(z)), ( new$WEEKLY_CUM_LEAFW_1))   ,
                   WEEKLY_CUM_LEAFW_2    =cbind(rep(NA, length(z)), ( new$WEEKLY_CUM_LEAFW_2))   ,
                   WEEKLY_CUM_LEAFW_3    =cbind(rep(NA, length(z)), ( new$WEEKLY_CUM_LEAFW_3))   ,
                   WEEKLY_CUM_LEAFW_01    =cbind(rep(NA, length(z)), ( new$WEEKLY_CUM_LEAFW_01))   ,
                   WEEKLY_CUM_LEAFW_02    =cbind(rep(NA, length(z)), ( new$WEEKLY_CUM_LEAFW_02))   ,
                   WEEKLY_CUM_LEAFW_03    =cbind(rep(NA, length(z)), ( new$WEEKLY_CUM_LEAFW_03))   ,
                   
                   WEEKLY_AV_LEAFW_0    =cbind(rep(NA, length(z)), ( new$WEEKLY_AV_LEAFW_0))   ,
                   WEEKLY_AV_LEAFW_1    =cbind(rep(NA, length(z)), ( new$WEEKLY_AV_LEAFW_1))   ,
                   WEEKLY_AV_LEAFW_2    =cbind(rep(NA, length(z)), ( new$WEEKLY_AV_LEAFW_2))   ,
                   WEEKLY_AV_LEAFW_3    =cbind(rep(NA, length(z)), ( new$WEEKLY_AV_LEAFW_3))   ,
                   WEEKLY_AV_LEAFW_01    =cbind(rep(NA, length(z)), ( new$WEEKLY_AV_LEAFW_01))   ,
                   WEEKLY_AV_LEAFW_02    =cbind(rep(NA, length(z)), ( new$WEEKLY_AV_LEAFW_02))   ,
                   WEEKLY_AV_LEAFW_03    =cbind(rep(NA, length(z)), ( new$WEEKLY_AV_LEAFW_03)) 
                   
                 )
  ), 
  tag = 'est.z') 


stk.y <- inla.stack(
  data = list(Y = cbind(y,NA), link=1),
  A = list(A, 1,1),
  effects = list(c(field.zc.idx, field.y.idx), y.intercept = rep(1, length(z)),
                 
                 list(
                   
                   WEEKLY_AV_TAVG_0_y =inla.group(new$WEEKLY_AV_TAVG_0, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_TAVG_1_y =inla.group(new$WEEKLY_AV_TAVG_1, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_TAVG_2_y =inla.group(new$WEEKLY_AV_TAVG_2, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_TAVG_3_y =inla.group(new$WEEKLY_AV_TAVG_3, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_TAVG_01_y =inla.group(new$WEEKLY_AV_TAVG_01, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_TAVG_02_y =inla.group(new$WEEKLY_AV_TAVG_02, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_TAVG_03_y =inla.group(new$WEEKLY_AV_TAVG_03, n = 30, idx.only = FALSE),         
                   
                   WEEKLY_AV_TMIN_0_y =inla.group(new$WEEKLY_AV_TMIN_0, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_TMIN_1_y =inla.group(new$WEEKLY_AV_TMIN_1, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_TMIN_2_y =inla.group(new$WEEKLY_AV_TMIN_2, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_TMIN_3_y =inla.group(new$WEEKLY_AV_TMIN_3, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_TMIN_01_y =inla.group(new$WEEKLY_AV_TMIN_01, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_TMIN_02_y =inla.group(new$WEEKLY_AV_TMIN_02, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_TMIN_03_y =inla.group(new$WEEKLY_AV_TMIN_03, n = 30, idx.only = FALSE),         
                   
                   WEEKLY_AV_TMAX_0_y =inla.group(new$WEEKLY_AV_TMAX_0, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_TMAX_1_y =inla.group(new$WEEKLY_AV_TMAX_1, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_TMAX_2_y =inla.group(new$WEEKLY_AV_TMAX_2, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_TMAX_3_y =inla.group(new$WEEKLY_AV_TMAX_3, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_TMAX_01_y =inla.group(new$WEEKLY_AV_TMAX_01, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_TMAX_02_y =inla.group(new$WEEKLY_AV_TMAX_02, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_TMAX_03_y =inla.group(new$WEEKLY_AV_TMAX_03, n = 30, idx.only = FALSE),         
                   
                   WEEKLY_AV_ETOPM_0_y =inla.group(new$WEEKLY_AV_ETOPM_0, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_ETOPM_1_y =inla.group(new$WEEKLY_AV_ETOPM_1, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_ETOPM_2_y =inla.group(new$WEEKLY_AV_ETOPM_2, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_ETOPM_3_y =inla.group(new$WEEKLY_AV_ETOPM_3, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_ETOPM_01_y =inla.group(new$WEEKLY_AV_ETOPM_01, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_ETOPM_02_y =inla.group(new$WEEKLY_AV_ETOPM_02, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_ETOPM_03_y =inla.group(new$WEEKLY_AV_ETOPM_03, n = 30, idx.only = FALSE),         
                   
                   WEEKLY_AV_RAD_0_y =inla.group(new$WEEKLY_AV_RAD_0, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_RAD_1_y =inla.group(new$WEEKLY_AV_RAD_1, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_RAD_2_y =inla.group(new$WEEKLY_AV_RAD_2, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_RAD_3_y =inla.group(new$WEEKLY_AV_RAD_3, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_RAD_01_y =inla.group(new$WEEKLY_AV_RAD_01, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_RAD_02_y =inla.group(new$WEEKLY_AV_RAD_02, n = 30, idx.only = FALSE),         
                   WEEKLY_AV_RAD_03_y =inla.group(new$WEEKLY_AV_RAD_03, n = 30, idx.only = FALSE),         
                   
                   WEEKLY_MIN_TMIN_0_y =inla.group(new$WEEKLY_MIN_TMIN_0, n = 30, idx.only = FALSE),         
                   WEEKLY_MIN_TMIN_1_y =inla.group(new$WEEKLY_MIN_TMIN_1, n = 30, idx.only = FALSE),         
                   WEEKLY_MIN_TMIN_2_y =inla.group(new$WEEKLY_MIN_TMIN_2, n = 30, idx.only = FALSE),         
                   WEEKLY_MIN_TMIN_3_y =inla.group(new$WEEKLY_MIN_TMIN_3, n = 30, idx.only = FALSE),         
                   WEEKLY_MIN_TMIN_01_y =inla.group(new$WEEKLY_MIN_TMIN_01, n = 30, idx.only = FALSE),         
                   WEEKLY_MIN_TMIN_02_y =inla.group(new$WEEKLY_MIN_TMIN_02, n = 30, idx.only = FALSE),         
                   WEEKLY_MIN_TMIN_03_y =inla.group(new$WEEKLY_MIN_TMIN_03, n = 30, idx.only = FALSE),         
                   
                   WEEKLY_MAX_TMAX_0_y =inla.group(new$WEEKLY_MAX_TMAX_0, n = 30, idx.only = FALSE),         
                   WEEKLY_MAX_TMAX_1_y =inla.group(new$WEEKLY_MAX_TMAX_1, n = 30, idx.only = FALSE),         
                   WEEKLY_MAX_TMAX_2_y =inla.group(new$WEEKLY_MAX_TMAX_2, n = 30, idx.only = FALSE),         
                   WEEKLY_MAX_TMAX_3_y =inla.group(new$WEEKLY_MAX_TMAX_3, n = 30, idx.only = FALSE),         
                   WEEKLY_MAX_TMAX_01_y =inla.group(new$WEEKLY_MAX_TMAX_01, n = 30, idx.only = FALSE),         
                   WEEKLY_MAX_TMAX_02_y =inla.group(new$WEEKLY_MAX_TMAX_02, n = 30, idx.only = FALSE),         
                   WEEKLY_MAX_TMAX_03_y =inla.group(new$WEEKLY_MAX_TMAX_03, n = 30, idx.only = FALSE),
                   
                   avian_WNV_bin    =cbind(( new$avian_WNV_bin), rep(NA, length(z)))   , 
                   
                   alt    =cbind(( new$alt), rep(NA, length(z)))   , 
                   
                   CLC1    =cbind((new$CLC1),rep(NA, length(z)))   , 
                   CLC2    =cbind((new$CLC2),rep(NA, length(z)))   , 
                   CLC3    =cbind((new$CLC3),rep(NA, length(z)))   , 
                   CLC4    =cbind((new$CLC4),rep(NA, length(z)))   , 
                   CLC5    =cbind((new$CLC5),rep(NA, length(z)))   ,
                   
                   WEEKLY_AV_RHAVG_0    =cbind(( new$WEEKLY_AV_RHAVG_0),rep(NA, length(z)))   ,
                   WEEKLY_AV_RHAVG_1    =cbind(( new$WEEKLY_AV_RHAVG_1),rep(NA, length(z)))   ,
                   WEEKLY_AV_RHAVG_2    =cbind(( new$WEEKLY_AV_RHAVG_2),rep(NA, length(z)))   ,
                   WEEKLY_AV_RHAVG_3    =cbind(( new$WEEKLY_AV_RHAVG_3),rep(NA, length(z)))   ,
                   WEEKLY_AV_RHAVG_01    =cbind(( new$WEEKLY_AV_RHAVG_01),rep(NA, length(z)))   ,
                   WEEKLY_AV_RHAVG_02    =cbind(( new$WEEKLY_AV_RHAVG_02),rep(NA, length(z)))   ,
                   WEEKLY_AV_RHAVG_03    =cbind(( new$WEEKLY_AV_RHAVG_03),rep(NA, length(z)))   ,
                   
                   WEEKLY_AV_LEAFW_0    =cbind(( new$WEEKLY_AV_LEAFW_0),rep(NA, length(z)))   ,
                   WEEKLY_AV_LEAFW_1    =cbind(( new$WEEKLY_AV_LEAFW_1),rep(NA, length(z)))   ,
                   WEEKLY_AV_LEAFW_2    =cbind(( new$WEEKLY_AV_LEAFW_2),rep(NA, length(z)))   ,
                   WEEKLY_AV_LEAFW_3    =cbind(( new$WEEKLY_AV_LEAFW_3),rep(NA, length(z)))   ,
                   WEEKLY_AV_LEAFW_01    =cbind(( new$WEEKLY_AV_LEAFW_01),rep(NA, length(z)))   ,
                   WEEKLY_AV_LEAFW_02    =cbind(( new$WEEKLY_AV_LEAFW_02),rep(NA, length(z)))   ,
                   WEEKLY_AV_LEAFW_03    =cbind(( new$WEEKLY_AV_LEAFW_03),rep(NA, length(z)))   ,
                   
                   WEEKLY_CUM_LEAFW_0    =cbind(( new$WEEKLY_CUM_LEAFW_0),rep(NA, length(z)))   ,
                   WEEKLY_CUM_LEAFW_1    =cbind(( new$WEEKLY_CUM_LEAFW_1),rep(NA, length(z)))   ,
                   WEEKLY_CUM_LEAFW_2    =cbind(( new$WEEKLY_CUM_LEAFW_2),rep(NA, length(z)))   ,
                   WEEKLY_CUM_LEAFW_3    =cbind(( new$WEEKLY_CUM_LEAFW_3),rep(NA, length(z)))   ,
                   WEEKLY_CUM_LEAFW_01    =cbind(( new$WEEKLY_CUM_LEAFW_01),rep(NA, length(z)))   ,
                   WEEKLY_CUM_LEAFW_02    =cbind(( new$WEEKLY_CUM_LEAFW_02),rep(NA, length(z)))   ,
                   WEEKLY_CUM_LEAFW_03    =cbind(( new$WEEKLY_CUM_LEAFW_03),rep(NA, length(z)))   ,
                   
                   WEEKLY_CUM_PREC_0    =cbind(( new$WEEKLY_CUM_PREC_0),rep(NA, length(z)))   ,
                   WEEKLY_CUM_PREC_1    =cbind(( new$WEEKLY_CUM_PREC_1),rep(NA, length(z)))   ,
                   WEEKLY_CUM_PREC_2    =cbind(( new$WEEKLY_CUM_PREC_2),rep(NA, length(z)))   ,
                   WEEKLY_CUM_PREC_3    =cbind(( new$WEEKLY_CUM_PREC_3),rep(NA, length(z)))   ,
                   WEEKLY_CUM_PREC_01    =cbind(( new$WEEKLY_CUM_PREC_01),rep(NA, length(z)))   ,
                   WEEKLY_CUM_PREC_02    =cbind(( new$WEEKLY_CUM_PREC_02),rep(NA, length(z)))   ,
                   WEEKLY_CUM_PREC_03    =cbind(( new$WEEKLY_CUM_PREC_03),rep(NA, length(z)))   
                   
                 )
  ), 
  tag = 'est.y') 

### join all the data stacks:
stk.yz <- inla.stack(stk.z, stk.y)
