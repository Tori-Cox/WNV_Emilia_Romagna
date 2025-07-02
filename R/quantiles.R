
quantiles_variables <-function(new){
## set up stack with variables split into quantiles
new %>% 
  mutate(WEEKLY_AV_TMIN_0 = 
           case_when(WEEKLY_AV_TMIN_0 <= quantile(new$WEEKLY_AV_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_TMIN_0 > quantile(new$WEEKLY_AV_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_TMIN_0 <= quantile(new$WEEKLY_AV_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_TMIN_0 > quantile(new$WEEKLY_AV_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_TMIN_0 <= quantile(new$WEEKLY_AV_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_TMIN_0 > quantile(new$WEEKLY_AV_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_TMIN_0 <= quantile(new$WEEKLY_AV_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_TMIN_0 > quantile(new$WEEKLY_AV_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_TMIN_0 <= quantile(new$WEEKLY_AV_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_TMIN_0 > quantile(new$WEEKLY_AV_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_TMIN_0 <= quantile(new$WEEKLY_AV_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_TMIN_0 > quantile(new$WEEKLY_AV_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_TMIN_0 <= quantile(new$WEEKLY_AV_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_TMIN_0 > quantile(new$WEEKLY_AV_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_TMIN_0 <= quantile(new$WEEKLY_AV_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_TMIN_0 > quantile(new$WEEKLY_AV_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_TMIN_0 <= quantile(new$WEEKLY_AV_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_TMIN_0 > quantile(new$WEEKLY_AV_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_TMAX_0 = 
           case_when(WEEKLY_AV_TMAX_0 <= quantile(new$WEEKLY_AV_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_TMAX_0 > quantile(new$WEEKLY_AV_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_TMAX_0 <= quantile(new$WEEKLY_AV_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_TMAX_0 > quantile(new$WEEKLY_AV_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_TMAX_0 <= quantile(new$WEEKLY_AV_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_TMAX_0 > quantile(new$WEEKLY_AV_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_TMAX_0 <= quantile(new$WEEKLY_AV_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_TMAX_0 > quantile(new$WEEKLY_AV_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_TMAX_0 <= quantile(new$WEEKLY_AV_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_TMAX_0 > quantile(new$WEEKLY_AV_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_TMAX_0 <= quantile(new$WEEKLY_AV_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_TMAX_0 > quantile(new$WEEKLY_AV_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_TMAX_0 <= quantile(new$WEEKLY_AV_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_TMAX_0 > quantile(new$WEEKLY_AV_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_TMAX_0 <= quantile(new$WEEKLY_AV_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_TMAX_0 > quantile(new$WEEKLY_AV_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_TMAX_0 <= quantile(new$WEEKLY_AV_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_TMAX_0 > quantile(new$WEEKLY_AV_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_TAVG_0 = 
           case_when(WEEKLY_AV_TAVG_0 <= quantile(new$WEEKLY_AV_TAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_TAVG_0 > quantile(new$WEEKLY_AV_TAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_TAVG_0 <= quantile(new$WEEKLY_AV_TAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_TAVG_0 > quantile(new$WEEKLY_AV_TAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_TAVG_0 <= quantile(new$WEEKLY_AV_TAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_TAVG_0 > quantile(new$WEEKLY_AV_TAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_TAVG_0 <= quantile(new$WEEKLY_AV_TAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_TAVG_0 > quantile(new$WEEKLY_AV_TAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_TAVG_0 <= quantile(new$WEEKLY_AV_TAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_TAVG_0 > quantile(new$WEEKLY_AV_TAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_TAVG_0 <= quantile(new$WEEKLY_AV_TAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_TAVG_0 > quantile(new$WEEKLY_AV_TAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_TAVG_0 <= quantile(new$WEEKLY_AV_TAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_TAVG_0 > quantile(new$WEEKLY_AV_TAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_TAVG_0 <= quantile(new$WEEKLY_AV_TAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_TAVG_0 > quantile(new$WEEKLY_AV_TAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_TAVG_0 <= quantile(new$WEEKLY_AV_TAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_TAVG_0 > quantile(new$WEEKLY_AV_TAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_RHAVG_0 = 
           case_when(WEEKLY_AV_RHAVG_0 <= quantile(new$WEEKLY_AV_RHAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_RHAVG_0 > quantile(new$WEEKLY_AV_RHAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_RHAVG_0 <= quantile(new$WEEKLY_AV_RHAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_RHAVG_0 > quantile(new$WEEKLY_AV_RHAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_RHAVG_0 <= quantile(new$WEEKLY_AV_RHAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_RHAVG_0 > quantile(new$WEEKLY_AV_RHAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_RHAVG_0 <= quantile(new$WEEKLY_AV_RHAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_RHAVG_0 > quantile(new$WEEKLY_AV_RHAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_RHAVG_0 <= quantile(new$WEEKLY_AV_RHAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_RHAVG_0 > quantile(new$WEEKLY_AV_RHAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_RHAVG_0 <= quantile(new$WEEKLY_AV_RHAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_RHAVG_0 > quantile(new$WEEKLY_AV_RHAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_RHAVG_0 <= quantile(new$WEEKLY_AV_RHAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_RHAVG_0 > quantile(new$WEEKLY_AV_RHAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_RHAVG_0 <= quantile(new$WEEKLY_AV_RHAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_RHAVG_0 > quantile(new$WEEKLY_AV_RHAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_RHAVG_0 <= quantile(new$WEEKLY_AV_RHAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_RHAVG_0 > quantile(new$WEEKLY_AV_RHAVG_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_RAD_0 = 
           case_when(WEEKLY_AV_RAD_0 <= quantile(new$WEEKLY_AV_RAD_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_RAD_0 > quantile(new$WEEKLY_AV_RAD_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_RAD_0 <= quantile(new$WEEKLY_AV_RAD_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_RAD_0 > quantile(new$WEEKLY_AV_RAD_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_RAD_0 <= quantile(new$WEEKLY_AV_RAD_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_RAD_0 > quantile(new$WEEKLY_AV_RAD_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_RAD_0 <= quantile(new$WEEKLY_AV_RAD_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_RAD_0 > quantile(new$WEEKLY_AV_RAD_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_RAD_0 <= quantile(new$WEEKLY_AV_RAD_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_RAD_0 > quantile(new$WEEKLY_AV_RAD_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_RAD_0 <= quantile(new$WEEKLY_AV_RAD_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_RAD_0 > quantile(new$WEEKLY_AV_RAD_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_RAD_0 <= quantile(new$WEEKLY_AV_RAD_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_RAD_0 > quantile(new$WEEKLY_AV_RAD_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_RAD_0 <= quantile(new$WEEKLY_AV_RAD_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_RAD_0 > quantile(new$WEEKLY_AV_RAD_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_RAD_0 <= quantile(new$WEEKLY_AV_RAD_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_RAD_0 > quantile(new$WEEKLY_AV_RAD_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_ETOPM_0 = 
           case_when(WEEKLY_AV_ETOPM_0 <= quantile(new$WEEKLY_AV_ETOPM_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_ETOPM_0 > quantile(new$WEEKLY_AV_ETOPM_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_ETOPM_0 <= quantile(new$WEEKLY_AV_ETOPM_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_ETOPM_0 > quantile(new$WEEKLY_AV_ETOPM_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_ETOPM_0 <= quantile(new$WEEKLY_AV_ETOPM_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_ETOPM_0 > quantile(new$WEEKLY_AV_ETOPM_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_ETOPM_0 <= quantile(new$WEEKLY_AV_ETOPM_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_ETOPM_0 > quantile(new$WEEKLY_AV_ETOPM_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_ETOPM_0 <= quantile(new$WEEKLY_AV_ETOPM_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_ETOPM_0 > quantile(new$WEEKLY_AV_ETOPM_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_ETOPM_0 <= quantile(new$WEEKLY_AV_ETOPM_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_ETOPM_0 > quantile(new$WEEKLY_AV_ETOPM_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_ETOPM_0 <= quantile(new$WEEKLY_AV_ETOPM_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_ETOPM_0 > quantile(new$WEEKLY_AV_ETOPM_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_ETOPM_0 <= quantile(new$WEEKLY_AV_ETOPM_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_ETOPM_0 > quantile(new$WEEKLY_AV_ETOPM_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_ETOPM_0 <= quantile(new$WEEKLY_AV_ETOPM_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_ETOPM_0 > quantile(new$WEEKLY_AV_ETOPM_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_LEAFW_0 = 
           case_when(WEEKLY_AV_LEAFW_0 <= quantile(new$WEEKLY_AV_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_LEAFW_0 > quantile(new$WEEKLY_AV_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_LEAFW_0 <= quantile(new$WEEKLY_AV_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_LEAFW_0 > quantile(new$WEEKLY_AV_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_LEAFW_0 <= quantile(new$WEEKLY_AV_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_LEAFW_0 > quantile(new$WEEKLY_AV_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_LEAFW_0 <= quantile(new$WEEKLY_AV_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_LEAFW_0 > quantile(new$WEEKLY_AV_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_LEAFW_0 <= quantile(new$WEEKLY_AV_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_LEAFW_0 > quantile(new$WEEKLY_AV_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_LEAFW_0 <= quantile(new$WEEKLY_AV_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_LEAFW_0 > quantile(new$WEEKLY_AV_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_LEAFW_0 <= quantile(new$WEEKLY_AV_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_LEAFW_0 > quantile(new$WEEKLY_AV_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_LEAFW_0 <= quantile(new$WEEKLY_AV_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_LEAFW_0 > quantile(new$WEEKLY_AV_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_LEAFW_0 <= quantile(new$WEEKLY_AV_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_LEAFW_0 > quantile(new$WEEKLY_AV_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_CUM_LEAFW_0 = 
           case_when(WEEKLY_CUM_LEAFW_0 <= quantile(new$WEEKLY_CUM_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_CUM_LEAFW_0 > quantile(new$WEEKLY_CUM_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_CUM_LEAFW_0 <= quantile(new$WEEKLY_CUM_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_CUM_LEAFW_0 > quantile(new$WEEKLY_CUM_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_CUM_LEAFW_0 <= quantile(new$WEEKLY_CUM_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_CUM_LEAFW_0 > quantile(new$WEEKLY_CUM_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_CUM_LEAFW_0 <= quantile(new$WEEKLY_CUM_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_CUM_LEAFW_0 > quantile(new$WEEKLY_CUM_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_CUM_LEAFW_0 <= quantile(new$WEEKLY_CUM_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_CUM_LEAFW_0 > quantile(new$WEEKLY_CUM_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_CUM_LEAFW_0 <= quantile(new$WEEKLY_CUM_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_CUM_LEAFW_0 > quantile(new$WEEKLY_CUM_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_CUM_LEAFW_0 <= quantile(new$WEEKLY_CUM_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_CUM_LEAFW_0 > quantile(new$WEEKLY_CUM_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_CUM_LEAFW_0 <= quantile(new$WEEKLY_CUM_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_CUM_LEAFW_0 > quantile(new$WEEKLY_CUM_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_CUM_LEAFW_0 <= quantile(new$WEEKLY_CUM_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_CUM_LEAFW_0 > quantile(new$WEEKLY_CUM_LEAFW_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_CUM_PREC_0 = 
           case_when(WEEKLY_CUM_PREC_0 <= quantile(new$WEEKLY_CUM_PREC_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_CUM_PREC_0 > quantile(new$WEEKLY_CUM_PREC_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_CUM_PREC_0 <= quantile(new$WEEKLY_CUM_PREC_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_CUM_PREC_0 > quantile(new$WEEKLY_CUM_PREC_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_CUM_PREC_0 <= quantile(new$WEEKLY_CUM_PREC_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_CUM_PREC_0 > quantile(new$WEEKLY_CUM_PREC_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_CUM_PREC_0 <= quantile(new$WEEKLY_CUM_PREC_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_CUM_PREC_0 > quantile(new$WEEKLY_CUM_PREC_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_CUM_PREC_0 <= quantile(new$WEEKLY_CUM_PREC_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_CUM_PREC_0 > quantile(new$WEEKLY_CUM_PREC_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_CUM_PREC_0 <= quantile(new$WEEKLY_CUM_PREC_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_CUM_PREC_0 > quantile(new$WEEKLY_CUM_PREC_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_CUM_PREC_0 <= quantile(new$WEEKLY_CUM_PREC_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_CUM_PREC_0 > quantile(new$WEEKLY_CUM_PREC_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_CUM_PREC_0 <= quantile(new$WEEKLY_CUM_PREC_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_CUM_PREC_0 > quantile(new$WEEKLY_CUM_PREC_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_CUM_PREC_0 <= quantile(new$WEEKLY_CUM_PREC_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_CUM_PREC_0 > quantile(new$WEEKLY_CUM_PREC_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_MIN_TMIN_0 = 
           case_when(WEEKLY_MIN_TMIN_0 <= quantile(new$WEEKLY_MIN_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_MIN_TMIN_0 > quantile(new$WEEKLY_MIN_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_MIN_TMIN_0 <= quantile(new$WEEKLY_MIN_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_MIN_TMIN_0 > quantile(new$WEEKLY_MIN_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_MIN_TMIN_0 <= quantile(new$WEEKLY_MIN_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_MIN_TMIN_0 > quantile(new$WEEKLY_MIN_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_MIN_TMIN_0 <= quantile(new$WEEKLY_MIN_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_MIN_TMIN_0 > quantile(new$WEEKLY_MIN_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_MIN_TMIN_0 <= quantile(new$WEEKLY_MIN_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_MIN_TMIN_0 > quantile(new$WEEKLY_MIN_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_MIN_TMIN_0 <= quantile(new$WEEKLY_MIN_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_MIN_TMIN_0 > quantile(new$WEEKLY_MIN_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_MIN_TMIN_0 <= quantile(new$WEEKLY_MIN_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_MIN_TMIN_0 > quantile(new$WEEKLY_MIN_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_MIN_TMIN_0 <= quantile(new$WEEKLY_MIN_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_MIN_TMIN_0 > quantile(new$WEEKLY_MIN_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_MIN_TMIN_0 <= quantile(new$WEEKLY_MIN_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_MIN_TMIN_0 > quantile(new$WEEKLY_MIN_TMIN_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_MAX_TMAX_0 = 
           case_when(WEEKLY_MAX_TMAX_0 <= quantile(new$WEEKLY_MAX_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_MAX_TMAX_0 > quantile(new$WEEKLY_MAX_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_MAX_TMAX_0 <= quantile(new$WEEKLY_MAX_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_MAX_TMAX_0 > quantile(new$WEEKLY_MAX_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_MAX_TMAX_0 <= quantile(new$WEEKLY_MAX_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_MAX_TMAX_0 > quantile(new$WEEKLY_MAX_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_MAX_TMAX_0 <= quantile(new$WEEKLY_MAX_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_MAX_TMAX_0 > quantile(new$WEEKLY_MAX_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_MAX_TMAX_0 <= quantile(new$WEEKLY_MAX_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_MAX_TMAX_0 > quantile(new$WEEKLY_MAX_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_MAX_TMAX_0 <= quantile(new$WEEKLY_MAX_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_MAX_TMAX_0 > quantile(new$WEEKLY_MAX_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_MAX_TMAX_0 <= quantile(new$WEEKLY_MAX_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_MAX_TMAX_0 > quantile(new$WEEKLY_MAX_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_MAX_TMAX_0 <= quantile(new$WEEKLY_MAX_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_MAX_TMAX_0 > quantile(new$WEEKLY_MAX_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_MAX_TMAX_0 <= quantile(new$WEEKLY_MAX_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_MAX_TMAX_0 > quantile(new$WEEKLY_MAX_TMAX_0, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_TMIN_1 = 
           case_when(WEEKLY_AV_TMIN_1 <= quantile(new$WEEKLY_AV_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_TMIN_1 > quantile(new$WEEKLY_AV_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_TMIN_1 <= quantile(new$WEEKLY_AV_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_TMIN_1 > quantile(new$WEEKLY_AV_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_TMIN_1 <= quantile(new$WEEKLY_AV_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_TMIN_1 > quantile(new$WEEKLY_AV_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_TMIN_1 <= quantile(new$WEEKLY_AV_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_TMIN_1 > quantile(new$WEEKLY_AV_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_TMIN_1 <= quantile(new$WEEKLY_AV_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_TMIN_1 > quantile(new$WEEKLY_AV_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_TMIN_1 <= quantile(new$WEEKLY_AV_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_TMIN_1 > quantile(new$WEEKLY_AV_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_TMIN_1 <= quantile(new$WEEKLY_AV_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_TMIN_1 > quantile(new$WEEKLY_AV_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_TMIN_1 <= quantile(new$WEEKLY_AV_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_TMIN_1 > quantile(new$WEEKLY_AV_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_TMIN_1 <= quantile(new$WEEKLY_AV_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_TMIN_1 > quantile(new$WEEKLY_AV_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_TMAX_1 = 
           case_when(WEEKLY_AV_TMAX_1 <= quantile(new$WEEKLY_AV_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_TMAX_1 > quantile(new$WEEKLY_AV_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_TMAX_1 <= quantile(new$WEEKLY_AV_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_TMAX_1 > quantile(new$WEEKLY_AV_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_TMAX_1 <= quantile(new$WEEKLY_AV_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_TMAX_1 > quantile(new$WEEKLY_AV_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_TMAX_1 <= quantile(new$WEEKLY_AV_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_TMAX_1 > quantile(new$WEEKLY_AV_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_TMAX_1 <= quantile(new$WEEKLY_AV_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_TMAX_1 > quantile(new$WEEKLY_AV_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_TMAX_1 <= quantile(new$WEEKLY_AV_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_TMAX_1 > quantile(new$WEEKLY_AV_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_TMAX_1 <= quantile(new$WEEKLY_AV_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_TMAX_1 > quantile(new$WEEKLY_AV_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_TMAX_1 <= quantile(new$WEEKLY_AV_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_TMAX_1 > quantile(new$WEEKLY_AV_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_TMAX_1 <= quantile(new$WEEKLY_AV_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_TMAX_1 > quantile(new$WEEKLY_AV_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_TAVG_1 = 
           case_when(WEEKLY_AV_TAVG_1 <= quantile(new$WEEKLY_AV_TAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_TAVG_1 > quantile(new$WEEKLY_AV_TAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_TAVG_1 <= quantile(new$WEEKLY_AV_TAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_TAVG_1 > quantile(new$WEEKLY_AV_TAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_TAVG_1 <= quantile(new$WEEKLY_AV_TAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_TAVG_1 > quantile(new$WEEKLY_AV_TAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_TAVG_1 <= quantile(new$WEEKLY_AV_TAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_TAVG_1 > quantile(new$WEEKLY_AV_TAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_TAVG_1 <= quantile(new$WEEKLY_AV_TAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_TAVG_1 > quantile(new$WEEKLY_AV_TAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_TAVG_1 <= quantile(new$WEEKLY_AV_TAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_TAVG_1 > quantile(new$WEEKLY_AV_TAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_TAVG_1 <= quantile(new$WEEKLY_AV_TAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_TAVG_1 > quantile(new$WEEKLY_AV_TAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_TAVG_1 <= quantile(new$WEEKLY_AV_TAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_TAVG_1 > quantile(new$WEEKLY_AV_TAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_TAVG_1 <= quantile(new$WEEKLY_AV_TAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_TAVG_1 > quantile(new$WEEKLY_AV_TAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_RHAVG_1 = 
           case_when(WEEKLY_AV_RHAVG_1 <= quantile(new$WEEKLY_AV_RHAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_RHAVG_1 > quantile(new$WEEKLY_AV_RHAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_RHAVG_1 <= quantile(new$WEEKLY_AV_RHAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_RHAVG_1 > quantile(new$WEEKLY_AV_RHAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_RHAVG_1 <= quantile(new$WEEKLY_AV_RHAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_RHAVG_1 > quantile(new$WEEKLY_AV_RHAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_RHAVG_1 <= quantile(new$WEEKLY_AV_RHAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_RHAVG_1 > quantile(new$WEEKLY_AV_RHAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_RHAVG_1 <= quantile(new$WEEKLY_AV_RHAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_RHAVG_1 > quantile(new$WEEKLY_AV_RHAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_RHAVG_1 <= quantile(new$WEEKLY_AV_RHAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_RHAVG_1 > quantile(new$WEEKLY_AV_RHAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_RHAVG_1 <= quantile(new$WEEKLY_AV_RHAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_RHAVG_1 > quantile(new$WEEKLY_AV_RHAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_RHAVG_1 <= quantile(new$WEEKLY_AV_RHAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_RHAVG_1 > quantile(new$WEEKLY_AV_RHAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_RHAVG_1 <= quantile(new$WEEKLY_AV_RHAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_RHAVG_1 > quantile(new$WEEKLY_AV_RHAVG_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_RAD_1 = 
           case_when(WEEKLY_AV_RAD_1 <= quantile(new$WEEKLY_AV_RAD_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_RAD_1 > quantile(new$WEEKLY_AV_RAD_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_RAD_1 <= quantile(new$WEEKLY_AV_RAD_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_RAD_1 > quantile(new$WEEKLY_AV_RAD_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_RAD_1 <= quantile(new$WEEKLY_AV_RAD_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_RAD_1 > quantile(new$WEEKLY_AV_RAD_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_RAD_1 <= quantile(new$WEEKLY_AV_RAD_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_RAD_1 > quantile(new$WEEKLY_AV_RAD_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_RAD_1 <= quantile(new$WEEKLY_AV_RAD_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_RAD_1 > quantile(new$WEEKLY_AV_RAD_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_RAD_1 <= quantile(new$WEEKLY_AV_RAD_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_RAD_1 > quantile(new$WEEKLY_AV_RAD_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_RAD_1 <= quantile(new$WEEKLY_AV_RAD_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_RAD_1 > quantile(new$WEEKLY_AV_RAD_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_RAD_1 <= quantile(new$WEEKLY_AV_RAD_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_RAD_1 > quantile(new$WEEKLY_AV_RAD_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_RAD_1 <= quantile(new$WEEKLY_AV_RAD_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_RAD_1 > quantile(new$WEEKLY_AV_RAD_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_ETOPM_1 = 
           case_when(WEEKLY_AV_ETOPM_1 <= quantile(new$WEEKLY_AV_ETOPM_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_ETOPM_1 > quantile(new$WEEKLY_AV_ETOPM_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_ETOPM_1 <= quantile(new$WEEKLY_AV_ETOPM_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_ETOPM_1 > quantile(new$WEEKLY_AV_ETOPM_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_ETOPM_1 <= quantile(new$WEEKLY_AV_ETOPM_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_ETOPM_1 > quantile(new$WEEKLY_AV_ETOPM_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_ETOPM_1 <= quantile(new$WEEKLY_AV_ETOPM_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_ETOPM_1 > quantile(new$WEEKLY_AV_ETOPM_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_ETOPM_1 <= quantile(new$WEEKLY_AV_ETOPM_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_ETOPM_1 > quantile(new$WEEKLY_AV_ETOPM_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_ETOPM_1 <= quantile(new$WEEKLY_AV_ETOPM_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_ETOPM_1 > quantile(new$WEEKLY_AV_ETOPM_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_ETOPM_1 <= quantile(new$WEEKLY_AV_ETOPM_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_ETOPM_1 > quantile(new$WEEKLY_AV_ETOPM_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_ETOPM_1 <= quantile(new$WEEKLY_AV_ETOPM_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_ETOPM_1 > quantile(new$WEEKLY_AV_ETOPM_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_ETOPM_1 <= quantile(new$WEEKLY_AV_ETOPM_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_ETOPM_1 > quantile(new$WEEKLY_AV_ETOPM_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_LEAFW_1 = 
           case_when(WEEKLY_AV_LEAFW_1 <= quantile(new$WEEKLY_AV_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_LEAFW_1 > quantile(new$WEEKLY_AV_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_LEAFW_1 <= quantile(new$WEEKLY_AV_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_LEAFW_1 > quantile(new$WEEKLY_AV_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_LEAFW_1 <= quantile(new$WEEKLY_AV_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_LEAFW_1 > quantile(new$WEEKLY_AV_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_LEAFW_1 <= quantile(new$WEEKLY_AV_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_LEAFW_1 > quantile(new$WEEKLY_AV_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_LEAFW_1 <= quantile(new$WEEKLY_AV_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_LEAFW_1 > quantile(new$WEEKLY_AV_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_LEAFW_1 <= quantile(new$WEEKLY_AV_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_LEAFW_1 > quantile(new$WEEKLY_AV_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_LEAFW_1 <= quantile(new$WEEKLY_AV_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_LEAFW_1 > quantile(new$WEEKLY_AV_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_LEAFW_1 <= quantile(new$WEEKLY_AV_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_LEAFW_1 > quantile(new$WEEKLY_AV_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_LEAFW_1 <= quantile(new$WEEKLY_AV_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_LEAFW_1 > quantile(new$WEEKLY_AV_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_CUM_LEAFW_1 = 
           case_when(WEEKLY_CUM_LEAFW_1 <= quantile(new$WEEKLY_CUM_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_CUM_LEAFW_1 > quantile(new$WEEKLY_CUM_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_CUM_LEAFW_1 <= quantile(new$WEEKLY_CUM_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_CUM_LEAFW_1 > quantile(new$WEEKLY_CUM_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_CUM_LEAFW_1 <= quantile(new$WEEKLY_CUM_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_CUM_LEAFW_1 > quantile(new$WEEKLY_CUM_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_CUM_LEAFW_1 <= quantile(new$WEEKLY_CUM_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_CUM_LEAFW_1 > quantile(new$WEEKLY_CUM_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_CUM_LEAFW_1 <= quantile(new$WEEKLY_CUM_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_CUM_LEAFW_1 > quantile(new$WEEKLY_CUM_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_CUM_LEAFW_1 <= quantile(new$WEEKLY_CUM_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_CUM_LEAFW_1 > quantile(new$WEEKLY_CUM_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_CUM_LEAFW_1 <= quantile(new$WEEKLY_CUM_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_CUM_LEAFW_1 > quantile(new$WEEKLY_CUM_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_CUM_LEAFW_1 <= quantile(new$WEEKLY_CUM_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_CUM_LEAFW_1 > quantile(new$WEEKLY_CUM_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_CUM_LEAFW_1 <= quantile(new$WEEKLY_CUM_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_CUM_LEAFW_1 > quantile(new$WEEKLY_CUM_LEAFW_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_CUM_PREC_1 = 
           case_when(WEEKLY_CUM_PREC_1 <= quantile(new$WEEKLY_CUM_PREC_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_CUM_PREC_1 > quantile(new$WEEKLY_CUM_PREC_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_CUM_PREC_1 <= quantile(new$WEEKLY_CUM_PREC_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_CUM_PREC_1 > quantile(new$WEEKLY_CUM_PREC_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_CUM_PREC_1 <= quantile(new$WEEKLY_CUM_PREC_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_CUM_PREC_1 > quantile(new$WEEKLY_CUM_PREC_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_CUM_PREC_1 <= quantile(new$WEEKLY_CUM_PREC_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_CUM_PREC_1 > quantile(new$WEEKLY_CUM_PREC_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_CUM_PREC_1 <= quantile(new$WEEKLY_CUM_PREC_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_CUM_PREC_1 > quantile(new$WEEKLY_CUM_PREC_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_CUM_PREC_1 <= quantile(new$WEEKLY_CUM_PREC_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_CUM_PREC_1 > quantile(new$WEEKLY_CUM_PREC_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_CUM_PREC_1 <= quantile(new$WEEKLY_CUM_PREC_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_CUM_PREC_1 > quantile(new$WEEKLY_CUM_PREC_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_CUM_PREC_1 <= quantile(new$WEEKLY_CUM_PREC_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_CUM_PREC_1 > quantile(new$WEEKLY_CUM_PREC_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_CUM_PREC_1 <= quantile(new$WEEKLY_CUM_PREC_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_CUM_PREC_1 > quantile(new$WEEKLY_CUM_PREC_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_MIN_TMIN_1 = 
           case_when(WEEKLY_MIN_TMIN_1 <= quantile(new$WEEKLY_MIN_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_MIN_TMIN_1 > quantile(new$WEEKLY_MIN_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_MIN_TMIN_1 <= quantile(new$WEEKLY_MIN_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_MIN_TMIN_1 > quantile(new$WEEKLY_MIN_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_MIN_TMIN_1 <= quantile(new$WEEKLY_MIN_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_MIN_TMIN_1 > quantile(new$WEEKLY_MIN_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_MIN_TMIN_1 <= quantile(new$WEEKLY_MIN_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_MIN_TMIN_1 > quantile(new$WEEKLY_MIN_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_MIN_TMIN_1 <= quantile(new$WEEKLY_MIN_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_MIN_TMIN_1 > quantile(new$WEEKLY_MIN_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_MIN_TMIN_1 <= quantile(new$WEEKLY_MIN_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_MIN_TMIN_1 > quantile(new$WEEKLY_MIN_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_MIN_TMIN_1 <= quantile(new$WEEKLY_MIN_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_MIN_TMIN_1 > quantile(new$WEEKLY_MIN_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_MIN_TMIN_1 <= quantile(new$WEEKLY_MIN_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_MIN_TMIN_1 > quantile(new$WEEKLY_MIN_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_MIN_TMIN_1 <= quantile(new$WEEKLY_MIN_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_MIN_TMIN_1 > quantile(new$WEEKLY_MIN_TMIN_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_MAX_TMAX_1 = 
           case_when(WEEKLY_MAX_TMAX_1 <= quantile(new$WEEKLY_MAX_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_MAX_TMAX_1 > quantile(new$WEEKLY_MAX_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_MAX_TMAX_1 <= quantile(new$WEEKLY_MAX_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_MAX_TMAX_1 > quantile(new$WEEKLY_MAX_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_MAX_TMAX_1 <= quantile(new$WEEKLY_MAX_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_MAX_TMAX_1 > quantile(new$WEEKLY_MAX_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_MAX_TMAX_1 <= quantile(new$WEEKLY_MAX_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_MAX_TMAX_1 > quantile(new$WEEKLY_MAX_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_MAX_TMAX_1 <= quantile(new$WEEKLY_MAX_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_MAX_TMAX_1 > quantile(new$WEEKLY_MAX_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_MAX_TMAX_1 <= quantile(new$WEEKLY_MAX_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_MAX_TMAX_1 > quantile(new$WEEKLY_MAX_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_MAX_TMAX_1 <= quantile(new$WEEKLY_MAX_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_MAX_TMAX_1 > quantile(new$WEEKLY_MAX_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_MAX_TMAX_1 <= quantile(new$WEEKLY_MAX_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_MAX_TMAX_1 > quantile(new$WEEKLY_MAX_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_MAX_TMAX_1 <= quantile(new$WEEKLY_MAX_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_MAX_TMAX_1 > quantile(new$WEEKLY_MAX_TMAX_1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         
         WEEKLY_AV_TMIN_2 = 
           case_when(WEEKLY_AV_TMIN_2 <= quantile(new$WEEKLY_AV_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_TMIN_2 > quantile(new$WEEKLY_AV_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_TMIN_2 <= quantile(new$WEEKLY_AV_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_TMIN_2 > quantile(new$WEEKLY_AV_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_TMIN_2 <= quantile(new$WEEKLY_AV_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_TMIN_2 > quantile(new$WEEKLY_AV_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_TMIN_2 <= quantile(new$WEEKLY_AV_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_TMIN_2 > quantile(new$WEEKLY_AV_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_TMIN_2 <= quantile(new$WEEKLY_AV_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_TMIN_2 > quantile(new$WEEKLY_AV_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_TMIN_2 <= quantile(new$WEEKLY_AV_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_TMIN_2 > quantile(new$WEEKLY_AV_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_TMIN_2 <= quantile(new$WEEKLY_AV_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_TMIN_2 > quantile(new$WEEKLY_AV_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_TMIN_2 <= quantile(new$WEEKLY_AV_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_TMIN_2 > quantile(new$WEEKLY_AV_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_TMIN_2 <= quantile(new$WEEKLY_AV_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_TMIN_2 > quantile(new$WEEKLY_AV_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_TMAX_2 = 
           case_when(WEEKLY_AV_TMAX_2 <= quantile(new$WEEKLY_AV_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_TMAX_2 > quantile(new$WEEKLY_AV_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_TMAX_2 <= quantile(new$WEEKLY_AV_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_TMAX_2 > quantile(new$WEEKLY_AV_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_TMAX_2 <= quantile(new$WEEKLY_AV_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_TMAX_2 > quantile(new$WEEKLY_AV_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_TMAX_2 <= quantile(new$WEEKLY_AV_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_TMAX_2 > quantile(new$WEEKLY_AV_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_TMAX_2 <= quantile(new$WEEKLY_AV_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_TMAX_2 > quantile(new$WEEKLY_AV_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_TMAX_2 <= quantile(new$WEEKLY_AV_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_TMAX_2 > quantile(new$WEEKLY_AV_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_TMAX_2 <= quantile(new$WEEKLY_AV_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_TMAX_2 > quantile(new$WEEKLY_AV_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_TMAX_2 <= quantile(new$WEEKLY_AV_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_TMAX_2 > quantile(new$WEEKLY_AV_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_TMAX_2 <= quantile(new$WEEKLY_AV_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_TMAX_2 > quantile(new$WEEKLY_AV_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_TAVG_2 = 
           case_when(WEEKLY_AV_TAVG_2 <= quantile(new$WEEKLY_AV_TAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_TAVG_2 > quantile(new$WEEKLY_AV_TAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_TAVG_2 <= quantile(new$WEEKLY_AV_TAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_TAVG_2 > quantile(new$WEEKLY_AV_TAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_TAVG_2 <= quantile(new$WEEKLY_AV_TAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_TAVG_2 > quantile(new$WEEKLY_AV_TAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_TAVG_2 <= quantile(new$WEEKLY_AV_TAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_TAVG_2 > quantile(new$WEEKLY_AV_TAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_TAVG_2 <= quantile(new$WEEKLY_AV_TAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_TAVG_2 > quantile(new$WEEKLY_AV_TAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_TAVG_2 <= quantile(new$WEEKLY_AV_TAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_TAVG_2 > quantile(new$WEEKLY_AV_TAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_TAVG_2 <= quantile(new$WEEKLY_AV_TAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_TAVG_2 > quantile(new$WEEKLY_AV_TAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_TAVG_2 <= quantile(new$WEEKLY_AV_TAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_TAVG_2 > quantile(new$WEEKLY_AV_TAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_TAVG_2 <= quantile(new$WEEKLY_AV_TAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_TAVG_2 > quantile(new$WEEKLY_AV_TAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_RHAVG_2 = 
           case_when(WEEKLY_AV_RHAVG_2 <= quantile(new$WEEKLY_AV_RHAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_RHAVG_2 > quantile(new$WEEKLY_AV_RHAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_RHAVG_2 <= quantile(new$WEEKLY_AV_RHAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_RHAVG_2 > quantile(new$WEEKLY_AV_RHAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_RHAVG_2 <= quantile(new$WEEKLY_AV_RHAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_RHAVG_2 > quantile(new$WEEKLY_AV_RHAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_RHAVG_2 <= quantile(new$WEEKLY_AV_RHAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_RHAVG_2 > quantile(new$WEEKLY_AV_RHAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_RHAVG_2 <= quantile(new$WEEKLY_AV_RHAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_RHAVG_2 > quantile(new$WEEKLY_AV_RHAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_RHAVG_2 <= quantile(new$WEEKLY_AV_RHAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_RHAVG_2 > quantile(new$WEEKLY_AV_RHAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_RHAVG_2 <= quantile(new$WEEKLY_AV_RHAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_RHAVG_2 > quantile(new$WEEKLY_AV_RHAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_RHAVG_2 <= quantile(new$WEEKLY_AV_RHAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_RHAVG_2 > quantile(new$WEEKLY_AV_RHAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_RHAVG_2 <= quantile(new$WEEKLY_AV_RHAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_RHAVG_2 > quantile(new$WEEKLY_AV_RHAVG_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_RAD_2 = 
           case_when(WEEKLY_AV_RAD_2 <= quantile(new$WEEKLY_AV_RAD_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_RAD_2 > quantile(new$WEEKLY_AV_RAD_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_RAD_2 <= quantile(new$WEEKLY_AV_RAD_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_RAD_2 > quantile(new$WEEKLY_AV_RAD_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_RAD_2 <= quantile(new$WEEKLY_AV_RAD_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_RAD_2 > quantile(new$WEEKLY_AV_RAD_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_RAD_2 <= quantile(new$WEEKLY_AV_RAD_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_RAD_2 > quantile(new$WEEKLY_AV_RAD_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_RAD_2 <= quantile(new$WEEKLY_AV_RAD_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_RAD_2 > quantile(new$WEEKLY_AV_RAD_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_RAD_2 <= quantile(new$WEEKLY_AV_RAD_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_RAD_2 > quantile(new$WEEKLY_AV_RAD_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_RAD_2 <= quantile(new$WEEKLY_AV_RAD_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_RAD_2 > quantile(new$WEEKLY_AV_RAD_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_RAD_2 <= quantile(new$WEEKLY_AV_RAD_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_RAD_2 > quantile(new$WEEKLY_AV_RAD_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_RAD_2 <= quantile(new$WEEKLY_AV_RAD_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_RAD_2 > quantile(new$WEEKLY_AV_RAD_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_ETOPM_2 = 
           case_when(WEEKLY_AV_ETOPM_2 <= quantile(new$WEEKLY_AV_ETOPM_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_ETOPM_2 > quantile(new$WEEKLY_AV_ETOPM_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_ETOPM_2 <= quantile(new$WEEKLY_AV_ETOPM_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_ETOPM_2 > quantile(new$WEEKLY_AV_ETOPM_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_ETOPM_2 <= quantile(new$WEEKLY_AV_ETOPM_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_ETOPM_2 > quantile(new$WEEKLY_AV_ETOPM_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_ETOPM_2 <= quantile(new$WEEKLY_AV_ETOPM_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_ETOPM_2 > quantile(new$WEEKLY_AV_ETOPM_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_ETOPM_2 <= quantile(new$WEEKLY_AV_ETOPM_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_ETOPM_2 > quantile(new$WEEKLY_AV_ETOPM_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_ETOPM_2 <= quantile(new$WEEKLY_AV_ETOPM_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_ETOPM_2 > quantile(new$WEEKLY_AV_ETOPM_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_ETOPM_2 <= quantile(new$WEEKLY_AV_ETOPM_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_ETOPM_2 > quantile(new$WEEKLY_AV_ETOPM_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_ETOPM_2 <= quantile(new$WEEKLY_AV_ETOPM_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_ETOPM_2 > quantile(new$WEEKLY_AV_ETOPM_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_ETOPM_2 <= quantile(new$WEEKLY_AV_ETOPM_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_ETOPM_2 > quantile(new$WEEKLY_AV_ETOPM_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_LEAFW_2 = 
           case_when(WEEKLY_AV_LEAFW_2 <= quantile(new$WEEKLY_AV_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_LEAFW_2 > quantile(new$WEEKLY_AV_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_LEAFW_2 <= quantile(new$WEEKLY_AV_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_LEAFW_2 > quantile(new$WEEKLY_AV_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_LEAFW_2 <= quantile(new$WEEKLY_AV_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_LEAFW_2 > quantile(new$WEEKLY_AV_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_LEAFW_2 <= quantile(new$WEEKLY_AV_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_LEAFW_2 > quantile(new$WEEKLY_AV_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_LEAFW_2 <= quantile(new$WEEKLY_AV_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_LEAFW_2 > quantile(new$WEEKLY_AV_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_LEAFW_2 <= quantile(new$WEEKLY_AV_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_LEAFW_2 > quantile(new$WEEKLY_AV_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_LEAFW_2 <= quantile(new$WEEKLY_AV_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_LEAFW_2 > quantile(new$WEEKLY_AV_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_LEAFW_2 <= quantile(new$WEEKLY_AV_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_LEAFW_2 > quantile(new$WEEKLY_AV_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_LEAFW_2 <= quantile(new$WEEKLY_AV_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_LEAFW_2 > quantile(new$WEEKLY_AV_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_CUM_LEAFW_2 = 
           case_when(WEEKLY_CUM_LEAFW_2 <= quantile(new$WEEKLY_CUM_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_CUM_LEAFW_2 > quantile(new$WEEKLY_CUM_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_CUM_LEAFW_2 <= quantile(new$WEEKLY_CUM_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_CUM_LEAFW_2 > quantile(new$WEEKLY_CUM_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_CUM_LEAFW_2 <= quantile(new$WEEKLY_CUM_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_CUM_LEAFW_2 > quantile(new$WEEKLY_CUM_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_CUM_LEAFW_2 <= quantile(new$WEEKLY_CUM_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_CUM_LEAFW_2 > quantile(new$WEEKLY_CUM_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_CUM_LEAFW_2 <= quantile(new$WEEKLY_CUM_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_CUM_LEAFW_2 > quantile(new$WEEKLY_CUM_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_CUM_LEAFW_2 <= quantile(new$WEEKLY_CUM_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_CUM_LEAFW_2 > quantile(new$WEEKLY_CUM_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_CUM_LEAFW_2 <= quantile(new$WEEKLY_CUM_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_CUM_LEAFW_2 > quantile(new$WEEKLY_CUM_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_CUM_LEAFW_2 <= quantile(new$WEEKLY_CUM_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_CUM_LEAFW_2 > quantile(new$WEEKLY_CUM_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_CUM_LEAFW_2 <= quantile(new$WEEKLY_CUM_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_CUM_LEAFW_2 > quantile(new$WEEKLY_CUM_LEAFW_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_CUM_PREC_2 = 
           case_when(WEEKLY_CUM_PREC_2 <= quantile(new$WEEKLY_CUM_PREC_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_CUM_PREC_2 > quantile(new$WEEKLY_CUM_PREC_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_CUM_PREC_2 <= quantile(new$WEEKLY_CUM_PREC_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_CUM_PREC_2 > quantile(new$WEEKLY_CUM_PREC_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_CUM_PREC_2 <= quantile(new$WEEKLY_CUM_PREC_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_CUM_PREC_2 > quantile(new$WEEKLY_CUM_PREC_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_CUM_PREC_2 <= quantile(new$WEEKLY_CUM_PREC_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_CUM_PREC_2 > quantile(new$WEEKLY_CUM_PREC_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_CUM_PREC_2 <= quantile(new$WEEKLY_CUM_PREC_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_CUM_PREC_2 > quantile(new$WEEKLY_CUM_PREC_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_CUM_PREC_2 <= quantile(new$WEEKLY_CUM_PREC_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_CUM_PREC_2 > quantile(new$WEEKLY_CUM_PREC_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_CUM_PREC_2 <= quantile(new$WEEKLY_CUM_PREC_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_CUM_PREC_2 > quantile(new$WEEKLY_CUM_PREC_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_CUM_PREC_2 <= quantile(new$WEEKLY_CUM_PREC_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_CUM_PREC_2 > quantile(new$WEEKLY_CUM_PREC_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_CUM_PREC_2 <= quantile(new$WEEKLY_CUM_PREC_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_CUM_PREC_2 > quantile(new$WEEKLY_CUM_PREC_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_MIN_TMIN_2 = 
           case_when(WEEKLY_MIN_TMIN_2 <= quantile(new$WEEKLY_MIN_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_MIN_TMIN_2 > quantile(new$WEEKLY_MIN_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_MIN_TMIN_2 <= quantile(new$WEEKLY_MIN_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_MIN_TMIN_2 > quantile(new$WEEKLY_MIN_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_MIN_TMIN_2 <= quantile(new$WEEKLY_MIN_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_MIN_TMIN_2 > quantile(new$WEEKLY_MIN_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_MIN_TMIN_2 <= quantile(new$WEEKLY_MIN_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_MIN_TMIN_2 > quantile(new$WEEKLY_MIN_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_MIN_TMIN_2 <= quantile(new$WEEKLY_MIN_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_MIN_TMIN_2 > quantile(new$WEEKLY_MIN_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_MIN_TMIN_2 <= quantile(new$WEEKLY_MIN_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_MIN_TMIN_2 > quantile(new$WEEKLY_MIN_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_MIN_TMIN_2 <= quantile(new$WEEKLY_MIN_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_MIN_TMIN_2 > quantile(new$WEEKLY_MIN_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_MIN_TMIN_2 <= quantile(new$WEEKLY_MIN_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_MIN_TMIN_2 > quantile(new$WEEKLY_MIN_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_MIN_TMIN_2 <= quantile(new$WEEKLY_MIN_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_MIN_TMIN_2 > quantile(new$WEEKLY_MIN_TMIN_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_MAX_TMAX_2 = 
           case_when(WEEKLY_MAX_TMAX_2 <= quantile(new$WEEKLY_MAX_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_MAX_TMAX_2 > quantile(new$WEEKLY_MAX_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_MAX_TMAX_2 <= quantile(new$WEEKLY_MAX_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_MAX_TMAX_2 > quantile(new$WEEKLY_MAX_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_MAX_TMAX_2 <= quantile(new$WEEKLY_MAX_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_MAX_TMAX_2 > quantile(new$WEEKLY_MAX_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_MAX_TMAX_2 <= quantile(new$WEEKLY_MAX_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_MAX_TMAX_2 > quantile(new$WEEKLY_MAX_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_MAX_TMAX_2 <= quantile(new$WEEKLY_MAX_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_MAX_TMAX_2 > quantile(new$WEEKLY_MAX_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_MAX_TMAX_2 <= quantile(new$WEEKLY_MAX_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_MAX_TMAX_2 > quantile(new$WEEKLY_MAX_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_MAX_TMAX_2 <= quantile(new$WEEKLY_MAX_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_MAX_TMAX_2 > quantile(new$WEEKLY_MAX_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_MAX_TMAX_2 <= quantile(new$WEEKLY_MAX_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_MAX_TMAX_2 > quantile(new$WEEKLY_MAX_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_MAX_TMAX_2 <= quantile(new$WEEKLY_MAX_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_MAX_TMAX_2 > quantile(new$WEEKLY_MAX_TMAX_2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         
         WEEKLY_AV_TMIN_3 = 
           case_when(WEEKLY_AV_TMIN_3 <= quantile(new$WEEKLY_AV_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_TMIN_3 > quantile(new$WEEKLY_AV_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_TMIN_3 <= quantile(new$WEEKLY_AV_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_TMIN_3 > quantile(new$WEEKLY_AV_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_TMIN_3 <= quantile(new$WEEKLY_AV_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_TMIN_3 > quantile(new$WEEKLY_AV_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_TMIN_3 <= quantile(new$WEEKLY_AV_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_TMIN_3 > quantile(new$WEEKLY_AV_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_TMIN_3 <= quantile(new$WEEKLY_AV_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_TMIN_3 > quantile(new$WEEKLY_AV_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_TMIN_3 <= quantile(new$WEEKLY_AV_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_TMIN_3 > quantile(new$WEEKLY_AV_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_TMIN_3 <= quantile(new$WEEKLY_AV_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_TMIN_3 > quantile(new$WEEKLY_AV_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_TMIN_3 <= quantile(new$WEEKLY_AV_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_TMIN_3 > quantile(new$WEEKLY_AV_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_TMIN_3 <= quantile(new$WEEKLY_AV_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_TMIN_3 > quantile(new$WEEKLY_AV_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_TMAX_3 = 
           case_when(WEEKLY_AV_TMAX_3 <= quantile(new$WEEKLY_AV_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_TMAX_3 > quantile(new$WEEKLY_AV_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_TMAX_3 <= quantile(new$WEEKLY_AV_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_TMAX_3 > quantile(new$WEEKLY_AV_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_TMAX_3 <= quantile(new$WEEKLY_AV_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_TMAX_3 > quantile(new$WEEKLY_AV_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_TMAX_3 <= quantile(new$WEEKLY_AV_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_TMAX_3 > quantile(new$WEEKLY_AV_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_TMAX_3 <= quantile(new$WEEKLY_AV_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_TMAX_3 > quantile(new$WEEKLY_AV_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_TMAX_3 <= quantile(new$WEEKLY_AV_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_TMAX_3 > quantile(new$WEEKLY_AV_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_TMAX_3 <= quantile(new$WEEKLY_AV_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_TMAX_3 > quantile(new$WEEKLY_AV_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_TMAX_3 <= quantile(new$WEEKLY_AV_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_TMAX_3 > quantile(new$WEEKLY_AV_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_TMAX_3 <= quantile(new$WEEKLY_AV_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_TMAX_3 > quantile(new$WEEKLY_AV_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_TAVG_3 = 
           case_when(WEEKLY_AV_TAVG_3 <= quantile(new$WEEKLY_AV_TAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_TAVG_3 > quantile(new$WEEKLY_AV_TAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_TAVG_3 <= quantile(new$WEEKLY_AV_TAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_TAVG_3 > quantile(new$WEEKLY_AV_TAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_TAVG_3 <= quantile(new$WEEKLY_AV_TAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_TAVG_3 > quantile(new$WEEKLY_AV_TAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_TAVG_3 <= quantile(new$WEEKLY_AV_TAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_TAVG_3 > quantile(new$WEEKLY_AV_TAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_TAVG_3 <= quantile(new$WEEKLY_AV_TAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_TAVG_3 > quantile(new$WEEKLY_AV_TAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_TAVG_3 <= quantile(new$WEEKLY_AV_TAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_TAVG_3 > quantile(new$WEEKLY_AV_TAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_TAVG_3 <= quantile(new$WEEKLY_AV_TAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_TAVG_3 > quantile(new$WEEKLY_AV_TAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_TAVG_3 <= quantile(new$WEEKLY_AV_TAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_TAVG_3 > quantile(new$WEEKLY_AV_TAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_TAVG_3 <= quantile(new$WEEKLY_AV_TAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_TAVG_3 > quantile(new$WEEKLY_AV_TAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_RHAVG_3 = 
           case_when(WEEKLY_AV_RHAVG_3 <= quantile(new$WEEKLY_AV_RHAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_RHAVG_3 > quantile(new$WEEKLY_AV_RHAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_RHAVG_3 <= quantile(new$WEEKLY_AV_RHAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_RHAVG_3 > quantile(new$WEEKLY_AV_RHAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_RHAVG_3 <= quantile(new$WEEKLY_AV_RHAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_RHAVG_3 > quantile(new$WEEKLY_AV_RHAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_RHAVG_3 <= quantile(new$WEEKLY_AV_RHAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_RHAVG_3 > quantile(new$WEEKLY_AV_RHAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_RHAVG_3 <= quantile(new$WEEKLY_AV_RHAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_RHAVG_3 > quantile(new$WEEKLY_AV_RHAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_RHAVG_3 <= quantile(new$WEEKLY_AV_RHAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_RHAVG_3 > quantile(new$WEEKLY_AV_RHAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_RHAVG_3 <= quantile(new$WEEKLY_AV_RHAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_RHAVG_3 > quantile(new$WEEKLY_AV_RHAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_RHAVG_3 <= quantile(new$WEEKLY_AV_RHAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_RHAVG_3 > quantile(new$WEEKLY_AV_RHAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_RHAVG_3 <= quantile(new$WEEKLY_AV_RHAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_RHAVG_3 > quantile(new$WEEKLY_AV_RHAVG_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_RAD_3 = 
           case_when(WEEKLY_AV_RAD_3 <= quantile(new$WEEKLY_AV_RAD_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_RAD_3 > quantile(new$WEEKLY_AV_RAD_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_RAD_3 <= quantile(new$WEEKLY_AV_RAD_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_RAD_3 > quantile(new$WEEKLY_AV_RAD_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_RAD_3 <= quantile(new$WEEKLY_AV_RAD_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_RAD_3 > quantile(new$WEEKLY_AV_RAD_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_RAD_3 <= quantile(new$WEEKLY_AV_RAD_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_RAD_3 > quantile(new$WEEKLY_AV_RAD_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_RAD_3 <= quantile(new$WEEKLY_AV_RAD_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_RAD_3 > quantile(new$WEEKLY_AV_RAD_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_RAD_3 <= quantile(new$WEEKLY_AV_RAD_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_RAD_3 > quantile(new$WEEKLY_AV_RAD_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_RAD_3 <= quantile(new$WEEKLY_AV_RAD_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_RAD_3 > quantile(new$WEEKLY_AV_RAD_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_RAD_3 <= quantile(new$WEEKLY_AV_RAD_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_RAD_3 > quantile(new$WEEKLY_AV_RAD_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_RAD_3 <= quantile(new$WEEKLY_AV_RAD_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_RAD_3 > quantile(new$WEEKLY_AV_RAD_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_ETOPM_3 = 
           case_when(WEEKLY_AV_ETOPM_3 <= quantile(new$WEEKLY_AV_ETOPM_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_ETOPM_3 > quantile(new$WEEKLY_AV_ETOPM_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_ETOPM_3 <= quantile(new$WEEKLY_AV_ETOPM_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_ETOPM_3 > quantile(new$WEEKLY_AV_ETOPM_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_ETOPM_3 <= quantile(new$WEEKLY_AV_ETOPM_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_ETOPM_3 > quantile(new$WEEKLY_AV_ETOPM_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_ETOPM_3 <= quantile(new$WEEKLY_AV_ETOPM_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_ETOPM_3 > quantile(new$WEEKLY_AV_ETOPM_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_ETOPM_3 <= quantile(new$WEEKLY_AV_ETOPM_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_ETOPM_3 > quantile(new$WEEKLY_AV_ETOPM_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_ETOPM_3 <= quantile(new$WEEKLY_AV_ETOPM_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_ETOPM_3 > quantile(new$WEEKLY_AV_ETOPM_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_ETOPM_3 <= quantile(new$WEEKLY_AV_ETOPM_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_ETOPM_3 > quantile(new$WEEKLY_AV_ETOPM_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_ETOPM_3 <= quantile(new$WEEKLY_AV_ETOPM_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_ETOPM_3 > quantile(new$WEEKLY_AV_ETOPM_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_ETOPM_3 <= quantile(new$WEEKLY_AV_ETOPM_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_ETOPM_3 > quantile(new$WEEKLY_AV_ETOPM_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_LEAFW_3 = 
           case_when(WEEKLY_AV_LEAFW_3 <= quantile(new$WEEKLY_AV_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_LEAFW_3 > quantile(new$WEEKLY_AV_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_LEAFW_3 <= quantile(new$WEEKLY_AV_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_LEAFW_3 > quantile(new$WEEKLY_AV_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_LEAFW_3 <= quantile(new$WEEKLY_AV_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_LEAFW_3 > quantile(new$WEEKLY_AV_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_LEAFW_3 <= quantile(new$WEEKLY_AV_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_LEAFW_3 > quantile(new$WEEKLY_AV_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_LEAFW_3 <= quantile(new$WEEKLY_AV_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_LEAFW_3 > quantile(new$WEEKLY_AV_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_LEAFW_3 <= quantile(new$WEEKLY_AV_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_LEAFW_3 > quantile(new$WEEKLY_AV_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_LEAFW_3 <= quantile(new$WEEKLY_AV_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_LEAFW_3 > quantile(new$WEEKLY_AV_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_LEAFW_3 <= quantile(new$WEEKLY_AV_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_LEAFW_3 > quantile(new$WEEKLY_AV_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_LEAFW_3 <= quantile(new$WEEKLY_AV_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_LEAFW_3 > quantile(new$WEEKLY_AV_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_CUM_LEAFW_3 = 
           case_when(WEEKLY_CUM_LEAFW_3 <= quantile(new$WEEKLY_CUM_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_CUM_LEAFW_3 > quantile(new$WEEKLY_CUM_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_CUM_LEAFW_3 <= quantile(new$WEEKLY_CUM_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_CUM_LEAFW_3 > quantile(new$WEEKLY_CUM_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_CUM_LEAFW_3 <= quantile(new$WEEKLY_CUM_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_CUM_LEAFW_3 > quantile(new$WEEKLY_CUM_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_CUM_LEAFW_3 <= quantile(new$WEEKLY_CUM_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_CUM_LEAFW_3 > quantile(new$WEEKLY_CUM_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_CUM_LEAFW_3 <= quantile(new$WEEKLY_CUM_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_CUM_LEAFW_3 > quantile(new$WEEKLY_CUM_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_CUM_LEAFW_3 <= quantile(new$WEEKLY_CUM_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_CUM_LEAFW_3 > quantile(new$WEEKLY_CUM_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_CUM_LEAFW_3 <= quantile(new$WEEKLY_CUM_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_CUM_LEAFW_3 > quantile(new$WEEKLY_CUM_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_CUM_LEAFW_3 <= quantile(new$WEEKLY_CUM_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_CUM_LEAFW_3 > quantile(new$WEEKLY_CUM_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_CUM_LEAFW_3 <= quantile(new$WEEKLY_CUM_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_CUM_LEAFW_3 > quantile(new$WEEKLY_CUM_LEAFW_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_CUM_PREC_3 = 
           case_when(WEEKLY_CUM_PREC_3 <= quantile(new$WEEKLY_CUM_PREC_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_CUM_PREC_3 > quantile(new$WEEKLY_CUM_PREC_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_CUM_PREC_3 <= quantile(new$WEEKLY_CUM_PREC_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_CUM_PREC_3 > quantile(new$WEEKLY_CUM_PREC_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_CUM_PREC_3 <= quantile(new$WEEKLY_CUM_PREC_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_CUM_PREC_3 > quantile(new$WEEKLY_CUM_PREC_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_CUM_PREC_3 <= quantile(new$WEEKLY_CUM_PREC_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_CUM_PREC_3 > quantile(new$WEEKLY_CUM_PREC_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_CUM_PREC_3 <= quantile(new$WEEKLY_CUM_PREC_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_CUM_PREC_3 > quantile(new$WEEKLY_CUM_PREC_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_CUM_PREC_3 <= quantile(new$WEEKLY_CUM_PREC_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_CUM_PREC_3 > quantile(new$WEEKLY_CUM_PREC_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_CUM_PREC_3 <= quantile(new$WEEKLY_CUM_PREC_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_CUM_PREC_3 > quantile(new$WEEKLY_CUM_PREC_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_CUM_PREC_3 <= quantile(new$WEEKLY_CUM_PREC_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_CUM_PREC_3 > quantile(new$WEEKLY_CUM_PREC_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_CUM_PREC_3 <= quantile(new$WEEKLY_CUM_PREC_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_CUM_PREC_3 > quantile(new$WEEKLY_CUM_PREC_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_MIN_TMIN_3 = 
           case_when(WEEKLY_MIN_TMIN_3 <= quantile(new$WEEKLY_MIN_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_MIN_TMIN_3 > quantile(new$WEEKLY_MIN_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_MIN_TMIN_3 <= quantile(new$WEEKLY_MIN_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_MIN_TMIN_3 > quantile(new$WEEKLY_MIN_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_MIN_TMIN_3 <= quantile(new$WEEKLY_MIN_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_MIN_TMIN_3 > quantile(new$WEEKLY_MIN_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_MIN_TMIN_3 <= quantile(new$WEEKLY_MIN_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_MIN_TMIN_3 > quantile(new$WEEKLY_MIN_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_MIN_TMIN_3 <= quantile(new$WEEKLY_MIN_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_MIN_TMIN_3 > quantile(new$WEEKLY_MIN_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_MIN_TMIN_3 <= quantile(new$WEEKLY_MIN_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_MIN_TMIN_3 > quantile(new$WEEKLY_MIN_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_MIN_TMIN_3 <= quantile(new$WEEKLY_MIN_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_MIN_TMIN_3 > quantile(new$WEEKLY_MIN_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_MIN_TMIN_3 <= quantile(new$WEEKLY_MIN_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_MIN_TMIN_3 > quantile(new$WEEKLY_MIN_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_MIN_TMIN_3 <= quantile(new$WEEKLY_MIN_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_MIN_TMIN_3 > quantile(new$WEEKLY_MIN_TMIN_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_MAX_TMAX_3 = 
           case_when(WEEKLY_MAX_TMAX_3 <= quantile(new$WEEKLY_MAX_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_MAX_TMAX_3 > quantile(new$WEEKLY_MAX_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_MAX_TMAX_3 <= quantile(new$WEEKLY_MAX_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_MAX_TMAX_3 > quantile(new$WEEKLY_MAX_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_MAX_TMAX_3 <= quantile(new$WEEKLY_MAX_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_MAX_TMAX_3 > quantile(new$WEEKLY_MAX_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_MAX_TMAX_3 <= quantile(new$WEEKLY_MAX_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_MAX_TMAX_3 > quantile(new$WEEKLY_MAX_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_MAX_TMAX_3 <= quantile(new$WEEKLY_MAX_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_MAX_TMAX_3 > quantile(new$WEEKLY_MAX_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_MAX_TMAX_3 <= quantile(new$WEEKLY_MAX_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_MAX_TMAX_3 > quantile(new$WEEKLY_MAX_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_MAX_TMAX_3 <= quantile(new$WEEKLY_MAX_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_MAX_TMAX_3 > quantile(new$WEEKLY_MAX_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_MAX_TMAX_3 <= quantile(new$WEEKLY_MAX_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_MAX_TMAX_3 > quantile(new$WEEKLY_MAX_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_MAX_TMAX_3 <= quantile(new$WEEKLY_MAX_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_MAX_TMAX_3 > quantile(new$WEEKLY_MAX_TMAX_3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         
         WEEKLY_AV_TMIN_01 = 
           case_when(WEEKLY_AV_TMIN_01 <= quantile(new$WEEKLY_AV_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_TMIN_01 > quantile(new$WEEKLY_AV_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_TMIN_01 <= quantile(new$WEEKLY_AV_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_TMIN_01 > quantile(new$WEEKLY_AV_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_TMIN_01 <= quantile(new$WEEKLY_AV_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_TMIN_01 > quantile(new$WEEKLY_AV_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_TMIN_01 <= quantile(new$WEEKLY_AV_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_TMIN_01 > quantile(new$WEEKLY_AV_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_TMIN_01 <= quantile(new$WEEKLY_AV_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_TMIN_01 > quantile(new$WEEKLY_AV_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_TMIN_01 <= quantile(new$WEEKLY_AV_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_TMIN_01 > quantile(new$WEEKLY_AV_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_TMIN_01 <= quantile(new$WEEKLY_AV_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_TMIN_01 > quantile(new$WEEKLY_AV_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_TMIN_01 <= quantile(new$WEEKLY_AV_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_TMIN_01 > quantile(new$WEEKLY_AV_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_TMIN_01 <= quantile(new$WEEKLY_AV_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_TMIN_01 > quantile(new$WEEKLY_AV_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_TMAX_01 = 
           case_when(WEEKLY_AV_TMAX_01 <= quantile(new$WEEKLY_AV_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_TMAX_01 > quantile(new$WEEKLY_AV_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_TMAX_01 <= quantile(new$WEEKLY_AV_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_TMAX_01 > quantile(new$WEEKLY_AV_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_TMAX_01 <= quantile(new$WEEKLY_AV_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_TMAX_01 > quantile(new$WEEKLY_AV_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_TMAX_01 <= quantile(new$WEEKLY_AV_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_TMAX_01 > quantile(new$WEEKLY_AV_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_TMAX_01 <= quantile(new$WEEKLY_AV_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_TMAX_01 > quantile(new$WEEKLY_AV_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_TMAX_01 <= quantile(new$WEEKLY_AV_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_TMAX_01 > quantile(new$WEEKLY_AV_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_TMAX_01 <= quantile(new$WEEKLY_AV_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_TMAX_01 > quantile(new$WEEKLY_AV_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_TMAX_01 <= quantile(new$WEEKLY_AV_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_TMAX_01 > quantile(new$WEEKLY_AV_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_TMAX_01 <= quantile(new$WEEKLY_AV_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_TMAX_01 > quantile(new$WEEKLY_AV_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_TAVG_01 = 
           case_when(WEEKLY_AV_TAVG_01 <= quantile(new$WEEKLY_AV_TAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_TAVG_01 > quantile(new$WEEKLY_AV_TAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_TAVG_01 <= quantile(new$WEEKLY_AV_TAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_TAVG_01 > quantile(new$WEEKLY_AV_TAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_TAVG_01 <= quantile(new$WEEKLY_AV_TAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_TAVG_01 > quantile(new$WEEKLY_AV_TAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_TAVG_01 <= quantile(new$WEEKLY_AV_TAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_TAVG_01 > quantile(new$WEEKLY_AV_TAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_TAVG_01 <= quantile(new$WEEKLY_AV_TAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_TAVG_01 > quantile(new$WEEKLY_AV_TAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_TAVG_01 <= quantile(new$WEEKLY_AV_TAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_TAVG_01 > quantile(new$WEEKLY_AV_TAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_TAVG_01 <= quantile(new$WEEKLY_AV_TAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_TAVG_01 > quantile(new$WEEKLY_AV_TAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_TAVG_01 <= quantile(new$WEEKLY_AV_TAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_TAVG_01 > quantile(new$WEEKLY_AV_TAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_TAVG_01 <= quantile(new$WEEKLY_AV_TAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_TAVG_01 > quantile(new$WEEKLY_AV_TAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_RHAVG_01 = 
           case_when(WEEKLY_AV_RHAVG_01 <= quantile(new$WEEKLY_AV_RHAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_RHAVG_01 > quantile(new$WEEKLY_AV_RHAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_RHAVG_01 <= quantile(new$WEEKLY_AV_RHAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_RHAVG_01 > quantile(new$WEEKLY_AV_RHAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_RHAVG_01 <= quantile(new$WEEKLY_AV_RHAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_RHAVG_01 > quantile(new$WEEKLY_AV_RHAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_RHAVG_01 <= quantile(new$WEEKLY_AV_RHAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_RHAVG_01 > quantile(new$WEEKLY_AV_RHAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_RHAVG_01 <= quantile(new$WEEKLY_AV_RHAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_RHAVG_01 > quantile(new$WEEKLY_AV_RHAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_RHAVG_01 <= quantile(new$WEEKLY_AV_RHAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_RHAVG_01 > quantile(new$WEEKLY_AV_RHAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_RHAVG_01 <= quantile(new$WEEKLY_AV_RHAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_RHAVG_01 > quantile(new$WEEKLY_AV_RHAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_RHAVG_01 <= quantile(new$WEEKLY_AV_RHAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_RHAVG_01 > quantile(new$WEEKLY_AV_RHAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_RHAVG_01 <= quantile(new$WEEKLY_AV_RHAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_RHAVG_01 > quantile(new$WEEKLY_AV_RHAVG_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_RAD_01 = 
           case_when(WEEKLY_AV_RAD_01 <= quantile(new$WEEKLY_AV_RAD_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_RAD_01 > quantile(new$WEEKLY_AV_RAD_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_RAD_01 <= quantile(new$WEEKLY_AV_RAD_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_RAD_01 > quantile(new$WEEKLY_AV_RAD_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_RAD_01 <= quantile(new$WEEKLY_AV_RAD_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_RAD_01 > quantile(new$WEEKLY_AV_RAD_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_RAD_01 <= quantile(new$WEEKLY_AV_RAD_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_RAD_01 > quantile(new$WEEKLY_AV_RAD_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_RAD_01 <= quantile(new$WEEKLY_AV_RAD_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_RAD_01 > quantile(new$WEEKLY_AV_RAD_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_RAD_01 <= quantile(new$WEEKLY_AV_RAD_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_RAD_01 > quantile(new$WEEKLY_AV_RAD_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_RAD_01 <= quantile(new$WEEKLY_AV_RAD_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_RAD_01 > quantile(new$WEEKLY_AV_RAD_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_RAD_01 <= quantile(new$WEEKLY_AV_RAD_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_RAD_01 > quantile(new$WEEKLY_AV_RAD_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_RAD_01 <= quantile(new$WEEKLY_AV_RAD_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_RAD_01 > quantile(new$WEEKLY_AV_RAD_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_ETOPM_01 = 
           case_when(WEEKLY_AV_ETOPM_01 <= quantile(new$WEEKLY_AV_ETOPM_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_ETOPM_01 > quantile(new$WEEKLY_AV_ETOPM_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_ETOPM_01 <= quantile(new$WEEKLY_AV_ETOPM_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_ETOPM_01 > quantile(new$WEEKLY_AV_ETOPM_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_ETOPM_01 <= quantile(new$WEEKLY_AV_ETOPM_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_ETOPM_01 > quantile(new$WEEKLY_AV_ETOPM_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_ETOPM_01 <= quantile(new$WEEKLY_AV_ETOPM_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_ETOPM_01 > quantile(new$WEEKLY_AV_ETOPM_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_ETOPM_01 <= quantile(new$WEEKLY_AV_ETOPM_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_ETOPM_01 > quantile(new$WEEKLY_AV_ETOPM_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_ETOPM_01 <= quantile(new$WEEKLY_AV_ETOPM_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_ETOPM_01 > quantile(new$WEEKLY_AV_ETOPM_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_ETOPM_01 <= quantile(new$WEEKLY_AV_ETOPM_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_ETOPM_01 > quantile(new$WEEKLY_AV_ETOPM_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_ETOPM_01 <= quantile(new$WEEKLY_AV_ETOPM_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_ETOPM_01 > quantile(new$WEEKLY_AV_ETOPM_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_ETOPM_01 <= quantile(new$WEEKLY_AV_ETOPM_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_ETOPM_01 > quantile(new$WEEKLY_AV_ETOPM_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_LEAFW_01 = 
           case_when(WEEKLY_AV_LEAFW_01 <= quantile(new$WEEKLY_AV_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_LEAFW_01 > quantile(new$WEEKLY_AV_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_LEAFW_01 <= quantile(new$WEEKLY_AV_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_LEAFW_01 > quantile(new$WEEKLY_AV_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_LEAFW_01 <= quantile(new$WEEKLY_AV_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_LEAFW_01 > quantile(new$WEEKLY_AV_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_LEAFW_01 <= quantile(new$WEEKLY_AV_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_LEAFW_01 > quantile(new$WEEKLY_AV_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_LEAFW_01 <= quantile(new$WEEKLY_AV_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_LEAFW_01 > quantile(new$WEEKLY_AV_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_LEAFW_01 <= quantile(new$WEEKLY_AV_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_LEAFW_01 > quantile(new$WEEKLY_AV_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_LEAFW_01 <= quantile(new$WEEKLY_AV_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_LEAFW_01 > quantile(new$WEEKLY_AV_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_LEAFW_01 <= quantile(new$WEEKLY_AV_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_LEAFW_01 > quantile(new$WEEKLY_AV_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_LEAFW_01 <= quantile(new$WEEKLY_AV_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_LEAFW_01 > quantile(new$WEEKLY_AV_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_CUM_LEAFW_01 = 
           case_when(WEEKLY_CUM_LEAFW_01 <= quantile(new$WEEKLY_CUM_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_CUM_LEAFW_01 > quantile(new$WEEKLY_CUM_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_CUM_LEAFW_01 <= quantile(new$WEEKLY_CUM_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_CUM_LEAFW_01 > quantile(new$WEEKLY_CUM_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_CUM_LEAFW_01 <= quantile(new$WEEKLY_CUM_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_CUM_LEAFW_01 > quantile(new$WEEKLY_CUM_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_CUM_LEAFW_01 <= quantile(new$WEEKLY_CUM_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_CUM_LEAFW_01 > quantile(new$WEEKLY_CUM_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_CUM_LEAFW_01 <= quantile(new$WEEKLY_CUM_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_CUM_LEAFW_01 > quantile(new$WEEKLY_CUM_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_CUM_LEAFW_01 <= quantile(new$WEEKLY_CUM_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_CUM_LEAFW_01 > quantile(new$WEEKLY_CUM_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_CUM_LEAFW_01 <= quantile(new$WEEKLY_CUM_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_CUM_LEAFW_01 > quantile(new$WEEKLY_CUM_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_CUM_LEAFW_01 <= quantile(new$WEEKLY_CUM_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_CUM_LEAFW_01 > quantile(new$WEEKLY_CUM_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_CUM_LEAFW_01 <= quantile(new$WEEKLY_CUM_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_CUM_LEAFW_01 > quantile(new$WEEKLY_CUM_LEAFW_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_CUM_PREC_01 = 
           case_when(WEEKLY_CUM_PREC_01 <= quantile(new$WEEKLY_CUM_PREC_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_CUM_PREC_01 > quantile(new$WEEKLY_CUM_PREC_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_CUM_PREC_01 <= quantile(new$WEEKLY_CUM_PREC_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_CUM_PREC_01 > quantile(new$WEEKLY_CUM_PREC_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_CUM_PREC_01 <= quantile(new$WEEKLY_CUM_PREC_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_CUM_PREC_01 > quantile(new$WEEKLY_CUM_PREC_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_CUM_PREC_01 <= quantile(new$WEEKLY_CUM_PREC_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_CUM_PREC_01 > quantile(new$WEEKLY_CUM_PREC_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_CUM_PREC_01 <= quantile(new$WEEKLY_CUM_PREC_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_CUM_PREC_01 > quantile(new$WEEKLY_CUM_PREC_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_CUM_PREC_01 <= quantile(new$WEEKLY_CUM_PREC_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_CUM_PREC_01 > quantile(new$WEEKLY_CUM_PREC_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_CUM_PREC_01 <= quantile(new$WEEKLY_CUM_PREC_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_CUM_PREC_01 > quantile(new$WEEKLY_CUM_PREC_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_CUM_PREC_01 <= quantile(new$WEEKLY_CUM_PREC_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_CUM_PREC_01 > quantile(new$WEEKLY_CUM_PREC_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_CUM_PREC_01 <= quantile(new$WEEKLY_CUM_PREC_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_CUM_PREC_01 > quantile(new$WEEKLY_CUM_PREC_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_MIN_TMIN_01 = 
           case_when(WEEKLY_MIN_TMIN_01 <= quantile(new$WEEKLY_MIN_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_MIN_TMIN_01 > quantile(new$WEEKLY_MIN_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_MIN_TMIN_01 <= quantile(new$WEEKLY_MIN_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_MIN_TMIN_01 > quantile(new$WEEKLY_MIN_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_MIN_TMIN_01 <= quantile(new$WEEKLY_MIN_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_MIN_TMIN_01 > quantile(new$WEEKLY_MIN_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_MIN_TMIN_01 <= quantile(new$WEEKLY_MIN_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_MIN_TMIN_01 > quantile(new$WEEKLY_MIN_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_MIN_TMIN_01 <= quantile(new$WEEKLY_MIN_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_MIN_TMIN_01 > quantile(new$WEEKLY_MIN_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_MIN_TMIN_01 <= quantile(new$WEEKLY_MIN_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_MIN_TMIN_01 > quantile(new$WEEKLY_MIN_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_MIN_TMIN_01 <= quantile(new$WEEKLY_MIN_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_MIN_TMIN_01 > quantile(new$WEEKLY_MIN_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_MIN_TMIN_01 <= quantile(new$WEEKLY_MIN_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_MIN_TMIN_01 > quantile(new$WEEKLY_MIN_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_MIN_TMIN_01 <= quantile(new$WEEKLY_MIN_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_MIN_TMIN_01 > quantile(new$WEEKLY_MIN_TMIN_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_MAX_TMAX_01 = 
           case_when(WEEKLY_MAX_TMAX_01 <= quantile(new$WEEKLY_MAX_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_MAX_TMAX_01 > quantile(new$WEEKLY_MAX_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_MAX_TMAX_01 <= quantile(new$WEEKLY_MAX_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_MAX_TMAX_01 > quantile(new$WEEKLY_MAX_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_MAX_TMAX_01 <= quantile(new$WEEKLY_MAX_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_MAX_TMAX_01 > quantile(new$WEEKLY_MAX_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_MAX_TMAX_01 <= quantile(new$WEEKLY_MAX_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_MAX_TMAX_01 > quantile(new$WEEKLY_MAX_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_MAX_TMAX_01 <= quantile(new$WEEKLY_MAX_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_MAX_TMAX_01 > quantile(new$WEEKLY_MAX_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_MAX_TMAX_01 <= quantile(new$WEEKLY_MAX_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_MAX_TMAX_01 > quantile(new$WEEKLY_MAX_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_MAX_TMAX_01 <= quantile(new$WEEKLY_MAX_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_MAX_TMAX_01 > quantile(new$WEEKLY_MAX_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_MAX_TMAX_01 <= quantile(new$WEEKLY_MAX_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_MAX_TMAX_01 > quantile(new$WEEKLY_MAX_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_MAX_TMAX_01 <= quantile(new$WEEKLY_MAX_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_MAX_TMAX_01 > quantile(new$WEEKLY_MAX_TMAX_01, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         
         WEEKLY_AV_TMIN_02 = 
           case_when(WEEKLY_AV_TMIN_02 <= quantile(new$WEEKLY_AV_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_TMIN_02 > quantile(new$WEEKLY_AV_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_TMIN_02 <= quantile(new$WEEKLY_AV_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_TMIN_02 > quantile(new$WEEKLY_AV_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_TMIN_02 <= quantile(new$WEEKLY_AV_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_TMIN_02 > quantile(new$WEEKLY_AV_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_TMIN_02 <= quantile(new$WEEKLY_AV_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_TMIN_02 > quantile(new$WEEKLY_AV_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_TMIN_02 <= quantile(new$WEEKLY_AV_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_TMIN_02 > quantile(new$WEEKLY_AV_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_TMIN_02 <= quantile(new$WEEKLY_AV_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_TMIN_02 > quantile(new$WEEKLY_AV_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_TMIN_02 <= quantile(new$WEEKLY_AV_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_TMIN_02 > quantile(new$WEEKLY_AV_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_TMIN_02 <= quantile(new$WEEKLY_AV_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_TMIN_02 > quantile(new$WEEKLY_AV_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_TMIN_02 <= quantile(new$WEEKLY_AV_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_TMIN_02 > quantile(new$WEEKLY_AV_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_TMAX_02 = 
           case_when(WEEKLY_AV_TMAX_02 <= quantile(new$WEEKLY_AV_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_TMAX_02 > quantile(new$WEEKLY_AV_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_TMAX_02 <= quantile(new$WEEKLY_AV_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_TMAX_02 > quantile(new$WEEKLY_AV_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_TMAX_02 <= quantile(new$WEEKLY_AV_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_TMAX_02 > quantile(new$WEEKLY_AV_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_TMAX_02 <= quantile(new$WEEKLY_AV_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_TMAX_02 > quantile(new$WEEKLY_AV_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_TMAX_02 <= quantile(new$WEEKLY_AV_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_TMAX_02 > quantile(new$WEEKLY_AV_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_TMAX_02 <= quantile(new$WEEKLY_AV_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_TMAX_02 > quantile(new$WEEKLY_AV_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_TMAX_02 <= quantile(new$WEEKLY_AV_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_TMAX_02 > quantile(new$WEEKLY_AV_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_TMAX_02 <= quantile(new$WEEKLY_AV_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_TMAX_02 > quantile(new$WEEKLY_AV_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_TMAX_02 <= quantile(new$WEEKLY_AV_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_TMAX_02 > quantile(new$WEEKLY_AV_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_TAVG_02 = 
           case_when(WEEKLY_AV_TAVG_02 <= quantile(new$WEEKLY_AV_TAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_TAVG_02 > quantile(new$WEEKLY_AV_TAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_TAVG_02 <= quantile(new$WEEKLY_AV_TAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_TAVG_02 > quantile(new$WEEKLY_AV_TAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_TAVG_02 <= quantile(new$WEEKLY_AV_TAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_TAVG_02 > quantile(new$WEEKLY_AV_TAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_TAVG_02 <= quantile(new$WEEKLY_AV_TAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_TAVG_02 > quantile(new$WEEKLY_AV_TAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_TAVG_02 <= quantile(new$WEEKLY_AV_TAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_TAVG_02 > quantile(new$WEEKLY_AV_TAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_TAVG_02 <= quantile(new$WEEKLY_AV_TAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_TAVG_02 > quantile(new$WEEKLY_AV_TAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_TAVG_02 <= quantile(new$WEEKLY_AV_TAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_TAVG_02 > quantile(new$WEEKLY_AV_TAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_TAVG_02 <= quantile(new$WEEKLY_AV_TAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_TAVG_02 > quantile(new$WEEKLY_AV_TAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_TAVG_02 <= quantile(new$WEEKLY_AV_TAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_TAVG_02 > quantile(new$WEEKLY_AV_TAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_RHAVG_02 = 
           case_when(WEEKLY_AV_RHAVG_02 <= quantile(new$WEEKLY_AV_RHAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_RHAVG_02 > quantile(new$WEEKLY_AV_RHAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_RHAVG_02 <= quantile(new$WEEKLY_AV_RHAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_RHAVG_02 > quantile(new$WEEKLY_AV_RHAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_RHAVG_02 <= quantile(new$WEEKLY_AV_RHAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_RHAVG_02 > quantile(new$WEEKLY_AV_RHAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_RHAVG_02 <= quantile(new$WEEKLY_AV_RHAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_RHAVG_02 > quantile(new$WEEKLY_AV_RHAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_RHAVG_02 <= quantile(new$WEEKLY_AV_RHAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_RHAVG_02 > quantile(new$WEEKLY_AV_RHAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_RHAVG_02 <= quantile(new$WEEKLY_AV_RHAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_RHAVG_02 > quantile(new$WEEKLY_AV_RHAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_RHAVG_02 <= quantile(new$WEEKLY_AV_RHAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_RHAVG_02 > quantile(new$WEEKLY_AV_RHAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_RHAVG_02 <= quantile(new$WEEKLY_AV_RHAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_RHAVG_02 > quantile(new$WEEKLY_AV_RHAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_RHAVG_02 <= quantile(new$WEEKLY_AV_RHAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_RHAVG_02 > quantile(new$WEEKLY_AV_RHAVG_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_RAD_02 = 
           case_when(WEEKLY_AV_RAD_02 <= quantile(new$WEEKLY_AV_RAD_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_RAD_02 > quantile(new$WEEKLY_AV_RAD_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_RAD_02 <= quantile(new$WEEKLY_AV_RAD_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_RAD_02 > quantile(new$WEEKLY_AV_RAD_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_RAD_02 <= quantile(new$WEEKLY_AV_RAD_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_RAD_02 > quantile(new$WEEKLY_AV_RAD_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_RAD_02 <= quantile(new$WEEKLY_AV_RAD_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_RAD_02 > quantile(new$WEEKLY_AV_RAD_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_RAD_02 <= quantile(new$WEEKLY_AV_RAD_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_RAD_02 > quantile(new$WEEKLY_AV_RAD_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_RAD_02 <= quantile(new$WEEKLY_AV_RAD_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_RAD_02 > quantile(new$WEEKLY_AV_RAD_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_RAD_02 <= quantile(new$WEEKLY_AV_RAD_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_RAD_02 > quantile(new$WEEKLY_AV_RAD_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_RAD_02 <= quantile(new$WEEKLY_AV_RAD_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_RAD_02 > quantile(new$WEEKLY_AV_RAD_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_RAD_02 <= quantile(new$WEEKLY_AV_RAD_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_RAD_02 > quantile(new$WEEKLY_AV_RAD_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_ETOPM_02 = 
           case_when(WEEKLY_AV_ETOPM_02 <= quantile(new$WEEKLY_AV_ETOPM_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_ETOPM_02 > quantile(new$WEEKLY_AV_ETOPM_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_ETOPM_02 <= quantile(new$WEEKLY_AV_ETOPM_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_ETOPM_02 > quantile(new$WEEKLY_AV_ETOPM_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_ETOPM_02 <= quantile(new$WEEKLY_AV_ETOPM_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_ETOPM_02 > quantile(new$WEEKLY_AV_ETOPM_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_ETOPM_02 <= quantile(new$WEEKLY_AV_ETOPM_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_ETOPM_02 > quantile(new$WEEKLY_AV_ETOPM_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_ETOPM_02 <= quantile(new$WEEKLY_AV_ETOPM_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_ETOPM_02 > quantile(new$WEEKLY_AV_ETOPM_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_ETOPM_02 <= quantile(new$WEEKLY_AV_ETOPM_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_ETOPM_02 > quantile(new$WEEKLY_AV_ETOPM_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_ETOPM_02 <= quantile(new$WEEKLY_AV_ETOPM_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_ETOPM_02 > quantile(new$WEEKLY_AV_ETOPM_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_ETOPM_02 <= quantile(new$WEEKLY_AV_ETOPM_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_ETOPM_02 > quantile(new$WEEKLY_AV_ETOPM_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_ETOPM_02 <= quantile(new$WEEKLY_AV_ETOPM_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_ETOPM_02 > quantile(new$WEEKLY_AV_ETOPM_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_LEAFW_02 = 
           case_when(WEEKLY_AV_LEAFW_02 <= quantile(new$WEEKLY_AV_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_LEAFW_02 > quantile(new$WEEKLY_AV_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_LEAFW_02 <= quantile(new$WEEKLY_AV_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_LEAFW_02 > quantile(new$WEEKLY_AV_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_LEAFW_02 <= quantile(new$WEEKLY_AV_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_LEAFW_02 > quantile(new$WEEKLY_AV_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_LEAFW_02 <= quantile(new$WEEKLY_AV_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_LEAFW_02 > quantile(new$WEEKLY_AV_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_LEAFW_02 <= quantile(new$WEEKLY_AV_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_LEAFW_02 > quantile(new$WEEKLY_AV_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_LEAFW_02 <= quantile(new$WEEKLY_AV_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_LEAFW_02 > quantile(new$WEEKLY_AV_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_LEAFW_02 <= quantile(new$WEEKLY_AV_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_LEAFW_02 > quantile(new$WEEKLY_AV_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_LEAFW_02 <= quantile(new$WEEKLY_AV_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_LEAFW_02 > quantile(new$WEEKLY_AV_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_LEAFW_02 <= quantile(new$WEEKLY_AV_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_LEAFW_02 > quantile(new$WEEKLY_AV_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_CUM_LEAFW_02 = 
           case_when(WEEKLY_CUM_LEAFW_02 <= quantile(new$WEEKLY_CUM_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_CUM_LEAFW_02 > quantile(new$WEEKLY_CUM_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_CUM_LEAFW_02 <= quantile(new$WEEKLY_CUM_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_CUM_LEAFW_02 > quantile(new$WEEKLY_CUM_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_CUM_LEAFW_02 <= quantile(new$WEEKLY_CUM_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_CUM_LEAFW_02 > quantile(new$WEEKLY_CUM_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_CUM_LEAFW_02 <= quantile(new$WEEKLY_CUM_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_CUM_LEAFW_02 > quantile(new$WEEKLY_CUM_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_CUM_LEAFW_02 <= quantile(new$WEEKLY_CUM_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_CUM_LEAFW_02 > quantile(new$WEEKLY_CUM_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_CUM_LEAFW_02 <= quantile(new$WEEKLY_CUM_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_CUM_LEAFW_02 > quantile(new$WEEKLY_CUM_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_CUM_LEAFW_02 <= quantile(new$WEEKLY_CUM_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_CUM_LEAFW_02 > quantile(new$WEEKLY_CUM_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_CUM_LEAFW_02 <= quantile(new$WEEKLY_CUM_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_CUM_LEAFW_02 > quantile(new$WEEKLY_CUM_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_CUM_LEAFW_02 <= quantile(new$WEEKLY_CUM_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_CUM_LEAFW_02 > quantile(new$WEEKLY_CUM_LEAFW_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_CUM_PREC_02 = 
           case_when(WEEKLY_CUM_PREC_02 <= quantile(new$WEEKLY_CUM_PREC_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_CUM_PREC_02 > quantile(new$WEEKLY_CUM_PREC_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_CUM_PREC_02 <= quantile(new$WEEKLY_CUM_PREC_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_CUM_PREC_02 > quantile(new$WEEKLY_CUM_PREC_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_CUM_PREC_02 <= quantile(new$WEEKLY_CUM_PREC_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_CUM_PREC_02 > quantile(new$WEEKLY_CUM_PREC_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_CUM_PREC_02 <= quantile(new$WEEKLY_CUM_PREC_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_CUM_PREC_02 > quantile(new$WEEKLY_CUM_PREC_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_CUM_PREC_02 <= quantile(new$WEEKLY_CUM_PREC_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_CUM_PREC_02 > quantile(new$WEEKLY_CUM_PREC_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_CUM_PREC_02 <= quantile(new$WEEKLY_CUM_PREC_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_CUM_PREC_02 > quantile(new$WEEKLY_CUM_PREC_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_CUM_PREC_02 <= quantile(new$WEEKLY_CUM_PREC_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_CUM_PREC_02 > quantile(new$WEEKLY_CUM_PREC_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_CUM_PREC_02 <= quantile(new$WEEKLY_CUM_PREC_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_CUM_PREC_02 > quantile(new$WEEKLY_CUM_PREC_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_CUM_PREC_02 <= quantile(new$WEEKLY_CUM_PREC_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_CUM_PREC_02 > quantile(new$WEEKLY_CUM_PREC_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_MIN_TMIN_02 = 
           case_when(WEEKLY_MIN_TMIN_02 <= quantile(new$WEEKLY_MIN_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_MIN_TMIN_02 > quantile(new$WEEKLY_MIN_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_MIN_TMIN_02 <= quantile(new$WEEKLY_MIN_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_MIN_TMIN_02 > quantile(new$WEEKLY_MIN_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_MIN_TMIN_02 <= quantile(new$WEEKLY_MIN_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_MIN_TMIN_02 > quantile(new$WEEKLY_MIN_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_MIN_TMIN_02 <= quantile(new$WEEKLY_MIN_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_MIN_TMIN_02 > quantile(new$WEEKLY_MIN_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_MIN_TMIN_02 <= quantile(new$WEEKLY_MIN_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_MIN_TMIN_02 > quantile(new$WEEKLY_MIN_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_MIN_TMIN_02 <= quantile(new$WEEKLY_MIN_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_MIN_TMIN_02 > quantile(new$WEEKLY_MIN_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_MIN_TMIN_02 <= quantile(new$WEEKLY_MIN_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_MIN_TMIN_02 > quantile(new$WEEKLY_MIN_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_MIN_TMIN_02 <= quantile(new$WEEKLY_MIN_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_MIN_TMIN_02 > quantile(new$WEEKLY_MIN_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_MIN_TMIN_02 <= quantile(new$WEEKLY_MIN_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_MIN_TMIN_02 > quantile(new$WEEKLY_MIN_TMIN_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_MAX_TMAX_02 = 
           case_when(WEEKLY_MAX_TMAX_02 <= quantile(new$WEEKLY_MAX_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_MAX_TMAX_02 > quantile(new$WEEKLY_MAX_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_MAX_TMAX_02 <= quantile(new$WEEKLY_MAX_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_MAX_TMAX_02 > quantile(new$WEEKLY_MAX_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_MAX_TMAX_02 <= quantile(new$WEEKLY_MAX_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_MAX_TMAX_02 > quantile(new$WEEKLY_MAX_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_MAX_TMAX_02 <= quantile(new$WEEKLY_MAX_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_MAX_TMAX_02 > quantile(new$WEEKLY_MAX_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_MAX_TMAX_02 <= quantile(new$WEEKLY_MAX_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_MAX_TMAX_02 > quantile(new$WEEKLY_MAX_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_MAX_TMAX_02 <= quantile(new$WEEKLY_MAX_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_MAX_TMAX_02 > quantile(new$WEEKLY_MAX_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_MAX_TMAX_02 <= quantile(new$WEEKLY_MAX_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_MAX_TMAX_02 > quantile(new$WEEKLY_MAX_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_MAX_TMAX_02 <= quantile(new$WEEKLY_MAX_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_MAX_TMAX_02 > quantile(new$WEEKLY_MAX_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_MAX_TMAX_02 <= quantile(new$WEEKLY_MAX_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_MAX_TMAX_02 > quantile(new$WEEKLY_MAX_TMAX_02, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         
         WEEKLY_AV_TMIN_03 = 
           case_when(WEEKLY_AV_TMIN_03 <= quantile(new$WEEKLY_AV_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_TMIN_03 > quantile(new$WEEKLY_AV_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_TMIN_03 <= quantile(new$WEEKLY_AV_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_TMIN_03 > quantile(new$WEEKLY_AV_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_TMIN_03 <= quantile(new$WEEKLY_AV_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_TMIN_03 > quantile(new$WEEKLY_AV_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_TMIN_03 <= quantile(new$WEEKLY_AV_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_TMIN_03 > quantile(new$WEEKLY_AV_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_TMIN_03 <= quantile(new$WEEKLY_AV_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_TMIN_03 > quantile(new$WEEKLY_AV_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_TMIN_03 <= quantile(new$WEEKLY_AV_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_TMIN_03 > quantile(new$WEEKLY_AV_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_TMIN_03 <= quantile(new$WEEKLY_AV_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_TMIN_03 > quantile(new$WEEKLY_AV_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_TMIN_03 <= quantile(new$WEEKLY_AV_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_TMIN_03 > quantile(new$WEEKLY_AV_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_TMIN_03 <= quantile(new$WEEKLY_AV_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_TMIN_03 > quantile(new$WEEKLY_AV_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_TMAX_03 = 
           case_when(WEEKLY_AV_TMAX_03 <= quantile(new$WEEKLY_AV_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_TMAX_03 > quantile(new$WEEKLY_AV_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_TMAX_03 <= quantile(new$WEEKLY_AV_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_TMAX_03 > quantile(new$WEEKLY_AV_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_TMAX_03 <= quantile(new$WEEKLY_AV_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_TMAX_03 > quantile(new$WEEKLY_AV_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_TMAX_03 <= quantile(new$WEEKLY_AV_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_TMAX_03 > quantile(new$WEEKLY_AV_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_TMAX_03 <= quantile(new$WEEKLY_AV_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_TMAX_03 > quantile(new$WEEKLY_AV_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_TMAX_03 <= quantile(new$WEEKLY_AV_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_TMAX_03 > quantile(new$WEEKLY_AV_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_TMAX_03 <= quantile(new$WEEKLY_AV_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_TMAX_03 > quantile(new$WEEKLY_AV_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_TMAX_03 <= quantile(new$WEEKLY_AV_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_TMAX_03 > quantile(new$WEEKLY_AV_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_TMAX_03 <= quantile(new$WEEKLY_AV_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_TMAX_03 > quantile(new$WEEKLY_AV_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_TAVG_03 = 
           case_when(WEEKLY_AV_TAVG_03 <= quantile(new$WEEKLY_AV_TAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_TAVG_03 > quantile(new$WEEKLY_AV_TAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_TAVG_03 <= quantile(new$WEEKLY_AV_TAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_TAVG_03 > quantile(new$WEEKLY_AV_TAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_TAVG_03 <= quantile(new$WEEKLY_AV_TAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_TAVG_03 > quantile(new$WEEKLY_AV_TAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_TAVG_03 <= quantile(new$WEEKLY_AV_TAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_TAVG_03 > quantile(new$WEEKLY_AV_TAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_TAVG_03 <= quantile(new$WEEKLY_AV_TAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_TAVG_03 > quantile(new$WEEKLY_AV_TAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_TAVG_03 <= quantile(new$WEEKLY_AV_TAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_TAVG_03 > quantile(new$WEEKLY_AV_TAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_TAVG_03 <= quantile(new$WEEKLY_AV_TAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_TAVG_03 > quantile(new$WEEKLY_AV_TAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_TAVG_03 <= quantile(new$WEEKLY_AV_TAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_TAVG_03 > quantile(new$WEEKLY_AV_TAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_TAVG_03 <= quantile(new$WEEKLY_AV_TAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_TAVG_03 > quantile(new$WEEKLY_AV_TAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_RHAVG_03 = 
           case_when(WEEKLY_AV_RHAVG_03 <= quantile(new$WEEKLY_AV_RHAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_RHAVG_03 > quantile(new$WEEKLY_AV_RHAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_RHAVG_03 <= quantile(new$WEEKLY_AV_RHAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_RHAVG_03 > quantile(new$WEEKLY_AV_RHAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_RHAVG_03 <= quantile(new$WEEKLY_AV_RHAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_RHAVG_03 > quantile(new$WEEKLY_AV_RHAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_RHAVG_03 <= quantile(new$WEEKLY_AV_RHAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_RHAVG_03 > quantile(new$WEEKLY_AV_RHAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_RHAVG_03 <= quantile(new$WEEKLY_AV_RHAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_RHAVG_03 > quantile(new$WEEKLY_AV_RHAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_RHAVG_03 <= quantile(new$WEEKLY_AV_RHAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_RHAVG_03 > quantile(new$WEEKLY_AV_RHAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_RHAVG_03 <= quantile(new$WEEKLY_AV_RHAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_RHAVG_03 > quantile(new$WEEKLY_AV_RHAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_RHAVG_03 <= quantile(new$WEEKLY_AV_RHAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_RHAVG_03 > quantile(new$WEEKLY_AV_RHAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_RHAVG_03 <= quantile(new$WEEKLY_AV_RHAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_RHAVG_03 > quantile(new$WEEKLY_AV_RHAVG_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_RAD_03 = 
           case_when(WEEKLY_AV_RAD_03 <= quantile(new$WEEKLY_AV_RAD_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_RAD_03 > quantile(new$WEEKLY_AV_RAD_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_RAD_03 <= quantile(new$WEEKLY_AV_RAD_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_RAD_03 > quantile(new$WEEKLY_AV_RAD_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_RAD_03 <= quantile(new$WEEKLY_AV_RAD_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_RAD_03 > quantile(new$WEEKLY_AV_RAD_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_RAD_03 <= quantile(new$WEEKLY_AV_RAD_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_RAD_03 > quantile(new$WEEKLY_AV_RAD_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_RAD_03 <= quantile(new$WEEKLY_AV_RAD_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_RAD_03 > quantile(new$WEEKLY_AV_RAD_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_RAD_03 <= quantile(new$WEEKLY_AV_RAD_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_RAD_03 > quantile(new$WEEKLY_AV_RAD_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_RAD_03 <= quantile(new$WEEKLY_AV_RAD_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_RAD_03 > quantile(new$WEEKLY_AV_RAD_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_RAD_03 <= quantile(new$WEEKLY_AV_RAD_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_RAD_03 > quantile(new$WEEKLY_AV_RAD_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_RAD_03 <= quantile(new$WEEKLY_AV_RAD_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_RAD_03 > quantile(new$WEEKLY_AV_RAD_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_ETOPM_03 = 
           case_when(WEEKLY_AV_ETOPM_03 <= quantile(new$WEEKLY_AV_ETOPM_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_ETOPM_03 > quantile(new$WEEKLY_AV_ETOPM_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_ETOPM_03 <= quantile(new$WEEKLY_AV_ETOPM_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_ETOPM_03 > quantile(new$WEEKLY_AV_ETOPM_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_ETOPM_03 <= quantile(new$WEEKLY_AV_ETOPM_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_ETOPM_03 > quantile(new$WEEKLY_AV_ETOPM_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_ETOPM_03 <= quantile(new$WEEKLY_AV_ETOPM_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_ETOPM_03 > quantile(new$WEEKLY_AV_ETOPM_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_ETOPM_03 <= quantile(new$WEEKLY_AV_ETOPM_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_ETOPM_03 > quantile(new$WEEKLY_AV_ETOPM_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_ETOPM_03 <= quantile(new$WEEKLY_AV_ETOPM_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_ETOPM_03 > quantile(new$WEEKLY_AV_ETOPM_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_ETOPM_03 <= quantile(new$WEEKLY_AV_ETOPM_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_ETOPM_03 > quantile(new$WEEKLY_AV_ETOPM_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_ETOPM_03 <= quantile(new$WEEKLY_AV_ETOPM_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_ETOPM_03 > quantile(new$WEEKLY_AV_ETOPM_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_ETOPM_03 <= quantile(new$WEEKLY_AV_ETOPM_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_ETOPM_03 > quantile(new$WEEKLY_AV_ETOPM_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_AV_LEAFW_03 = 
           case_when(WEEKLY_AV_LEAFW_03 <= quantile(new$WEEKLY_AV_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_AV_LEAFW_03 > quantile(new$WEEKLY_AV_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_AV_LEAFW_03 <= quantile(new$WEEKLY_AV_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_AV_LEAFW_03 > quantile(new$WEEKLY_AV_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_AV_LEAFW_03 <= quantile(new$WEEKLY_AV_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_AV_LEAFW_03 > quantile(new$WEEKLY_AV_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_AV_LEAFW_03 <= quantile(new$WEEKLY_AV_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_AV_LEAFW_03 > quantile(new$WEEKLY_AV_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_AV_LEAFW_03 <= quantile(new$WEEKLY_AV_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_AV_LEAFW_03 > quantile(new$WEEKLY_AV_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_AV_LEAFW_03 <= quantile(new$WEEKLY_AV_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_AV_LEAFW_03 > quantile(new$WEEKLY_AV_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_AV_LEAFW_03 <= quantile(new$WEEKLY_AV_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_AV_LEAFW_03 > quantile(new$WEEKLY_AV_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_AV_LEAFW_03 <= quantile(new$WEEKLY_AV_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_AV_LEAFW_03 > quantile(new$WEEKLY_AV_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_AV_LEAFW_03 <= quantile(new$WEEKLY_AV_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_AV_LEAFW_03 > quantile(new$WEEKLY_AV_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_CUM_LEAFW_03 = 
           case_when(WEEKLY_CUM_LEAFW_03 <= quantile(new$WEEKLY_CUM_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_CUM_LEAFW_03 > quantile(new$WEEKLY_CUM_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_CUM_LEAFW_03 <= quantile(new$WEEKLY_CUM_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_CUM_LEAFW_03 > quantile(new$WEEKLY_CUM_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_CUM_LEAFW_03 <= quantile(new$WEEKLY_CUM_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_CUM_LEAFW_03 > quantile(new$WEEKLY_CUM_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_CUM_LEAFW_03 <= quantile(new$WEEKLY_CUM_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_CUM_LEAFW_03 > quantile(new$WEEKLY_CUM_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_CUM_LEAFW_03 <= quantile(new$WEEKLY_CUM_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_CUM_LEAFW_03 > quantile(new$WEEKLY_CUM_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_CUM_LEAFW_03 <= quantile(new$WEEKLY_CUM_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_CUM_LEAFW_03 > quantile(new$WEEKLY_CUM_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_CUM_LEAFW_03 <= quantile(new$WEEKLY_CUM_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_CUM_LEAFW_03 > quantile(new$WEEKLY_CUM_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_CUM_LEAFW_03 <= quantile(new$WEEKLY_CUM_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_CUM_LEAFW_03 > quantile(new$WEEKLY_CUM_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_CUM_LEAFW_03 <= quantile(new$WEEKLY_CUM_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_CUM_LEAFW_03 > quantile(new$WEEKLY_CUM_LEAFW_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_CUM_PREC_03 = 
           case_when(WEEKLY_CUM_PREC_03 <= quantile(new$WEEKLY_CUM_PREC_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_CUM_PREC_03 > quantile(new$WEEKLY_CUM_PREC_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_CUM_PREC_03 <= quantile(new$WEEKLY_CUM_PREC_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_CUM_PREC_03 > quantile(new$WEEKLY_CUM_PREC_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_CUM_PREC_03 <= quantile(new$WEEKLY_CUM_PREC_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_CUM_PREC_03 > quantile(new$WEEKLY_CUM_PREC_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_CUM_PREC_03 <= quantile(new$WEEKLY_CUM_PREC_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_CUM_PREC_03 > quantile(new$WEEKLY_CUM_PREC_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_CUM_PREC_03 <= quantile(new$WEEKLY_CUM_PREC_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_CUM_PREC_03 > quantile(new$WEEKLY_CUM_PREC_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_CUM_PREC_03 <= quantile(new$WEEKLY_CUM_PREC_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_CUM_PREC_03 > quantile(new$WEEKLY_CUM_PREC_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_CUM_PREC_03 <= quantile(new$WEEKLY_CUM_PREC_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_CUM_PREC_03 > quantile(new$WEEKLY_CUM_PREC_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_CUM_PREC_03 <= quantile(new$WEEKLY_CUM_PREC_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_CUM_PREC_03 > quantile(new$WEEKLY_CUM_PREC_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_CUM_PREC_03 <= quantile(new$WEEKLY_CUM_PREC_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_CUM_PREC_03 > quantile(new$WEEKLY_CUM_PREC_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_MIN_TMIN_03 = 
           case_when(WEEKLY_MIN_TMIN_03 <= quantile(new$WEEKLY_MIN_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_MIN_TMIN_03 > quantile(new$WEEKLY_MIN_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_MIN_TMIN_03 <= quantile(new$WEEKLY_MIN_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_MIN_TMIN_03 > quantile(new$WEEKLY_MIN_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_MIN_TMIN_03 <= quantile(new$WEEKLY_MIN_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_MIN_TMIN_03 > quantile(new$WEEKLY_MIN_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_MIN_TMIN_03 <= quantile(new$WEEKLY_MIN_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_MIN_TMIN_03 > quantile(new$WEEKLY_MIN_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_MIN_TMIN_03 <= quantile(new$WEEKLY_MIN_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_MIN_TMIN_03 > quantile(new$WEEKLY_MIN_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_MIN_TMIN_03 <= quantile(new$WEEKLY_MIN_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_MIN_TMIN_03 > quantile(new$WEEKLY_MIN_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_MIN_TMIN_03 <= quantile(new$WEEKLY_MIN_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_MIN_TMIN_03 > quantile(new$WEEKLY_MIN_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_MIN_TMIN_03 <= quantile(new$WEEKLY_MIN_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_MIN_TMIN_03 > quantile(new$WEEKLY_MIN_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_MIN_TMIN_03 <= quantile(new$WEEKLY_MIN_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_MIN_TMIN_03 > quantile(new$WEEKLY_MIN_TMIN_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         WEEKLY_MAX_TMAX_03 = 
           case_when(WEEKLY_MAX_TMAX_03 <= quantile(new$WEEKLY_MAX_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     WEEKLY_MAX_TMAX_03 > quantile(new$WEEKLY_MAX_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       WEEKLY_MAX_TMAX_03 <= quantile(new$WEEKLY_MAX_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     WEEKLY_MAX_TMAX_03 > quantile(new$WEEKLY_MAX_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       WEEKLY_MAX_TMAX_03 <= quantile(new$WEEKLY_MAX_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     WEEKLY_MAX_TMAX_03 > quantile(new$WEEKLY_MAX_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       WEEKLY_MAX_TMAX_03 <= quantile(new$WEEKLY_MAX_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     WEEKLY_MAX_TMAX_03 > quantile(new$WEEKLY_MAX_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       WEEKLY_MAX_TMAX_03 <= quantile(new$WEEKLY_MAX_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     WEEKLY_MAX_TMAX_03 > quantile(new$WEEKLY_MAX_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       WEEKLY_MAX_TMAX_03 <= quantile(new$WEEKLY_MAX_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     WEEKLY_MAX_TMAX_03 > quantile(new$WEEKLY_MAX_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       WEEKLY_MAX_TMAX_03 <= quantile(new$WEEKLY_MAX_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     WEEKLY_MAX_TMAX_03 > quantile(new$WEEKLY_MAX_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       WEEKLY_MAX_TMAX_03 <= quantile(new$WEEKLY_MAX_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     WEEKLY_MAX_TMAX_03 > quantile(new$WEEKLY_MAX_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       WEEKLY_MAX_TMAX_03 <= quantile(new$WEEKLY_MAX_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     WEEKLY_MAX_TMAX_03 > quantile(new$WEEKLY_MAX_TMAX_03, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         CLC1 = 
           case_when(CLC1 <= quantile(new$CLC1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     CLC1 > quantile(new$CLC1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       CLC1 <= quantile(new$CLC1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     CLC1 > quantile(new$CLC1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       CLC1 <= quantile(new$CLC1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     CLC1 > quantile(new$CLC1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       CLC1 <= quantile(new$CLC1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     CLC1 > quantile(new$CLC1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       CLC1 <= quantile(new$CLC1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     CLC1 > quantile(new$CLC1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       CLC1 <= quantile(new$CLC1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     CLC1 > quantile(new$CLC1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       CLC1 <= quantile(new$CLC1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     CLC1 > quantile(new$CLC1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       CLC1 <= quantile(new$CLC1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     CLC1 > quantile(new$CLC1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       CLC1 <= quantile(new$CLC1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     CLC1 > quantile(new$CLC1, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         CLC2 = 
           case_when(CLC2 <= quantile(new$CLC2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     CLC2 > quantile(new$CLC2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       CLC2 <= quantile(new$CLC2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     CLC2 > quantile(new$CLC2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       CLC2 <= quantile(new$CLC2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     CLC2 > quantile(new$CLC2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       CLC2 <= quantile(new$CLC2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     CLC2 > quantile(new$CLC2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       CLC2 <= quantile(new$CLC2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     CLC2 > quantile(new$CLC2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       CLC2 <= quantile(new$CLC2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     CLC2 > quantile(new$CLC2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       CLC2 <= quantile(new$CLC2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     CLC2 > quantile(new$CLC2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       CLC2 <= quantile(new$CLC2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     CLC2 > quantile(new$CLC2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       CLC2 <= quantile(new$CLC2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     CLC2 > quantile(new$CLC2, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         CLC3 = 
           case_when(CLC3 <= quantile(new$CLC3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     CLC3 > quantile(new$CLC3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       CLC3 <= quantile(new$CLC3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     CLC3 > quantile(new$CLC3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       CLC3 <= quantile(new$CLC3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     CLC3 > quantile(new$CLC3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       CLC3 <= quantile(new$CLC3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     CLC3 > quantile(new$CLC3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       CLC3 <= quantile(new$CLC3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     CLC3 > quantile(new$CLC3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       CLC3 <= quantile(new$CLC3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     CLC3 > quantile(new$CLC3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       CLC3 <= quantile(new$CLC3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     CLC3 > quantile(new$CLC3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       CLC3 <= quantile(new$CLC3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     CLC3 > quantile(new$CLC3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       CLC3 <= quantile(new$CLC3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     CLC3 > quantile(new$CLC3, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         CLC4 = 
           case_when(CLC4 <= quantile(new$CLC4, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     CLC4 > quantile(new$CLC4, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       CLC4 <= quantile(new$CLC4, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     CLC4 > quantile(new$CLC4, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       CLC4 <= quantile(new$CLC4, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     CLC4 > quantile(new$CLC4, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       CLC4 <= quantile(new$CLC4, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     CLC4 > quantile(new$CLC4, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       CLC4 <= quantile(new$CLC4, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     CLC4 > quantile(new$CLC4, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       CLC4 <= quantile(new$CLC4, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     CLC4 > quantile(new$CLC4, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       CLC4 <= quantile(new$CLC4, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     CLC4 > quantile(new$CLC4, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       CLC4 <= quantile(new$CLC4, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     CLC4 > quantile(new$CLC4, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       CLC4 <= quantile(new$CLC4, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     CLC4 > quantile(new$CLC4, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"),
         CLC5 = 
           case_when(CLC5 <= quantile(new$CLC5, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] ~ "q10",
                     CLC5 > quantile(new$CLC5, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[1] &
                       CLC5 <= quantile(new$CLC5, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] ~ "q20",
                     CLC5 > quantile(new$CLC5, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[2] &
                       CLC5 <= quantile(new$CLC5, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] ~ "q30",
                     CLC5 > quantile(new$CLC5, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[3] &
                       CLC5 <= quantile(new$CLC5, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] ~ "q40",
                     CLC5 > quantile(new$CLC5, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[4] &
                       CLC5 <= quantile(new$CLC5, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] ~ "q50",
                     CLC5 > quantile(new$CLC5, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[5] &
                       CLC5 <= quantile(new$CLC5, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] ~ "q60",
                     CLC5 > quantile(new$CLC5, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[6] &
                       CLC5 <= quantile(new$CLC5, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] ~ "q70",
                     CLC5 > quantile(new$CLC5, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[7] &
                       CLC5 <= quantile(new$CLC5, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] ~ "q80",
                     CLC5 > quantile(new$CLC5, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[8] &
                       CLC5 <= quantile(new$CLC5, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9] ~ "q90",
                     CLC5 > quantile(new$CLC5, probs=c(0.1,.2,.3,.4,.5,.6,.7,.8,.9), na.rm=T)[9]  ~ "q100"))->new
return(new)
}