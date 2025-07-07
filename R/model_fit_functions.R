# assessment of model fit functions

# ROC curve
ROC_curve <- function(res, stk.yz, z){
  
  idx.z <- inla.stack.index(stk.yz, 'est.z')$data
  idx.y <- inla.stack.index(stk.yz, 'est.y')$data
  
t<-plogis(res$summary.linear.predictor$mean[idx.z])
t2<-plogis(res$summary.linear.predictor$`0.025quant`[idx.z])
t3<-plogis(res$summary.linear.predictor$`0.975quant`[idx.z])
plot<-data.frame(obs=z, exp=t,low=t2,upp=t3)

#define object to plot and calculate AUC
rocobj <- roc(plot$obs, plot$exp)
auc <- round(auc(plot$obs, plot$exp),4)

# create ROC plot
A <- ggroc(rocobj, colour = 'steelblue', size = 1) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')'))+
  labs(x="False positive rate", y="True positive rate")+theme_bw()

return(A)
}

# model fit
model_fit<- function(res, stk.yz, z, data){
  
  idx.z <- inla.stack.index(stk.yz, 'est.z')$data
  idx.y <- inla.stack.index(stk.yz, 'est.y')$data
  
t<-plogis(res$summary.linear.predictor$mean[idx.y])
t2<-plogis(res$summary.linear.predictor$`0.025quant`[idx.y])
t3<-plogis(res$summary.linear.predictor$`0.975quant`[idx.y])

plot2<-data.frame(obs=y, exp=t,low=t2,upp=t3, week_no = as.numeric(data$week_no))

B <- plot2 |>
  filter(!is.na(obs))|>
  ggplot()+
  geom_errorbar(aes(x= obs,ymin = low, ymax=upp, col=week_no), alpha=0.5)+ 
  geom_point(aes(x=obs, y= exp,col=week_no))+
  labs(x="Prevalence", y="Model estimated prevalence", col="Week\nnumber")+theme_bw()+
  geom_abline(slope=1)+
  coord_cartesian(xlim = c(0,0.025),ylim = c(0,0.02))+
  scale_color_distiller(palette="RdYlGn", direction=-1)+
  theme(legend.position = "bottom")+
  guides(col = guide_colorbar(barwidth=10))+
  theme(text=element_text(size=15))

return(B)
}

# model mae yearly
model_mae_annual<- function(res, stk.yz, data){  
  
  idx.z <- inla.stack.index(stk.yz, 'est.z')$data
  idx.y <- inla.stack.index(stk.yz, 'est.y')$data
  
  t<-plogis(res$summary.linear.predictor$mean[idx.y])
  data$estimated_IR<-t
  
  year_MAE <- data |> filter(!is.na(IR))|> 
    group_by(week_no,year)|>
    mutate(error=abs(IR-estimated_IR))|>
    group_by(week_no,year)|>
    summarise(error_t=sum(error)/n())|>
    ggplot()+
    geom_point(aes(x=year,y=error_t,col=as.numeric(week_no)),size=3)+
    theme_bw()+
    labs(y="MAE",x="Year",col="Week\nnumber")+
    scale_color_distiller(palette="RdYlGn", direction=-1)+
    theme(legend.position = "bottom", text=element_text(size=15))+
    guides(col = guide_colorbar(barwidth=10))
    
return(year_MAE)
}