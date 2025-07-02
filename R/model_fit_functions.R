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

#create ROC plot
A<-ggroc(rocobj, colour = 'steelblue', size = 1) +
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
pal <- colorRampPalette(c("#008450", "#FFE733", "#b81d13"))(20)

B<-ggplot(plot2)+
  geom_errorbar(aes(x= obs,ymin = low, ymax=upp, col=week_no),alpha=0.5)+ 
  geom_point(aes(x=obs, y= exp,col=week_no))+
  labs(x="Prevalence", y="Model estimated Prevalence", col="Week number")+theme_bw()+
  geom_abline(slope=1)+
  coord_cartesian(xlim = c(0,0.02), ylim =c(0,0.02))+
  scale_colour_gradientn(colours = pal)+
  guides(col = guide_colorbar(barwidth=10 , position="bottom"))
B
return(B)
}


# mae map
model_mae_map<- function(res, stk.yz, z, data, shapefile){
  
  idx.z <- inla.stack.index(stk.yz, 'est.z')$data
  idx.y <- inla.stack.index(stk.yz, 'est.y')$data
  
  t<-plogis(res$summary.linear.predictor$mean[idx.y])
  data$estimated_IR<-t
  data$error <- data$IR-data$estimated_IR
  
  data %>% filter(is.na(IR)==F)%>% 
    group_by(x,y,trap)%>%
    mutate(error=abs(IR-estimated_IR))%>%
    group_by(x,y,trap)%>%
    summarise(error_t=sum(error)/n())%>%
    ggplot()+#facet_wrap("year")+
    geom_sf(data=st_as_sf(shapefile),color='lightgrey',fill='lightgrey')+  
    geom_point(aes(col=error_t, x= x,y=y),size=5)+
    theme_classic()+
    labs(col="MAE",x=NULL,y=NULL)+
    theme(strip.background = element_blank(),
          axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom")+
    scale_color_continuous(type="viridis")+
    guides(col = guide_colorbar(barwidth=10 )) -> map_mae
  
  return(map_mae)
}

# model mae yearly
model_mae_annual<- function(res, stk.yz, data){  
  
  pal <- colorRampPalette(c("#008450", "#FFE733", "#b81d13"))(20)
  
  idx.z <- inla.stack.index(stk.yz, 'est.z')$data
  idx.y <- inla.stack.index(stk.yz, 'est.y')$data
  
  t<-plogis(res$summary.linear.predictor$mean[idx.y])
  data$estimated_IR<-t
  
year_MAE<-data %>% filter(is.na(IR)==F)%>% 
  group_by(week_no,year)%>%
  mutate(error=abs(IR-estimated_IR))%>%
  group_by(week_no,year)%>%
  summarise(error_t=sum(error)/n())%>%
  ggplot()+
  geom_point(aes(x=year,y=error_t,col=as.numeric(week_no)),size=3)+
  theme_bw()+
  labs(y="MAE",x="Year",col="Week\nnumber")+
  scale_colour_gradientn(colours = pal)+
  guides(col = guide_colorbar(barwidth=10 , position="bottom"))
  
return(year_MAE)
}