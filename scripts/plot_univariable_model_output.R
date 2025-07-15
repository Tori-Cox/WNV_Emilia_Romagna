## plot univariable model outputs

### linear
data <- readRDS("output/univar/univar_linear.RDS")
data <- bind_rows(data)
data$var <- data$variable

data |> 
  mutate(var=variable) |>
  mutate(var=case_when(str_detect(var, "_3") ~ "Week t -3",
                       str_detect(var, "_2") ~ "Week t -2",
                       str_detect(var, "_1") ~ "Week t -1",
                       str_detect(var, "_03") ~ "Previous 0-3 weeks",
                       str_detect(var, "_02") ~ "Previous 0-2 weeks",
                       str_detect(var, "_01") ~ "Previous 0-1 weeks",
                       str_detect(var, "_0") ~ "Week t",
                       str_detect(var, "alt") ~ "Elevation",
                       .default = var)) |>
  mutate(group2 = case_when(str_detect(variable, "alt") ~ "Elevation",
                            str_detect(variable, "CLC") ~ "CLC Landuse category",
                            str_detect(variable, "WEEKLY_AV_RH") ~ "Mean relative humidity",
         str_detect(variable, "WEEKLY_AV_RA") ~ "Radiation",
         str_detect(variable, "WEEKLY_AV_LE") ~ "Average hours of leaf wetness",
         str_detect(variable, "WEEKLY_CUM_LE") ~ "Cumulative hours of leaf wetness",
         str_detect(variable, "WEEKLY_CUM_P") ~ "Cumulative precipitation",
         str_detect(variable, "avian") ~ "Avian WNV presence",
         .default=NA)) |>
  mutate(group = case_when(str_detect(group, "z") ~ "Presence",
                           str_detect(group, "y") ~ "Prevalence",.default=group)) |>
  rename(beta_low=`0.025quant`, beta_upp=`0.975quant`, beta_mean=mean) -> data

plots <- list()
for(i in 1:length(unique(data$group2))){
  sub<-data[data$group2 == unique(data$group2)[i],]
  lim1<- ifelse(min(sub$beta_low,na.rm=T)< -max(sub$beta_upp,na.rm=T),
                min(sub$beta_low,na.rm=T), - max(sub$beta_upp,na.rm=T))
  lim2<-ifelse(min(sub$beta_low,na.rm=T)< -max(sub$beta_upp,na.rm=T),
               - min(sub$beta_low,na.rm=T), max(sub$beta_upp,na.rm=T))
  
  if(unique(data$group2)[i]=="Elevation"|unique(data$group2)[i]=="Avian WNV presence"){
    plots[[i]]<-
      ggplot(sub)+ 
      geom_hline(aes(yintercept = 0), linetype="dashed") +
      geom_point(aes(x=var, y=beta_mean, col=group), 
                 size=2,position=position_dodge(width=0.2)) +
      geom_pointrange(aes(x=var, y=beta_mean, 
                          ymin=beta_low, 
                          ymax=beta_upp, col=group), 
                      size=0.3,position=position_dodge(width=0.2)) + 
      labs(x =NULL,
           y = "Beta value", title = unique(data$group2)[i]) +
      coord_flip(ylim=c(lim1,lim2)) + #makes horizontal
      theme_classic() +
      theme(legend.title=element_blank(),
            legend.text = element_text(size=15),
            legend.position="none",
            axis.text=element_text(size=8),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(size=8,hjust=0.5))}else{
              plots[[i]]<-
                ggplot(sub)+ 
                geom_hline(aes(yintercept = 0), linetype="dashed") +
                geom_point(aes(x=var, y=beta_mean, col=group), 
                           size=2,position=position_dodge(width=0.2)) +
                geom_pointrange(aes(x=var, y=beta_mean, 
                                    ymin=beta_low, 
                                    ymax=beta_upp, col=group), 
                                size=0.3,position=position_dodge(width=0.2)) + 
                labs(x = NULL,
                     y = "Beta value", title = unique(data$group2)[i]) +
                coord_flip(ylim=c(lim1,lim2)) + #makes horizontal
                theme_classic() +
                theme(legend.title=element_blank(),
                      legend.position="none",
                      legend.text = element_text(size=15),
                      plot.title = element_text(size=8,hjust=0.5),
                      axis.text=element_text(size=8))
            }
}

ggplot(sub, aes(x=var, y=beta_mean, col=group))+
  lims(y = c(0,0))+
  labs(col=NULL)+
  geom_line()+
  geom_point()+
  theme_void()+
  theme(legend.position.inside = c(0.5,0.5),
        legend.text = element_text(size =  15))+
  guides(colour = guide_legend(override.aes = list(size=5)))->leg

if(run!="short"){
ggarrange(plots[[3]],
          plots[[6]],
          plots[[5]],
          plots[[4]],
          plots[[7]],
          plots[[2]],
          plots[[1]],
          leg,
          ncol=2,nrow=4,
          align="hv",
          labels=c('A', 'B','C', 'D', 'E', 'F','G',  NA),
          common.legend = F)-> plots_linear
  print(plots_linear)
}else{
  ggarrange(plots[[1]],
            leg,
            ncol=2,nrow=1,
            align="hv",
            common.legend = F)-> plots_linear
  print(plots_linear)
}



### non linear
beta1 <- readRDS("output/univar/univar_nonlinear.RDS")
nonlinear_beta <- bind_rows(beta1)
unique(nonlinear_beta$var)


nonlinear_beta$group2 <- paste0(sapply(strsplit(nonlinear_beta$variable, "_"), "[", 2),"_",sapply(strsplit(nonlinear_beta$variable, "_"), "[", 3))
nonlinear_beta$lag <- sapply(strsplit(nonlinear_beta$variable, "_"), "[", 4)

nonlinear_beta$group2 <- sub("AV_TMIN", "Average minimum temperature (\u00B0C)", nonlinear_beta$group2)
nonlinear_beta$group2 <- sub("AV_TMAX", "Average maximum temperature (\u00B0C)", nonlinear_beta$group2)
nonlinear_beta$group2 <- sub("AV_TAVG", "Average mean temperature (\u00B0C)", nonlinear_beta$group2)
nonlinear_beta$group2 <- sub("AV_ETOPM", "Average evapotranspiration (mm)", nonlinear_beta$group2)
nonlinear_beta$group2 <- sub("AV_RAD", "Average solar radiation (MJm-2)", nonlinear_beta$group2)
nonlinear_beta$group2 <- sub("MIN_TMIN", "Absolute minimum temperature (\u00B0C)", nonlinear_beta$group2)
nonlinear_beta$group2 <- sub("MAX_TMAX", "Absolute maximum temperature (\u00B0C)", nonlinear_beta$group2)

nonlinear_beta$lag <- sub("01", "0-1", nonlinear_beta$lag)
nonlinear_beta$lag <- sub("02", "0-2", nonlinear_beta$lag)
nonlinear_beta$lag <- sub("03", "0-3", nonlinear_beta$lag)


ggplot(nonlinear_beta[nonlinear_beta$group=="z",])+
  facet_wrap("group2",scales="free",ncol=4)+
  geom_line(aes(ID,y=mean,col=lag))+
  geom_ribbon(aes(x=ID,ymin=`0.025quant`,ymax=`0.975quant`, fill=lag), alpha=0.2)+
  geom_hline(yintercept=0,linetype="dashed")+
  theme_bw()+
  theme(legend.position="none")+
  guides(fill = guide_legend(nrow = 1),col = guide_legend(nrow = 1))+
  labs(x=NULL, y="Coefficients (95% CrI)", fill="Time lag (weeks)",
       col="Time lag (weeks)")->a
ggplot(nonlinear_beta[nonlinear_beta$group=="y",])+
  facet_wrap("group2",scales="free",ncol=4)+
  geom_line(aes(ID,y=mean,col=lag))+
  geom_ribbon(aes(x=ID,ymin=`0.025quant`,ymax=`0.975quant`, fill=lag), alpha=0.2)+
  geom_hline(yintercept=0,linetype="dashed")+
  theme_bw()+
  theme(legend.position="bottom")+
  guides(fill = guide_legend(nrow = 1),col = guide_legend(nrow = 1))+
  labs(x="Value", y="Coefficients (95% CrI)",
       fill="Time lag (weeks)", col="Time lag (weeks)")->b

cowplot::plot_grid(a,b,ncol=1,rel_heights = c(0.9,1),labels="AUTO")-> plots_nonlinear
print(plots_nonlinear)
