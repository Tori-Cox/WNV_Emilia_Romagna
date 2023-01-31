library(INLA)
library(ggplot2)
library(forcats)
library(dplyr)
library(beepr)

## read in data
#inla_df_norm <-readRDS("inla_df_norm.RDS")
inla_df_norm <-read.csv("inla_df_5_10_22.csv")
head(inla_df_norm)
inla_df_norm$temp_med <- ((inla_df_norm$temp_med-mean(inla_df_norm$temp_med))/sd(inla_df_norm$temp_med))
inla_df_norm$temp_med_lag1 <- ((inla_df_norm$temp_med_lag1-mean(inla_df_norm$temp_med_lag1))/sd(inla_df_norm$temp_med_lag1))
inla_df_norm$temp_med_lag2 <- ((inla_df_norm$temp_med_lag2-mean(inla_df_norm$temp_med_lag2))/sd(inla_df_norm$temp_med_lag2))
inla_df_norm$temp_med_lag3 <- ((inla_df_norm$temp_med_lag3-mean(inla_df_norm$temp_med_lag3))/sd(inla_df_norm$temp_med_lag3))
inla_df_norm$temp_min <- ((inla_df_norm$temp_min-mean(inla_df_norm$temp_min))/sd(inla_df_norm$temp_min))
inla_df_norm$temp_min_lag1 <- ((inla_df_norm$temp_min_lag1-mean(inla_df_norm$temp_min_lag1))/sd(inla_df_norm$temp_min_lag1))
inla_df_norm$temp_min_lag2 <- ((inla_df_norm$temp_min_lag2-mean(inla_df_norm$temp_min_lag2))/sd(inla_df_norm$temp_min_lag2))
inla_df_norm$temp_min_lag3 <- ((inla_df_norm$temp_min_lag3-mean(inla_df_norm$temp_min_lag3))/sd(inla_df_norm$temp_min_lag3))
inla_df_norm$temp_max <- ((inla_df_norm$temp_max-mean(inla_df_norm$temp_max))/sd(inla_df_norm$temp_max))
inla_df_norm$temp_max_lag1 <- ((inla_df_norm$temp_max_lag1-mean(inla_df_norm$temp_max_lag1))/sd(inla_df_norm$temp_max_lag1))
inla_df_norm$temp_max_lag2 <- ((inla_df_norm$temp_max_lag2-mean(inla_df_norm$temp_max_lag2))/sd(inla_df_norm$temp_max_lag2))
inla_df_norm$temp_max_lag3 <- ((inla_df_norm$temp_max_lag3-mean(inla_df_norm$temp_max_lag3))/sd(inla_df_norm$temp_max_lag3))
inla_df_norm$temp_med <- ((inla_df_norm$temp_med-mean(inla_df_norm$temp_med))/sd(inla_df_norm$temp_med))
inla_df_norm$hum_med_lag1 <- ((inla_df_norm$hum_med_lag1-mean(inla_df_norm$hum_med_lag1))/sd(inla_df_norm$hum_med_lag1))
inla_df_norm$hum_med_lag2 <- ((inla_df_norm$hum_med_lag2-mean(inla_df_norm$hum_med_lag2))/sd(inla_df_norm$hum_med_lag2))
inla_df_norm$hum_med_lag3 <- ((inla_df_norm$hum_med_lag3-mean(inla_df_norm$hum_med_lag3))/sd(inla_df_norm$hum_med_lag3))
inla_df_norm$hum_min <- ((inla_df_norm$hum_min-mean(inla_df_norm$hum_min))/sd(inla_df_norm$hum_min))
inla_df_norm$hum_min_lag1 <- ((inla_df_norm$hum_min_lag1-mean(inla_df_norm$hum_min_lag1))/sd(inla_df_norm$hum_min_lag1))
inla_df_norm$hum_min_lag2 <- ((inla_df_norm$hum_min_lag2-mean(inla_df_norm$hum_min_lag2))/sd(inla_df_norm$hum_min_lag2))
inla_df_norm$hum_min_lag3 <- ((inla_df_norm$hum_min_lag3-mean(inla_df_norm$hum_min_lag3))/sd(inla_df_norm$hum_min_lag3))
inla_df_norm$hum_max <- ((inla_df_norm$hum_max-mean(inla_df_norm$hum_max))/sd(inla_df_norm$hum_max))
inla_df_norm$hum_max_lag1 <- ((inla_df_norm$hum_max_lag1-mean(inla_df_norm$hum_max_lag1))/sd(inla_df_norm$hum_max_lag1))
inla_df_norm$hum_max_lag2 <- ((inla_df_norm$hum_max_lag2-mean(inla_df_norm$hum_max_lag2))/sd(inla_df_norm$hum_max_lag2))
inla_df_norm$hum_max_lag3 <- ((inla_df_norm$hum_max_lag3-mean(inla_df_norm$hum_max_lag3))/sd(inla_df_norm$hum_max_lag3))
inla_df_norm$prec <- ((inla_df_norm$prec-mean(inla_df_norm$prec))/sd(inla_df_norm$prec))
inla_df_norm$prec_lag1 <- ((inla_df_norm$prec_lag1-mean(inla_df_norm$prec_lag1))/sd(inla_df_norm$prec_lag1))
inla_df_norm$prec_lag2 <- ((inla_df_norm$prec_lag2-mean(inla_df_norm$prec_lag2))/sd(inla_df_norm$prec_lag2))
inla_df_norm$prec_lag3 <- ((inla_df_norm$prec_lag3-mean(inla_df_norm$prec_lag3))/sd(inla_df_norm$prec_lag3))
head(inla_df_norm)
range(inla_df_norm$temp_med)


emplogit<-function(Y,N) {top=Y*N+0.5;bottom=N*(1-Y)+0.5;return(log(top/bottom))} 
inla_df_norm$mosq_prev_emplogit<-emplogit(inla_df_norm$mosq_prevalence, inla_df_norm$mosq_abundance)
hist(inla_df_norm$mosq_prev_emplogit)
head(inla_df_norm)

ggplot(inla_df_norm) +
  geom_point(aes(x = mosq_abundance, y = mosq_prev_emplogit)) +
  geom_smooth(aes(x = mosq_abundance, y = mosq_prev_emplogit)) +
  xlab("Abundance") + ylab("Prevalence") +
  theme_classic()


## ADD IN THE PREVALENCE THE WEEK BEFORE AS A COVARIATE
# dates as numerical time sequence
head(inla_df_norm)
inla_df_norm <- inla_df_norm %>% 
  mutate(time = dense_rank((week)))

unique(inla_df_norm$time)
inla_df_norm$prev_lag1 <- NA
for(i in 2:109){
  time <- i
  time_lag <- i-1
  
  for(x in 1:70){
    
    trap <- unique(inla_df_norm$trap_code)[x]
    sub <- inla_df_norm[inla_df_norm$trap_code==trap,] 
    sub$breaks<-0
    for(y in 2:nrow(sub)){
      sub$breaks[y] <- ifelse(substring(sub$week[y],1,4)==substring(sub$week[y-1],1,4),0,1)
    }
    year_breaks <- sub$time[sub$breaks==1 & is.na(sub$breaks) !=T]
    
    if(length(unique(year_breaks %in% time))==1){
      
      if(length(unique(is.element(inla_df_norm$mosq_prevalence[inla_df_norm$trap_code==trap],
                                  inla_df_norm$mosq_prevalence[inla_df_norm$time == time_lag])))>1){
        inla_df_norm$prev_lag1[inla_df_norm$trap_code==trap & inla_df_norm$time == time] <- inla_df_norm$mosq_prevalence[inla_df_norm$trap_code==trap & inla_df_norm$time == time_lag]
      }
      if(length(unique(is.element(inla_df_norm$mosq_prevalence[inla_df_norm$trap_code==trap],
                                  inla_df_norm$mosq_prevalence[inla_df_norm$time == time_lag])))==1 &
         length(unique(is.element(inla_df_norm$mosq_prevalence[inla_df_norm$trap_code==trap],
                                  inla_df_norm$mosq_prevalence[inla_df_norm$time == (time_lag-1)])))>1){
        inla_df_norm$prev_lag1[inla_df_norm$trap_code==trap & inla_df_norm$time == time] <- inla_df_norm$mosq_prevalence[inla_df_norm$trap_code==trap & inla_df_norm$time == (time_lag-1)]
      }
    }
  }
}
test <- inla_df_norm[inla_df_norm$trap_code=="WN046A" ,]
#inla_df <- data[which(!is.na(data$mosq_prev_emplogit)),]

unique(inla_df_norm$prev_lag1)



### Univariate model selection

op1 <- inla(mosq_prev_emplogit ~ 1 + 
              temp_med,
            data = inla_df_norm,
            family="gaussian",
            control.compute = list(dic = TRUE))

op2 <- inla(mosq_prev_emplogit ~ 1 +
              temp_med_lag1,
            data = inla_df_norm,
            family="gaussian",
            control.compute = list(dic = TRUE))

op3 <- inla(mosq_prev_emplogit ~ 1 + 
              temp_med_lag2,
            data = inla_df_norm,
            family="gaussian",
            control.compute = list(dic = TRUE))

op4 <- inla(mosq_prev_emplogit ~ 1 + 
              temp_med_lag3,
            data = inla_df_norm,
            family="gaussian",
            control.compute = list(dic = TRUE))


op5 <- inla(mosq_prev_emplogit ~ 1 + 
              temp_min,
            data = inla_df_norm,
            family="gaussian",
            control.compute = list(dic = TRUE))

op6 <- inla(mosq_prev_emplogit ~ 1 + 
              temp_min_lag1,
            data = inla_df_norm,
            family="gaussian",
            control.compute = list(dic = TRUE))

op7 <- inla(mosq_prev_emplogit ~ 1 + 
              temp_min_lag2,
            data = inla_df_norm,
            family="gaussian",
            control.compute = list(dic = TRUE))

op8 <- inla(mosq_prev_emplogit ~ 1 +
              temp_min_lag3,
            data = inla_df_norm,
            family="gaussian",
            control.compute = list(dic = TRUE))


op9 <- inla(mosq_prev_emplogit ~ 1 + 
              temp_max,
            data = inla_df_norm,
            family="gaussian",
            control.compute = list(dic = TRUE))

op10 <- inla(mosq_prev_emplogit ~ 1 + 
              temp_max_lag1,
            data = inla_df_norm,
            family="gaussian",
            control.compute = list(dic = TRUE))

op11 <- inla(mosq_prev_emplogit ~ 1 + 
              temp_max_lag2,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op12 <- inla(mosq_prev_emplogit ~ 1 + 
              temp_max_lag3,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))



op13 <- inla(mosq_prev_emplogit ~ 1 + 
              hum_med,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op14 <- inla(mosq_prev_emplogit ~ 1 + 
              hum_med_lag1,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op15 <- inla(mosq_prev_emplogit ~ 1 + 
              hum_med_lag2,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op16 <- inla(mosq_prev_emplogit ~ 1 + 
              hum_med_lag3,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))


op17 <- inla(mosq_prev_emplogit ~ 1 + 
              hum_min,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op18 <- inla(mosq_prev_emplogit ~ 1 + 
              hum_min_lag1,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op19 <- inla(mosq_prev_emplogit ~ 1 + 
              hum_min_lag2,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op20 <- inla(mosq_prev_emplogit ~ 1 + 
              hum_min_lag3,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))


op21 <- inla(mosq_prev_emplogit ~ 1 + 
              hum_max,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op22 <- inla(mosq_prev_emplogit ~ 1 + 
              hum_max_lag1,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op23 <- inla(mosq_prev_emplogit ~ 1 + 
              hum_max_lag2,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op24 <- inla(mosq_prev_emplogit ~ 1 + 
              hum_max_lag3,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))


op25 <- inla(mosq_prev_emplogit ~ 1 + 
              prec,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op26 <- inla(mosq_prev_emplogit ~ 1 + 
              prec_lag1,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op27 <- inla(mosq_prev_emplogit ~ 1 + 
              prec_lag2,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op28 <- inla(mosq_prev_emplogit ~ 1 + 
              prec_lag3,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op29 <- inla(mosq_prev_emplogit ~ 1 + 
               f(avian_WNV, model="iid"),
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op30 <- inla(mosq_prev_emplogit ~ 1 + 
               biodiversity,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op31 <- inla(mosq_prev_emplogit ~ 1 + 
               land_use_1,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op32 <- inla(mosq_prev_emplogit ~ 1 + 
               land_use_2,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op33 <- inla(mosq_prev_emplogit ~ 1 + 
               land_use_3,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op34 <- inla(mosq_prev_emplogit ~ 1 + 
               land_use_4,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op35 <- inla(mosq_prev_emplogit ~ 1 + 
               land_use_5,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op36 <- inla(mosq_prev_emplogit ~ 1 + 
               prev_lag1,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

dic <- c(op1$dic$dic,op2$dic$dic,op3$dic$dic,op4$dic$dic,op5$dic$dic,op6$dic$dic,
         op7$dic$dic,op8$dic$dic,op9$dic$dic,op10$dic$dic,op11$dic$dic,op12$dic$dic,
         op13$dic$dic,op14$dic$dic,op15$dic$dic,op16$dic$dic,op17$dic$dic,op18$dic$dic,
         op19$dic$dic,op20$dic$dic,op21$dic$dic,op22$dic$dic,op23$dic$dic,op24$dic$dic,
         op25$dic$dic,op26$dic$dic,op27$dic$dic,op28$dic$dic,op29$dic$dic,op30$dic$dic,
         op31$dic$dic,op32$dic$dic,op33$dic$dic,op34$dic$dic,op35$dic$dic,op36$dic$dic) #store dic

var_names <- c(names(inla_df_norm[,c(5:39,42)])) #vector of variable names
outputs <- rbind(op1$summary.fixed[2,], op2$summary.fixed[2,], op3$summary.fixed[2,],
                 op4$summary.fixed[2,], op5$summary.fixed[2,], op6$summary.fixed[2,],
                 op7$summary.fixed[2,], op8$summary.fixed[2,], op9$summary.fixed[2,],
                 op10$summary.fixed[2,], op11$summary.fixed[2,], op12$summary.fixed[2,],
                 op13$summary.fixed[2,], op14$summary.fixed[2,], op15$summary.fixed[2,],
                 op16$summary.fixed[2,], op17$summary.fixed[2,], op18$summary.fixed[2,],
                 op19$summary.fixed[2,], op20$summary.fixed[2,], op21$summary.fixed[2,],
                 op22$summary.fixed[2,], op23$summary.fixed[2,], op24$summary.fixed[2,],
                 op25$summary.fixed[2,], op26$summary.fixed[2,], op27$summary.fixed[2,],
                 op28$summary.fixed[2,], op29$summary.random$avian_WNV[2,2:8],
                 op30$summary.fixed[2,],
                 op31$summary.fixed[2,], op32$summary.fixed[2,], op33$summary.fixed[2,],
                 op34$summary.fixed[2,], op35$summary.fixed[2,], op36$summary.fixed[2,]) #store outputs
group <- c(rep('Temperature',12),rep('Humidity',12),rep('Precipitation',4),rep('Avian',2),rep('Land use',5),"Mosquito") #group variables for colour-coding
univariate_df <- cbind(var_names,outputs,dic,group) #combine outputs for graph

head(univariate_df)
tail(univariate_df)

### Plotting beta values 
univariate_df$var_names <- c("TMed","TMed1","TMed2","TMed3","TMin","TMin1","TMin2","TMin3",
                             "TMax","TMax1","TMax2","TMax3","HMed","HMed1","HMed2","HMed3",
                             "HMin","HMin1","HMin2","HMin3","Hmax","HMax1","HMax2","HMax3",
                             "P","P1","P2","P3","avianWNV","D","CLC1","CLC2","CLC3","CLC4","CLC5","Prev_lag1")
univariate_df$var_names <- fct_reorder(univariate_df$var_names, dic) #needs forcats() package
univariate_df$var_names <- fct_rev(univariate_df$var_names)

colours <- c("mediumorchid4","orangered3","steelblue3","gray","chartreuse4","pink")

beta_plot <- ggplot(univariate_df) +
  geom_hline(aes(yintercept = 0)) +
  geom_pointrange(aes(x=var_names, y=mean, 
                      ymin=univariate_df$'0.975quant', 
                      ymax=univariate_df$'0.025quant'), 
                  colour="black", size=0.3) + #makes points with bars (points are beta values and bars are 95% CI)
  geom_point(data=univariate_df, 
             aes(x=var_names, y=mean, color=group), size=2) +
  scale_color_manual(values=colours, breaks = c('Temperature','Humidity','Precipitation','Avian','Land use',"Mosquito")) +
  labs(x = "Variables", y = "Beta value") +
  coord_flip() + #makes horizontal
  theme_classic() +
#  theme(legend.title=element_blank(), legend.position = c(.85, .875), legend.box.background = element_rect(colour = "black"))
  theme(legend.title=element_blank(), legend.position = c(.85, .8))
beta_plot

# only include covariates where the CI do not cross zero
univariate_df2<-univariate_df[univariate_df$group!="Mosquito",]
univariate_df2$include <- ifelse(univariate_df2$`0.025quant`<0 & univariate_df2$`0.975quant`>0, 0,1)
univariate_df2 <- univariate_df2[univariate_df2$include==1,]
beta_plot2 <- ggplot(univariate_df2) +
  geom_hline(aes(yintercept = 0)) +
  geom_pointrange(aes(x=var_names, y=mean, ymin=univariate_df2$'0.975quant', 
                      ymax=univariate_df2$'0.025quant'), colour="black", size=0.3) + #makes points with bars (points are beta values and bars are 95% CI)
  geom_point(data=univariate_df2, aes(x=var_names, y=mean, color=group), size=2) +
  scale_color_manual(values=colours, breaks = c('Temperature','Humidity',
                                                'Precipitation','Avian','Land use',"Mosquito")) +
  labs(x = "Variables", y = "Beta value") +
  coord_flip() + #makes horizontal
  theme_classic() +
  theme(legend.title=element_blank(), 
        legend.position = c(.85, .8))
beta_plot2
new <- univariate_df3[univariate_df3$group!="Avian" & univariate_df3$group!="Mosquito"& univariate_df3$group!="Precipitation",]
ggplot(new) + facet_wrap("group",scales="free")+
  geom_hline(aes(yintercept = 0)) +
  geom_pointrange(aes(x=dic, y=mean, ymin=new$'0.975quant', 
                      ymax=new$'0.025quant'), size=0.3, position=position_jitter(width=1)) +
  #geom_point(aes(x=group, y=mean, color=group), size=2,position=position_jitter(width=10)) +
  geom_label_repel(aes(x=dic,y=mean,label=var_names),size=3)+
  # scale_color_manual(values=colours, breaks = c('Temperature','Humidity','Precipitation','Avian','Land use',"Mosquito")) +
  labs(x = "DIC", y = "Beta value") +
  #coord_flip() + #makes horizontal
  theme_classic() 

# Continue with best model selection
var1 <- var_names[which(dic==min(dic, na.rm=T))] #which variable had lowest dic

collinear <- data.frame(matrix(ncol = 36, nrow = 0)) #make collinearity matrix
f <- var_names
colnames(collinear) <- f#[4:74]
for (j in 1:36) {
  for (i in 1:36){
    c <- cor(inla_df_norm[c(var_names[j],var_names[i])], use="complete.obs", method="pearson")
    collinear[j,i] <- c[1,2]
  }
}

sub1 <- collinear[,colnames(collinear)==var1] #subset just the chosen variable
test_df1 <- data.frame(names = var_names,
                      values = sub1)
test_df1[which(test_df1$values<0.6 & test_df1$values > -0.6),1] #names of the variables not colinear to be included in next round
test_df1[which(test_df1$values>0.6 | test_df1$values < -0.6),1] #names of the variables colinear not to be included in next round

a<-test_df1[which(test_df1$values<0.6 & test_df1$values > -0.6),1] #names of the variables not colinear to be included in next round
b<-data.frame(a=a,b=1:length(a))
b
beep()




### Multivariate model selection

op1.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
               
                temp_med,
            data = inla_df_norm,
            family="gaussian",
            control.compute = list(dic = TRUE))

op2.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
               
              temp_med_lag1,
            data = inla_df_norm,
            family="gaussian",
            control.compute = list(dic = TRUE))

op3.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
               
              temp_med_lag2,
            data = inla_df_norm,
            family="gaussian",
            control.compute = list(dic = TRUE))

op4.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
               
              temp_med_lag3,
            data = inla_df_norm,
            family="gaussian",
            control.compute = list(dic = TRUE))

op5.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
               
                temp_min,
            data = inla_df_norm,
            family="gaussian",
            control.compute = list(dic = TRUE))

op6.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
               
                temp_min_lag1,
            data = inla_df_norm,
            family="gaussian",
            control.compute = list(dic = TRUE))

op7.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
               
              temp_min_lag2,
            data = inla_df_norm,
            family="gaussian",
            control.compute = list(dic = TRUE))

op8.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
               
              temp_min_lag3,
            data = inla_df_norm,
            family="gaussian",
            control.compute = list(dic = TRUE))

op9.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
               
              temp_max,
            data = inla_df_norm,
            family="gaussian",
            control.compute = list(dic = TRUE))

op10.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
               
              temp_max_lag1,
            data = inla_df_norm,
            family="gaussian",
            control.compute = list(dic = TRUE))

op11.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
               
              temp_max_lag2,
            data = inla_df_norm,
            family="gaussian",
            control.compute = list(dic = TRUE))

op12.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
               temp_max_lag3,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op13.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
                 hum_med ,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op14.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
                 hum_med_lag1 ,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op15.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
                 hum_med_lag2,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op16.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
                 hum_med_lag3,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op17.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
                 hum_min,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op18.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
               hum_min_lag1,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op19.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
               hum_min_lag2,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op20.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
               hum_min_lag3,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op21.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
                 hum_max,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op22.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
                 hum_max_lag1,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op23.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
                 hum_max_lag2,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op24.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
                 hum_max_lag3,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op25.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
                 prec ,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op26.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
                 prec_lag1,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op27.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
                 prec_lag2,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op28.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
                 prec_lag3,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op29.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
                 f(avian_WNV, model="iid"),
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op30.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
                 biodiversity ,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op31.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
                 land_use_1 ,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op32.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
                 land_use_2,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op33.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
                 land_use_3,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op34.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
                 land_use_4,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))

op35.2 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 +
                
                 land_use_5,
             data = inla_df_norm,
             family="gaussian",
             control.compute = list(dic = TRUE))


dic.2 <- c(op1.2$dic$dic,op2.2$dic$dic,op3.2$dic$dic,op4.2$dic$dic,op5.2$dic$dic,op6.2$dic$dic,
         op7.2$dic$dic,op8.2$dic$dic,op9.2$dic$dic,op10.2$dic$dic,op11.2$dic$dic,op12.2$dic$dic,
         op13.2$dic$dic,op14.2$dic$dic,op15.2$dic$dic,op16.2$dic$dic,op17.2$dic$dic,op18.2$dic$dic,
         op19.2$dic$dic,op20.2$dic$dic,op21.2$dic$dic,op22.2$dic$dic,op23.2$dic$dic,op24.2$dic$dic,
         op25.2$dic$dic,op26.2$dic$dic,op27.2$dic$dic,op28.2$dic$dic,op29.2$dic$dic,op30.2$dic$dic,
         op31.2$dic$dic,op32.2$dic$dic,op33.2$dic$dic,op34.2$dic$dic,op35.2$dic$dic) #store dic

var_names2 <- b$a
var2 <- var_names2[which(dic.2==min(dic.2, na.rm=T))] #which variable had lowest dic
var2
min(dic, na.rm=T) - min(dic.2, na.rm=T) >4 #difference in dic between previous variable and this must be >4
beep()

sub2 <- collinear[,colnames(collinear)==var2] #subset just the chosen variable
test_df2 <- data.frame(names = var_names,
                      values = sub2)
test_df2 <- test_df2[test_df2$names %in% var_names2, ]
test_df2[which(test_df2$values<0.6 & test_df2$values>-0.6),1] #names of the variables not colinear to be included in next round
test_df2[which(test_df2$values>0.6 | test_df2$values < -0.6),1] #names of the variables colinear not to be included in next round

a<-test_df2[which(test_df2$values<0.6 & test_df2$values > -0.6),1] #names of the variables not colinear to be included in next round
b<-data.frame(a=a,b=1:length(a))
b


## choice 3

op1.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                
                temp_med_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op2.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                
                temp_med_lag3,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op3.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                
                temp_min_lag1,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op4.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                
                temp_min_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op5.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                
                temp_min_lag3,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))


op6.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 temp_max_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op7.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 temp_max_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op8.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 hum_med ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op9.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 hum_med_lag1 ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op10.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 hum_med_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op11.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 hum_med_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op12.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 hum_min,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op13.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 hum_min_lag1,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op14.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 hum_min_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op15.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 hum_min_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op16.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 hum_max,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op17.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 hum_max_lag1,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op18.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 hum_max_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op19.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 hum_max_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op20.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 prec ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op21.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 prec_lag1,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op22.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 prec_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op23.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 prec_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op24.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 f(avian_WNV, model="iid"),
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op25.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 biodiversity ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op26.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 land_use_1 ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op27.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 land_use_2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op28.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 land_use_3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op29.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 land_use_4,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op30.3 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med +
                 
                 land_use_5,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))



dic.3 <- c(op1.3$dic$dic,op2.3$dic$dic,op3.3$dic$dic,op4.3$dic$dic,op5.3$dic$dic,op6.3$dic$dic,
           op7.3$dic$dic,op8.3$dic$dic,op9.3$dic$dic,op10.3$dic$dic,op11.3$dic$dic,op12.3$dic$dic,
           op13.3$dic$dic,op14.3$dic$dic,op15.3$dic$dic,op16.3$dic$dic,op17.3$dic$dic,op18.3$dic$dic,
           op19.3$dic$dic,op20.3$dic$dic,op21.3$dic$dic,op22.3$dic$dic,op23.3$dic$dic,op24.3$dic$dic,
           op25.3$dic$dic,op26.3$dic$dic,op27.3$dic$dic,op28.3$dic$dic,op29.3$dic$dic,op30.3$dic$dic) #store dic

var_names3 <- b$a
var3 <- var_names3[which(dic.3==min(dic.3, na.rm=T))] #which variable had lowest dic
var3
min(dic.2, na.rm=T) - min(dic.3, na.rm=T) >4 #difference in dic between previous variable and this must be >4
beep()

sub3 <- collinear[,colnames(collinear)==var3] #subset just the chosen variable
test_df3 <- data.frame(names = var_names,
                       values = sub3)
test_df3 <- test_df3[test_df3$names %in% var_names3, ]
test_df3[which(test_df3$values<0.6 & test_df3$values>-0.6),1] #names of the variables not colinear to be included in next round
test_df3[which(test_df3$values>0.6 | test_df3$values < -0.6),1] #names of the variables colinear not to be included in next round

a<-test_df3[which(test_df3$values<0.6 & test_df3$values > -0.6),1] #names of the variables not colinear to be included in next round
b<-data.frame(a=a,b=1:length(a))
b


## choice 4 

op1.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                
                temp_med_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))


op2.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                
                temp_min_lag1,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))


op3.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                
                temp_max_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op4.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                
                hum_med ,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op5.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                
                hum_med_lag1 ,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op6.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                 
                 hum_med_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op7.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                 
                 hum_med_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op8.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                 
                 hum_min,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op9.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                 
                 hum_min_lag1,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op10.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                 
                 hum_min_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op11.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                 
                 hum_min_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op12.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                 
                 hum_max,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op13.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                 
                 hum_max_lag1,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op14.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                 
                 hum_max_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op15.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                 
                 hum_max_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op16.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                 
                 prec ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op17.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                 
                 prec_lag1,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op18.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                 
                 prec_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op19.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                 
                 prec_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op20.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                 
                 f(avian_WNV, model="iid"),
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op21.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                 
                 biodiversity ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op22.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                 
                 land_use_1 ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op23.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                 
                 land_use_2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op24.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                 
                 land_use_3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op25.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                 
                 land_use_4,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op26.4 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 +
                 
                 land_use_5,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))


dic.4 <- c(op1.4$dic$dic,op2.4$dic$dic,op3.4$dic$dic,op4.4$dic$dic,op5.4$dic$dic,op6.4$dic$dic,
           op7.4$dic$dic,op8.4$dic$dic,op9.4$dic$dic,op10.4$dic$dic,op11.4$dic$dic,op12.4$dic$dic,
           op13.4$dic$dic,op14.4$dic$dic,op15.4$dic$dic,op16.4$dic$dic,op17.4$dic$dic,op18.4$dic$dic,
           op19.4$dic$dic,op20.4$dic$dic,op21.4$dic$dic,op22.4$dic$dic,op23.4$dic$dic,op24.4$dic$dic,
           op25.4$dic$dic,op26.4$dic$dic) #store dic

var_names4 <- b$a
var4 <- var_names4[which(dic.4==min(dic.4, na.rm=T))] #which variable had lowest dic
var4
min(dic.3, na.rm=T) - min(dic.4, na.rm=T) >4 #difference in dic between previous variable and this must be >4
beep()

sub4 <- collinear[,colnames(collinear)==var4] #subset just the chosen variable
test_df4 <- data.frame(names = var_names,
                       values = sub4)
test_df4 <- test_df4[test_df4$names %in% var_names4, ]
test_df4[which(test_df4$values<0.6 & test_df4$values>-0.6),1] #names of the variables not colinear to be included in next round
test_df4[which(test_df4$values>0.6 | test_df4$values < -0.6),1] #names of the variables colinear not to be included in next round

a<-test_df4[which(test_df4$values<0.6 & test_df4$values > -0.6),1] #names of the variables not colinear to be included in next round
b<-data.frame(a=a,b=1:length(a))
b



## choice 5
op1.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                
                temp_med_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))


op2.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                
                temp_min_lag1,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))


op3.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                
                temp_max_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op4.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                
                hum_med ,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op5.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                
                hum_med_lag1 ,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op6.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                
                hum_med_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op7.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                
                hum_med_lag3,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op8.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                
                hum_min,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op9.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                
                hum_min_lag1,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op10.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                 
                 hum_min_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op11.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                 
                 hum_min_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op12.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                 
                 hum_max,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op13.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                 
                 hum_max_lag1,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op14.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                 
                 hum_max_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op15.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                 
                 hum_max_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op16.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                 
                 prec ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op17.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                 
                 prec_lag1,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op18.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                 
                 prec_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op19.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                 
                 prec_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op20.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                 
                 biodiversity ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op21.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                 
                 land_use_1 ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op22.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                 
                 land_use_2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op23.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                 
                 land_use_3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op24.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                 
                 land_use_4,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op25.5 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") +
                 
                 land_use_5,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))


dic.5 <- c(op1.5$dic$dic,op2.5$dic$dic,op3.5$dic$dic,op4.5$dic$dic,op5.5$dic$dic,op6.5$dic$dic,
           op7.5$dic$dic,op8.5$dic$dic,op9.5$dic$dic,op10.5$dic$dic,op11.5$dic$dic,op12.5$dic$dic,
           op13.5$dic$dic,op14.5$dic$dic,op15.5$dic$dic,op16.5$dic$dic,op17.5$dic$dic,op18.5$dic$dic,
           op19.5$dic$dic,op20.5$dic$dic,op21.5$dic$dic,op22.5$dic$dic,op23.5$dic$dic,op24.5$dic$dic,
           op25.5$dic$dic) #store dic

var_names5 <- b$a
var5 <- var_names5[which(dic.5==min(dic.5, na.rm=T))] #which variable had lowest dic
var5
min(dic.4, na.rm=T) - min(dic.5, na.rm=T) >4 #difference in dic between previous variable and this must be >4

beep()

sub5 <- collinear[,colnames(collinear)==var5] #subset just the chosen variable
test_df5 <- data.frame(names = var_names,
                       values = sub5)
test_df5 <- test_df5[test_df5$names %in% var_names5, ]
test_df5[which(test_df5$values<0.6 & test_df5$values>-0.6),1] #names of the variables not colinear to be included in next round
test_df5[which(test_df5$values>0.6 | test_df5$values < -0.6),1] #names of the variables colinear not to be included in next round

a<-test_df5[which(test_df5$values<0.6 & test_df5$values > -0.6),1] #names of the variables not colinear to be included in next round
b<-data.frame(a=a,b=1:length(a))
b






## choice 6
op1.6 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") + hum_max +
                
                temp_med_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))


op2.6 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") + hum_max +
                
                temp_min_lag1,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))


op3.6 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") + hum_max +
                
                temp_max_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))


op4.6 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") + hum_max +
                
                hum_med_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op5.6 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") + hum_max +
                
                hum_med_lag3,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))


op6.6 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") + hum_max +
                
                hum_min_lag1,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op7.6 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") + hum_max +
                 
                 hum_min_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op8.6 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") + hum_max +
                 
                 hum_min_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))


op9.6 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") + hum_max +
                 
                 prec ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op10.6 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") + hum_max +
                 
                 prec_lag1,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op11.6 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") + hum_max +
                 
                 prec_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op12.6 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") + hum_max +
                 
                 prec_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op13.6 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") + hum_max +
                 
                 biodiversity ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op14.6 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") + hum_max +
                 
                 land_use_1 ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op15.6 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") + hum_max +
                 
                 land_use_2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op16.6 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") + hum_max +
                 
                 land_use_3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op17.6 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") + hum_max +
                 
                 land_use_4,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op18.6 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid") + hum_max +
                 
                 land_use_5,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))


dic.6 <- c(op1.6$dic$dic,op2.6$dic$dic,op3.6$dic$dic,op4.6$dic$dic,op5.6$dic$dic,op6.6$dic$dic,
           op7.6$dic$dic,op8.6$dic$dic,op9.6$dic$dic,op10.6$dic$dic,op11.6$dic$dic,op12.6$dic$dic,
           op13.6$dic$dic,op14.6$dic$dic,op15.6$dic$dic,op16.6$dic$dic,op17.6$dic$dic,op18.6$dic$dic) #store dic

var_names6 <- b$a
var6 <- var_names6[which(dic.6==min(dic.6, na.rm=T))] #which variable had lowest dic
var6
min(dic.5, na.rm=T) - min(dic.6, na.rm=T) >4 #difference in dic between previous variable and this must be >4

beep()
sub6 <- collinear[,colnames(collinear)==var6] #subset just the chosen variable
test_df6 <- data.frame(names = var_names,
                       values = sub6)
test_df6 <- test_df6[test_df6$names %in% var_names6, ]
test_df6[which(test_df6$values<0.6 & test_df6$values>-0.6),1] #names of the variables not colinear to be included in next round
test_df6[which(test_df6$values>0.6 | test_df6$values < -0.6),1] #names of the variables colinear not to be included in next round

a<-test_df6[which(test_df6$values<0.6 & test_df6$values > -0.6),1] #names of the variables not colinear to be included in next round
b<-data.frame(a=a,b=1:length(a))
b



## choice 7
op1.7 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 +
                temp_max_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))


op2.7 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 +
                hum_med_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op3.7 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 +
                hum_med_lag3,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))


op4.7 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 +
                hum_min_lag1,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op5.7 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 +
                hum_min_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op6.7 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 +
                hum_min_lag3,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))


op7.7 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 +
                prec ,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op8.7 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 +
                 prec_lag1,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op9.7 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 +
                 prec_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op10.7 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 +
                 prec_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op11.7 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 +
                 biodiversity ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op12.7 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 +
                 land_use_1 ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op13.7 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 +
                 land_use_2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op14.7 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 +
                 land_use_3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op15.7 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 +
                 land_use_4,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op16.7 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 +
                 land_use_5,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

dic.7 <- c(op1.7$dic$dic,op2.7$dic$dic,op3.7$dic$dic,op4.7$dic$dic,op5.7$dic$dic,op6.7$dic$dic,
           op7.7$dic$dic,op8.7$dic$dic,op9.7$dic$dic,op10.7$dic$dic,op11.7$dic$dic,op12.7$dic$dic,
           op13.7$dic$dic,op14.7$dic$dic,op15.7$dic$dic,op16.7$dic$dic) #store dic

var_names7 <- b$a
var7 <- var_names7[which(dic.7==min(dic.7, na.rm=T))] #which variable had lowest dic
var7
min(dic.6, na.rm=T) - min(dic.7, na.rm=T) >4 #difference in dic between previous variable and this must be >4

beep()
sub7 <- collinear[,colnames(collinear)==var7] #subset just the chosen variable
test_df7 <- data.frame(names = var_names,
                       values = sub7)
test_df7 <- test_df7[test_df7$names %in% var_names7, ]
test_df7[which(test_df7$values<0.6 & test_df7$values>-0.6),1] #names of the variables not colinear to be included in next round
test_df7[which(test_df7$values>0.6 | test_df7$values < -0.6),1] #names of the variables colinear not to be included in next round

a<-test_df7[which(test_df7$values<0.6 & test_df7$values > -0.6),1] #names of the variables not colinear to be included in next round
b<-data.frame(a=a,b=1:length(a))
b



## choice 8
op1.8 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 + 
                temp_max_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))


op2.8 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 +
                hum_med_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op3.8 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 +
                hum_med_lag3,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))


op4.8 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 +
                hum_min_lag1,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op5.8 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 +
                hum_min_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op6.8 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 +
                hum_min_lag3,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))


op7.8 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 +
                prec ,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op8.8 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 +
                prec_lag1,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op9.8 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 +
                prec_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op10.8 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 +
                 prec_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op11.8 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 +
                 biodiversity ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op12.8 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 +
                 land_use_1 ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op13.8 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 +
                 land_use_3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op14.8 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 +
                 land_use_4,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

dic.8 <- c(op1.8$dic$dic,op2.8$dic$dic,op3.8$dic$dic,op4.8$dic$dic,op5.8$dic$dic,op6.8$dic$dic,
           op7.8$dic$dic,op8.8$dic$dic,op9.8$dic$dic,op10.8$dic$dic,op11.8$dic$dic,op12.8$dic$dic,
           op13.8$dic$dic,op14.8$dic$dic) #store dic

var_names8 <- b$a
var8 <- var_names8[which(dic.8==min(dic.8, na.rm=T))] #which variable had lowest dic
var8
min(dic.7, na.rm=T) - min(dic.8, na.rm=T) >4 #difference in dic between previous variable and this must be >4

beep()
sub8 <- collinear[,colnames(collinear)==var8] #subset just the chosen variable
test_df8 <- data.frame(names = var_names,
                       values = sub8)
test_df8 <- test_df8[test_df8$names %in% var_names8, ]
test_df8[which(test_df8$values<0.6 & test_df8$values>-0.6),1] #names of the variables not colinear to be included in next round
test_df8[which(test_df8$values>0.6 | test_df8$values < -0.6),1] #names of the variables colinear not to be included in next round

a<-test_df8[which(test_df8$values<0.6 & test_df8$values > -0.6),1] #names of the variables not colinear to be included in next round
b<-data.frame(a=a,b=1:length(a))
b


## choice 9 
op1.9 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 +  temp_max_lag2 +
                hum_med_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op2.9 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 +  temp_max_lag2 +
                hum_med_lag3,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))


op3.9 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 +
                hum_min_lag1,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op4.9 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 +
                hum_min_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op5.9 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 +
                hum_min_lag3,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))


op6.9 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 +
                prec ,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op7.9 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 +
                prec_lag1,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op8.9 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 +
                prec_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op9.9 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 +
                 prec_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op10.9 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 +
                 biodiversity ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op11.9 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 +
                 land_use_1 ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op12.9 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2+ temp_max_lag2 +
                 land_use_3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op13.9 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 +  temp_max_lag2 +
                 land_use_4,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))


dic.9 <- c(op1.9$dic$dic,op2.9$dic$dic,op3.9$dic$dic,op4.9$dic$dic,op5.9$dic$dic,op6.9$dic$dic,
           op7.9$dic$dic,op8.9$dic$dic,op9.9$dic$dic,op10.9$dic$dic,op11.9$dic$dic,op12.9$dic$dic,
           op13.9$dic$dic) #store dic

beepr::beep()
var_names9 <- b$a
var9 <- var_names9[which(dic.9==min(dic.9, na.rm=T))] #which variable had lowest dic
var9
min(dic.8, na.rm=T) - min(dic.9, na.rm=T) >4 #difference in dic between previous variable and this must be >4


sub9 <- collinear[,colnames(collinear)==var9] #subset just the chosen variable
test_df9 <- data.frame(names = var_names,
                       values = sub9)
test_df9 <- test_df9[test_df9$names %in% var_names9, ]
test_df9[which(test_df9$values<0.6 & test_df9$values>-0.6),1] #names of the variables not colinear to be included in next round
test_df9[which(test_df9$values>0.6 | test_df9$values < -0.6),1] #names of the variables colinear not to be included in next round

a<-test_df9[which(test_df9$values<0.6 & test_df9$values > -0.6),1] #names of the variables not colinear to be included in next round
b<-data.frame(a=a,b=1:length(a))
b



## choice 10 
op1.10 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 +  temp_max_lag2 + biodiversity +
                hum_med_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op2.10 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 +  temp_max_lag2 + biodiversity +
                hum_med_lag3,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))


op3.10 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity +
                hum_min_lag1,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op4.10 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity +
                hum_min_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op5.10 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity +
                hum_min_lag3,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))


op6.10 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity +
                prec ,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op7.10 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity +
                prec_lag1,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op8.10 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity +
                prec_lag2,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op9.10 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity +
                prec_lag3,
              data = inla_df_norm,
              family="gaussian",
              control.compute = list(dic = TRUE))

op10.10 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity +
                 land_use_1 ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op11.10 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2+ temp_max_lag2 + biodiversity +
                 land_use_3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op12.10 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 +  temp_max_lag2 + biodiversity +
                 land_use_4,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))
dic.10 <- c(op1.10$dic$dic,op2.10$dic$dic,op3.10$dic$dic,op4.10$dic$dic,op5.10$dic$dic,op6.10$dic$dic,
           op7.10$dic$dic,op8.10$dic$dic,op9.10$dic$dic,op10.10$dic$dic,op11.10$dic$dic,op12.10$dic$dic) #store dic

beepr::beep()
var_names10 <- b$a
var10 <- var_names10[which(dic.10==min(dic.10, na.rm=T))] #which variable had lowest dic
var10
min(dic.9, na.rm=T) - min(dic.10, na.rm=T) >4 #difference in dic between previous variable and this must be >4


sub10 <- collinear[,colnames(collinear)==var10] #subset just the chosen variable
test_df10 <- data.frame(names = var_names,
                       values = sub10)
test_df10 <- test_df10[test_df10$names %in% var_names10, ]
test_df10[which(test_df10$values<0.6 & test_df10$values>-0.6),1] #names of the variables not colinear to be included in next round
test_df10[which(test_df10$values>0.6 | test_df10$values < -0.6),1] #names of the variables colinear not to be included in next round

a<-test_df10[which(test_df10$values<0.6 & test_df10$values > -0.6),1] #names of the variables not colinear to be included in next round
b<-data.frame(a=a,b=1:length(a))
b



## choice 11 
op1.11 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 +  temp_max_lag2 + biodiversity +  land_use_3 +
                 hum_med_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op2.11 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 +  temp_max_lag2 + biodiversity + land_use_3 +
                 hum_med_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))


op3.11 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity + land_use_3 +
                 hum_min_lag1,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op4.11 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity + land_use_3 +
                 hum_min_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op5.11 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity + land_use_3 +
                 hum_min_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))


op6.11 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity + land_use_3 +
                 prec ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op7.11 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity + land_use_3 +
                 prec_lag1,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op8.11 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity + land_use_3 +
                 prec_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op9.11 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity + land_use_3 +
                 prec_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op10.11 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                  hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity + land_use_3 +
                  land_use_1 ,
                data = inla_df_norm,
                family="gaussian",
                control.compute = list(dic = TRUE))


op11.11 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                  hum_max + temp_min_lag1 + land_use_2 +  temp_max_lag2 + biodiversity + land_use_3 +
                  land_use_4,
                data = inla_df_norm,
                family="gaussian",
                control.compute = list(dic = TRUE))

dic.11 <- c(op1.11$dic$dic,op2.11$dic$dic,op3.11$dic$dic,op4.11$dic$dic,op5.11$dic$dic,op6.11$dic$dic,
            op7.11$dic$dic,op8.11$dic$dic,op9.11$dic$dic,op10.11$dic$dic,op11.11$dic$dic) #store dic

beepr::beep()
var_names11 <- b$a
var11 <- var_names11[which(dic.11==min(dic.11, na.rm=T))] #which variable had lowest dic
var11
min(dic.10, na.rm=T) - min(dic.11, na.rm=T) >4 #difference in dic between previous variable and this must be >4


sub11 <- collinear[,colnames(collinear)==var11] #subset just the chosen variable
test_df11 <- data.frame(names = var_names,
                        values = sub11)
test_df11 <- test_df11[test_df11$names %in% var_names11, ]
test_df11[which(test_df11$values<0.6 & test_df11$values>-0.6),1] #names of the variables not colinear to be included in next round
test_df11[which(test_df11$values>0.6 | test_df11$values < -0.6),1] #names of the variables colinear not to be included in next round

a<-test_df11[which(test_df11$values<0.6 & test_df11$values > -0.6),1] #names of the variables not colinear to be included in next round
b<-data.frame(a=a,b=1:length(a))
b


#### add in choice 12

op1.12 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 +  temp_max_lag2 + biodiversity +  land_use_3 +
                 prec +
                 hum_med_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op2.12 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 +  temp_max_lag2 + biodiversity + land_use_3 +
                 prec +
                 hum_med_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))


op3.12 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity + land_use_3 +
                 prec +
                 hum_min_lag1,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op4.12 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity + land_use_3 +
                 prec +
                 hum_min_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op5.12 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity + land_use_3 +
                 prec +
                 hum_min_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))


op6.12 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity + land_use_3 +
                 prec +
                 prec_lag1,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op7.12 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity + land_use_3 +
                 prec +
                 prec_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op8.12 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity + land_use_3 +
                 prec +
                 prec_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op9.12 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                  hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity + land_use_3 +
                  prec +
                  land_use_1 ,
                data = inla_df_norm,
                family="gaussian",
                control.compute = list(dic = TRUE))


op10.12 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                  hum_max + temp_min_lag1 + land_use_2 +  temp_max_lag2 + biodiversity + land_use_3 +
                  prec +
                  land_use_4,
                data = inla_df_norm,
                family="gaussian",
                control.compute = list(dic = TRUE))

dic.12 <- c(op1.12$dic$dic,op2.12$dic$dic,op3.12$dic$dic,op4.12$dic$dic,op5.12$dic$dic,op6.12$dic$dic,
            op7.12$dic$dic,op8.12$dic$dic,op9.12$dic$dic,op10.12$dic$dic) #store dic

beepr::beep()
var_names12 <- b$a
var12 <- var_names12[which(dic.12==min(dic.12, na.rm=T))] #which variable had lowest dic
var12
min(dic.11, na.rm=T) - min(dic.12, na.rm=T) >4 #difference in dic between previous variable and this must be >4


sub12 <- collinear[,colnames(collinear)==var12] #subset just the chosen variable
test_df12 <- data.frame(names = var_names,
                        values = sub12)
test_df12 <- test_df12[test_df12$names %in% var_names12, ]
test_df12[which(test_df12$values<0.6 & test_df12$values>-0.6),1] #names of the variables not colinear to be included in next round
test_df12[which(test_df12$values>0.6 | test_df12$values < -0.6),1] #names of the variables colinear not to be included in next round

a<-test_df12[which(test_df12$values<0.6 & test_df12$values > -0.6),1] #names of the variables not colinear to be included in next round
b<-data.frame(a=a,b=1:length(a))
b



### choice 13


op1.13 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 +  temp_max_lag2 + biodiversity +  land_use_3 +
                 prec + prec_lag1 +
                 hum_med_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op2.13 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 +  temp_max_lag2 + biodiversity + land_use_3 +
                 prec +prec_lag1 +
                 hum_med_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))


op3.13 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity + land_use_3 +
                 prec +prec_lag1 +
                 hum_min_lag1,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op4.13 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity + land_use_3 +
                 prec +prec_lag1 +
                 hum_min_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op5.13 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity + land_use_3 +
                 prec +prec_lag1 +
                 hum_min_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op6.13 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity + land_use_3 +
                 prec +prec_lag1 +
                 prec_lag2,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op7.13 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity + land_use_3 +
                 prec +prec_lag1 +
                 prec_lag3,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))

op8.13 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                 hum_max + temp_min_lag1 + land_use_2 + temp_max_lag2 + biodiversity + land_use_3 +
                 prec +prec_lag1 +
                 land_use_1 ,
               data = inla_df_norm,
               family="gaussian",
               control.compute = list(dic = TRUE))


op9.13 <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                  hum_max + temp_min_lag1 + land_use_2 +  temp_max_lag2 + biodiversity + land_use_3 +
                  prec +prec_lag1 +
                  land_use_4,
                data = inla_df_norm,
                family="gaussian",
                control.compute = list(dic = TRUE))

dic.13 <- c(op1.13$dic$dic,op2.13$dic$dic,op3.13$dic$dic,op4.13$dic$dic,op5.13$dic$dic,op6.13$dic$dic,
            op7.13$dic$dic,op8.13$dic$dic,op9.13$dic$dic) #store dic

beepr::beep()
var_names13 <- b$a
var13 <- var_names13[which(dic.13==min(dic.13, na.rm=T))] #which variable had lowest dic
var13
min(dic.12, na.rm=T) - min(dic.13, na.rm=T) >4 #difference in dic between previous variable and this must be >4

summary(op11.13)

sub13 <- collinear[,colnames(collinear)==var13] #subset just the chosen variable
test_df13 <- data.frame(names = var_names,
                        values = sub13)
test_df13 <- test_df13[test_df13$names %in% var_names13, ]
test_df13[which(test_df13$values<0.6 & test_df13$values>-0.6),1] #names of the variables not colinear to be included in next round
test_df13[which(test_df13$values>0.6 | test_df13$values < -0.6),1] #names of the variables colinear not to be included in next round

a<-test_df13[which(test_df13$values<0.6 & test_df13$values > -0.6),1] #names of the variables not colinear to be included in next round
b<-data.frame(a=a,b=1:length(a))
b



### not smaller so don't keep going, best model:



op_best <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + temp_med + temp_min_lag3 + f(avian_WNV, model="iid")  +
                  hum_max + temp_min_lag1 + land_use_2 +  temp_max_lag2 + biodiversity + land_use_3 +
                  prec +prec_lag1,
                data = inla_df_norm,
                family="gaussian",
                control.compute = list(dic = TRUE))

summary(op_best)


## add in RE

op_best <- inla(mosq_prev_emplogit ~ 1 + prev_lag1 + 
                  temp_med + 
                  temp_min_lag3 +
                  #f(avian_WNV, model="iid")  +
                  hum_max + temp_min_lag1 + 
                  land_use_2 + 
                  temp_max_lag2+
                 biodiversity +
                 #land_use_3 +
                  prec +
                  prec_lag1+
                
                  f(trap_code, model = "iid"),# +
                  #f(week, model = 'iid'),
                data = inla_df_norm,
                family="gaussian",
                control.compute = list(dic = TRUE))
summary(op_best)

df <- op_best$summary.hyperpar
df$precision <- df$mean
df$precision_lowCI <- df$`0.025quant`
df$precision_highCI <- df$`0.975quant`
precision_random_effects <- df
precision_random_effects


op_best$summary.random$avian_WNV

#saveRDS(op_best, "221005_inla_model_cov_spatial.RDS")

# Check if model outputs look reasonable
obs_vs_exp <- ggplot() +
  geom_point(aes(x = inla_df_norm$mosq_prev_emplogit, 
                 y = op_best$summary.fitted.values$mean,
                 #col=inla_df_norm$trap_code)) 
))+
 geom_errorbar(aes(x = inla_df_norm$mosq_prev_emplogit, 
                 ymin = op_best$summary.fitted.values$'0.025quant', 
                 ymax = op_best$summary.fitted.values$'0.975quant'), alpha = 0.1) +
  geom_abline(slope=1)+
  ggtitle("Model Precision (all data)") +
  xlab("Observed values") +
  ylab("Expected values") +
  theme_classic()
obs_vs_exp


### Plot spatial regression coefficients
coords <- read.csv("trap_coordinates.csv")

# PLOT IN ORDER OF LONGITUDE
random <- op_best$summary.random$trap_code #output from INLA model
coeff_plot_df <- data.frame(ID = coords$trap_code,
                            longitude = coords$x)
coeff_plot_df$coeff <- random$mean 
coeff_plot_df$quant2.5 <- random[,4]
coeff_plot_df$quant97.5 <- random[,6] #colnames "ID" "longitude" "coeff" "quant2.5" "quant97.5"
coeff_plot_df <- coeff_plot_df %>%
  mutate(ID = fct_reorder(ID, longitude))

ggplot(coeff_plot_df) + geom_point(aes(x=ID, y= coeff), col= "blue") +
  theme_bw() + 
  geom_errorbar(aes(x=ID, ymin = quant2.5, ymax= quant97.5), 
                alpha = 0.3, col="blue", size=1)+
  labs( x="Trap", y = "Regression Coefficient")+
  geom_smooth(aes(x=as.numeric(ID), y=coeff), method="lm") +
  scale_y_continuous(breaks=c(-0.5,0.0,0.5)) +
  theme(axis.text.x = element_text(angle=65, hjust=1, size=8),
        axis.title = element_text(size=15),
        axis.text.y = element_text(size=15))

# PLOT IN ORDER OF LATITUDE
random <- op_best$summary.random$trap_code #output from INLA model
coeff_plot_df2 <- data.frame(ID = coords$trap_code,
                             latitude = coords$y)
coeff_plot_df2$coeff <- random$mean 
coeff_plot_df2$quant2.5 <- random[,4]
coeff_plot_df2$quant97.5 <- random[,6] #colnames "ID" "longitude" "coeff" "quant2.5" "quant97.5"
coeff_plot_df2 <- coeff_plot_df2 %>%
  mutate(ID = fct_reorder(ID, latitude))

ggplot(coeff_plot_df2) + geom_point(aes(x=ID, y= coeff), col= "blue") +
  theme_bw() + 
  geom_errorbar(aes(x=ID, ymin = quant2.5, ymax= quant97.5), 
                alpha = 0.3, col="blue", size=1)+
  labs( x="Trap", y = "Regression Coefficient")+
  geom_smooth(aes(x=as.numeric(ID), y=coeff), method="lm") +
  scale_y_continuous(breaks=c(-0.5,0.0,0.5)) +
  theme(axis.text.x = element_text(angle=65, hjust=1, size=8),
        axis.title = element_text(size=15),
        axis.text.y = element_text(size=15))

# PLOT ONTO MAP
random <- op_best$summary.random$trap_code #output from INLA model
coeff_plot_df3 <- data.frame(ID = coords$trap_code,
                             longitude = coords$x,
                             latitude = coords$y)
coeff_plot_df3$coeff <- random$mean 
coeff_plot_df3$quant2.5 <- random[,4]
coeff_plot_df3$quant97.5 <- random[,6] #colnames "ID" "longitude" "coeff" "quant2.5" "quant97.5"

library("raster")                   # Needed for getData()
library("sf")
italy1 <- getData('GADM',country='ITA',level=1)
italy1_sf <- st_as_sf(italy1)

g <- ggplot() +
  geom_sf(data = italy1_sf, fill = "grey") +
  #  geom_sf(data = RIVER, color = "blue4") +
  geom_point(data = coeff_plot_df3, aes(x=longitude, y=latitude, color = coeff), size = 5) +
  #  geom_point(data = coeff_plot_df3, aes(x=longitude, y=latitude), shape = 1, size=2.5, color = "black") +
  scale_color_gradient2(low = "darkblue", mid = "white", high = "red", midpoint = 0) +
  coord_sf(xlim = c(9.49, 12.5), ylim = c(44.35, 45.35), expand = FALSE) +
  scale_x_continuous(breaks = seq(9.5, 12.5, by = 1)) +
  scale_y_continuous(breaks = seq(44, 45.5, by = 0.5)) +
  xlab("Longitude") + ylab("Latitude") +
  theme(panel.background = element_rect(fill = "white"))
g


##plot temporal random effect
random <- op_best$summary.random$week 
random$num <- 1:nrow(random)
random$week_in_year <- substring(random$ID,7,8)
ggplot(random) + geom_point(aes(x=num, y= mean, col= week_in_year),size=3) +
  theme_bw() + 
  geom_line(aes(x=num, y= mean))





### Prediction model ###------
inla_df_norm$year <-substring(inla_df_norm$week,1,4)#add year column
predict_df <- inla_df_norm

unique(predict_df$week)
predict_df$mosq_prevalence[which(predict_df$week == '2018-W42')] <- NA

op_predict <- inla(mosq_prevalence ~ 1 +
                  temp_med +
                  temp_med_lag123 +
                  f(trap_code, model = "iid"),# +
                  #f(week, model = 'iid'),
                data = predict_df,
                family="beta",
                control.compute = list(dic = TRUE))
observed <- inla_df_norm[which(inla_df_norm$week == '2018-W42'),3] #observed data from 2018
predicted <- op_predict$summary.fitted.values[((length(op_predict$summary.fitted.values$mean)-length(observed))+1):length(op_predict$summary.fitted.values$mean),c(1,3,5)] #only model outputs from 2018
length(observed) == length(predicted) # check worked okay

observed
predicted$mean

predict_df$predicted[predict_df$week=='2018-W42'] <- predicted$mean
prediction <- ggplot(predict_df) +
              geom_point(aes(x = week, 
                            y = mosq_prevalence), col="red") +
  #geom_ribbon(aes(x = time, ymin = predicted$'0.025quant', 
            #      ymax = predicted$'0.975quant'), col="red",alpha = 0.1) +
  geom_point(x=week, y=predicted)+
  ggtitle("Model Precision (predicted data)") +
  #xlab("Observed values") +
  #ylab("Expected values") +
  theme_classic()
prediction

