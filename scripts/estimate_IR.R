install_github("CDCgov/PooledInfRate")
library(PooledInfRate)

trap_data<- readRDS("data/mosquito_data.RDS")

## aggregate pools of the same size
trap_data$pool_count<-1
agg<-aggregate.data.frame(list(as.numeric(as.character(trap_data$WNV.test)),
                               trap_data$pool_count),
                          by=list(trap_data$trap_code,
                                  trap_data$week, 
                                  trap_data$pool.size), FUN=sum)
colnames(agg)<-c("trap","week","pool_size","pos","n_pools")


## set storage for end of loop
store <- data.frame(week = rep(unique(trap_data$week),
                               times = length(unique(trap_data$trap_code))),
                    trap = rep(unique(trap_data$trap_code),each = length(unique(trap_data$week))),
                    IR=NA,IR_low=NA, IR_upp=NA)
w<- unique(agg$week)[4]
trap<- unique(agg$trap)[4]

## loop
for(w in unique(agg$week)){
  
  sub2<-agg[agg$week==w,]
  TOT2<- sum(sub2$pool_size*sub2$n_pools)
  av_trap_night <- TOT2 / length(unique(sub2$trap))
  
  for(trap in unique(agg$trap)){
    
    if(trap%in%sub2$trap){
      sub<-agg[agg$week==w & agg$trap==trap,]
      
      
      ## IR ######################
      pool.dat <- data.frame(Species = "Cx. pipiens",
                             Pos = sub$pos,
                             PoolSize = sub$pool_size, 
                             NumPools = sub$n_pools)
      vpir.combined.out <- pIR(Pos ~ m(PoolSize) + n(NumPools), 
                               pt.method = "firth",
                               scale=1,
                               pool.dat)
      summary(vpir.combined.out)
      store$IR[store$week==w & store$trap==trap]<-vpir.combined.out$P
      store$IR_low[store$week==w & store$trap==trap]<-vpir.combined.out$Lower
      store$IR_upp[store$week==w & store$trap==trap]<-vpir.combined.out$Upper
      
    }
  }
  print(w)
}

store$year <- substring(store$week,1,4)
store$week_no<-substring(store$week,7,8)

saveRDS(store, "data/estimated_IR.RDS")