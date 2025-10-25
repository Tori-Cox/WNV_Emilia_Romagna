## comparing final model output with human data

plot_comparison_human_model <- function(res, data, human_data){
  
max_week <-39 
min_week <-18 

## prepare mosquito estimate metrics -------------------------------------------------

# model fit
t<-plogis(res$summary.linear.predictor$mean[idx.y]) 
t2<-plogis(res$summary.linear.predictor$mean[idx.z])

model_fit <- data.frame(exp=t2*t,
                        obvs = data$IR,
                        trap = data$trap,
                        year = as.numeric(data$year),
                        week = as.numeric(data$week_no))

store_m_traps<-data.frame(year=2013:2022, 
                          m_week_peak=NA,
                          m_week_peak_n=NA)
store_timeseries_traps <- NULL
store_est_mosq<-NULL
for(year_i in 2013:2022){
  
  model_fit |> 
    filter(year==year_i)|>
    filter(!is.na(obvs))-> model_fit_sub
  
  ## METRICS 
  # WEEK OF PEAK and peak value
  model_fit_sub |>
    filter(week >= min_week & week <= max_week)|>
    group_by(week)|>
    reframe(exp=median(exp))|>
    filter(exp==max(exp)) -> week_peak
  
  model_fit_sub |>
    filter(week >= min_week & week <= max_week)|>
    group_by(week)|>
    reframe(exp=median(exp))|>mutate(year=year_i) -> weekly_timeseries
  
  
  # SMOOTHED TIMESERIES PEAK CASES
  weekly_timeseries |>
    complete(week = seq(from=min_week, to=max_week, by=1))|>
    dplyr::mutate(exp=case_when(is.na(exp)~0, .default=exp), year=year_i)->comp
  
  smoothed_ts2 <- loess(comp$exp ~ seq_along(comp$exp), span = 0.5)$fitted
  data.frame(smooth = smoothed_ts2, 
             week = comp$week, 
             year=year_i)|>
    mutate(smooth=case_when(smooth<0~0,.default=smooth))->smoothed_ts2
  store_timeseries_traps<-rbind(store_timeseries_traps,smoothed_ts2)
  
  smoothed_ts2 |>
    filter(week >= min_week & week <= max_week)|>
    filter(smooth==max(smooth, na.rm=T)) -> week_peak_smooth
  
  store_m_traps$m_week_peak[store_m_traps$year==year_i] <- week_peak$week
  store_m_traps$m_week_peak_n[store_m_traps$year==year_i] <- week_peak$exp
  store_m_traps$m_week_peak_smooth[store_m_traps$year==year_i] <- week_peak_smooth$week
  store_m_traps$m_week_peak_n_smooth[store_m_traps$year==year_i] <- week_peak_smooth$smooth
  
  store_est_mosq<-rbind(store_est_mosq, model_fit_sub)
}


## prepare human data metrics --------------------------------------------------------

store_timeseries<-NULL
store<-data.frame(year=2013:2022, h_week_first=NA, h_week_peak=NA, h_week_peak_n=NA, h_total=NA, h_week_peak_smooth=NA)

for(year_i in 2013:2022){
  
  human_data|>
    dplyr::select(year, week_no, n)|>
    dplyr::filter(year==year_i)|>
    dplyr::filter(week_no<=max_week)|>
    dplyr::mutate(week_no=as.numeric(week_no))|>
    complete(week_no = seq(from=min_week, to=max_week, by=1))|>
    dplyr::mutate(n=case_when(is.na(n)~0, .default=n), year=year_i)->human_data_sub
  
  human_data_sub|>
    dplyr::select(n)|>as.vector()-> time_series1
  
  ## METRICS NOT SMOOTHED
  # WEEK OF FIRST CASE
  human_data_sub |>
    filter(n>0)|>
    filter(week_no==min(week_no))|>
    dplyr::select(week_no) -> week_first
  
  # WEEK OF PEAK CASE
  human_data_sub |>
    filter(n>0)|>
    filter(n==max(n)) -> week_peak
  
  # TOTAL CASES
  sum(human_data_sub$n) -> total_cases
  
  ## SMOOTHED TIMESERIES
  smoothed_ts1 <- loess(time_series1$n ~ seq_along(time_series1$n), span = 0.5)$fitted
  data.frame(smooth=smoothed_ts1, 
             week = min_week:max_week, year=year_i)|>
    mutate(smooth=case_when(smooth<0~0,.default=smooth))->smoothed_ts1
  
  # PEAK CASES ON SMOOTHED
  smoothed_ts1 |>
    filter(smooth==max(smooth, na.rm=T)) -> week_peak_smooth
  
  store$h_week_first[store$year==year_i] <- week_first$week_no
  store$h_week_peak[store$year==year_i] <- week_peak$week_no
  store$h_week_peak_n[store$year==year_i] <- week_peak$n
  store$h_total[store$year==year_i] <- total_cases
  store$h_week_peak_smooth[store$year==year_i] <- week_peak_smooth$week
  
  store_timeseries<-rbind(store_timeseries,smoothed_ts1)
}

M <- (cor(store[c(2,3,6,4,5)], store_m_traps[c(2,7,3,8)]))

colnames(M) <- c("1M\nPeak week", "2M\nPeak week\n(smoothed)",
                 "3M\nValue at peak", "4M\nValue at peak\n(smoothed)")
rownames(M) <- c("1H\nWeek first case",
                 "2H\nPeak week", "3H\nPeak week\n(smoothed)",
                 "4H\nCases at peak", 
                 "5H\nTotal cases")
custom_colors <- colorRampPalette(c("blue","lightblue", "white","white","pink", "red"))(300)


## correlation on annual timeseries
ccf_list<-list()
ccf_data<-data.frame(Year= 2013:2022, lag0=NA, lag1=NA, lag2=NA, lag3=NA,lag4=NA, lag5=NA, lag6=NA)
for(year_i in 1:10){
  ccf_list[[year_i]] <- ccf(store_timeseries$smooth[store_timeseries$year==year_i+2012], 
                            store_timeseries_traps$smooth[store_timeseries_traps$year==year_i+2012], 
                            
                            lag.max = 6, plot = TRUE)
  ccf_data[ccf_data$Year==year_i+2012, 2:8] <- ccf_list[[year_i]]$acf[,,][7:13]
}

ccf_data |>
  pivot_longer(cols=2:8)|>
  ggplot() +
  geom_tile(aes(name, as.character(Year), fill = (value)))+
  labs(x="Lag (Weeks)",y="Year", fill="Correlation")+
  theme_minimal()+
  scale_fill_viridis_c(na.value="white",guide = guide_colorbar(direction = "horizontal",
                                                               title.position = "top", barwidth=10))+
  theme(text=element_text(size=15),
        legend.position = "bottom",
        legend.title = element_text(hjust=0.5)) -> corr_plot_h

### plots
plots_store<-list()
temporary_store<-NULL
for(year_i in 2013:2022){
  
  human_data|>
    dplyr::select(year, week_no, n)|>
    dplyr::filter(year==year_i)|>
    dplyr::filter(week_no<=max_week)|>
    dplyr::mutate(week_no=as.numeric(week_no))|>
    complete(week_no = seq(from=min_week, to=max_week, by=1))|>
    dplyr::mutate(n=case_when(is.na(n)~0, .default=n), year=year_i)|>
    dplyr::select(n)|>as.vector()-> time_series1
  
  store_est_mosq|>
    dplyr::filter(year==year_i)|>
    dplyr::filter(week<=max_week)|>
    rename(week_no=week)|>
    dplyr::mutate(week_no=as.numeric(week_no),type="Model output")|>
    dplyr::select(exp, week_no,type)->time_series_est
  
  store_timeseries |> filter(year==year_i)|>
    mutate(type="Human cases",exp=NA)|>cbind(time_series1)->store_timeseries2
  
  store_timeseries_traps|>
    mutate(type="Model output") |>
    filter(year==year_i)|>
    mutate(n=0)|>
    left_join(time_series_est|>
                rename(week=week_no))|>
    rbind(store_timeseries2)|>
    ggplot()+facet_grid("type", scales="free_y")+
    geom_col(aes(x=as.numeric(week), y=n, fill=type), alpha=0.3)+
    geom_point(#data=time_series_est, 
      aes(x=as.numeric(week), 
          y=exp),col="lightblue", alpha=0.5)+
    geom_line(aes(x=as.numeric(week), y=smooth, col=type), linewidth=1)+
    theme_minimal()+
    labs(x="Week", 
         y=NULL, title=year_i, 
         fill=NULL, col=NULL)+
    theme(text=element_text(size=10),
          legend.position="none",
          plot.title = element_text(hjust=0.5),
          strip.text = element_blank())+
    scale_y_continuous(labels = scales::label_number(digits = 2))+
    coord_cartesian(xlim=c(24,40))-> plots_store[[year_i-2012]]
  
  store_timeseries_traps|>
    mutate(type="Mosquito risk") |>
    filter(year==year_i)|>
    mutate(n=0)|>
    left_join(time_series_est|>
                rename(week=week_no))|>
    rbind(store_timeseries2)->temporary
  
  temporary_store <- rbind(temporary_store,temporary)
  
}

leg <- cowplot::get_legend(plots_store[[1]]+
                             theme(legend.position = "right",
                                   legend.text=element_text(size=15)))

cowplot::plot_grid(plots_store[[1]],plots_store[[2]],plots_store[[3]],
                   plots_store[[4]],plots_store[[5]],plots_store[[6]],
                   plots_store[[7]],plots_store[[8]],plots_store[[9]],
                   plots_store[[10]],
                   leg,
                   nrow=4, ncol=3)-> human_plot

get_corr_plot <- function(matrix) {
  corrplot(matrix, method = "circle", col = custom_colors,
           title="",#"Correlation between human cases (y)\nand estimated mosquito prevalence (x)", mar=c(0,0,4,0),
           tl.col="black", tl.cex=1, tl.srt=90)
  
  p <- recordPlot()
  
  return(p)
}

grob_corr <- ggplotify::as.grob(~get_corr_plot(matrix = M))
plot_grid(corr_plot_h, grob_corr, ncol=1,rel_heights = c(1,1.5), labels=c("B","C")) -> panel
plot_grid(human_plot, panel, ncol=2, rel_widths=c(1,0.8), labels=c("A",NULL,NULL)) -> main_plot
return(main_plot)

}