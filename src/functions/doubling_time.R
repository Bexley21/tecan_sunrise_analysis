##function calculates the doubling time and time out of lag for each tested condition
##doubling time is identified by fitting a linear regression to just the most exponential region of the curve
#identifies region where gradient of log2(OD)~time is most constant - uses this linear region to calculate doubling time from gradient
## returns .csv of 'doubling time' and 'time to exit lag' per condition. Returns graph of each region used for calculation + R2, for manual check.

doubling_time <- function(input_df,save_folder){
  
  input_df <- input_df%>%
    pivot_longer(!time,names_to = "sample", values_to = "OD")%>%
    mutate(sample = str_sub(sample,1,-3))%>%
    group_by(time,sample)%>%
    summarise(OD = median(OD))
  
  sample_group_vector <- unique(input_df$sample)
  doubling_time_vector <- c()
  lag_exit_vector <- c()
  
  for(i in 1:length(sample_group_vector)){
    
    fit_regression <- function (df) {
      m <- lm(log2(OD) ~ time, as.data.frame(df))  #fit a linear model with time(x) as the predictor and OD(y) as response - log of OD as exponential
      return(coef(m)[2]) #coef function extracts model coefficients - returns R2 as a measure of goodness of fit
    } 
    
    exponential_data <- input_df%>%
      filter(sample== as.character(sample_group_vector[i]))%>%
      select(c(time,OD))%>%
      subset(0.03 < OD & 0.4 > OD) #limit the data to exponential range
    
    co <- rollapply(exponential_data, 3, fit_regression, by.column=F) #rolling function to find gradient of linear regression over every window of width 3
    co.cl <- kmeans(co, 2) #kmeans clustering with two centers, separate 'good fit/linear' and 'bad fit/non-linear'as region with constant gradient is linear (values very similar and cluster together)
    b.points <- which(co.cl$cluster == match(max(co.cl$centers), co.cl$centers))+1 #returns positions allocated to cluster with higher center
    RES <- exponential_data[b.points,] #select just data in the 'most-linear' range
    doubling_time <- 1/coef(lm(log2(OD) ~ time, RES))[[2]] #fits a linear regression to the 'linear' data and uses gradient for doubling time
    lag_exit_time <- exponential_data$time[which.max(exponential_data$OD >0.05)] #finds first time OD exceeds 0.05
    
    doubling_time_vector <- c(doubling_time_vector,doubling_time)
    lag_exit_vector <- c(lag_exit_vector, lag_exit_time)
    
    range_graph <- ggplot(NULL, aes(x=time,y=log2(OD)))+
      geom_point(data = exponential_data, col = "black", size = 3)+
      geom_point(data = RES, col = "red", size = 3)+
      geom_smooth(data = RES, method='lm', formula= y ~ x)+ #DOESNT WORK DK WHY
      ggtitle(label = paste(as.character(sample_group_vector[i]),"linear range",''),
              subtitle = paste("Doubling Time =", 
                               paste(substr(as.character(doubling_time),1,4), "minutes",''),''))+
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5) )+
      labs(x= "time (min)")+
      stat_regline_equation(data = RES, label.y = 0.35, aes(label = after_stat(rr.label)))
    
    ggsave(gsub(" ","",paste(save_folder,
                             paste(as.character(sample_group_vector[i]),".png",''),'')),
           plot = range_graph)
  }
  doubling_time_df <- data.frame(sample_group_vector, doubling_time_vector, lag_exit_vector)
  colnames(doubling_time_df) <- c("Condition","Doubling Time (min)", "Time to exceed OD 0.05 (min)")
  write.csv(doubling_time_df, gsub(" ","",paste(save_folder, "doubling_times.csv",'')), row.names = FALSE)
  return(doubling_time_df)
}