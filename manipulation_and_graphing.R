##packages
install.packages("zoo")
library(zoo)
install.packages("mgcv")
library(mgcv)

##read in corrected samples if not in work-space
corrected_samples <- read.csv(file = "output/test1_tecanplate_backgroundremoved.csv")

##function to make a growth curve per condition to assess replicate correlation (three replicates displayed together)
replicate_graphs <-function(input_df,save_folder){
  
  input_df <- input_df%>%
    pivot_longer(!time,names_to = "sample", 
                 values_to = "OD")%>%
    mutate(sample_group = str_sub(sample,1,-3))

  sample_group_vector <- unique(input_df$sample_group)
  
  for(i in 1:length(sample_group_vector)){
    
    replicate_graph <- input_df%>%
      filter(sample_group == as.character(sample_group_vector[i]))%>%
      ggplot(aes(x=time,y=OD, color = sample))+
      geom_point()+
      geom_line()+
      ggtitle(paste(as.character(sample_group_vector[i]),"replicates",''))+
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
      labs(x= "time (min)")

      ggsave(gsub(" ","",paste(save_folder,
                             paste(as.character(sample_group_vector[i]),".png",''),'')),
             plot = replicate_graph)
  }
}
    
replicate_graphs(corrected_samples,"output/replicate_graphs/")

##function for triplicate samples to take the median and produce SEM for them
collapse_triplicate <- function(input_df){
  
  standard_error <- function(x) sd(x) / sqrt(length(x)) # Create SEM function
  
  input_df <- input_df%>%
    pivot_longer(!time,names_to = "sample", values_to = "OD")%>%
    mutate(sample = str_sub(sample,1,-3))%>%
    group_by(time,sample)%>%
    summarise(SEM = standard_error(OD), OD = median(OD))
  
  return(input_df)
}

##function to do doubling time for each condition, add to vector and produce graph of the region
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
      
      co <- rollapply(exponential_data, 3, fit_regression, by.column=F) #rolling function to find R2 of linear regression over every window of width 3
      co.cl <- kmeans(co, 2) #kmeans clustering with two centers, separate 'good fit/linear' and 'bad fit/non-linear'
      b.points <- which(co.cl$cluster == match(max(co.cl$centers), co.cl$centers))+1 #returns positions allocated to cluster with higher center (better R2)
      RES <- exponential_data[b.points,] #select just data in the 'most-linear' range
      doubling_time <- 1/coef(lm(log2(OD) ~ time, RES))[[2]]
      lag_exit_time <- exponential_data$time[which.max(exponential_data$OD >0.05)]
      
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
        labs(x= "time (min)")
      
      ggsave(gsub(" ","",paste(save_folder,
                               paste(as.character(sample_group_vector[i]),".png",''),'')),
             plot = range_graph)
  }
  doubling_time_df <- data.frame(sample_group_vector, doubling_time_vector, lag_exit_vector)
  colnames(doubling_time_df) <- c("Condition","Doubling Time (min)", "Time to exceed OD 0.05 (min)")
  write.csv(doubling_time_df, gsub(" ","",paste(save_folder, "doubling_times.csv",'')), row.names = FALSE)
}
      
doubling_time_data <- doubling_time(corrected_samples,"output/doubling_time_graphs/")

##want to now produce graph of the median curve for a vector of medians, comparing them - over whole range or just initial phase (time out of lag)
install.packages("ggpubr")
library(ggpubr)

plot_function <- function(input_df,sample_names, sample_labels, save_location){
  
  comparison_graph <- input_df%>%
    pivot_longer(!time,names_to = "sample", values_to = "OD")%>%
    mutate(sample = str_sub(sample,1,-3))%>%
    group_by(time,sample)%>%
    summarise(OD = median(OD))%>%
    filter(sample %in% sample_names)%>%
    ggplot(aes(x=time,y=OD, color = sample))+
    geom_point()+
    geom_line()+
    ggtitle("Analysis of Growth")+
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
    labs(x= "time (min)", y = "OD")+
    scale_color_discrete(labels= sample_labels)
  
  lag_graph <- comparison_graph + ylim(0,0.5) + xlim(0,750) + ggtitle("Duration of lag")
  
  ggarrange(comparison_graph, lag_graph, heights = c(2, 1), ncol = 1, nrow = 2)%>%
    ggsave(save_location,.,)
  
}

sample_names <- c("WT_SD","WT_softshift","WT_SD_02AA")
sample_labels <- c("WT in SD", "WT soft shift to GE", "WT shift to GE (0.1% glucose)")
save_location <- "output/comparison_plots/WT_shifts.png"

plot_function(corrected_samples, sample_names, sample_labels, save_location)

##run tomorrow for actual 