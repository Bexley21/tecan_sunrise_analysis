##function for plates with samples loaded in triplicate, will return median and SEM for each condition
collapse_triplicate <- function(input_df, save_folder){
  
  standard_error <- function(x) sd(x) / sqrt(length(x)) # Create SEM function
  
  input_df <- input_df%>%
    pivot_longer(!time,names_to = "sample", values_to = "OD")%>%
    mutate(sample = str_sub(sample,1,-3))%>%
    group_by(time,sample)%>%
    summarise(SEM = standard_error(OD), OD = median(OD))
  
  sample_vector <- unique(input_df$sample)
  
  for(i in 1:length(sample_vector)){
    
  error_graph <- input_df%>%
      filter(sample == as.character(sample_vector[i]))%>%
      ggplot(aes(x=time,y=OD))+
      geom_line(size = 0.8)+
      geom_linerange(aes(ymin = OD-SEM, ymax=OD+SEM), size=0.5, color="red", alpha = 0.7) +
      ggtitle(paste(as.character(sample_vector[i]),"growth",'')) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
      labs(x= "time (min)")
      
  ggsave(gsub(" ","",paste(save_folder,
                               paste(as.character(sample_vector[i]),"_gc.png",''),'')),
             plot = error_graph)
  }
return(input_df)
}