##for plates with data samples loaded in duplicate/triplicate
##function returns a graph per 'condition' to assess replicate growth curve correlation (three replicates displayed together)
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