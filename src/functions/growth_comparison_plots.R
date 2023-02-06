##function takes a list of provided conditions, and returns growth curves of each on same graph - a subset graph is provided showing just initial region

growth_comaprison_plots <- function(input_df,sample_names, sample_labels, save_location){
  
  comparison_graph <- input_df%>%
    pivot_longer(!time,names_to = "sample", values_to = "OD")%>%
    mutate(sample = str_sub(sample,1,-3))%>%
    group_by(time,sample)%>%
    summarise(OD = median(OD))%>%
    filter(sample %in% sample_names)%>%
    ggplot(aes(x=time,y=OD, color = sample))+
   # geom_point()+
    geom_line()+
    ggtitle("Analysis of Growth")+
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
    labs(x= "time (min)", y = "OD")+
    scale_color_discrete(labels= sample_labels)
  
  lag_graph <- comparison_graph + ylim(0,0.5) + xlim(0,750) + ggtitle("Duration of lag")
  
  ggarrange(comparison_graph, lag_graph, heights = c(2, 1), ncol = 1, nrow = 2)%>%
    ggsave(save_location,.,)
  
}
