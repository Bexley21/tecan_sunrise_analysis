##function for plates with samples loaded in triplicate, will return median and SEM for each condition
collapse_triplicate <- function(input_df){
  
  standard_error <- function(x) sd(x) / sqrt(length(x)) # Create SEM function
  
  input_df <- input_df%>%
    pivot_longer(!time,names_to = "sample", values_to = "OD")%>%
    mutate(sample = str_sub(sample,1,-3))%>%
    group_by(time,sample)%>%
    summarise(SEM = standard_error(OD), OD = median(OD))
  
  return(input_df)
}