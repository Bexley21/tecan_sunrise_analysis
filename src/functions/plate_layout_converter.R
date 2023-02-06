##code input: an excel file containing each well of a Tecan plate A1->H12 with OD measurements OR equivalent df
##code input:excel file with a sample name for each plate location (blank if no sample). Names must end '_X'. Wells used as a blank must be 'blank'.
#function will return a .csv with a column per named sample, with OD measurements at each time

plate_layout_converter <- function(all_wells, plate_layout, save_location){
  
  if(is.character(all_wells == TRUE)){
    all_wells_df <- read.csv(file = all_wells)
  } else {
    all_wells_df <- all_wells
  }
  
  plate_layout_df <- na.omit(read.csv(file = plate_layout, header = FALSE, col.names = c("location","sample",""),na.strings=c("","NA"))[-1,1:2])%>%
    pivot_wider(names_from = location, values_from = sample)
  
  location_names <- colnames(plate_layout_df)
  sample_names <- as.character(plate_layout_df[1,])
  time <- all_wells_df[,1]
  
  sample_wells <- all_wells_df %>%
    select(all_of(location_names)) %>%
    setnames(old = location_names, new = sample_names)
  
  sample_wells <- cbind(time,sample_wells)
  
  return(sample_wells)
  write.csv(sample_wells,save_location, row.names = FALSE)
}
