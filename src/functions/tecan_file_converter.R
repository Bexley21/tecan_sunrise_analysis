##function to read in tecan output (.xlsx) as and convert to dataframe, with observations of OD in each well (columns) at every measured time (rows)

tecan_file_converter <- function(file_source, save_location){
  data <- read_xlsx(file_source,sheet = 1)[,-1] #read in the .xlsx file and remove the column of row names
  rows <- c("A", "B","C","D","E","F","G","H")
  df <- matrix(seq(0,2625,by = 15)) #set a single column dataframe of the time values
  colnames(df) <- c("Time")
  #for loop takes every 9 rows (so new time-point) from each of position 1-8 (corresponding A-H), making df per plate row - each row is OD read seperated by 15 min
  for(i in 1:8) {
    df_temp <- data[seq(i, nrow(data), 9), ] 
    colnames(df_temp) <- gsub(" ","",paste(as.character(rows[i]),colnames(df_temp),'')) #rename the columns to represent plate row in question, gsub removes the spaces introduced by paste
    df_time <- matrix(seq(0,2625,by = 15))
    colnames(df_time) <- c("Time")
    df_temp2 <- cbind(df_time, df_temp) #add the timepoints as a seperate column to the data frame
    
    df <- merge(df,df_temp2,by = "Time") #merge the data frames for each row of plate together, giving frame with a column per well
  }
  write.csv(df,save_location, row.names = FALSE)
  return(df)
}