#wrapper for plate_layout_converter() - used to remove background OD read from sample containing reads
#function takes median of all 'blank' columns, then subtract from sample containing columns on a rowise basis (accounts for shift over time)

subtract_background <- function(input_df,save_location){
  
  input_df$blank_med <- rowMedians(as.matrix(input_df), cols = grep("^blank$", colnames(input_df)))
  input_df <-input_df[,-grep("^blank$", colnames(input_df))]
  
  for(i in 1:ncol(input_df)) {  # for-loop over columns
    if(i!= 1){
      input_df[ , i] <- input_df[ , i] - input_df[,"blank_med"]
    }
  }
  
  input_df <- input_df[,colSums(input_df !=0) > 0] #remove columns with all 0's, so remove blank_med
  write.csv(input_df, save_location, row.names = FALSE)
  return(input_df) 
}