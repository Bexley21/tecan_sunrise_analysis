#read packages
install.packages("dplyr")
library("dplyr") 
install.packages("readxl")
library(readxl)

##function to read in .xlsx file as tecan output, and convert to each well as a timecourse
data_merge_function <- function(file_source){
  data <- read_xlsx(file_source,sheet = 1)[,-1] #read in the .xlsx file and remove the column of row names
  rows <- c("A", "B","C","D","E","F","G","H")
  df <- matrix(seq(0,2625,by = 15)). #set a single column dataframe of the time values
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
  return(df)
}

#set working directory and then the particular tecan output .xlsx you want to call
setwd("~/OneDrive/PhD_UoE/Year2_PhD/lab_book/January_2023/respiration_dependence/tecan_r_tool")
file_source <- "data/tecanoutput_antimycinA_test1.xlsx"
save_location <- "output/pin4_tecanplate_all_wells.csv"

#call the data merge function on your file, and save
tecan_output <- data_merge_function(file_source)
write.csv(tecan_output,save_location, row.names = FALSE)