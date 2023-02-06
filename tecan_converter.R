#set correct working directory and read packages
setwd("~/OneDrive/PhD_UoE/Year2_PhD/lab_book/January_2023/respiration_dependence/tecan_r_tool")

install.packages("dplyr")
library("dplyr") 
install.packages("readxl")
library(readxl)

##function to read in .xlsx file as tecan output, and convert to each well as a timecourse
data_merge_function <- function(file_source){
  data <- read_xlsx(file_source,sheet = 1)[,-1]
  rows <- c("A", "B","C","D","E","F","G","H")
  df <- matrix(seq(0,2625,by = 15))
  colnames(df) <- c("Time(min)")
  for(i in 1:8) {
    df_temp <- data[seq(i, nrow(data), 9), ]
    colnames(df_temp) <- paste(as.character(rows[i]),colnames(df_temp),'')
    df_time <- matrix(seq(0,2625,by = 15))
    colnames(df_time) <- c("Time(min)")
    df_temp2 <- cbind(df_time, df_temp)
    
  df <- merge(df,df_temp2,by = "Time(min)")
  }
  return(df)
}

file_source <- "data/tecanoutput_antimycinA_test1.xlsx"
tecan_output <- data_merge_function(file_source)