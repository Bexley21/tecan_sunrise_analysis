##notes for the read me - for plate-layout file, name samples that are replicates with '_1''_2' etc for end. Provide well to be the blank.

##code input: an excel file containing each well of a Tecan plate A1->H12 with OD measurements, excel file with a sample name for each plate location (blank if no sample)
##code output: .csv file with a column per sample, with OD measurements at each time. Option (subtract_background): with background removed from every value

##attach packages
install.packages("dplyr")
library("dplyr") 
install.packages("tidyverse")
library("tidyverse") 
install.packages("data.table")
library("data.table") 
install.packages("readxl")
library(readxl)
install.packages("matrixStats")
library(matrixStats)

##load all functions
#plate_layout_converter - take standard excel file naming well contents, use to select and rename only sample containing columns in all_wells file

plate_layout_converter <- function(all_wells, plate_layout){
  
  all_wells_df <- read.csv(file = all_wells)
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
}

##subtract_background - optional function to remove the background from every read
#FOR:plate_layout_converter outputs. Per row, will take median of any column named 'blank' and then subtract this from all other columns. (accounts for blank shift over time)

subtract_background <- function(input_df){
  
  input_df$blank_med <- rowMedians(as.matrix(input_df), cols = grep("^blank$", colnames(input_df)))
  input_df <-input_df[,-grep("^blank$", colnames(input_df))]
  
  for(i in 1:ncol(input_df)) {  # for-loop over columns
    if(i!= 1){
      input_df[ , i] <- input_df[ , i] - input_df[,"blank_med"]
    }
  }
  
  return(input_df[,colSums(input_df !=0) > 0]) #remove columns with all 0's, so remove blank_med
}
#USE INPUT VARIABLES
setwd("~/OneDrive/PhD_UoE/Year2_PhD/lab_book/January_2023/respiration_dependence/tecan_r_tool") #working directory
all_wells_file <- "output/test1_tecanplate_all_wells.csv" #the excel file with all wells that you would like to call
plate_layout_file <- "data/plate_layout_completed.csv" #the standard plate_layout_excel file completed for your plate
save_location <- "output/test1_tecanplate_samplewells.csv"

#run plate converter function with user inputs and save
sample_well_df <- plate_layout_converter(all_wells_file,plate_layout_file)
corrected_df <-subtract_background(sample_well_df)
write.csv(sample_well_df,save_location, row.names = FALSE) ##write.csv(sample_well_df[,-grep("^blank$", colnames(input_df))],save_location, row.names = FALSE)