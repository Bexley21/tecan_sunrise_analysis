##attach packages
library(dplyr) 
library(tidyverse) 
library(data.table) 
library(readxl)
library(matrixStats)
library(zoo)
library(mgcv)

#functions from src/
source("src/functions/tecan_file_converter.R")
source("src/functions/plate_layout_converter.R")
source("src/functions/subtract_background.R")
source("src/functions/collapse_triplicates.R") ##modify this to save you a graph of individual sample
source("src/functions/replicate_graphs.R")
source("src/functions/doubling_time.R")
source("src/functions/growth_comparison_plots.R")

#USER SPECIFIC INPUTS
setwd("~/OneDrive/PhD_UoE/Year2_PhD/lab_book/January_2023/respiration_dependence/tecan_r_tool") #working directory
tecan_Rawfile<- "data/tecanoutput_antimycinA_test1.xlsx" #tecan output .xlsx you want to call
plate_layout_file <- "data/plate_layout_completed.csv" #the standard plate_layout file completed for YOUR plate

##MODEL WORKFLOW
#convert the file to all wells, store as data frame
all_wells_df <- tecan_file_converter(tecan_Rawfile,"output/check_run/test1_AllWells.csv")

#create data frame of relevant samples, and remove background. THIS IS INPUT FOR DOWNSTREAM GRAPHING
corrected_sample_df <- subtract_background(plate_layout_converter(all_wells_dataframe, plate_layout_file),
                                           "output/check_run/test1_corrected_sample_growth.csv")

##use replicate_growth to assess the relationship between replicates - save graphs to allocated folder
replicate_graphs(corrected_sample_df, "output/check_run/replicate_graphs/")

##use doubling_time for doubling time + time out of lag. Provide folder for .csv and plots of linear region.
growth_metric_df <- doubling_time(corrected_sample_df,"output/check_run/doubling_time_graphs/")

##use collapse triplicate to produce a growth curve for each condition, with error bars
summarised_growth <- collapse_triplicate(corrected_sample_df,"output/check_run/growth_curves/")

##compare conditions of interest using growth comparison
display_samples <- c("WT_SD","WT_softshift","WT_SD_02AA") #need to be names from 'plate_layout'
sample_labels <- c("WT in SD", "WT soft shift to GE", "WT SD with 0.2 µg/ml antimycin")
graph_save <- "output/check_run/comparison_graphs/WT1.png" #change per saved graph

growth_comaprison_plots(corrected_sample_df, display_samples, sample_labels, graph_save)