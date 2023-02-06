# tecan_sunrise_analysis
R scripts designed for manipulation of Tecan Sunrise Plate reader output files.
Used to:
* Remove background using 'blank' wells
* Combine and assess technical replicates
* Calculate doubling time
* Graphically compare growth between sample conditions

# input files #
- Tecan/Magellan output file - saved as .xlsx
- Plate layout file
     - __complete 'plate_layout_blank.csv' from 'file_templates' folder with your sample names for each well__
          - all sample names need to end "_X " - for replicates use same name and differnetiate by X
          - any wells being used as a blank should be filled " blank "
          - unused wells should have no entry
     - example of the filled file format is in 'file_templates' folder

# functions #
Package functions are stored in 'src/functions/' and can be called using source("src/functions/__FUNCTION_NAME__.R")

# workflow #
'src/example_workflow.R' is an example workflow for modification and graphing of input Tecan files
1. Tecan output is loaded in and converted to a dataframe - OD at time (rows) for each well (columns)
2. The all wells file is modified to just sample containing wells, and the background (blank) read is removed from OD values
3. The __corrected_samples__ data frame acts as the input to functions to:
     1. merge replicates (collapse triplicate)
     2. calculate metrics of growth (doubling_time)
     3.  Compare growth curves for different conditions (growth_comparison_plots)

