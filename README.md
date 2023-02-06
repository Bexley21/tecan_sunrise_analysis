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
     - complete 'plate_layout_blank.csv' from 'file_templates' folder with your sample names for each well
          - all sample names need to end "_X " - for replicates use same name and differnetiate by X
          - any wells being used as a blank should be filled " blank "
          - unused wells should have no entry
     - example of the filled file format is in 'file_templates' folder
