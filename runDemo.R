# The LCs data is contained within the 'Data/Original_reports' folder, see demo_original_report.xlsx, to see the columns (and column names) required for the analysis.

# A) Install all packages (This only needs to be done once, takes ~ 1 hour)
source('install.packages.R')

# 1) Enrich the individual reports
if(F) source('enrich_original_reports.R') # Not run as the datasets need creating. 
# This creates the folder "Data/Enriched report for meta collection" where the enriched reports are saved as .rda files.

# 2) Combine the individual reports into a meta collection
source('create_meta_collection.R')
# This requires "Data/LCs_BRU3_native.csv" to be complete where each row contains the BRU level 3 codes that are native to each collection. This creates "meta_collection_all_AccYear.rda" and "meta_collection.rda" files containing the meta collection for all years and only 1921-2021 respectively. These, .rda files are used in the meta-collection analysis.


# 3) Run analysis and create plots.
source('analyse_meta_collection.R')
# Run analyses and creates plots used in the paper. These are saved in the "Plots" (which is created in the R-script)
