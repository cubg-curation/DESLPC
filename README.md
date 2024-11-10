# DESLPC
Code repository for 'The dynamics of ex-situ living plant collections: insights from a century of data'

### Instructions

This folder contains scripts to perform the analysis presented in "The dynamics of ex-situ living plant collections: insights from a century of data". Note that the data used across 50 LCs is not available publicly. 

Therefore, we provide a single demo LC (contained within Data/Original_reports) to which the analysis can be performed. For sections of the analysis that cannot be run on the demo LC (e.g. comparison to ICCP) the code remains but is wrapped in a if false statement.

Moreover, in the article we enrich collections records with information from:
- World Checklist of Vascular Plants (WCVP),
- IUCN's Red List,
- BGCI's GlobalTreeSearch,
- BGCI's PlantSearch.

We are not able to provide these datasets due to their licences. However, where possible we provide code explaining how to create these datasets for yourselves. This is contained within `Create_datasets.R'.

See the file "runDemo.R" to perform the analysis on the demo dataset. This script runs "create_meta_collection.R" and  "analyse_meta_collection.R". Note that the script also contains code to run "enrich_original_reports.R", however this is removed as the datasets required for the enrichment need to be created. The enriched demo dataset is included in the "Enriched report for meta collection" folder.

These scripts are described below:

- enrich_original_reports.R: An R-script to enrich all individual botanic garden datasets with information from World Checklist of Vascular Plants (WCVP), IUCN's Red List, BGCI's GlobalTreeSearch and an extract of BGCI's PlantSearch. 

- create_meta_collection.R: An R-script to combine enriched datasets into a single meta-collection.

- analyse_meta_collection.R: An R-script the produces the figures in the paper. 

Within the Data folder there is:

- Enriching_info: A folder of the enrichment (see enrich_original_reports.R above) datasets we use to enrich botanic gardens information.

- Original_reports: A folder where excel (.xlsx) extracts of botanic garden records are placed prior to analysis. These files are used by enrich_original_reports.R to enrich with new information, before the creation of the meta-collection and analysis. The folder only contains a demo dataset (demo_report.xlsx) which allows the the R-scripts to be run.

- phy.RDS: for the calculation of PD.

- RedList_threatened_history.xlsx (Not provided, see Create_datasets.R): used in Figure 2C for the history of of threatened plants within IUCN's Redl list.

### Demo dataset benchmarks:

For the single demo dataset the R-scripts should take approximately the following time

install_packages.R ~ 1 hour.

enrich_original_reports.R: ~ 2 minutes.

create_meta_collection.R: ~ 1s

analyse_meta_collection.R: ~ 1 minutes.



### Operating system, programming languages and software requirements

The analysis was performed an a MacBook Air (13-inch, 2017) running macOS Monterey with:

- Processor: 2.2 GHz Dual-Core Intel Core i7

- Memory: 8 GB 1600 MHz DDR3

The analysis is performed entirely using the R programming language

platform       x86_64-apple-darwin17.0     
arch           x86_64                      
os             darwin17.0                  
system         x86_64, darwin17.0          
status                                     
major          4                           
minor          2.3                         
year           2023                        
month          03                          
day            15                          
svn rev        83980                       
language       R                           
version.string R version 4.2.3 (2023-03-15)

For code dependencies see "install.packages.R"



