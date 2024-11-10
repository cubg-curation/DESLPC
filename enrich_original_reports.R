### This script is for enriching reports for Angie to do further analysis.
tic = Sys.time()
### 1) Load the required packages.
library(tidyverse)
library(LivingCollectionDynamics)

############
# A) load enrichment info.
############
### WCVP (containing wcvp_names, wcvp_distribution and matched to IUCN red list)
load('Data/Enriching_info/wcvp_with_redlistcategory.rda')
### IUCN Red list.
load("Data/Enriching_info/prepped_IUCN_redlist.rda")
### BGCI number of gardens.
load("Data/Enriching_info/BGCI_extra.RData")
### Trees database (BGCI GlobalTreeSearch and WCVP combined)
load('Data/Enriching_info/prepped_trees_database.rda')
trees_database = trees_database[match(unique(trees_database$sanitise_name),trees_database$sanitise_name),]
trees_database$single_entry = TRUE
############
# B) List of gardens to generate the enriched report.
############
original_report_folder = "Data/Original_reports"
output_folder = "Data/Enriched report for meta collection"
dir.create(output_folder, showWarnings = FALSE)

files = list.files(original_report_folder)
gardens = stringr::str_remove(files, pattern = '_original_report.xlsx')

############
# C) Load original report, enrich with POWO and save.
############
### Loop over all gardens we want to enrich.
for(i in 1:length(gardens)){

  ############
  # 1) Pre-running enrichment
  ############
  # 1.1) Load the current garden.
  garden = gardens[i]
  print(paste0(i, ': ',garden ))
  original_report <- openxlsx::read.xlsx(xlsxFile = paste(original_report_folder, "/", garden, "_original_report.xlsx", sep = ""),
                               colNames = T,
                               rowNames = F,
                               detectDates = F)

  ### 1.2) Filter the garden to only the records we desire.
  if('ItemType' %in% names(original_report)){
    original_report <- original_report |> filter(ItemType == "Planting")
  }
  original_report <- original_report |> filter(!ItemStatusType == "Unknown")
  original_report <- original_report |> filter(!ItemStatusType == "Procedure")

  # Remove those with missing taxonName.
  original_report = original_report[!is.na(original_report$TaxonName),]

  ### 1.4) If CUBG remove annoying characters.
  if(grepl('CUBG',garden)){
    original_report$TaxonNameFull[is.na(original_report$TaxonNameFull)] = original_report$TaxonName[is.na(original_report$TaxonNameFull)]

    AA =stringr::str_replace_all(original_report$TaxonName,pattern = '\u00C3', replacement = '\u00D7')
    AA =stringr::str_replace_all(AA,pattern = '^\\?', replacement = '\u00D7')
    AA=stringr::str_replace_all(AA, pattern = '\\\u0097', replacement = '')
    original_report$TaxonName = AA
  }

  ### 1.3) Check if `TaxonNameFull` (or the typo `TaxonNameFul`) is contained in the original report.
  ### If it is not then we create a new column  called TaxonNameFull where all values are empty strings ''.
  if(!'TaxonNameFull' %in% names(original_report)){
    if('TaxonNameFul' %in%  names(original_report)){
      original_report$TaxonNameFull =  original_report$TaxonNameFul
    }
    else{
      original_report$TaxonNameFull =  rep('', nrow(original_report))
    }
  }
  
  if('TaxonName_with_authors' %in% names(original_report)){
    original_report$TaxonNameFull =  original_report$TaxonName_with_authors
  }

  # Change star to hybrid marker when required.
  original_report$TaxonName = stringr::str_replace_all(original_report$TaxonName,'◊','\u00D7')
  if('TaxonNameFull' %in% names(original_report)){
    original_report$TaxonNameFull = stringr::str_replace_all(original_report$TaxonNameFull,'◊','\u00D7')
    taxon_name_full_column = 'TaxonNameFull'
  }

  ############
  # 2) Enrich the report.
  ############
  ### 2.1) Enrich with WCVP, IUCNredlist and BGCI number of gardens.
  enriched_report = enrich_collection(original_report,
                                      wcvp = wcvp,
                                      iucnRedlist = IUCN_redlist,
                                      BGCI = Garden_count,
                                      taxon_name_column = 'TaxonName',
                                      taxon_name_full_column = taxon_name_full_column,
                                      do_is_autonym = FALSE, # Not required.
                                      do_status_year = TRUE,
                                      do_taxon_types = TRUE,
                                      # typo_method = 'Data frame only'
                                      typo_method = 'All'
                                      )

  ### 2.2) Enrich with trees database.
  # Create genus_species for tree matching
  genus_species = enriched_report$TaxonName
  genus_species  = stringr::str_remove(genus_species, pattern = ' [a-zA-z]*\\..*')
  genus_species  = stringr::str_remove(genus_species, pattern = '\\+ |\u00D7 ')
  genus_species  = stringr::str_remove(genus_species, pattern = "'[a-zA-z \\-]*'")
  first_two_words = stringr::str_extract(string = genus_species,pattern = '^([^ ]+[ ]+[^ ]+)[ ]')
  genus_species[!is.na(first_two_words)] = first_two_words[!is.na(first_two_words)]
  genus_species = stringr::str_trim(genus_species)
  enriched_report$genus_species = genus_species

  # Enrich with trees.
  enriched_report = LivingCollectionDynamics::enrich_collection_from_enrich_database(
    collection = enriched_report,
    enrich_database = trees_database,
    taxon_name_column = 'genus_species', # use newly made genus species column
    taxon_name_full_column = NA,
    taxon_author_column = NA,
    enrich_taxon_name_column = 'sanitise_name',
    enrich_taxon_authors_column = 'sanitise_author',
    columns_to_enrich = c('is_tree', 'from'),
    do_fix_taxon_name = FALSE, # No need to try fixes since we use genus species.
    do_rm_cultivar_indeterminates = FALSE, # Don't want to remove cultivars
    do_match_multiple = FALSE,
    typo_method = 'Data frame only'
    # typo_method = 'All'

  )

  ### 2.1) Save after enrichment (version for Jake's analysis)
  save(enriched_report, file = paste0(output_folder, "/", garden,'_enriched_report.rda'))

}

toc = Sys.time()
toc-tic



