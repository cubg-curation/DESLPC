# Data for global analysis
tic = Sys.time()
library(tidyverse)

## 1) List the enriched original reports used in the meta-collection and the columns we want to extract. 
enrich_folder = list.files('Data/Enriched report for meta collection', full.names = T)
columns = c('AccNoFull', 'AccYear', 'TaxonNameFull', 'ItemStatusDate', 'ItemStatusType', 'TaxonName','sanitised_taxon', 'POWO_taxon_name', 'POWO_taxon_authors', 'POWO_Red_category', 'redList_match_taxon_name', 'redList_category', 'taxon_type', 'endemic', 'native', 'tree', 'threatened', 'threatened_category', 'ProvenanceCode', 'geography_codes', 'no_gardens', 'best_name', 'use_family')

## 2) Load the native regions for each LC.
LC_native = read.csv('Data/LCs_BRU3_native.csv') 
LC_native = LC_native[order(LC_native$LC),3]
LC_native = LC_native |> stringr::str_split('; ')

AA = data.frame(enrich_folder, LC_native)

# Loop over all collections in enrich folder and extract the required information.
All_data = NULL ; set.seed(46)
counter <<- 1
data = pbapply::pblapply(enrich_folder, function(filepath){
  load(filepath)
  print(filepath)
  
  # Get the name of the LC.
  LC = paste0('LC_', counter)
  LC_regions = stringr::str_extract_all(string = LC_native[counter], pattern = '[A-Z]{3}')[[1]]
  counter <<- counter + 1
  
  # Need to combine the geography codes across columns representing native/introduced, extinct/non-extinct, doubtful/non-doubtful locations. 
  # Choose the criteria for geographic information, and strip out location codes.
  native = 'Naturally occurring only' ; extinct = TRUE ; doubtful_locations = FALSE
  geography_data = enriched_report[,grepl('_area_code_l3', names(enriched_report))]
  want = c('000','010','001','011','100','110','101')
  #Depending on the values of the options reduce `want` to only the satisfactory values.
  ##A) introduced / naturally occurring only.
  if(native == 'Introduced only'){
    want = want[grepl('^1',want)]
  }else if(native == 'Naturally occurring only'){
    want = want[grepl('^0',want)]
  }
  ## B) Extinct
  if(!extinct){
    want = want[!grepl('010|011|110',want)]
  }
  ## C) Doubtful
  if(!doubtful_locations){
    want = want[!grepl('001|011|101',want)]
  }
  # Get the columns in wanted info that contain the geography information we want.
  geography_data = geography_data[,grepl(paste0(want,collapse='|'), names(geography_data))]
  if(length(want) > 1){
    level3codes =do.call("paste", c(geography_data, sep = ", "))
    level3codes = stringr::str_remove(level3codes, ', NA$')
    level3codes = stringr::str_remove(level3codes, 'NA, ')
  }else{
    level3codes = geography_data
  }
  level3codes[level3codes == "NA"] = NA
  enriched_report$geography_codes = level3codes
  
  #Extract endemic information from enriched_report
  endemic = rep('Widespread', nrow(enriched_report))
  endemic_index = which(stringr::str_length(enriched_report$geography_codes) == 3)
  endemic[endemic_index] = 'Endemic'
  enriched_report$endemic = endemic
  rm(endemic)
  
  #Extract native information from enriched_report
  native = rep('Not Native', nrow(enriched_report))
  native_index = which(grepl(paste0(LC_regions, collapse = '|'),enriched_report$geography_codes))
  native[native_index] = 'Native'
  native[is.na(enriched_report$POWO_taxon_name)] = NA
  enriched_report$native = native
  rm(native)
  
  # Extract tree.
  tree =rep('Not Tree', nrow(enriched_report))
  tree[enriched_report$Enrich_is_tree] = 'Tree'
  enriched_report$tree = tree
  rm(tree)
  
  # Extract family
  family = enriched_report$Family
  family[!is.na(enriched_report$POWO_plant_name_id)] = enriched_report$POWO_family[!is.na(enriched_report$POWO_plant_name_id)]
  enriched_report$use_family = family
  rm(family)
  
  # Extract threatened. 
  threat_cat = c('VU','EN','CR','EW','EX')
  threatened = rep('Not Threatened', nrow(enriched_report))
  threatened[which(enriched_report$redList_category %in% threat_cat)] = 'Threatened'
  threatened[which(enriched_report$POWO_Red_category %in% threat_cat)] = 'Threatened'
  enriched_report$threatened = threatened
  rm(threatened)
  
  # Extract threatened category. 
  threatened_category = rep(NA, nrow(enriched_report))
  for(i in 1:length(threat_cat)){
    threatened_category[which(enriched_report$redList_category == threat_cat[i])] = threat_cat[i]
    threatened_category[which(enriched_report$POWO_Red_category == threat_cat[i])] = threat_cat[i]
  }
  enriched_report$threatened_category = threatened_category
  rm(threatened_category)
  
  # # Random Accession Number
  # acc = enriched_report$AccNoFull
  # no_acc = length(unique(acc[!is.na(acc)]))
  # replace = paste0(LC, '-',sample(1:no_acc, no_acc))
  # acc_new = as.character(factor(acc, unique(acc), replace))
  # enriched_report$AccNoFull = acc_new
  
  
  # Convert Provenance code to text.
  enriched_report$ProvenanceCode[enriched_report$ProvenanceCode == 'G'] = 'Garden'
  enriched_report$ProvenanceCode[enriched_report$ProvenanceCode == 'U'] = 'Unknown'
  enriched_report$ProvenanceCode[enriched_report$ProvenanceCode == 'W'] = 'Wild'
  enriched_report$ProvenanceCode[enriched_report$ProvenanceCode == 'Z'] = 'Wild-derived'
  
  # Add the best name from POWO (if exists) or original taxonomic name.
  best_name = enriched_report$sanitised_taxon
  best_name[!is.na(enriched_report$POWO_taxon_name)] = enriched_report$POWO_taxon_name[!is.na(enriched_report$POWO_taxon_name)]
  best_name = stringr::str_remove_all(best_name, 'xml:space="preserve">') |> stringr::str_squish()
  enriched_report$best_name = best_name
  
  want = enriched_report[,match(columns, names(enriched_report))]
  want$LC = rep(LC, nrow(want))
  want$ItemStatusDate = as.character(want$ItemStatusDate)
  
  
  want
})

# Join together the individual collections.
data <- do.call("rbind", data)
data$AccYear = as.numeric(data$AccYear)


data$ItemStatusType[data$ItemStatusType %in% c("NotExisting" , "Not Existing", "NotExisting ")] = 'NotExisting'

# Remove those with missing Accession number.
data = data[!is.na(data$AccNoFull),]

# Remove accessions that have multiple accession year.
Accession_year_of_accessions <- data |>
  group_by(AccNoFull) |>
  summarise(accession_years = paste0(unique(AccYear), collapse = ', '))
with_multiple = Accession_year_of_accessions$AccNoFull[grep(',',Accession_year_of_accessions$accession_years)]

data = data[!data$AccNoFull %in% with_multiple,]

# Save a version will all accessions years.
data_all = data
save(data_all, file  = 'Data/meta_collection_all_AccYear.rda')
rm(data.all)

# reduce data to only desired accessions years.
data = data[which(data$AccYear >= 1921 & data$AccYear <= 2021),]
data = data[!is.na(data$AccYear),]
save(data, file  = 'Data/meta_collection.rda')

toc = Sys.time()
toc-tic