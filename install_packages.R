# This script must be run to setup the R library with the required packages for the analysis.
# Please use R version 4.2 when installing the packages and running the analysis.
# 
# R 4.2 can be downloaded from: - https://cran.r-project.org/bin/windows/base/old/ (Windows)
#                               - https://cran.r-project.org/bin/macosx/ (max)
#                               - https://cran.r-project.org (linux)
#                           
# We recommend using the R Studio IDE which can be downloaded from https://posit.co/downloads/. 
# 
# 
# Run the following lines for installing dependencies. If issues arise in installing the packages you can try installing the most recent version of the packages but this may affect the code. 
# Installing the packages and dependencies takes ~ 50 minutes.

tic = Sys.time()
install.packages('devtools')
install.packages('tidyverse')
install.packages('picante')
install.packages('ggpubr')
devtools::install_version("openxlsx", version = "4.2.5.2", repos = "http://cran.us.r-project.org", dependencies = T  )
devtools::install_version("stringr", version = "1.5.1", repos = "http://cran.us.r-project.org", dependencies = T  )
devtools::install_version("pbapply", version = "1.7-2", repos = "http://cran.us.r-project.org", dependencies = T  )
devtools::install_version("ggplot2", version = "3.5.1", repos = "http://cran.us.r-project.org", dependencies = T  )
devtools::install_version("scales", version = "1.3.0", repos = "http://cran.us.r-project.org", dependencies = T  )
devtools::install_version("zoo", version = "1.8-12", repos = "http://cran.us.r-project.org", dependencies = T  )
devtools::install_version("readxl", version = "1.4.2", repos = "http://cran.us.r-project.org", dependencies = T  )
devtools::install_version("dplyr", version = "1.1.4", repos = "http://cran.us.r-project.org", dependencies = T  )
devtools::install_version("reshape", version = "0.8.9", repos = "http://cran.us.r-project.org", dependencies = T  )
devtools::install_version("ivs", version = "0.2.0", repos = "http://cran.us.r-project.org", dependencies = T  )
devtools::install_version("lubridate", version = "1.9.2", repos = "http://cran.us.r-project.org", dependencies = T  )
devtools::install_version("ggsurvfit", version = "1.0.0", repos = "http://cran.us.r-project.org", dependencies = T  )
devtools::install_version("survival", version = "3.5-3", repos = "http://cran.us.r-project.org", dependencies = T  )
devtools::install_version("igraph", version = "1.5.1", repos = "http://cran.us.r-project.org", dependencies = T  )
devtools::install_version("ape", version = "5.7-1", repos = "http://cran.us.r-project.org", dependencies = T  )
devtools::install_version("Hmisc", version = "5.1-1", repos = "http://cran.us.r-project.org", dependencies = T  )
devtools::install_version("taxize", version = "0.9.100", repos = "http://cran.us.r-project.org", dependencies = T  )
devtools::install_version("phangorn", version = "2.11.1", repos = "http://cran.us.r-project.org", dependencies = T  )
devtools::install_version("bold", version = "1.3.0", repos = "http://cran.us.r-project.org", dependencies = T  )
devtools::install_version("taxize", version = "0.9.100", repos = "http://cran.us.r-project.org", dependencies = T  )

devtools::install_github("cubg-curation/LivingCollectionDynamics", dependencies = FALSE) 

toc = Sys.time()
toc-tic
