#Using bcgovr to create a repo and upload it to github

install.packages("remotes")

remotes::install_github("bcgov/bcgovr")
library(bcgovr)

create_bcgov_project(path = "C:\\Users\\DGILCHRI\\OneDrive - Government of BC\\Hydro-Summary-Report\\Clustering_WSC_Temp", coc_email = "drew.gilchrist@gov.bc.ca")

use_bcgov_git(organisation = "bcgov", coc_email = "drew.gilchrist@gov.bc.ca")
