## CREATE PROJECT METADATA TABLE

project_metadata <- data.frame(project_name = unique(sample_metadata$project_name)) %>% 
  rbind(data.frame(project_name = "u01_francesca_telese") %>% mutate_all(as.character)) %>% 
  mutate_all(as.character) %>% 
  mutate(project_title = NA,
         project_description = NA,
         phenotypic_data = NA,
         omics = NA,
         comments = NA) %>% 
  mutate(phenotypic_data = replace(phenotypic_data, project_name != "pcal_brian_trainor","yes"),
         phenotypic_data = replace(phenotypic_data, project_name == "pcal_brian_trainor","no"))
## XX update this in the pgadmin
# "u01_francesca_telese_scrna" update project_name
#  project_title = SINGLE-CELL RESOLUTION ANALYSIS OF CHROMATIN ACCESSIBILITY AND GENE EXPRESSION CHANGES IN A MODEL OF DRUG ADDICTION



## upload into the dropbox 
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user='postgres', password='postgres', dbname='PalmerLab_Datasets')
dbWriteTable(con, c("sample_tracking", "project_metadata"), value = project_metadata, row.names = FALSE, overwrite = T)
dbExecute(con,"ALTER TABLE sample_tracking.project_metadata ADD CONSTRAINT project_unique UNIQUE(project_name)")

# disconnect
dbDisconnect(con)

## in terminal
cd /tmp
sudo su postgres
pg_dump -d PalmerLab_Datasets -t sample_tracking.project_metadata > project_metadata.sql
exit



