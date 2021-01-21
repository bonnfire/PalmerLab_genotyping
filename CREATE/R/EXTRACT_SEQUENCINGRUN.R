## Sequencing Run Log
library(lubridate)
library(dplyr)
library(tidyverse)
library(janitor)

# extract the spreadsheet notebook (first sheet; sheet = 1, "sequencing")
sequencing_run_log_IGM <- flipAPI::DownloadXLSX("https://www.dropbox.com/s/8pqkjfib37fric8/sequencing%20run%20log%20at%20IGM%20post%2004-2017.xlsx?dl=0", sheet = 1) 


## functions for wrangling
## create new rows to expand on the projects 
expand.project.dash <- function(txt) {
  
  if(grepl("\\d[[:space:]]?-[[:space:]]?\\d", txt)){
  str <- gsub('\\d+\\s?-\\s?\\d+', '%s', txt)
  dashed_str <- gsub('[a-zA-Z ]+', '', txt)
  
  expand.dash <- function(dashed) {
    limits <- as.numeric(unlist(strsplit(dashed, '-')))
    seq(limits[1], limits[2])
  } 
  
  paste0(sprintf(str, str_pad(expand.dash(dashed_str), 2, side = "left", pad = "0")), sep = "", collapse = ",")
  }
  else
    paste0(txt)
  } 
expand.project.dash <- Vectorize(expand.project.dash)


# modify our copy 
sequencing_run_log_IGM_df <- sequencing_run_log_IGM %>%
  clean_names %>% 
  mutate_all(as.character) %>% 
  mutate_at(vars(matches("date", ignore.case = T)), 
            ~ openxlsx::convertToDate(as.numeric(.))) %>% 
  mutate_at(vars(matches("number_of_pools")), as.numeric) %>% 
  mutate(project_details = ifelse(grepl(" [(]", project), gsub(".* [(]", "", project), NA),
         library_name = gsub(" [(].*", "", project), 
         project_details = gsub("[()]", "", project_details)) %>%
  mutate(project_details = expand.project.dash(project_details)) %>% # keep project column unmodified 
  separate_rows(project_details, sep=c(",|and|&")) %>% 
  separate_rows(library_name, sep = ",") %>% 
  mutate(library_name = expand.project.dash(library_name)) %>% 
  separate_rows(library_name, sep = ",") %>% 
  mutate_at(vars(matches("library_")), str_trim) %>% # remove leading and trailing whitespace from string
  mutate_at(vars(matches("library_")), str_squish) %>% # remove repeated whitespace inside string
  left_join(., library_project_name, by = c("library_name" = "riptide_plate_number")) %>% 
  separate_rows(project_name, sep=c(","))  %>% # create separate rows for each plate
  rowwise() %>% 
  mutate(project_name = replace(project_name, is.na(project_name), case_when(
      grepl("umich", library_name, ignore.case = T) ~ "u01_huda_akil",
      grepl("zebrafish", library_name, ignore.case = T) ~ "r01_su_guo",
      grepl("Mortazavi-Sebat HS", project, ignore.case = T) ~ "u01_gymrek_sebat", 
      TRUE ~ "NA"
    )
  )) %>% # include the schema names
  ungroup() %>% 
  mutate(full_run_id = gsub("/", "", full_run_id)) %>% 
  rowwise() %>% 
  mutate(library_name = replace(library_name, !grepl("Riptide", library_name)&grepl("KN", project_details), paste("Riptide", library_name))) %>% 
  ungroup()

# save object temporarily for apurva's review 06/09/2020 # 01/13/2021
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/20190829_WFU_U01_ShippingMaster/Tissues/Processed")
sequencing_run_log_IGM_df %>% openxlsx::write.xlsx(file = "sequencing_run_log_IGM_df.xlsx")
sequencing_run_log_IGM_df %>% openxlsx::write.xlsx(file = "sequencing_run_log_IGM_df.csv", row.names = F)

# CREATE SEQUENCING RUN LOG
sequencing_run_log <- sequencing_run_log_IGM_df %>% 
  mutate(sequencing_facility_name = "IGM") %>%  # placeholder XX 07/23/2020
  naniar::replace_with_na_all(~.x %in% c("", "\"\"", "NA", "N/A")) %>% 
  subset(!is.na(project_name)) %>% 
  subset(library_name != "zebrafish") ## XX temporary, waiting for oksana to update me about the zebrafish names

sequencing_run_log %>% get_dupes(date_samples_submitted, library_name, project_name)

setwd("~/Desktop/Database/csv files/sample_tracking")
write.csv(sequencing_run_log, "sequencing_run_log.csv", row.names = F)




## upload into the dropbox 
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user='postgres', password='postgres', dbname='PalmerLab_Datasets')
dbWriteTable(con, c("sample_tracking", "sequencing_run_log"), value = sequencing_run_log, row.names = FALSE, overwrite = T)
dbExecute(con,"ALTER TABLE sample_tracking.sequencing_run_log ADD CONSTRAINT flowcell_unique UNIQUE(date_samples_submitted,library_name,project_name)")

# disconnect
dbDisconnect(con)

## in terminal
cd /tmp
sudo su postgres
pg_dump -d PalmerLab_Datasets -t sample_tracking.sequencing_run_log > sequencing_run_log.sql
exit


