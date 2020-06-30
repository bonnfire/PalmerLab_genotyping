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
  left_join(., library_riptide %>% 
              select(riptide_number_library_name, plate_name) %>% 
              mutate(riptide_number_library_name = gsub("[[:space:]]", "", riptide_number_library_name)), 
            by = c("library_name" = "riptide_number_library_name")) %>% 
  mutate(
  project_name = case_when(
    grepl("Olivier") ~ "",
    
    TRUE ~ "NA"
  )
) # include the schema names

# save object temporarily for apurva's review 06/09/2020
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/20190829_WFU_U01_ShippingMaster/Tissues/Processed")
sequencing_run_log_IGM_df %>% openxlsx::write.xlsx(file = "sequencing_run_log_IGM_df.xlsx")

## upload into the dropbox 


