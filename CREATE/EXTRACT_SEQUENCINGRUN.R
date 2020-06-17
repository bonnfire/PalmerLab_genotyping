## Sequencing Run Log
library(lubridate)


# extract the spreadsheet notebook (first sheet; sheet = 1, "sequencing")
sequencing_run_log_IGM <- flipAPI::DownloadXLSX("https://www.dropbox.com/s/8pqkjfib37fric8/sequencing%20run%20log%20at%20IGM%20post%2004-2017.xlsx?dl=0", sheet = 1) 

# modify our copy 
sequencing_run_log_IGM_df <- sequencing_run_log_IGM %>%
  clean_names %>% 
  mutate_all(as.character) %>% 
  mutate_at(vars(matches("date", ignore.case = T)), 
            ~ openxlsx::convertToDate(as.numeric(.))) %>% 
  mutate_at(vars(matches("number_of_pools")), as.numeric) %>% 
  separate_rows(project,sep=c(",|and|&")) %>% 
  mutate(project_details = gsub(".* [(]", "", project), 
         project = gsub("[(].*", "", project)) %>% 
  subset(select = c(date_samples_submitted:project, project_details, for_palmer_lab_member:igm_billed_yet)) %>%  #changing the order of the df
  mutate_at(vars(matches("project")), ~ gsub("^ ","", .)) %>% 
  mutate(project_details = gsub("[()]", "", project_details)) %>%
  mutate_at(vars(matches("project")), ~ gsub(" $", "", .)) %>% 
  mutate_at(vars(matches("project")), ~ gsub("  ", " ", .)) %>%  #remove extra spaces and spaces at the end of string
  rowwise() %>% 
  mutate(project = replace(project, grepl("Wis\\d+", project_details, ignore.case = T), "Wisconsin"),
         project_details = replace(project_details, grepl("Wis\\d+", project_details, ignore.case = T), gsub("Wis", "", project_details, ignore.case = T)),
         project = replace(project, grepl("UMich\\d+", project_details, ignore.case = T), "UMich"),
         project_details = replace(project_details, grepl("UMich\\d+", project_details, ignore.case = T), gsub("UMich", "", project_details, ignore.case = T)),
         project_details = replace(project_details, project_details == project, NA), #remove the details if detail is equal to the project 
         project = replace(project, grepl("lib prep", project), paste0(project, " and seq")),
         project_details = replace(project_details, grepl("lib prep", project_details), paste0(project_details, " and seq"))) %>% 
  subset(project != "seq") # remove the rows that only have seq as project because that should be joined with lib prep
sequencing_run_log_IGM_df$project %>% unique

# save object temporarily for apurva's review 06/09/2020
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/20190829_WFU_U01_ShippingMaster/Tissues/Processed")
sequencing_run_log_IGM_df %>% write.xlsx(file = "sequencing_run_log_IGM_df.xlsx")

## upload into the dropbox 


