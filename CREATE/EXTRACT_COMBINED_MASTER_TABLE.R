## create combined_master_table

## have a table of rfid, project_name, and sex

######################## 
## COMBINED_MASTER_TABLE
######################## 

## update the following items in the following R files
#for p50
source("/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/P50/after_renewal/CREATE/CREATE_P50DATABASENAMES.R")
#for u01
source("/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/WFU_U01_MasterTables/CREATE/CREATE_WFUDATABASENAMES.R")
#for pcal
source("/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/pcal_brian_trainor/CREATE/CREATE_SAMPLEIDS_LIBPREP.R")
#for zebrafish
source("/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Zebrafish/CREATE/CREATE_SAMPLEIDS_LIBPREP.R")
#for huda
source("/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/u01_huda_akil/data_cleanup_Akil_SD.R")




combined <- list(p50 = shipments_p50_df[, c("p50", "rfid", "sex", "coatcolor")],
     u01 = shipments_df[, c("u01", "rfid", "sex", "coatcolor")], 
     pcal = pcal_sample_info_df[ , c("plate", "rfid")],
     zebrafish = zebrafish_sample_info_df[, c("plate", "rfid")], 
     huda_df = huda_df) 

# run when you have the information for pcal, zebrafish, and huda
# previously named combined_df
sample_metadata <- combined %>% 
  rbindlist(fill = T) %>% 
  mutate_all(~gsub(" ", "", .)) %>%  # remove all spaces for better joining (Library # -> Library#)
  rowwise() %>% 
  mutate(project_name = replace(project_name, is.na(project_name), 
                                case_when(
                                  grepl("p\\.cal", rfid) ~ "pcal_brian_trainor",
                                  grepl("Plate", rfid) ~ "r01_su_guo",
                                  grepl("Olivier_Oxy", u01) ~ "u01_olivier_george_oxycodone",
                                  grepl("Olivier_Co", u01) ~ "u01_olivier_george_cocaine",
                                  grepl("Mitchell", u01) ~ "u01_suzanne_mitchell",
                                  grepl("Jhou", u01) ~ "u01_tom_jhou",
                                  grepl("Kalivas_Italy", u01) ~ "u01_peter_kalivas_italy",
                                  grepl("Kalivas$", u01) ~ "u01_peter_kalivas_us",
                                  grepl("Meyer", p50) ~ "p50_paul_meyer",
                                  grepl("Richards", p50) ~ "p50_jerry_richards",
                                  grepl("Chen", p50) ~ "p50_hao_chen",
                                TRUE ~ "NA")
  )) %>% 
  mutate(organism = case_when(
    project_name %in% c("p50_paul_meyer", "p50_jerry_richards", "p50_hao_chen",
                        "u01_peter_kalivas_italy", "u01_peter_kalivas_us","u01_tom_jhou",
                        "u01_suzanne_mitchell", "u01_olivier_george_cocaine", "u01_olivier_george_oxycodone",
                        "u01_huda_akil") ~ "rat", 
    project_name == "r01_su_guo" ~ "zebrafish",
    project_name == "pcal_brian_trainor" ~ "california mouse"
  )) %>% 
  ungroup() %>% 
  mutate(strain = NA, 
         comments = NA) %>% 
  add_count(rfid) %>% 
  mutate(comments = replace(comments, n == 2, "Scrub")) %>% 
  select(rfid, sex, coatcolor, project_name, organism, strain, comments) 
  

