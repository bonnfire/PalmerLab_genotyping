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
source("/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Zebrafish/CREATE/CREATE_EXCEL.R") # instead of CREATE_SAMPLEIDS_LIBPREP.R
#for huda
source("/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/u01_huda_akil/data_cleanup_Akil_SD.R")


## XX fix the source of this -- temporary 07/30/2020 
shipments_df <- shipments_df %>% 
  mutate(u01 = replace(u01, grepl("scrub", comment, ignore.case=T)&grepl("Olivier", u01), "Olivier_Scrub")) %>% 
  mutate(cohort = replace(cohort, u01 == "Olivier_Scrub", NA)) %>% 
  distinct()

# this is after the id's have been processed
# zebrafish_sample_info_df <- zebrafish_sample_info_df %>% mutate(project_name = "r01_su_guo")
Zebrafish_Guo_xl <- Zebrafish_Guo_xl %>% mutate(project_name = "r01_su_guo") %>% 
  rename("rfid" = "fish_id") %>% 
  rowwise() %>% 
  mutate(rfid = replace(rfid, grepl("\\d+_Plate\\d+_", rfid), gsub("_", "", rfid))) %>% 
  ungroup()



combined <- list(p50 = shipments_p50_df[, c("p50", "rfid", "sex", "coatcolor")],
     u01 = shipments_df[, c("u01", "rfid", "sex", "coatcolor")], 
     pcal = pcal_sample_info_df[ , c("plate", "rfid")],
     zebrafish = Zebrafish_Guo_xl[, c("rfid", "project_name")],
     # zebrafish = zebrafish_sample_info_df[, c("plate", "rfid", "project_name")], 
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
                                  grepl("Olivier_Scrub", u01) ~ "u01_olivier_george_scrub",
                                  grepl("Mitchell", u01) ~ "u01_suzanne_mitchell",
                                  grepl("Jhou", u01) ~ "u01_tom_jhou",
                                  grepl("Kalivas_Italy", u01) ~ "u01_peter_kalivas_italy",
                                    grepl("Kalivas$", u01) ~ "u01_peter_kalivas_us",
                                  grepl("Meyer", p50) ~ "p50_paul_meyer_2020",
                                  grepl("Richards", p50) ~ "p50_jerry_richards_2020",
                                  grepl("Chen", p50) ~ "p50_hao_chen_2020",
                                TRUE ~ "NA")
  )) %>% 
  mutate(organism = case_when(
    project_name %in% c("p50_paul_meyer", "p50_jerry_richards", "p50_hao_chen",
                        "u01_peter_kalivas_italy", "u01_peter_kalivas_us","u01_tom_jhou",
                        "u01_suzanne_mitchell", "u01_olivier_george_cocaine", "u01_olivier_george_oxycodone",
                        "u01_oliver_george_scrub",
                        "u01_huda_akil") ~ "rat", 
    project_name == "r01_su_guo" ~ "zebrafish",
    project_name == "pcal_brian_trainor" ~ "california mouse"
  )) %>% 
  ungroup() %>% 
  mutate(strain = NA, 
         comments = NA) %>% distinct() %>%  
  # add_count(rfid) %>% 
  # mutate(comments = replace(comments, n == 2, "Scrub")) %>% 
  select(rfid, sex, coatcolor, project_name, organism, strain, comments) 
  
# write.csv(sample_metadata, file = "sample_metadata.csv", row.names = F)


## exception for getting data from genotyping file 
seq01_fish <- flowcell_df %>% 
  mutate(rfid = coalesce(rfid, sample_id)) %>% 
  left_join(sample_metadata[, c("rfid", "project_name")], by ="rfid") %>% 
  mutate(library = gsub("Riptide-", "Riptide", library)) %>% subset(is.na(project_name)&library=='UMich8_Fish') %>% 
  rename("comments" = "comment") %>% 
  mutate(comments = "Test samples with high missing rate, exclude from imputation, no phenotypes",
         sex = NA, 
         coatcolor = NA, 
         project_name = "r01_su_guo", 
         organism = NA, 
         strain = "Ekkwill fish") %>% 
  select(rfid, sex, coatcolor, project_name, organism, strain, comments) 

genotyping_fish_uniform_ids <- seq01_fish %>% 
  select(rfid) %>% 
  mutate(rfid_genotyping = gsub("[ -]", "_", rfid)) 


# %>% 
# write.csv(file = "no_phenotype_fish_sample_metadata.csv", row.names = F)
# seq01_fish %>% write.csv(file = "no_phenotype_fish_sample_metadata_fixrfid.csv", row.names = F) # don't change the original rfid's assigned by the center


sample_metadata <- rbind(sample_metadata, seq01_fish)

## updates after 09/04/2020
# Jerry shipment 03
setwd("/home/bonnie/Desktop/Database/csv files/p50_jerry_richards_2020")
shipments_p50_df %>% subset(p50 == "Richards"&cohort == "C03") %>%
  mutate_all(~gsub(" ", "", .)) %>%  # remove all spaces for better joining (Library # -> Library#)
  mutate(project_name = "p50_jerry_richards_2020",
         organism = NA, 
         strain = NA, 
         comments = NA) %>% select(rfid, sex, coatcolor, project_name, organism, strain, comments) %>% write.csv("c03_samptrack_sampmetadata.csv", row.names = F)
## Jerry shipment 03.5?? 

## xx Jhou shipment 16