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


## XX fix the source of this -- temporary 07/30/2020 
shipments_df <- shipments_df %>% 
  mutate(u01 = replace(u01, grepl("scrub", comment, ignore.case=T)&grepl("Olivier", u01), "Olivier_Scrub")) %>% 
  mutate(cohort = replace(cohort, u01 == "Olivier_Scrub", NA)) %>% 
  distinct()

zebrafish_sample_info_df <- zebrafish_sample_info_df %>% mutate(project_name = "r01_su_guo")

combined <- list(p50 = shipments_p50_df[, c("p50", "rfid", "sex", "coatcolor")],
     u01 = shipments_df[, c("u01", "rfid", "sex", "coatcolor")], 
     pcal = pcal_sample_info_df[ , c("plate", "rfid")],
     zebrafish = zebrafish_sample_info_df[, c("plate", "rfid", "project_name")], 
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
  



dbExecute(con, "CREATE TABLE sample_tracking.sample_metadata (
	rfid VARCHAR(19) NOT NULL, 
	sex VARCHAR(4), 
	coatcolor VARCHAR(9), 
	project_name VARCHAR(28) NOT NULL, 
	organism VARCHAR(9) NOT NULL, 
	strain VARCHAR(32), 
	comments VARCHAR(5),
CONSTRAINT sample_metadata_pk PRIMARY KEY(rfid)
);")



## upload the pgdump file into the db

con <- dbConnect(dbDriver("PostgreSQL"), dbname="PalmerLab_Datasets",user="postgres",password="postgres")
dbWriteTable(con, c("sample_tracking","sample_metadata"), value = sample_metadata, row.names = FALSE, overwrite = T)
dbExecute(con,"ALTER TABLE sample_tracking.sample_metadata ADD CONSTRAINT rfid_unique UNIQUE(rfid)")

# disconnect
dbDisconnect(con)

## in terminal
cd /tmp
sudo su postgres
pg_dump -d PalmerLab_Datasets -t sample_tracking.sample_metadata > sample_metadata.sql
exit
