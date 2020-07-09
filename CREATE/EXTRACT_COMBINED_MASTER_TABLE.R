## create combined_master_table

## have a table of rfid, project_name, and sex

######################## 
## COMBINED_MASTER_TABLE
######################## 

## update the following items in the following R files
#for p50
source("/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/P50/after_renewal/CREATE/CREATE_P50DATABASENAMES.R")
#for u01
source()
#for pcal

#for zebrafish
source("/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Zebrafish/CREATE/CREATE_SAMPLEIDS_LIBPREP.R")
#for huda
source("/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/u01_huda_akil/data_cleanup_Akil_SD.R")




combined <- list(p50 = shipments_p50_df[, c("p50", "rfid", "sex")],
     u01 = shipments_df[, c("u01", "rfid", "sex")], 
     zebrafish = zebrafish_sample_info_df[, c("plate", "rfid")], 
     huda_df = huda_df,
     ...) 

# run when you have the information for pcal, zebrafish, and huda
combined_df <- combined %>% 
  rbindlist(fill = T) %>% 
  mutate_all(~gsub(" ", "", .)) # remove all spaces for better joining (Library # -> Library#)