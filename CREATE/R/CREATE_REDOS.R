### create redo's table for sample_tracking 
redoes <- read.csv("~/Desktop/Database/csv files/snapshots/sample_tracking.genotyping_log_04202021_n5176.csv", colClasses = "character") %>% 
  subset(qc_sex == "reject"|qc_missing == "reject"|qc_heterozygosity == "reject"|qc_coat_color_albino == "reject") %>% 
  distinct(rfid, date) %>% 
  subset(rfid %in% reextract_01$rfid) %>% 
  mutate(submitted_for_extraction = ifelse(rfid %in% (reextract_01 %>% subset(!is.na(storage_box_of_spleen)) %>%
                              subset(!grepl("mismatch", comments, ignore.case = T)) %>% 
                              subset(!(QC_coat_color_albino == "reject"&num_reject==1&rfid!="933000320046318")) %>% 
                              select(rfid) %>% 
                              unlist() %>% as.character), "T", "F")) %>% 
  mutate(comments = NA)
  
write.csv(redoes, "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/sample_tracking/generated/redoes_n110.csv", row.names = F)
