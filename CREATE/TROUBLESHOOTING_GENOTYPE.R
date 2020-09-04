## troubleshooting for Den 08/07/2020

# Merge the sequencing run log and sample ID barcode tables by library_name
# Merge the sequencing run log and sample ID barcode tables by library_name
sample_barcode_lib %>% 
  left_join(sequencing_run_log[, c("full_run_id", "library_name")], by = c("library" = "library_name")) %>% 
  subset(!grepl("UMich", library)) %>% 
  subset(rfid %in% genotyping_df2$rfid) %>% 
  distinct() %>% # not sure why duplicates, but it makes the right number of samples
  # write.csv("sequencing01_n672_wfullrunid.csv") # sent in slack
  write.csv("sequencing01.csv") # update the dropbox object

## create object in tscc, in projects/ps-palmer/sequencing_data/
# fastq_seq01_filenames <- data.frame(filename = c(list.files(path = "200221_A00953_0069_BH5T5LDSXY", full.names=T),  
# list.files(path = "200326_A00953_0086_BHC2FMDSXY", full.names = T)))

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE")
fastq_seq_01_filenames <- read.csv("fastq_seq_01_filenames.csv") %>% 
  separate(filename, into = c("runid", "filename"), sep = "/") %>% 
  select(-X) %>% 
  mutate(plate = str_extract(filename, "Plate_\\d+") %>% parse_number)
  
sample_barcode_lib %>% 
  left_join(sequencing_run_log[, c("full_run_id", "library_name")], by = c("library" = "library_name")) %>% 
  subset(!grepl("UMich", library)) %>% 
  subset(rfid %in% genotyping_df2$rfid) %>% 
  distinct() %>% 
  mutate(pcr_barcode = as.character(pcr_barcode)) %>% 
  left_join(fastq_seq_01_filenames %>% 
              mutate(Rnum = str_extract(filename, "R\\d"), file = gsub("R\\d", "", filename)) %>% 
              select(-filename) %>% 
              mutate(runid = paste0(runid, ";", file)) %>% spread(Rnum, file) %>% 
              separate(runid, c("runid", "file"), ";") %>% 
              select(-file) %>% mutate(R1 = gsub("__", "_R1_", R1), R2 = gsub("__", "_R2_", R2)) %>% mutate(files = paste0(R1, ";", R2)) %>% 
              select(-c("R1", "R2")) %>% # 08/31/2020 both R1 and R2
              mutate(library = str_extract(files, "Riptide\\d+"),
                     plate = as.character(plate)) %>% 
              mutate(plate = coalesce(plate, library)) %>% 
              select(-library), 
            by = c("full_run_id" = "runid")) %>% 
  subset(pcr_barcode == plate | library == plate) %>% 
  select(-plate) %>% 
  # read.csv("sample_sheet_n672.csv", row.names = F) ## haven't run yet 08/12/2020 ; should this be another name 
                   # ,
                   # "pcr_barcode" = "plate")) %>% 
  # naniar::vis_miss()
  
  
