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
              subset(grepl("R1", filename)) %>% # 08/12/2020 limit to just R1 for now
              mutate(library = str_extract(filename, "Riptide\\d+"),
                     plate = as.character(plate)) %>% 
              mutate(plate = coalesce(plate, library)) %>% 
              select(-library), 
            by = c("full_run_id" = "runid")) %>% 
  subset(pcr_barcode == plate | library == plate) %>% 
  select(-plate) %>% 
  # read.csv("fastq_seq_01_filenames.csv") ## haven't run yet 08/12/2020 ; should this be another name 
                   # ,
                   # "pcr_barcode" = "plate")) %>% 
  # naniar::vis_miss()
