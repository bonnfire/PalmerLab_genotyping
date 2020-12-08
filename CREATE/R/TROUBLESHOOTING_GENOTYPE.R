## "repeat mother and father cases (n=17)"
read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/hs_metadata_n1536_20201125.csv") %>% 
  mutate_all(as.character) %>% 
  # subset(dames == sires) # three issues 
  # subset(dames == ) # three issues 
  # subset(sires == ) # three issues 

unique_dames <- read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/hs_metadata_n1536_20201125.csv") %>% 
  mutate_all(as.character) %>% 
  select(dames) %>% 
  unlist() %>% 
  unique()

unique_dames_projects <- read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/hs_metadata_n1536_20201125.csv") %>% 
  mutate_all(as.character) %>% 
  distinct(dames, project_name) 

unique_sires <- read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/hs_metadata_n1536_20201125.csv") %>% 
  mutate_all(as.character) %>% 
  select(sires) %>% 
  unlist() %>% 
  unique()

unique_sires_projects <- read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/hs_metadata_n1536_20201125.csv") %>% 
  mutate_all(as.character) %>% 
  distinct(sires, project_name) 

read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/hs_metadata_n1536_20201125.csv") %>% 
  mutate_all(as.character) %>% 
  subset(dames %in% unique_sires)

# with cohort 
read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/hs_metadata_n1536_20201125.csv") %>% 
  mutate_all(as.character) %>% 
  subset(dames %in% unique_sires) %>% 
  distinct(dames, project_name) %>% 
  left_join(shipments_df[, c("u01", "dames", "cohort")] %>% 
              mutate(u01 = replace(u01, !is.na(u01), 
                                   case_when(grepl("Olivier_Oxy", u01) ~ "u01_olivier_george_oxycodone",
                                             grepl("Olivier_Co", u01) ~ "u01_olivier_george_cocaine",
                                             grepl("Olivier_Scrub", u01) ~ "u01_olivier_george_scrub",
                                             grepl("Mitchell", u01) ~ "u01_suzanne_mitchell",
                                             grepl("Jhou", u01) ~ "u01_tom_jhou",
                                             grepl("Kalivas_Italy", u01) ~ "u01_peter_kalivas_italy",
                                             grepl("Kalivas$", u01) ~ "u01_peter_kalivas_us",
                                             TRUE ~ "NA"))) %>% 
              rename("u01_dames" = "u01", 
                     "cohort_dames" = "cohort"), by = c("project_name"="u01_dames", "dames")) %>%  # where they were found as dames
  rename("u01_dames" = "project_name") %>% 
  left_join(shipments_df[, c("u01", "sires", "cohort")] %>% 
              mutate(u01 = replace(u01, !is.na(u01), 
                                   case_when(grepl("Olivier_Oxy", u01) ~ "u01_olivier_george_oxycodone",
                                             grepl("Olivier_Co", u01) ~ "u01_olivier_george_cocaine",
                                             grepl("Olivier_Scrub", u01) ~ "u01_olivier_george_scrub",
                                             grepl("Mitchell", u01) ~ "u01_suzanne_mitchell",
                                             grepl("Jhou", u01) ~ "u01_tom_jhou",
                                             grepl("Kalivas_Italy", u01) ~ "u01_peter_kalivas_italy",
                                             grepl("Kalivas$", u01) ~ "u01_peter_kalivas_us",
                                             TRUE ~ "NA"))) %>% 
              rename("u01_sires" = "u01", 
                     "cohort_sires" = "cohort"), by = c("dames" = "sires")) %>% distinct() %>% rename("breeder_id" = "dames") %>% 
  write.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/breeder_id_sire_or_dames_n17.xlsx")

shipments_df[, c("u01", "sires", "cohort")] %>% 
  rename("u01_sires", "sires", "cohort_sires") # where they were found as sires


## qc metadata for failed concordance checks
hs_qc_outliers <- read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/temp/hs_QC_outliers_n28_20201118.csv") %>% 
  mutate_all(as.character)



# create sample form for nida conference/retreat (verify database number with pi's)
nida <- read_excel("~/Downloads/nida_presentation (1).xlsx") %>% 
  janitor::clean_names() %>% 
  subset(grepl("u01|p50", project))
# For the letter
nida_formletter <- nida %>% 
  mutate(letter = paste0(project, "\n", "Good afternoon, \n As you know, I am a database specialist in the Palmer lab. In preparation for the upcoming NIDA retreat, I am putting together the summary of how HS rats are progressing. Here is the record that I have for your project. I wanted to double-check with you that the records showing in my database are correct. Please tell me if there are any discrepancies. \n The ", str_extract(project, "p50|u01") %>% toupper(), " grant funded ", funded, " HS rats. We have record of Wake Forest shipping ", wake_forest_shipments, " rats and phenotype data from your lab for ", phenotype_data_shared, " of these, while ", dead_exclude, " died.", "On our end, we have extracted the DNA from the spleens of ", dna_extracted_from_spleens, " and have genotyped ", riptide_sequenced, ". \n Thank you for your time. 
                         \n Bonnie"))

# troubleshooting mysterious rfid's found in riyan's sample sheet October 2020
read.csv("pgadmin_kn02_sample_barcodelib.csv") %>% 
  mutate_all(as.character) %>% subset(library_name == "Riptide30") %>% 
  anti_join(read.xlsx("Riptide_control_96.xlsx") %>% mutate_all(as.character) %>% select(sample.id, user.id), ., by = c("sample.id"= "rfid"))

khai_riptide30 <-read.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Khai-Minh Nguyen/Sequencing Submission Files/2020-08-06-Flowcell Sample-Barcode list (KN02 Pool) .xlsx") %>% 
  mutate_all(as.character) %>% janitor::clean_names() %>% rename("rfid" = "sample_id") %>% subset(library == "Riptide30") 

db_riptide30 <- read.csv("pgadmin_kn02_sample_barcodelib.csv") %>% 
  mutate_all(as.character) %>% subset(library_name == "Riptide30") 

apurva_riptide30 <- read.xlsx("Riptide_control_96.xlsx") %>% mutate_all(as.character) %>% 
  rename("rfid" = "sample.id")

milad_24 <- read.csv("pedigree_info_24_samples.csv") %>% 
  mutate_all(as.character)


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
  
  
