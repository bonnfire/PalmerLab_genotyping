## troubleshooting sire x dame 
metadata_n1536_corrected <- read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/hs_metadata_n1536_20201125.csv") %>% 
  mutate_all(as.character) %>%
  left_join(wfu_corrections_sire_dame, by = c("dames" = "breeder_id")) %>% 
  left_join(wfu_corrections_sire_dame, by = c("sires" = "breeder_id")) %>%
  rowwise() %>% 
  mutate(breeder_sex.y = replace(breeder_sex.y, breeder_sex.y == "dam", dames),
         dames = replace(dames, breeder_sex.x == "sire", sires),
         sires = replace(sires, breeder_sex.x == "sire", breeder_sex.y)) %>% 
  ungroup() %>% 
  select(-matches('breeder_sex[.][xy]')) %>% 
  mutate_all(str_trim)

gbs_pedigree <- read.table("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/GBS6170.fam4Mendel") %>% 
  mutate_all(as.character) %>% 
  select(-V1) %>% 
  rename("rfid" = "V2", 
         "sires" = "V3",
         "dame" = "V4")

metadata_n1536_corrected %>% 
  subset(dames %in% c("HSF02609", "HSF02616", "HSF02620", "HSF02629", "HSF02654", "HSF02661")|sires %in% c("HSM02602", "HSM02614", "HSM02627", "HSM02628", "HSM02649", "HSM02661")) %>% 
  as.data.frame() %>% left_join(gbs_pedigree, by = c("dames"="V4"))


# checking for fixes
metadata_n1536_corrected %>% 
  select(dames, sires) %>%
  gather("parent", "parent_id") %>%
  distinct() %>% 
  get_dupes(parent_id)

# pairing to pedigree (# checking if these match with den's file)
metadata_n1536_corrected %>% 
  select(project_name, dames, sires) %>%
  gather("parent", "parent_id", -project_name) %>%
  distinct(parent_id, project_name) %>%
  group_by(parent_id) %>%
  summarise(projects = paste(project_name, collapse = ",")) %>% 
  ungroup() %>% 
  distinct(parent_id, projects) %>% 
  # subset(grepl("_", parent_id)) %>% 
  left_join(pedigree_12082020_temp, by = c("parent_id" = "id_f51")) %>% 
  subset(projects != "p50_hao_chen_2014") %>% 
  subset(is.na(cc)) %>% dim

## temp metadata corrected (sire x dame pair for four rats are errors)
metadata_n1536_corrected %>% 
  distinct() %>% 
  mutate(sires = replace(sires, sires == dames, "72774_3")) %>% 
  write.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/metadata_hsrats_n1536_corrected_v2.csv", row.names = F)



# repeat repeat mother father cases 
wfu_corrections_sire_dame <- openxlsx::read.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/breeder_id_sire_or_dames_n17_wfu.xlsx") %>% 
  mutate_all(as.character) %>% 
  mutate(breeder_sex = ifelse(is.na(DAM), "sire", "dam")) %>% 
  select(breeder_id, breeder_sex)
  

  
    



## for oksana meeting with su guo 
# create phenotyping and sequencing table
sample_barcode_lib_12092020 <- read.csv("sample_barcode_lib_10262020_n3451.csv") %>% 
  select(rfid, project_name, barcode, library_name, pcr_barcode, filename, comments, flag) %>% 
  rbind(kn04_df %>% 
          rename("library_name" = "library") %>% 
          mutate(filename = "2020-10-29-Flowcell Sample-Barcode list (KN04 Pool).xlsx", 
                 flag = "NA") %>% 
          select(rfid, project_name, barcode, library_name, pcr_barcode, filename, comments, flag)) %>% 
  mutate_all(as.character)
# create a table for sample id,library prep,and phenotypes
oksana_fish_progress12092020 <- sample_barcode_lib_12092020 %> %
  subset(project_name == "r01_su_guo") %>% 
  mutate(larva_breeder = "NA") %>% 
  mutate(larva_breeder = replace(larva_breeder, grepl("Plate", rfid), "larva"), 
         larva_breeder = replace(larva_breeder, library_name == "Fish Breeders", "breeder")) %>% 
  bind_rows(riptide10 %>% 
              subset(!x2 %in% c(missingriptide10$sample_id)) %>% select(x2) %>% rename("rfid" = "x2") %>% 
              mutate(rfid = gsub("_(\\D)(\\d+)$", "_\\2\\1", rfid)) %>% 
              mutate(library_name = "Riptide10", 
                     larva_breeder = "larva",
                     project_name = "r01_su_guo",
                     comments = "not found in flowcell")) %>% 
  rowwise() %>% 
  mutate(rfid =  replace(rfid, grepl("Plate", rfid), gsub("-", "_", rfid) %>% gsub("_(\\D)(\\d+)$", "_\\2\\1", .))) %>% 
  ungroup() 
write.csv(oksana_fish_progress12092020, "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/oksana_fish_progress12092020_sample_bar_lib.csv", row.names = F)

missingriptide10 <- lapply(flowcell_files[2], function(x){
  x <- u01.importxlsx(x)[[1]] %>% 
    clean_names() 
  return(x)
})[[1]] %>% 
  subset(library == "Riptide10")
missingriptide10$library %>% table()

riptide10 <- openxlsx::read.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Khai-Minh Nguyen/Riptide library prep/Riptide 01-10/Riptide-10 Fish 20200204 Plate1 A904R7C7/Riptide10 Library.xlsx") %>% 
  mutate_all(as.character) %>% 
  clean_names() %>% 
  subset(grepl("Plate", x2))

riptide10 %>% 
  subset(!x2 %in% c(missingriptide10$sample_id)) 

# checking extraction records
khai_tissueextraction_df_join %>% subset(rfid %in% c(riptide10 %>% 
                                                       subset(!x2 %in% c(missingriptide10$sample_id)) %>% select(x2) %>% mutate(x2 = gsub("_", "", x2)) %>% unlist() %>% paste0)) %>% View()
3610
1190

# update db bc names are wrong
# drop from object the nontested 
# note when adding kn04 flowcell, exclude the suguo, processed here already
guo_sample_barcode_lib <- oksana_fish_progress12092020 %>% #these do not include the 5 that were not in the flowcell, find w comments == "not found in flowcell"
  subset(!is.na(barcode)) %>% 
  mutate(project_name = replace(project_name, larva_breeder=="breeder", "r01_su_guo_breeders"),
         project_name = replace(project_name, larva_breeder=="larva", "r01_su_guo_larvae")) %>%
  select(-larva_breeder) %>% 
  mutate(rfid = gsub("_", "-", rfid)) #1771-5
write.csv(guo_sample_barcode_lib, "~/Desktop/Database/csv files/sample_tracking/guo_sample_barcode_lib_n1766.csv", row.names = F)





## join to hs_metadata to make sure that sires and dames are represented


## repeat mother and father cases
read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/hs_metadata_n1536_20201125.csv")

read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/hs_metadata_n1536_20201125.csv") 

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
  
  
