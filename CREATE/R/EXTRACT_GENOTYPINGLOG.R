## genotyping log 

# table with rfid, project_name, date, qc pass, lcation on tscc 
# generated by Deng/Riyan with details about sample redos, QC failed samples, location of important files on TSCC

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE")
# from 07/23/2020
# cd /projects/ps-palmer
# bcftools concat rcheng/wgs/coreC/vcfs/HS/*.vcf.gz | gzip > b2lin/HS_n672_concat.vcf.gz
# cd b2lin/HS_genotype
# bcftools query -l HS_n672_concat.vcf.gz > HS_n672_samplenames.txt
HS_n672_orig <- read.table("HS_n672_samplenames.txt") %>% 
  separate(V1, c("sample_id", "rfid"), "-") %>% 
  mutate(rfid = replace(rfid, sample_id == "120138361", "933000120138561"))
# fix dupe ids and reassign the subject id to the vcf 
# HS_n672_orig %>% 
#   select(rfid) %>% 
#   write(file = "HS_n672_samplenames_fix.txt")


# send to apurva for the p50 id's 07/24/2020
# HS_n672_orig %>% 
#   subset(!grepl("^933000", rfid)) %>% 
#   select(rfid) %>%
#   unlist() %>% 
#   as.character() %>% 
#   write(file = "genotyped_P50_n48_07242020.txt")

HS_n672_orig <- HS_n672_orig %>% 
  mutate(user = "Riyan", 
         location_on_tscc = "/projects/ps-palmer/rcheng/wgs/coreC/vcfs/HS",
         date = NA,
         QC_pass = NA) %>%
  left_join(sample_barcode_lib[, c("rfid", "library", "project_name")], by = "rfid") %>% 
  subset(grepl("^933000", rfid)) %>% 
  select(rfid, project_name, date, library, QC_pass, location_on_tscc, user)


HS <- list(seqrun01 = HS_n672_orig) %>% 
  rbindlist()
###############
#### SD RATS
###############
# cd /projects/ps-palmer
# bcftools concat rcheng/wgs/coreC/vcfs/SD/*.vcf.gz | gzip > b2lin/SD_genotype/SD_n_concat.vcf.gz
# bcftools query -l b2lin/SD_genotype/SD_n_concat.vcf.gz > b2lin/SD_genotype/SD_n_samplenames.txt

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE")
SD_n_orig <- read.table("SD_n_samplenames.txt") 
SD_n_orig <- SD_n_orig %>% rename("rfid" = "V1") %>% mutate_all(as.character) %>% 
  mutate(user = "Riyan", 
         location_on_tscc = "/projects/ps-palmer/rcheng/wgs/coreC/vcfs/ZF",
         date = NA,
         QC_pass = NA) %>% 
  left_join(sample_barcode_lib[, c("rfid", "library", "project_name")], by = "rfid") %>% 
  # left_join(sample_metadata[, c("rfid", "project_name")], by = "rfid") %>%  # %>% select(project_name) %>% table()
  select(rfid, project_name, date, library, QC_pass, location_on_tscc, user)



## XX ?? 
HS_n672_orig %>% 
  mutate(user = "Riyan", 
         location_on_tscc = "/projects/ps-palmer/rcheng/wgs/coreC/vcfs/HS",
         date = NA,
         QC_pass = NA) %>%
  left_join(sample_barcode_lib[, c("rfid", "library", "project_name")], by = "rfid") %>% 
  subset(!grepl("^933000", rfid)) %>% 
  select(rfid, project_name, date, library, QC_pass, location_on_tscc, user)

SD = list(seqrun01 = SD_n_orig) %>% 
  rbindlist()

###############
#### ZEBRAFISH
###############
# cd /projects/ps-palmer
# mkdir b2lin/ZF_genotype
# bcftools concat rcheng/wgs/coreC/vcfs/ZF/*.vcf.gz | gzip > b2lin/ZF_genotype/ZF_n_concat.vcf.gz
# bcftools query -l b2lin/ZF_genotype/ZF_n_concat.vcf.gz > b2lin/ZF_genotype/ZF_n_samplenames.txt

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE")
ZF_n_orig <- read.table("ZF_n_samplenames.txt") 
ZF_n_orig %>% rename("rfid" = "V1") %>% mutate_all(as.character) %>% left_join(sample_metadata[, c("rfid", "project_name")], by = "rfid") %>% select(project_name) %>% table()

ZF_n_orig <- ZF_n_orig %>% rename("rfid" = "V1") %>% mutate_all(as.character) %>% 
  mutate(user = "Riyan", 
         location_on_tscc = "/projects/ps-palmer/rcheng/wgs/coreC/vcfs/ZF",
         date = NA,
         QC_pass = NA) %>%
  # left_join(sample_metadata[, c("rfid", "project_name")], by = "rfid") %>% 
  left_join(sample_barcode_lib[, c("rfid", "library", "project_name")], by = "rfid") %>% ## doesn't exist in the library?
  left_join(., genotyping_fish_uniform_ids, by = c("rfid" = "rfid_genotyping")) %>% # change to sample_metadata and phenotyping center format
  mutate(rfid = coalesce(rfid.y, rfid)) %>% 
  select(rfid, project_name, date, library, QC_pass, location_on_tscc, user)

ZF = list(seqrun01 = ZF_n_orig) %>% 
  rbindlist()


### MERGE ALL TABLES

# assign project names to rfid
genotyping = list(HS = HS, 
                  SD = SD, 
                  ZF = ZF) %>% rbindlist(fill = T)


### check if matching foreign key of sample_metadata (but could also be sample_barcode_lib)
genotyping %>% left_join(sample_metadata[, c("rfid", "project_name")], by = "rfid") %>% subset(is.na(project_name.y)) # fish have already been uploaded into database 

genotyping_df <- genotyping %>% mutate(rfid = replace(rfid, rfid == "933000120157342", "933000120117342"),
                                       rfid = replace(rfid, rfid == "933000520138331", "933000120138331"),
                                       rfid = replace(rfid, rfid == "933000320046825", "933000320045825")) %>% 
  select(-project_name) %>% 
  left_join(sample_metadata[, c("rfid", "project_name")], by = "rfid") %>% 
  select(rfid, project_name, everything())

# once fix, create csv file 
setwd("~/Desktop/Database/csv files/sample_tracking")

genotyping_df2 <- genotyping_df %>% 
  subset(!grepl("20200204", rfid))
write.csv(genotyping_df2, file = "genotyping_log2.csv", row.names = F)

## XX temporary, come back to this 
# write.csv(genotyping_df, file = "genotyping_log.csv", row.names = F)
# genotyping_df %>% select(project_name) %>% table()

