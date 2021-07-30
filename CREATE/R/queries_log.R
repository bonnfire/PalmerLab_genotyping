## data queries
# clean kn08 fastq files
read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/sample_tracking/generated/fastq_kn08_filenames.csv", stringsAsFactors = F) %>% 
  separate(fastq_filename, into = c("runid", "fastq_filename"), sep = "/") %>%
  mutate(Rnum = gsub(".*_(R\\d)_.*", "\\1", fastq_filename), 
         file = gsub("_(R\\d)_", "__", fastq_filename)) %>% 
  distinct(runid, file) %>% 
  mutate(fastq_files = paste0(gsub("__", "_R1_", file), "; ", gsub("__", "_R2_", file))) %>% 
  mutate(file = gsub("^(7[1-9]|80)", "R\\1", file)) %>% 
  mutate(library_name = paste0("Riptide", parse_number(gsub("(R\\d+)_.*", "\\1", file))),
         pcr_barcode = parse_number(gsub(".*_(S\\d+)_.*", "\\1", file)) %>% as.character) %>% 
  select(-file) %>% 
  subset(grepl("Riptide(7[1-9]|80)", library_name)) %>% 
  write.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/sample_tracking/generated/fastq_kn08_filenames_processed.csv", row.names = F)



# clean kn06 fastq files
read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/sample_tracking/generated/fastq_kn06_filenames.csv", stringsAsFactors = F) %>% 
  separate(fastq_filename, into = c("runid", "fastq_filename"), sep = "/") %>%
  mutate(Rnum = gsub(".*_(R\\d)_.*", "\\1", fastq_filename), 
         file = gsub("_(R\\d)_", "__", fastq_filename)) %>% 
  distinct(runid, file) %>% 
  mutate(fastq_files = paste0(gsub("__", "_R1_", file), "; ", gsub("__", "_R2_", file))) %>% 
  mutate(file = gsub("^(5[1-9]|60)", "R\\1", file)) %>% 
  mutate(library_name = paste0("Riptide", parse_number(gsub("(R\\d+)_.*", "\\1", file))),
         pcr_barcode = parse_number(gsub(".*_(S\\d+)_.*", "\\1", file)) %>% as.character) %>% 
  select(-file) %>% 
  subset(grepl("Riptide(5[1-9]|60)", library_name)) %>% 
  write.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/sample_tracking/generated/fastq_kn06_filenames_processed.csv", row.names = F)



## clean kn07 fastq files
read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/sample_tracking/generated/fastq_seq07_filenames.csv", stringsAsFactors = F) %>% 
  separate(fastq_filename, into = c("runid", "fastq_filename"), sep = "/") %>%
  mutate(Rnum = gsub(".*_(R\\d)_.*", "\\1", fastq_filename), 
         file = gsub("_(R\\d)_", "__", fastq_filename)) %>% 
  distinct(runid, file) %>% 
  mutate(fastq_files = paste0(gsub("__", "_R1_", file), "; ", gsub("__", "_R2_", file))) %>% 
  mutate(file = gsub("^(6[1-9]|70)", "R\\1", file)) %>% 
  mutate(library_name = paste0("Riptide", parse_number(gsub("(R\\d+)_.*", "\\1", file))),
         pcr_barcode = parse_number(gsub(".*_(S\\d+)_.*", "\\1", file)) %>% as.character) %>% 
  select(-file) %>% 
  subset(grepl("Riptide(6[1-9]|70)", library_name)) %>% 
  write.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/sample_tracking/generated/fastq_seq07_filenames_processed.csv", row.names = F)
  



## update database with all fastq files 
read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/metadata_kn05_n960.csv", colClasses = "character") %>% distinct(rfid, fastq_files, filename, project_name) %>% 
  rbind(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/kn04_fastq_sample_metadata_n952.csv", colClasses = "character") %>% 
          distinct(rfid, fastq_files, filename, project_name)) %>% 
  rbind(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/sample_barcode_lib_idconversion_kn03_n960.csv", colClasses = "character") %>% 
          distinct(rfid, fastq_files, filename, project_name)) %>%
  rbind(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/kn02_fastq_sample_metadata_n960.csv", colClasses = "character") %>% 
          distinct(rfid, fastq_files, filename, project_name)) %>% 
  rbind(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/sample_barcode_lib_idconversion_flowcell1_n960.csv", colClasses = "character") %>% 
          distinct(rfid, fastq_files, filename, project_name)) %>% 
  rbind(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/sample_barcode_lib_idconversion_flowcell2_n571.csv", colClasses = "character") %>% 
          distinct(rfid, fastq_files, filename, project_name)) %>% 
  mutate(rfid = ifelse(!(grepl("Casper|^2622", rfid)&(project_name == "r01_su_guo")), gsub(" ", "", rfid), rfid),
         rfid = gsub("-", "_", rfid),
         rfid = ifelse(grepl("Plate", rfid), gsub("(\\D)(\\d+)$", "\\2\\1", rfid), rfid)) %>%
  select(-project_name) %>% 
  write.csv("~/Desktop/Database/csv files/sample_tracking/sample_tracking_fastq_temp_n5363.csv",row.names = F)

read.csv("~/Desktop/Database/csv files/snapshots/sample_barcode_lib_03302021.csv", colClasses = "character") %>% 
  left_join(read.csv("~/Desktop/Database/csv files/snapshots/sample_tracking.sample_metadata_03242021.csv", colClasses = "character"), by = c("rfid")) %>% 
  subset(is.na(strain))
  naniar::vis_miss()


# %>% 
  # select(filename) %>% table()

# sample metadata for kn05

# extract khai's riptide libraries to make sure that the metadata is stored correctly (don't need to, straight copy and paste)
# join zebrafish excel and gdna excel to flowcell
# compare to other sample metadata and make sure columns are in order
# send gh dataframe to Riyan 

kn05_df %>%
  select(-sample_id, -project_name) %>% 
  select(rfid, everything()) %>% 
  mutate_all(as.character) %>% 
  mutate(rfid = gsub("20200617", "20200616", rfid)) %>% 
  left_join(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Zebrafish/CREATE/zebrafish_larvae_n6580_phenotypes.csv") %>% 
              mutate(dna_collected_y_n = replace(dna_collected_y_n, rfid == "20200616_Plate2_9A", "N")) %>% 
              mutate_all(as.character) %>% 
              mutate(project_name = "r01_su_guo_larvae") %>% 
              select(rfid, dna_collected_y_n, mother, father, project_name), by = "rfid") %>% 
  left_join(akil_gdna_df %>% select(rfid = sample_id, sex, project_name), by = "rfid") %>% 
  mutate(project_name = coalesce(project_name.x, project_name.y)) %>% 
  select(-matches("[.][xy]$")) %>% 
  left_join(openxlsx::read.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Zebrafish/CREATE/gh_classification_n147.xlsx") %>% 
              mutate(rfid = gsub("-", "_", rfid)) %>% 
              select(rfid, gh_cat), by = "rfid") %>% 
  mutate(comments = coalesce(comments, gh_cat)) %>% # since gh cat and comments are mutually exclusive, you can coalesce
  mutate(project_name = replace(project_name, grepl("casper", comments), "r01_su_guo_larvae")) %>% 
  mutate(original_rfid = rfid) %>% 
  mutate(original_rfid = gsub("20200616", "20200617", original_rfid)) %>%
  rename("library_name" = "library") %>% 
  left_join(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/fastq_seq05_filenames.csv") %>%
              separate(fastq_filename, into = c("runid", "fastq_filename"), sep = "/") %>%
              mutate(Rnum = gsub(".*_(R\\d)_.*", "\\1", fastq_filename), 
                     file = gsub("_(R\\d)_", "__", fastq_filename)) %>% 
              distinct(runid, file) %>% 
              mutate(fastq_files = paste0(gsub("__", "_R1_", file), "; ", gsub("__", "_R2_", file))) %>% 
              mutate(file = gsub("^(4[1-9]|50)", "R\\1", file)) %>% subset(grepl("^R\\d+", file)) %>% 
              mutate(library_name = paste0("Riptide", parse_number(gsub("(R\\d+)_.*", "\\1", file))),
                     pcr_barcode = parse_number(gsub(".*_(S\\d+)_.*", "\\1", file)) %>% as.character) %>% 
              select(-file), by = c("library_name", "pcr_barcode")) %>% 
  mutate(strain = case_when(grepl("akil", project_name) ~ "Sprague Dawley",
    grepl("guo", project_name)&!grepl("casper", comments) ~ "Ekkwill fish",
    grepl("guo", project_name)&grepl("casper", comments) ~ "AB line",
    TRUE ~ NA_character_),
    organism = ifelse(grepl("guo", project_name), "zebrafish", "rat")) %>% 
  mutate(rfid = gsub(" ", "", rfid)) %>% 
  mutate(flag = NA,
         coatcolor = NA,
         filename = "2021-01-21-Flowcell Sample-Barcode list (KN05 Pool).xlsx") %>% 
  # subset(is.na(project_name)) # for now, including casper as r01_su_guo_larvae
  select(rfid, original_rfid, father, mother, runid, fastq_files, library_name, pcr_barcode, project_name, barcode, filename, comments, flag, sex, coatcolor, organism, strain) %>% 
write.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/metadata_kn05_n960.csv", row.names = F)



read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/metadata_hsrats_n1536_corrected_v2.csv") %>% mutate_all(as.character) %>% 
  bind_rows(  read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/kn04_hs_parents_n376.csv") %>% mutate_all(as.character) ) %>% 
  write.csv("~/Desktop/Database/csv files/sample_tracking/metadata_hsrats_n1912.csv", row.names = F)


## find samples in database 

khai_tissueextraction_df_join %>% subset(rfid %in% c("00077E76C6",
"00078A19B7",
"00078A2315",
"00078A18A7",
"00078A01A6",
"00078A193E",
"00078A17A7",
"00077E67B5",
"00077E76FE",
"00078A02CB")) # did not find (these are Milad's samples )



## to reextract
reextract_01 <- read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/sample_tracking/received/genotype/genotype_log.csv", sep = ";") %>% 
  mutate_all(as.character) %>% 
  # rbind(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/genotype_log_933000320046318.csv", sep = ";") %>% mutate_all(as.character)) %>% 
  subset(QC_sex == "reject"|QC_missing == "reject"|QC_heterozygosity == "reject"|QC_coat_color_albino == "reject") %>% 
  left_join(khai_tissueextraction_df_join %>% 
              select(rfid, storage_box_of_spleen, freezer_location_of_tissue, position_in_box, comments), by = "rfid") %>% 
  mutate(comments = ifelse(rfid == "933000320046318", paste0("beagle imputed albino snp"), comments))  

reextract_01 %>% 
  subset(!is.na(storage_box_of_spleen)) %>%
  subset(!grepl("mismatch", comments, ignore.case = T)) %>% 
  subset(!(QC_coat_color_albino == "reject"&num_reject==1&rfid!="933000320046318")) %>% 
  write.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/rerun_n96_02052021.csv", row.names = F)

reextract_01 %>% 
  subset(is.na(storage_box_of_spleen)) %>% 
  left_join(khai_tissueextraction_df_join %>% 
              select(sample_id_barcode, storage_box_of_spleen, freezer_location_of_tissue, position_in_box), by = c("rfid" = "sample_id_barcode"))


## figure out these four cases and add constraint to foreign key 
read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/metadata_hsrats_n1536_corrected_v2.csv") %>% subset(library_name == "Riptide16") %>%  anti_join(khai_tissueextraction_df_join %>% subset(riptide_plate_number == "Riptide16"),. , by = "rfid") %>% select(transponder, sample_id_barcode, rfid)


## for db update, troubleshoot missing id's in extraction log 
khai_tissueextraction_df_1_52 %>% 
  select(rfid, project_name) %>% 
  subset(grepl("olivier", project_name)) %>% mutate(extracted = "khai_record") %>% 
  # left_join(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/sample_barcode_lib_01052021.csv") %>% 
  #             mutate_all(as.character) %>%  subset(grepl("olivier", project_name)), by = "rfid") %>% 
  mutate(project_name = replace(project_name, rfid %in% c("933000320047859", "933000320047950", "933000320047941", "933000320187183", "933000320186952"), "u01_olivier_george_cocaine"), 
         project_name = replace(project_name, rfid %in% c("933000320187305", "933000320187314"), "u01_olivier_george_oxycodone")) %>% 
  select(project_name) %>% table()
  

#fish breeders 

### kn04

# clean kn04 object, comments and x8 (No transponder and barcode doesn't match)
# append sample metadata (sex and coatcolor , leaving out sire and dame)
# join the fastq files info
kn04_sample_metadata <- read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/fastq_seq04_filenames.csv") %>%
  separate(fastq_filename, into = c("runid", "fastq_filename"), sep = "/") %>%
  mutate(Rnum = gsub(".*_(R\\d)_.*", "\\1", fastq_filename), 
         file = gsub("_(R\\d)_", "__", fastq_filename)) %>% 
  distinct(runid, file) %>% 
  mutate(fastq_files = paste0(gsub("__", "_R1_", file), "; ", gsub("__", "_R2_", file))) %>% subset(grepl("^Riptide([3][1-9]|40)", file)) %>% 
  mutate(library_name = str_extract(file, "Riptide\\d+"),
         pcr_barcode = parse_number(gsub(".*_(S\\d+)_.*", "\\1", file)) %>% as.character) %>% 
  select(-file) %>% 
  right_join(kn04_df %>%  
               rename("library_name" = "library") %>%
               mutate(comments = coalesce(comments, x8)) %>% 
               rowwise() %>% 
               mutate(comments = ifelse(!is.na(comments)&!is.na(x8), paste0(comments, x8), comments),
                      flag = NA,
                      comments = ifelse(grepl("Barcode does not match", comments), "Barcode does not match number written on top of tube", comments)),
             by = c("library_name", "pcr_barcode")) %>% 
  mutate(organism = ifelse(!grepl("guo", project_name), "rat", "zebrafish"), strain = ifelse(organism != "zebrafish", "Heterogenous stock", "Ekkwill fish")) %>% 
  mutate(rfid = gsub(" ", "", rfid)) %>% 
  left_join(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/samplemetadata_10192020.csv") %>% mutate_all(as.character), by = "rfid") %>% 
  mutate(filename = "2020-10-29-Flowcell Sample-Barcode list (KN04 Pool).xlsx") %>% 
  select(rfid, runid, fastq_files, library_name, pcr_barcode, project_name, barcode, filename, comments, flag, sex, coatcolor, organism, strain)
write.csv(kn04_sample_metadata, "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/kn04_fastq_sample_metadata_n952.csv", row.names = F)
# for db
kn04_sample_metadata %>% 
  select(rfid, project_name, barcode, library_name, pcr_barcode, filename, comments, flag) %>% 
  write.csv("~/Desktop/Database/csv files/sample_tracking/kn04_fastq_sample_barcode_lib_n952.csv", row.names = F) #slight rename 


## pull out the sires dames info from db 
kn04_hs_siredam <- read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/kn04_db_join.csv") %>% 
  mutate_all(as.character) %>% 
  distinct(rfid, sires, dames) %>% 
  right_join(kn04_sample_metadata, by = "rfid") %>% 
  left_join(pedigree_01062021_fix_temp %>% select(id_f51, "parent_sex" = sex) %>% subset(sex = "M"), by = c( "sires" = "id_f51")) %>% 
  left_join(pedigree_01062021_fix_temp %>% select(id_f51, "parent_sex" = sex) %>% subset(sex = "F"), by = c( "dames" = "id_f51")) %>% 
  subset(project_name != "r01_su_guo") %>% 
  rowwise() %>% 
  mutate(parent_sex.x = replace(parent_sex.x, parent_sex.x == "F"&project_name != "r01_su_guo", sires)) %>%  
  mutate(sires = replace(sires, parent_sex.y == "M"&project_name != "r01_su_guo", dames),
         dames = replace(dames, parent_sex.y == "M"&project_name != "r01_su_guo", parent_sex.x)) %>% 
  ungroup() %>%
  select(-matches("[.][xy]$"))

write.csv(kn04_hs_siredam, "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/kn04_hs_parents_n376.csv", row.names = F)


kn04_hs_siredam %>% subset(sires == dames)


### kn03

# add breeder rfid_genotype column 
kn03_idconv <- read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/sample_barcode_lib_idconversion_kn03_n960.csv") %>% 
  mutate_all(as.character) %>% 
  mutate_all(str_trim) 
# %>% 
  # left_join(Zebrafish_breeders_df %>% select(-sex), by = "rfid")
  
kn03_idconv %>% subset(library_name == "Fish Breeders") %>% mutate(rfid_id = gsub(".*_", "", rfid_genotype)) %>% 
  left_join(Zebrafish_breeders_df %>% select(-sex), by = "rfid_id")

# qc'ing 
anti_join(kn03_idconv %>% subset(library_name == "Fish Breeders") %>% select(rfid), Zebrafish_breeders_df %>% select(rfid), by = "rfid")
anti_join(Zebrafish_breeders_df %>% select(rfid), kn03_idconv %>% subset(library_name == "Fish Breeders") %>% select(rfid), by = "rfid")


kn03_idconv %>% subset(library_name == "Fish Breeders") %>% 
  mutate(rfid_id = gsub(".*_", "", rfid_genotype), 
         rfid_idstripped = gsub("(.*_\\D\\d+)\\D$", "\\1", rfid_genotype) %>% gsub(".*_", "", .)) %>% 
  left_join(Zebrafish_breeders_df %>% select(-sex), by = "rfid_idstripped") %>% 
  select(matches("rfid")) %>% 
  rename("rfid_manifest" = "rfid.x", 
         "rfid_genotype_manifest" = "rfid_genotype.x", 
         "rfid_id_manifest" = "rfid_id.x", 
         "rfid_genotype_guo" = "rfid_genotype.y",
         "rfid_id_guo" = "rfid_id.y") %>%
  select(-rfid.y) %>% 
  mutate(id_letter = ifelse(grepl("\\D$", rfid_id_manifest)&rfid_id_manifest==rfid_id_guo, "id and letter match", 
                            ifelse(grepl("\\D$", rfid_id_manifest)&rfid_id_manifest!=rfid_id_guo, "id and letter DO NOT MATCH", NA))) %>% 
  mutate(rfid_z_manifest = gsub("_.*", "", rfid_genotype_manifest),
         rfid_z_guo = gsub("-.*", "", rfid_genotype_guo)) %>% 
  mutate(z = ifelse(rfid_z_manifest == rfid_z_guo, "matches", "does not match")) %>% 
  write.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/Zebrafish/CREATE/fish_breeder_96_idqc.xlsx")

kn03_idconv %>% subset(library_name == "Fish Breeders") %>% left_join(plates_df_1, by = c("rfid" = "fish_id")) %>% subset(is.na(dna_conc)) %>% select(rfid)


### kn02

### kn01

# oksana 08/10/2020
genotyping_df2 %>% subset(project_name == "u01_huda_akil") %>%
  mutate(library_name = library, 
         library_name = replace(library_name, grepl("UMich", library_name), 
                                gsub("UMich(\\d+)", "UMich0\\1", library_name)),
         library_name = replace(library_name, library_name == "UMich08_Fish",
                                "UMich08"),
         library_name = replace(library_name, library_name == "UMich02redo",
                                "UMich02")) %>%  # to match the sequencing run log
  left_join(sequencing_run_log, by = c("project_name", "library_name")) %>% 
  left_join(sample_metadata, by = c("rfid", "project_name")) %>% 
  write.csv("sdakil_genotyped_n629.csv", row.names = F)

sample_metadata %>% subset(project_name == "u01_huda_akil") %>% 
  left_join(sample_barcode_lib) %>% 
  left_join(genotyping_df2) %>% 
  subset(is.na(barcode)) %>% write.csv("sdakil_notgenotyped_n220.csv", row.names = F)
