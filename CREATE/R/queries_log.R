## data queries

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
