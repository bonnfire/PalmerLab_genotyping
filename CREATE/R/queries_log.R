## data queries

#fish breeders 

### kn04

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
