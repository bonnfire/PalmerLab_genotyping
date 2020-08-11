## data queries

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
