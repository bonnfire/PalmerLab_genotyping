### PROCESSING PALMER LAB DATASETS 
# EXTRACTION AND FLOW CELL TABLES FROM HANNAH AND KHAI

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/20190829_WFU_U01_ShippingMaster/Tissues/Original")



# An important subset of this table comes from the shipments that we receive from the labs
# The shipment sheets for the tissue samples are in folder 20190829_wfu_u01_shippingmaster/TissueShipments and they are saved in the WFU_MasterTables github

######################## 
## KHAI EXTRACTION TABLE 
######################## 

# https://docs.google.com/spreadsheets/d/1ye6Vx7vuf7T2grUlGtH0lbKSCU-aLhIy_CVZHfN48yw/edit#gid=1476254388
# modifying code to extract from multiple google sheets
#install.packages("googlesheets4")
khai_tissueextractionNames <- googlesheets4::sheet_names(
  googlesheets4::gs4_find()$id[grep("Spleen&Fish", googlesheets4::gs4_find()$name, ignore.case = T)]
  ) #extract the id code associated with the U01 Spleen&Fish Extraction Database 

khai_tissueextraction <- lapply(khai_tissueextractionNames, function(x){             
  googlesheets4::range_read(googlesheets4::gs4_find()$id[grep("Spleen&Fish", googlesheets4::gs4_find()$name, ignore.case = T)],sheet = x) 
}) # extract all sheets in this workbook


  # clean for df 
khai_tissueextraction_df <- khai_tissueextraction %>% 
  lapply(., function(x){
    x %>%
      mutate_all(as.character)
  }) %>% 
  rbindlist(fill = T) %>% 
  clean_names %>% 
  naniar::replace_with_na_all(condition = ~.x %in% c("NA", "N/A", "None")) %>% 
  dplyr::filter(!is.na(dna_plate_code) & !is.na(transponder) | !is.na(sample_id_barcode)) %>% # if you exclude, you are removing pcal and zebrafish
  mutate(rfid = ifelse(
    grepl("^\\d+", sample_id_barcode) & nchar(sample_id_barcode) == 9 & !grepl("riptidecontrol", dna_plate_code, ignore.case = T),
    paste0("933000", sample_id_barcode),
    ifelse(
      grepl("^\\d+", sample_id_barcode) & nchar(sample_id_barcode) == 10 & !grepl("riptidecontrol", dna_plate_code, ignore.case = T),
      paste0("93300", sample_id_barcode),
      ifelse(!grepl("^\\d+", sample_id_barcode) & grepl("Plate|p\\.cal", sample_id_barcode) & !grepl("riptidecontrol", dna_plate_code, ignore.case = T),
             sample_id_barcode, sample_id_barcode)
    ) 
  )) %>% # create the rfid column from the sample_id_barcode to make them uniform and comparable to transponder id (rfid) in wfu
  mutate(rfid = replace(rfid, sample_id_barcode == "F507", "933000320047344")) %>% 
  rowwise() %>% 
  mutate(rfid = replace(rfid, grepl("mismatch", comments, ignore.case = T), transponder),
         rfid = replace(rfid, well == "C5"&dna_plate_code=="Kalina02/Jhou03", "933000320046294"), #fix the dupe rfid
         rfid = replace(rfid, grepl("\\d+?_Plate\\d+?_", rfid, ignore.case = T), gsub("_", "", rfid)),
         rfid = gsub(" ", "", rfid),
         rfid = gsub("20200617Plate", "20200616Plate", rfid)
  ) %>% ###
  mutate(rfid = replace(rfid, grepl("Plate", rfid), paste0(gsub("_", "-", rfid))),
         rfid = replace(rfid, grepl("Plate", rfid), paste0(gsub("-(\\D)(\\d)$", "-\\2\\1", rfid)))) %>% 
         # project_name = replace(project_name, grepl("Plate", rfid), "r01_su_guo")) %>% 
  # mutate(rfid = replace(rfid, grepl("\\d+?_Plate\\d+?_", rfid), gsub("_", "", rfid)),
  mutate(riptide_plate_number = gsub("[- _]", "", riptide_plate_number)) %>%
  ungroup() %>% 
  distinct() ## XX look into why p50_jerry_richards are multiplied 4x


# join to the founders (?) -- not found in khai's database 
# 07/31/2020
setwd("/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE")
ucsf_zebrafish <- read_xlsx("nanodrop zebrafish results 12-5-19.xlsx") %>% 
  clean_names() %>% 
  rename("rfid" = "sample_name",
         "nanodrop_ng_u_l" = "new_conc_ng_u_l",
         "x260_280" = "x260_280_19" ,
         "x260_230" = "x260_230_20") %>% # using the second time nanodrop
  mutate(user_id = "HVB",  
         date = "2019-12-05",
         comments = "Imported from nanodrop zebrafish results 12-5-19 file") %>% 
  subset(!is.na(rfid))


# check for dupes before joining to zebrafish and project names
khai_tissueextraction_df %>% get_dupes(rfid)

## join to sample_metadata instead 
khai_tissueextraction_df %>% 
  left_join(sample_metadata[, c("rfid", "project_name")], by = "rfid") %>% 
  select(transponder, sample_id_barcode, rfid, project_name) %>% naniar::vis_miss()

khai_tissueextraction_df_join <- khai_tissueextraction_df %>% bind_rows(ucsf_zebrafish %>% 
                                                                          select(rfid, well, nanodrop_ng_u_l, x260_280, x260_230, user_id, date, comments) %>% 
                                                                          mutate_all(as.character)) %>% 
  # left_join(sample_metadata[, c("rfid", "project_name")], by = "rfid") %>% 
  left_join(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/sample_metadata_rfidproject_10192020.csv") %>% mutate_all(as.character), by = "rfid") %>% 
  left_join(., tissue_df[, c("rfid", "project_name")], by = "rfid") %>% # to account for the naive animals
  mutate(project_name = coalesce(project_name.y, project_name.x)) %>% 
  select(-matches("project_name[.][xy]")) %>% 
  # subset(!(grepl("^000")&nchar(rfid) == 10)) ## XX temporarily remove these bc no metadata
  # subset(dna_plate_code != "RiptideControl") %>%
  # subset(!is.na(project_name)) %>% ## XX temporary until zebrafish get sorted out 
  distinct()
  #   
#   select(transponder, sample_id_barcode, rfid, project_name) %>% 
#   select(rfid) %>% View()


## XX re-investigate this after conference 10/19/2020
khai_tissueextraction_df_join %>% mutate(project_name = replace(project_name, grepl("Plate", rfid)|rfid %in% ucsf_zebrafish$rfid|grepl("^Z26", rfid), "r01_su_guo")) %>% subset(is.na(project_name)) %>% View()


## CREATE LIBRARY, PROJECT_NAME FOR SEQUENCING RUN
library_project_name <- khai_tissueextraction_df_join %>% 
  distinct(riptide_plate_number, project_name) %>% 
  group_by(riptide_plate_number) %>%
  summarise(project_name = paste(project_name, collapse = ',')) %>% 
  mutate_all(~gsub(" ", "", .)) %>% 
  mutate(riptide_plate_number = gsub("-","",riptide_plate_number)) # to join to flowcell format

### CREATE EXTRACTION LOG TABLE
extraction_log <- khai_tissueextraction_df_join %>%
  select_if(~sum(!is.na(.)) > 0) %>% 
  select(-matches("x(1[89])|x20|u01|p50"))


## create csv/sql file/upload + copy to pgadmin  
setwd("~/Desktop/Database/csv files")
write.csv(extraction_log, file = "extraction_log.csv", row.names = F)

# temporary
extraction_log_n6729 <- khai_tissueextraction_df_join %>%
  select_if(~sum(!is.na(.)) > 0) %>% 
  select(-matches("x(1[89])|x2[0-2]$|u01|p50|^e$")) %>% 
  mutate(project_name = replace(project_name, project_name == "r01_su_guo", "r01_su_guo_larvae")) %>% 
  rowwise() %>% 
  mutate(rfid = replace(rfid, grepl("Plate", rfid), gsub("-","_",rfid))) %>% 
  mutate(rfid = replace(rfid, is.na(project_name)&grepl("Plate", rfid), sample_id_barcode)) %>% 
  mutate(rfid = gsub(" ", "", rfid)) %>% 
  mutate(rfid = replace(rfid, grepl("Plate", rfid), gsub("_(\\D)(\\d+)$", "_\\2\\1", rfid)),
         project_name = replace(project_name, is.na(project_name)&grepl("Plate", rfid), "r01_su_guo_larvae")) %>% 
  ungroup() %>% 
  subset(rfid != "NULL") %>% 
  subset(!is.na(project_name)&comments != "Imported from nanodrop zebrafish results 12-5-19 file") %>%  ## XX return to these cases 03/01/2021 
  mutate(comments = ifelse(grepl("20200617", rfid), "processed and sequenced as 20200617, but phenotyping center uses 20200616", comments),
    rfid = gsub("20200617", "20200616", rfid))
write.csv(extraction_log_n6729, file = "~/Desktop/Database/csv files/sample_tracking/temp_extraction_log.csv", row.names = F)



################################ 
## LIBRARY RIPTIDE NAME LIST 
################################

library_riptide <- flipAPI::DownloadXLSX("https://www.dropbox.com/s/hq4g4fw4irubhes/Library%20Riptide%20Name%20List.xlsx?dl=0") %>% 
  clean_names() %>% 
  mutate_all(as.character) %>% 
  mutate_at(vars(matches("date")), as.POSIXct) 
  
## full run id and project_name primary key 







## sandbox

# extractions_khai_original <- u01.importxlsx("U01 spleen extraction database.xlsx") # 23 tables
# lapply(extractions_khai_original, function(x){x %>% mutate_all(as.character)}) %>% rbindlist(fill = T) %>% dim
# this is from the original code, where you are saving the excel sheets and reading in these different versions of sheets, and the lapply function is used to check if the flipapi code is working

extractions_hannah_original <- u01.importxlsx("High_Throughput_DNA_&_spleen_info.xlsx") # 33 tables 
flow_cell_original <- u01.importxlsx("2020-01-16-Flowcell Sample-Barcode list-Riptide-UMich2-Riptide03_NovaSeq01.xlsx") # 1 table

## are there cocaine spleens submitted in the flow cell submitted to the sequencing core? 


extractions_khai_df$u01_rfid_verified %>% table()
extractions_khai_df$u01 %>% table() ## fix the origin cells? also cocaine_oxy 2 cases?
extractions_khai_df %>% mutate(u01_group = gsub("_\\d+", "", u01), u01_cohort = parse_number(u01)) %>% select(u01_group, u01_cohort) %>% table() ## fix the origin cells? also cocaine_oxy 2 cases?


# extractions_khai_df %>% mutate_at(vars(one_of("u01", "comments")), as.factor) %>% 
#   summary()
# extractions_khai_df %>% dplyr::filter(u01 != "Template") %>% dplyr::filter(u01!=dnaplatecode) # note that olivier c06 shares with mitchell c01

## INVESTIGATE THIS
# extractions_flowcell %>% dim
# extractions_flowcell %>% subset(transponder %in% WFU_OlivierCocaine_test_df$rfid) %>% dim
# extractions_flowcell %>% subset(transponder %in% WFU_OlivierOxycodone_test_df$rfid) %>% dim




## 07/29/2020
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/Genotype")
ims <- read.csv("ims_hs.csv")
ims <- ims %>% 
  separate(INDV, into = c("INDV", "rfid"), sep = "-")

ims %>% 
  subset(F_MISS > 0.1) %>% 
  subset(!grepl("^000", rfid)) %>% 
  mutate(rfid = replace(rfid, rfid == "933000520138331", "933000120138331")) %>% 
  left_join(extraction_log[, c("rfid", "location_of_extracted_dna_plate_box", "storage_box_of_extracted_dna_plate", "freezer_location_of_tissue", "position_in_box")], by = "rfid") %>% 
  subset(!is.na(location_of_extracted_dna_plate_box)) %>%  
  head(96) %>% 
  openxlsx::write.xlsx(file = "redo_ims_location_n96.xlsx")
# %>% 
  # subset(is.na(location_of_extracted_dna_plate_box)) %>% 
  # left_join(sample_barcode_lib[ , c("library", "rfid")])

# load("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/dna_extractions_hannah_rebecca.RData")
# load("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/dna_extractions_celine.RData")


## 01/05/2021 dnaextractions to troubleshoot missing extractions from projects olivier

khai_tissueextractionNames <- googlesheets4::sheet_names(
  googlesheets4::gs4_find()$id[grep("Spleen&Fish", googlesheets4::gs4_find()$name, ignore.case = T)]
) #extract the id code associated with the U01 Spleen&Fish Extraction Database 

khai_tissueextraction_1_52 <- lapply(khai_tissueextractionNames[1:52], function(x){             
  googlesheets4::range_read(googlesheets4::gs4_find()$id[grep("Spleen&Fish", googlesheets4::gs4_find()$name, ignore.case = T)],sheet = x) 
}) # extract all sheets in this workbook


# clean for df 
khai_tissueextraction_1_52_df <- khai_tissueextraction_1_52 %>% 
  lapply(., function(x){
    x %>%
      mutate_all(as.character)
  }) %>% 
  rbindlist(fill = T) %>% 
  clean_names %>% 
  naniar::replace_with_na_all(condition = ~.x %in% c("NA", "N/A", "None")) %>% 
  dplyr::filter(!is.na(dna_plate_code) & !is.na(transponder) | !is.na(sample_id_barcode)) %>% # if you exclude, you are removing pcal and zebrafish
  mutate(rfid = ifelse(
    grepl("^\\d+", sample_id_barcode) & nchar(sample_id_barcode) == 9 & !grepl("riptidecontrol", dna_plate_code, ignore.case = T),
    paste0("933000", sample_id_barcode),
    ifelse(
      grepl("^\\d+", sample_id_barcode) & nchar(sample_id_barcode) == 10 & !grepl("riptidecontrol", dna_plate_code, ignore.case = T),
      paste0("93300", sample_id_barcode),
      ifelse(!grepl("^\\d+", sample_id_barcode) & grepl("Plate|p\\.cal", sample_id_barcode) & !grepl("riptidecontrol", dna_plate_code, ignore.case = T),
             sample_id_barcode, sample_id_barcode)
    ) 
  )) %>% # create the rfid column from the sample_id_barcode to make them uniform and comparable to transponder id (rfid) in wfu
  mutate(rfid = replace(rfid, sample_id_barcode == "F507", "933000320047344")) %>% 
  rowwise() %>% 
  mutate(rfid = replace(rfid, grepl("mismatch", comments, ignore.case = T), transponder),
         rfid = replace(rfid, well == "C5"&dna_plate_code=="Kalina02/Jhou03", "933000320046294"), #fix the dupe rfid
         rfid = gsub(" ", "", rfid),
         rfid = gsub("20200617Plate", "20200616Plate", rfid)
  ) %>% ###
  mutate(rfid = replace(rfid, grepl("Plate", rfid), paste0(gsub("(\\D)(\\d+)$", "\\2\\1", rfid)))) %>% 
  mutate(riptide_plate_number = gsub("[- _]", "", riptide_plate_number)) %>%
  ungroup() %>% 
  distinct()

khai_tissueextraction_1_52_df <- khai_tissueextraction_1_52_df %>% 
  select_if(~sum(!is.na(.)) > 0) %>% 
  select(-matches("x(1[89])|x2[0-2]$|^e$")) %>% 
  subset(rfid != "NULL") %>% 
  mutate(comments = ifelse(grepl("20200617", rfid), "processed and sequenced as 20200617, but phenotyping center uses 20200616", comments),
         rfid = gsub("20200617", "20200616", rfid)) %>% 
  mutate(rfid = gsub("-", "_", rfid))

khai_tissueextraction_1_52_df <- khai_tissueextraction_1_52_df %>% 
  select(dna_plate_code, plate_barcode, well, transponder, sample_id_barcode, user_id, date, nanodrop_ng_u_l, x260_280, x260_230, origin, no, storage_box_of_spleen, freezer_location_of_tissue, position_in_box, comments, riptide_plate_number, storage_box_of_extracted_dna_plate, location_of_extracted_dna_plate_box, number_written_on_the_top_of_tube, rfid) %>% 
  distinct() %>% 
  mutate(project_name = "NA") %>% # fill in with sql db
  rowwise() %>% 
  mutate(rfid = replace(rfid, grepl("Akil", dna_plate_code, ignore.case = T), transponder)) %>% ## using phenotyping center initiative
  ungroup() %>% 
  subset(dna_plate_code != "RiptideControl")

## extraction 53 to 101 (previously 96)

khai_tissueextraction_53_101 <- lapply(khai_tissueextractionNames[53:101], function(x){             
  googlesheets4::range_read(googlesheets4::gs4_find()$id[grep("Spleen&Fish", googlesheets4::gs4_find()$name, ignore.case = T)],sheet = x) 
}) # extract all sheets in this workbook

khai_tissueextraction_53_101_df <- khai_tissueextraction_53_101 %>% 
  lapply(., function(x){
  x %>%
    mutate_all(as.character)
}) %>% 
  rbindlist(fill = T) %>% 
  clean_names %>% 
  # subset(is.na(akil_lab_g_dna_from_sprague_dawley_outbred_rats_number_37_422)) %>% 
  naniar::replace_with_na_all(condition = ~.x %in% c("NA", "N/A", "None")) %>% 
  dplyr::filter(!is.na(dna_plate_code) & !is.na(transponder) | !is.na(sample_id_barcode)) %>% # if you exclude, you are removing pcal and zebrafish
  mutate(rfid = ifelse(
    grepl("^\\d+", sample_id_barcode) & nchar(sample_id_barcode) == 9 & !grepl("riptidecontrol", dna_plate_code, ignore.case = T),
    paste0("933000", sample_id_barcode),
    ifelse(
      grepl("^\\d+", sample_id_barcode) & nchar(sample_id_barcode) == 10 & !grepl("riptidecontrol", dna_plate_code, ignore.case = T),
      paste0("93300", sample_id_barcode),
      ifelse(!grepl("^\\d+", sample_id_barcode) & grepl("Plate|p\\.cal", sample_id_barcode) & !grepl("riptidecontrol", dna_plate_code, ignore.case = T),
             sample_id_barcode, sample_id_barcode)
    ) 
  )) %>% # create the rfid column from the sample_id_barcode to make them uniform and comparable to transponder id (rfid) in wfu
  rowwise() %>% 
  mutate(rfid = replace(rfid, grepl("mismatch", comments, ignore.case = T), transponder),
         rfid = replace(rfid, well == "C5"&dna_plate_code=="Kalina02/Jhou03", "933000320046294"), #fix the dupe rfid
         rfid = gsub(" ", "", rfid)
  ) %>% ###
  mutate(rfid = replace(rfid, grepl("Plate", rfid), paste0(gsub("-(\\D)(\\d+)$", "-\\2\\1", rfid))),
         rfid = replace(rfid, grepl("Plate", rfid), paste0(gsub("-", "_", rfid)))) %>% 
  mutate(riptide_plate_number = gsub("[- _]", "", riptide_plate_number)) %>%
  ungroup() %>% 
  mutate(location_of_extracted_dna_plate_box = "NA",
         number_written_on_the_top_of_tube = "NA") %>% # temporary placeholder XX 03/02/2021
  select(dna_plate_code, plate_barcode, well, transponder, sample_id_barcode, user_id, date, nanodrop_ng_u_l, x260_280, x260_230, origin, no, storage_box_of_spleen, freezer_location_of_tissue, position_in_box, comments, riptide_plate_number, storage_box_of_extracted_dna_plate, location_of_extracted_dna_plate_box, number_written_on_the_top_of_tube, rfid) %>% 
  distinct() %>% 
  mutate(project_name = "NA") %>% # use project_name from db
  rowwise() %>% 
  mutate(rfid = replace(rfid, grepl("Akil", dna_plate_code, ignore.case = T), transponder)) %>% ## using phenotyping center initiative
  subset(!(grepl("Plate", rfid)&is.na(plate_barcode))) %>% 
  ungroup() %>%  # fill in with sql db
  mutate(rfid = replace(rfid, well == "F10" & dna_plate_code == "Jhou05", "933000320187063")) %>% 
  subset(rfid != "NULL")
  

## add to db the rfid's that were not already present previously
khai_tissueextraction_53_101_df_db <- khai_tissueextraction_53_101_df %>% 
  anti_join(read.csv("~/Desktop/Database/csv files/sample_tracking/extraction_log_53_96.csv", colClasses = "character") %>% 
              select(rfid, riptide_plate_number), by = c("rfid", "riptide_plate_number")) %>% 
  subset(!is.na(riptide_plate_number)) %>% 
  mutate(db_comments = "NA") %>% 
  mutate(rfid = ifelse(grepl("Hao", dna_plate_code)&grepl("TN", origin), 
                gsub("^DD", "", rfid), rfid),
         db_comments = ifelse(grepl("Hao", dna_plate_code)&grepl("TN", origin), 
                          "remove appending DD to match sample tracking metadata", db_comments)) %>% 
  mutate(rfid = ifelse(grepl("Redo01", dna_plate_code)&grepl("NY", origin), 
                       gsub("^CC", "", rfid), rfid),
         db_comments = ifelse(grepl("Redo01", dna_plate_code)&grepl("NY", origin), 
                              "remove appending CC to match sample tracking metadata", db_comments)) %>% 
  mutate(rfid = gsub("-", "_", rfid)) %>% 
  rowwise() %>% 
  mutate(rfid = replace(rfid, dna_plate_code %in% c("Jhou05", "Jhou06") &grepl("^U", rfid, ignore.case = T)&grepl("^933000", transponder), transponder)) %>% 
  ungroup() %>% 
  mutate(rfid = replace(rfid, dna_plate_code == "Jhou05"&well == "A2"&rfid == "U159", "933000320046409"),
         rfid = replace(rfid, dna_plate_code == "Jhou05"&well == "C7"&rfid == "U172", "933000320045977"),
         rfid = replace(rfid, dna_plate_code == "Jhou05"&well == "D4"&rfid == "u145", "933000320045642"),
         rfid = replace(rfid, dna_plate_code == "Jhou05"&well == "E6"&rfid == "u191", "933000320046750"),
         rfid = replace(rfid, dna_plate_code == "Jhou06"&well == "D2", "933000320186902"))


khai_tissueextraction_53_101_df_db_temp <- khai_tissueextraction_53_101_df_db %>% 
  subset(!(grepl("Hao", dna_plate_code)&grepl("TN", origin)))


khai_tissueextraction_53_101_df_db_temp %>% 
  subset(rfid == "CC1DCD1790")
khai_tissueextraction_53_101_df_db_temp %>% 
  subset(rfid == "U195")


khai_tissueextraction_53_101_df_db %>% 
  subset(rfid == "1DCD2048") %>% View()


# animals that are in queue to be extracted 
khai_tissueextraction_53_101_df %>% 
  subset(is.na(riptide_plate_number)) %>% select(dna_plate_code) %>% rowwise() %>% mutate(project = "NA", 
                                                                                          project = case_when(grepl("Plate", dna_plate_code) ~ "larvae",
                                                                                            grepl("Jhou", dna_plate_code) ~ "jhou", 
                                                                                            grepl("Kalivas", dna_plate_code) ~ "kalivas_us", 
                                                                                            grepl("Redo", dna_plate_code) ~ "redos", 
                                                                                            TRUE ~ NA_character_)) %>% ungroup()%>% select(project) %>% table()



# clean up akil sheet
khai_tissueextraction_53_96 %>% 
  lapply(., function(x){
    x %>%
      mutate_all(as.character)
  }) %>% 
  rbindlist(fill = T) %>% 
  clean_names %>% 
  subset(!is.na(akil_lab_g_dna_from_sprague_dawley_outbred_rats_number_37_422))


## remaining
khai_tissueextractionNames <- googlesheets4::sheet_names(
  googlesheets4::gs4_find()$id[grep("Spleen&Fish", googlesheets4::gs4_find()$name, ignore.case = T)]
) #extract the id code associated with the U01 Spleen&Fish Extraction Database 


