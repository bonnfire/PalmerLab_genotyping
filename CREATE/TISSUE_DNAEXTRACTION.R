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
  googlesheets4::gs4_find()$id[grep("extraction", googlesheets4::gs4_find()$name, ignore.case = T)]
  ) #extract the id code associated with the U01 Spleen&Fish Extraction Database 

khai_tissueextraction <- lapply(khai_tissueextractionNames, function(x){             
  googlesheets4::range_read(googlesheets4::gs4_find()$id[1],sheet = x) 
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
    grepl("^\\d+", sample_id_barcode) & nchar(sample_id_barcode) == 9,
    paste0("933000", sample_id_barcode),
    ifelse(
      grepl("^\\d+", sample_id_barcode) & nchar(sample_id_barcode) == 10,
      paste0("93300", sample_id_barcode),
      ifelse(!grepl("^\\d+", sample_id_barcode) & grepl("Plate|p\\.cal", sample_id_barcode),
             sample_id_barcode, sample_id_barcode)
    ) 
  )) %>% # create the rfid column from the sample_id_barcode to make them uniform and comparable to transponder id (rfid) in wfu
  mutate(rfid = replace(rfid, sample_id_barcode == "F507", "933000320047344")) %>% 
  rowwise() %>% 
  mutate(rfid = replace(rfid, grepl("mismatch", comments, ignore.case = T), transponder),
         rfid = replace(rfid, well == "C5"&dna_plate_code=="Kalina02/Jhou03", "933000320046294") #fix the dupe rfid
  ) %>% ### 
  ungroup() 

# check for dupes before joining to project names
khai_tissueextraction_df %>% get_dupes(rfid)


# separate the joining to prevent needing to compile all shipment data from other u01s and p50 to generate project specific data
khai_tissueextraction_df_join <- khai_tissueextraction_df %>% 
  left_join(., olivier_spleens_df[, c("experiment", "rfid")], by = "rfid") %>% # to account for the naive animals
  left_join(., shipments_df[, c("rfid", "u01")], by = c("rfid")) %>% 
  ## XX left_join(., p50[, c("rfid", "p50)]) %>% 
  mutate(comments = replace(comments, grepl("Coc", experiment)&grepl("Oxy", u01)|grepl("Oxy", experiment)&grepl("Co", u01), "naive replacement"), 
         u01 = replace(u01, grepl("Coc", experiment)&!grepl("Co", u01), "Olivier_Oxy"),
         u01 = replace(u01, grepl("Ox", experiment)&!grepl("Ox", u01), "Olivier_Co")) %>% 
  rowwise() %>% 
  mutate(rfid = replace(rfid, grepl("933000", rfid)&is.na(u01), transponder)) %>% 
  select(-c("u01", "experiment")) %>% 
  left_join(., shipments_df[, c("rfid","u01")], by = c("rfid")) %>% 
  left_join(., shipments_p50_df[, c("rfid","p50")], by = c("rfid")) %>% 
  ungroup() %>% 
  mutate(dna_plate_code = replace(dna_plate_code, dna_plate_code == "1580526300", "Plate 1(FC20200305)")) %>% 
  mutate(
    project_name = case_when(
      grepl("p\\.cal", rfid) ~ "pcal_brian_trainor",
      grepl("Plate", rfid) ~ "r01_su_guo",
      grepl("Plate", dna_plate_code) ~ "r01_su_guo",
      grepl("Olivier_Oxy", u01) ~ "u01_olivier_george_oxycodone",
      grepl("Olivier_Co", u01) ~ "u01_olivier_george_cocaine",
      grepl("Mitchell", u01) ~ "u01_suzanne_mitchell",
      grepl("Jhou", u01) ~ "u01_tom_jhou",
      grepl("Kalivas$", u01) ~ "u01_peter_kalivas_us",
      grepl("Kalivas_Italy", u01) ~ "u01_peter_kalivas_italy",
      grepl("Chen", p50) ~ "p50_hao_chen",
      grepl("Richards", p50) ~ "p50_jerry_richards",
      grepl("Meyer", p50) ~ "p50_paul_meyer",
      TRUE ~ "NA"
    )
  )

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
  



# khai_spleenextraction_df_fordb <- khai_spleenextraction_df %>%
#   mutate(
#     project_name = case_when(
#       grepl("p\\.cal", rfid) ~ "pcal_brian_trainor",
#       grepl("Plate", rfid) ~ "r01_su_guo",
#       grepl("Plate", dna_plate_code) ~ "r01_su_guo",
#       grepl("Olivier_Oxy", u01) ~ "u01_olivier_george_oxycodone",
#       grepl("Olivier_Co", u01) ~ "u01_olivier_george_cocaine",
#       grepl("Mitchell", u01) ~ "u01_suzanne_mitchell",
#       grepl("Jhou", u01) ~ "u01_tom_jhou",
#       grepl("Kalivas", u01) ~ "u01_peter_kalivas_us",
#       grepl("Chen", p50) ~ "p50_hao_chen", 
#       grepl("Richards", p50) ~ "p50_jerry_richards", 
#       grepl("Meyer", p50) ~ "p50_paul_meyer", 
#       TRUE ~ "NA"
#     )
#   ) %>%   # for the animals for which we don't have shipment info for
#   naniar::replace_with_na_all(condition = ~ .x %in% c("NA", "N/A", "None")) %>% 
#   select(-one_of("u01", "p50", "v1", "v2"))
# 
# 
# con <- dbConnect(dbDriver("PostgreSQL"), dbname="PalmerLab_Datasets",user="postgres",password="postgres")
# dbWriteTable(con, c("sample_tracking","extraction_log"), value = khai_spleenextraction_df_fordb, row.names = FALSE)
# dbExecute(con,"ALTER TABLE sample_tracking.extraction_log ADD PRIMARY KEY(rfid,project_name)")
# dbExecute(con,"ALTER TABLE sample_tracking.extraction_log ADD UNIQUE(dna_plate_code,rfid,well)")
# # run when combined_master_table is complete
# # dbExecute(con,"ALTER TABLE sample_tracking.extraction_log ADD FOREIGN KEY(rfid) REFERENCES sample_tracking.combined_master_table(rfid)")



## in terminal
cd /tmp
sudo su postgres
pg_dump -d PalmerLab_Datasets -t sample_tracking.extraction_log > extraction_log.sql
exit
cp extraction_log.sql /home/bonnie/Dropbox\ \(Palmer\ Lab\)/PalmerLab_Datasets/sample_tracking





################################ 
## LIBRARY RIPTIDE NAME LIST 
################################

library_riptide <- flipAPI::DownloadXLSX("https://www.dropbox.com/s/hq4g4fw4irubhes/Library%20Riptide%20Name%20List.xlsx?dl=0") %>% 
  clean_names() %>% 
  mutate_all(as.character) %>% 
  mutate_at(vars(matches("date")), as.POSIXct) 
  
## full run id and project_name primary key 












########################### 
# HANNAH EXTRACTION TABLE #
###########################

## NONE OF THESE ARE OVERLAPPING WITH THE RIPTIDE ONES 
extractions_hannah_df <- lapply(extractions_hannah_original, function(x){
  x <- x %>% mutate_at(vars(contains('Date')), ~lubridate::ymd(.))
  return(x)
}) %>% rbindlist(fill = T, use.names = T) 
names(extractions_hannah_df) <- mgsub::mgsub(tolower(names(extractions_hannah_df)), 
                                           c("[#]", "[[:space:]]|[.]|[[:punct:]]$", "[[:punct:]]"), 
                                           c("num", "", "_"))

extractions_hannah_df <- extractions_hannah_df %>% 
  mutate(sampleid_barcode = coalesce(sampleid, sampleid_barcode)) %>% 
  select(-sampleid)

extractions_hannah_df %>% 
  subset(sampleid_barcode %in% flow_cell_original_rip$`Sample ID`) %>% 
  dim()

## library in flow cell "Riptide-"01 to 03 seem to be line with the "Olivier"01-03 counts




#### KHAI DNA EXTRACTION USING THE FLIPAPI

names(khai_spleenextraction_df) <- mgsub::mgsub(tolower(names(khai_spleenextraction_df)), 
                                           c("[#]", "[[:space:]]|[.]|[[:punct:]]$", "[[:punct:]]"), 
                                           c("num", "", "_"))


khai_spleenextraction_df <- khai_spleenextraction_df %>%
  mutate_all(as.character) %>% 
  mutate(rfid = paste0("933000", sampleid_barcode)) %>% 
  mutate(u01_rfid_verified = case_when(
    rfid %in%  WFU_OlivierCocaine_test_df$rfid ~ "u01_olivier_cocaine",
    # rfid == "933000120117313" ~ "u01_olivier_cocaine",
    rfid %in%  WFU_OlivierOxycodone_test_df$rfid ~ "u01_olivier_oxycodone",
    rfid %in%  WFU_Jhou_test_df$rfid ~ "u01_jhou",
    rfid %in%  WFU_Mitchell_test_df$rfid ~ "u01_mitchell",
    TRUE ~ "NA")) %>% 
  left_join(., shipments_df[,c("rfid", "cohort", "u01")], by = c("rfid")) %>% 
  mutate(u01 = paste0(u01, "_", cohort)) %>% 
  select(-cohort)

# origin is not cohort
# WFU_OlivierCocaine_test_df %>% subset(rfid %in% c("933000320047386", "933000320046848"))






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


