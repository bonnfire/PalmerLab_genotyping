### PROCESSING PALMER LAB DATASETS 
# EXTRACTION AND FLOW CELL TABLES FROM HANNAH AND KHAI

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/20190829_WFU_U01_ShippingMaster/Tissues/Original")



### 6/30/2020 
## XX do we have a shipment received table? 
## we do, it extracts from 20190829_wfu_u01_shippingmaster/TissueShipments and the script is saved in WFU github
### 

######################## 
## KHAI EXTRACTION TABLE 
######################## 

### extract khai's data 
# devtools::install_github("Displayr/flipAPI")
library(flipAPI)

i <- 1
khai_spleenextraction <- list()
while (TRUE){
  khai_spleenextraction[[i]] <- try(DownloadXLSX("https://www.dropbox.com/s/ps8hxgleh1lvo55/U01%20spleen%20extraction%20database.xlsx?dl=0", sheet = i))
  if (inherits(khai_spleenextraction[[i]], "try-error"))
    break
  i <- i + 1
}

khai_spleenextraction <- khai_spleenextraction %>% 
  discard( ~ is.null(nrow(.x)) == T) # discard items from list if nrow is null

khai_spleenextraction_df <- khai_spleenextraction %>%
  rbindlist(fill = T) %>% # does not need idcol since dna plate code is this  
  clean_names %>% 
  mutate_if(is.factor, as.character) %>% 
  dplyr::filter(!is.na(dna_plate_code) & !is.na(transponder)) %>%
  mutate(rfid = ifelse(
    grepl("^\\d+", sample_id_barcode) & nchar(sample_id_barcode) == 9,
    paste0("933000", sample_id_barcode),
    ifelse(
      grepl("^\\d+", sample_id_barcode) & nchar(sample_id_barcode) == 10,
      paste0("93300", sample_id_barcode),
      ifelse(!grepl("^\\d+", sample_id_barcode) & str_count(transponder) == 10,
             sample_id_barcode, sample_id_barcode)
    ) 
  )) %>% # create the rfid column from the sample_id_barcode to make them uniform and comparable to transponder id (rfid) in wfu
  # mutate(
  #   u01_rfid_verified = case_when(
  #     rfid %in%  WFU_OlivierCocaine_test_df$rfid ~ "yes",
  #     # rfid == "933000120117313" ~ "u01_olivier_cocaine",
  #     rfid %in%  WFU_OlivierOxycodone_test_df$rfid ~ "yes",
  #     rfid %in%  WFU_Jhou_test_df$rfid ~ "yes",
  #     rfid %in%  WFU_Mitchell_test_df$rfid ~ "yes",
  #     rfid %in%  WFU_Kalivas_test_df$rfid ~ "yes",
  #     rfid %in%  WFU_KalivasItaly_test_df$rfid ~ "yes",
  #     
  #     TRUE ~ "NA"
  #   )
  # ) %>%
  left_join(., shipments_df[, c("rfid", "cohort", "u01")], by = c("rfid")) 
# %>%
#   mutate(u01 = paste0(u01, "_", cohort)) %>%
#   select(-one_of("cohort"))
# 


################################ 
## LIBRARY RIPTIDE NAME LIST 
################################

library_riptide <- flipAPI::DownloadXLSX("https://www.dropbox.com/s/hq4g4fw4irubhes/Library%20Riptide%20Name%20List.xlsx?dl=0") %>% 
  clean_names() %>% 
  mutate_all(as.character) %>% 
  mutate_at(vars(matches("date")), as.POSIXct) 

library_riptide_df <- library_riptide %>% 
  ## pick up from here
  ## make this format the same   as others in library prep

  
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


