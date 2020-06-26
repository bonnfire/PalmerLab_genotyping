### PROCESSING PALMER LAB DATASETS 
# EXTRACTION AND FLOW CELL TABLES FROM HANNAH AND KHAI

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/20190829_WFU_U01_ShippingMaster/Tissues/Original")


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
  left_join(., shipments_df[, c("rfid", "cohort", "u01")], by = c("rfid")) %>%
  mutate(u01 = paste0(u01, "_", cohort)) %>%
  select(-one_of("cohort"))



################################ 
## LIBRARY RIPTIDE NAME LIST 
################################

library_riptide <- flipAPI::DownloadXLSX("https://www.dropbox.com/s/hq4g4fw4irubhes/Library%20Riptide%20Name%20List.xlsx?dl=0") %>% 
  clean_names() %>% 
  mutate_all(as.character) %>% 
  mutate_at(vars(matches("date")), as.POSIXct) 

library_riptide_df <- library_riptide %>% 
  ## pick up from here
  ## make this format the same as others in library prep

  
## full run id and project_name primary key 


################################# 
## SAMPLE BARCODE LIBRARY TABLE 
################################# 

# get rid of the Riptide filter to see the UMich data
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/20190829_WFU_U01_ShippingMaster/Tissues/Original")
flowcell_files <- list.files(path = ".", pattern = "^\\d{4}-\\d{2}-\\d{2}-Flowcell Sample-Barcode list.*[^)].xlsx")

u01.importxlsx <- function(xlname){
  path_sheetnames <- readxl::excel_sheets(xlname)
  df <- lapply(readxl::excel_sheets(path = xlname), readxl::read_excel, path = xlname)
  names(df) <- path_sheetnames
  return(df)
} 

flowcell <- lapply(flowcell_files, function(x){
  x <- u01.importxlsx(x)[[1]] %>% 
    clean_names() 
  return(x)
})
names(flowcell) <- flowcell_files 
flowcell_df <- flowcell %>% rbindlist(idcol = "flowcell_file", fill = T, use.names = T) %>% 
  mutate(comment = coalesce(comments_8, comments_9)) %>% 
  select(-c("x7", "comments_8", "comments_9", "flow_cell_lane")) %>%  # columns that only contain NA or have been coalesced
  dplyr::filter(grepl("Riptide", library)) %>% 
  mutate(comment = toupper(comment))
flowcell_df %>% mutate_at(vars(one_of("library")), as.factor) %>% summary()
flowcell_df %>% subset(!is.na(sample_id_demul)) %>% get_dupes(sample_id_demul)

## get_dupes()
## group by library

### XX pick up from here but 6/25 for abe 
flowcell_df %>%  
  mutate(rfid = ifelse(
  grepl("^\\d+", sample_id) & nchar(sample_id) == 9,
  paste0("933000", sample_id),
  ifelse(
    grepl("^\\d+", sample_id) & nchar(sample_id) == 10,
    paste0("93300", sample_id),
    ifelse(!grepl("^\\d+", sample_id) & str_count(sample_id) == 10,
           sample_id, sample_id)
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
left_join(., shipments_df[, c("rfid", "cohort", "u01")], by = c("rfid")) %>%
  mutate(u01 = paste0(u01, "_", cohort)) %>%
  select(-one_of("cohort")) %>% 
  subset(grepl("Kalivas", u01)) %>% 
  distinct(rfid) %>% 
  dim()




## create the sample sheet for Riyan
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/U01/20190829_WFU_U01_ShippingMaster/Tissues/Processed")

flow_cell_original_riyan <- u01.importxlsx("2020-01-16-Flowcell Sample-Barcode list-Riptide-UMich2-Riptide03_NovaSeq01 (copy for Riyan).xlsx") # 1 table
dataframe = list()
for(i in 1:10){
  dataframe[[i]] <- flow_cell_original_riyan$Sheet1 %>% group_by(Barcode) %>% slice(i)
}
write.xlsx(dataframe, file='Flowcell_Sample_Sheet.xlsx')





con <- dbConnect(dbDriver("PostgreSQL"), dbname="U01",user="postgres",password="postgres", )
dbWriteTable(con, c("public","extractions_ucsd"), value = extractions_khai_df, row.names = FALSE)

# gsub("\\\\", "", paste0(colnames(extractions_khai_df), collapse='","')) %>% cat to get an c+p version of the colnames 








## rewrite this using rdrop2 
install.packages("httpuv")
install.packages('rdrop2')
library(rdrop2)
token <- drop_auth()
saveRDS(token, file = "token.rds") #save the tokens for local/remote use; Then in any drop_* function, pass `dtoken = token


drop_search(query = "Flowcell", path = "https://www.dropbox.com/work/Palmer%20Lab/Khai-Minh%20Nguyen/Sequencing%20Submission%20Files")







#### SENT TO SEQUENCING CORE
## 06/09/2020
## XX fix project names before uploading the sheets in the database



extractions_flowcell <- extractions_khai_df %>% 
  subset(`sampleid_barcode` %in% flow_cell_original_rip$`Sample ID`) # 288

## create this table for rsm
text(barplot(table(extractions_flowcell$u01), beside = T), 0, table(extractions_flowcell$u01))
table(extractions_flowcell$u01, extractions_flowcell$userid)

# using origin will get you the wrong cohorts?

extractions_flowcell$u01 %>% table() 
extractions_flowcell$u01_rfid_verified %>% table() 

# extractions_flowcell$origin %>% table()

olivier_spleen_list_df %>% subset(rfid %in% extractions_flowcell[which(extractions_flowcell$comments == "mismatch"),]$transponder)
extractions_flowcell[which(extractions_flowcell$comments == "mismatch"),]
extractions_flowcell %>% subset(comments == "mismatch")
# searching for duplicate entries in the sampleid barcode column FALSE FOR BOTH extractions_flowcell %>% select(transponder AND sampleid_barcode) %>% duplicated() %>% any() 
agrep("933000120117342", extractions_flowcell$transponder, value = T)
"933000120138331"
"933000120138561"





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


