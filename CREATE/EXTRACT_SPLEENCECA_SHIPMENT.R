# process the ceca and spleen shipment data

library(dplyr)
library(data.table)
library(tidyverse)
library(readxl)
library(tidyxl)

# install.packages('splitstackshape')
# install.packages('janitor') 
library(splitstackshape)
library(janitor)
library(stringr)

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/20190829_WFU_U01_ShippingMaster")


###########################
###### JHOU ###############
###########################
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/20190829_wfu_u01_shippingmaster/TissueShipments")

jhou_tissue_shipments_files <- c("Jhou_2019- 06-11 U01 Spleen and Ceca shipment .xlsx", 
                                 "05-05-2020 Spleen shipment .xlsx",
                                 "05-12-2020 Spleen Shipment.xlsx")
                                 
jhou_tissue_shipments <- lapply(jhou_tissue_shipments_files, function(x){
  x <- read_excel(path = x, col_names = T) %>% 
    clean_names() %>% 
    rename_at(vars(matches("microchip")), function(x) "rfid") %>% 
    rename_at(vars(matches("jhou_lab_id|animal_id")), function(x) "labanimalid") %>% 
    subset(select = which(!duplicated(names(.)))) %>% #remove duplicated columns
    select(matches("rfid|labanimalid|sex|spleen|cecum|note|comment")) %>%  # select columns of interest
    mutate_all(as.character)
  return(x)
})
names(jhou_tissue_shipments) <- c("06-11-2019", "05-05-2020", "05-12-2020")
jhou_tissue_shipments_df <- jhou_tissue_shipments %>% 
  rbindlist(fill = T, idcol = "shipment_date") %>% 
  subset(!is.na(rfid))

# qc
jhou_tissue_shipments_df %>% get_dupes(rfid) 
# fix dupes # using the Summary All table from Jhou project
jhou_tissue_shipments_df <- jhou_tissue_shipments_df %>% 
  mutate(rfid = replace(rfid, labanimalid == "U93"&sex == "M", "933000320046784"),
         rfid = replace(rfid, labanimalid == "U94"&sex == "M", "933000320046780"),
         rfid = replace(rfid, labanimalid == "U97"&sex == "F", "933000320046788"),
         rfid = replace(rfid, labanimalid == "U98"&sex == "F", "933000320046789"))
# post fix qc
jhou_tissue_shipments_df %>% get_dupes(rfid) 

# remove unwanted columns
jhou_tissue_shipments_df <- jhou_tissue_shipments_df %>% 
  select(-matches("labanimalid|sex"))
  
# after rfid is fixed, fix format to get tissue type column
jhou_tissue_shipments_df <- jhou_tissue_shipments_df %>% 
  rowwise() %>% 
  mutate(tissue_type = ifelse(grepl("Y", cecum, ignore.case = T)&grepl("Y", spleen, ignore.case = T), paste0("cecum,", cecum_box_id, ";", "spleen,", spleen_box_id), "spleen")) %>% 
  # mutate(tissue_box = ifelse(!is.na(cecum_box_id)&!is.na(spleen_box_id), paste0(cecum_box_id, ",", spleen_box_id), NA)) %>% 
  ungroup() %>% 
  select(-matches("cecum|spleen")) %>% 
  separate_rows(tissue_type, sep = ";") %>% 
  separate(tissue_type, into = c("tissue_type", "tissue_box"), sep = ",") %>% 
  select(rfid, tissue_type, tissue_type, notes) 

## all of jhou spleen shipments


###########################
###### OLIVIER ############
###########################
# both cocaine and oxycodone are included here

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/20190829_wfu_u01_shippingmaster/TissueShipments")
olivier_spleen_raw <- readxl::read_excel(path = "Olivier Spleens Oxy and Coc 91319.xlsx", col_names = F)
# olivier_spleen_cells_raw <- tidyxl::xlsx_cells(path = "Olivier Spleens Oxy and Coc 91319.xlsx/TissueShipments")
# olivier_spleen_formats_raw <- xlsx_formats(path = "Olivier Spleens Oxy and Coc 91319.xlsx")

# parse through indices using colors, couldn't work because of two observations 

# using olivier_spleen_formats_raw$local$fill$patternFill$fgColor$rgb %>% unique()
# why is the result of this only 6 when there are 9 categories? olivier_spleen_formats_raw$local$fill$patternFill$fgColor$rgb %>% na.omit %>% unique() %>% length()

# test <- olivier_spleen_cells_raw %>% 
#   filter(grepl("Cohort", olivier_spleen_cells_raw$character)) 
# test$character_formatted is all NA 

# parse through the data using location and order 
# create dataframe with three cols: cohort, rfid, and labanimalid

# assign colnames 
names(olivier_spleen_raw)[seq(1, ncol(olivier_spleen_raw)-1, 2)] <- paste0("rfid")
names(olivier_spleen_raw)[seq(2, ncol(olivier_spleen_raw)-1, 2)] <- paste0("labanimalid")

# make list of dfs
olivier_spleen_list <- list()

for(i in seq(1, ncol(olivier_spleen_raw), 2)){
  j = (i/2)+0.5
  k = i + 1
  olivier_spleen_list[[j]] <- olivier_spleen_raw[, (i:k)]
}

# add cohort information from the legend provided in the excel sheet and remove after assignment
for(i in 1:9){
  olivier_spleen_list[[i]]$cohort = as.character(olivier_spleen_list[[10]][i,2])
} # replacement has 40 rows, data has 39 so it has to be outside the lapply function
olivier_spleen_list[[10]] <- NULL

# remove na rows and split the cohort into two
olivier_spleen_list_df <- lapply(olivier_spleen_list, function(df) {
  df <- df[-((grep("^total", df$rfid, ignore.case = T) - 1):nrow(df)), ]
  df <- df %>%
    mutate(row_num_location_in_box = dplyr::row_number()) %>%
    tidyr::separate(
      col = cohort,
      into = c("cohort", "experiment"),
      sep = " (?=[^ ]+$)"
    ) %>%
    mutate(sex = substring(labanimalid, 1, 1))
  return(df)
}) %>% rbindlist() %>%
  subset(!is.na(rfid)) %>% 
  mutate(cohort = paste0("C", parse_number(cohort) %>% str_pad(2, "left", "0")),
         experiment = replace(experiment, experiment == "Oxycodone", "Oxy"))


## add more spleens from second shipment
olivier_spleen_raw_2 <- u01.importxlsx("Spleens for Abe Oxy and Coc 20200110.xlsx")[[2]] %>% 
  clean_names %>% 
  rename("labanimalid" = "internal_id",
         "row_num_location_in_box" = "position_in_box") %>% 
  separate(box, c("experiment", "cohort"), sep = " ") %>% 
  mutate(sex = substring(labanimalid, 1, 1),
         cohort = paste0("C", parse_number(cohort) %>% str_pad(2, "left", "0")))

## join all shipment sheets
olivier_spleens_df <- plyr::rbind.fill(olivier_spleen_list_df, olivier_spleen_raw_2)

# USE BELOW OBJECT (cleaned)
## QC and verify that cohort information is correct and rfid's are valid

# print below to see which cohort info is wrong
WFU_OlivierCocaine_test_df %>% 
  mutate(experiment = "Cocaine") %>% 
  select(rfid, cohort, experiment) %>% 
  rbind(WFU_OlivierOxycodone_test_df %>% 
          mutate(experiment = "Oxy") %>% 
          select(rfid, cohort, experiment)) %>% 
  left_join(olivier_spleens_df, by = c("rfid", "experiment")) %>%
  rename("wfu_cohort" = "cohort.x",
         "shipment_cohort" = "cohort.y") %>% 
  mutate(wfu_cohort = paste0("C", wfu_cohort)) %>% 
  subset(shipment_cohort != wfu_cohort | 
           is.na(wfu_cohort))

# are the counts of the df's the same 
WFU_OlivierCocaine_test_df %>% 
  mutate(experiment = "Cocaine") %>% 
  select(rfid, cohort, experiment) %>% 
  rbind(WFU_OlivierOxycodone_test_df %>% 
          mutate(experiment = "Oxy") %>% 
          select(rfid, cohort, experiment)) %>% 
  left_join(.,olivier_spleens_df, by = c("rfid", "experiment")) %>%
  rename("wfu_cohort" = "cohort.x",
         "shipment_cohort" = "cohort.y") %>% 
  mutate(wfu_cohort = paste0("C", wfu_cohort)) %>% 
  subset(!is.na(shipment_cohort)) %>% dim
olivier_spleens_df %>% dim


## XX come back to this after you attach the missing rfids
anti_join(olivier_spleens_df, WFU_OlivierCocaine_test_df %>% 
            mutate(experiment = "Cocaine") %>% 
            select(rfid, cohort, experiment) %>% 
            rbind(WFU_OlivierOxycodone_test_df %>% 
                    mutate(experiment = "Oxy") %>% 
                    select(rfid, cohort, experiment)) %>% 
            left_join(.,olivier_spleens_df, by = c("rfid", "experiment")) %>%
            rename("wfu_cohort" = "cohort.x",
                   "shipment_cohort" = "cohort.y") %>% 
            mutate(wfu_cohort = paste0("C", wfu_cohort)) %>% 
            subset(!is.na(shipment_cohort)), by = "rfid")



# QC: 
# number of counts as the raw files: all numbers match cohorts 1:8 for cocaine, no cohort 6, and only 3 and 4 for oxy
##  olivier_spleen_list_df %>% group_by(cohort, experiment) %>% count

# sex count # a little unevent but not alarming
olivier_spleen_sex <- olivier_spleen_list_df %>% group_by(cohort, experiment, sex) %>% summarize(sexcount = n())
ggplot(olivier_spleen_list_df, aes(x = sex, fill = sex)) + 
  geom_histogram(stat = "count") + 
  facet_wrap(~cohort + experiment) + 
  labs(title = "Spleens sent to UCSD for Oxy and Coc 9/13/19")

# unique id's, no duplicate id's 
## olivier_spleen_list_df[duplicated(olivier_spleen_list_df$rfid)]

# are any of these found in naive 
## extract from the u01_qc file  
# list <- list(Olivier_co_naive <- rbindlist(WFU_Olivier_co_naive_test, use.names = T, idcol = "cohort"), 
#              Olivier_ox_naive <- rbindlist(WFU_Olivier_ox_naive_test, use.names = T, idcol = "cohort")) 
WFU_Olivier_co_naive_df <- rbindlist(WFU_Olivier_co_naive_test, use.names = T, idcol = "cohort")
olivier_spleen_list_df %>% 
  mutate(Cocaine_Naive = ifelse(experiment == "Cocaine" && rfid %in% WFU_Olivier_co_naive_df$rfid, "Naive", "Not Naive"),
         Oxycodone_Naive = ifelse(experiment == "Oxycodone" && rfid %in% WFU_Olivier_co_naive_df$rfid, "Naive", "Not Naive")) %>% 
  group_by(Cocaine_Naive, Oxycodone_Naive) %>% 
  count() # All Not Naive

# are any of the shipped spleens from the naive dataset 
olivier_spleen_list_df %>% 
  dplyr::filter(experiment == "Cocaine", rfid %in% WFU_Olivier_co_naive_df$rfid)
# are any of the shipped spleens from the collected data 
olivier_spleen_list_df %>% 
  dplyr::filter(experiment == "Cocaine", rfid %in% selfadmin_df$rfid) %>%
  group_by(cohort) %>% 
  summarise(n = n())

olivier_spleen_list_df %>%
  dplyr::filter(experiment == "Cocaine") %>% 
  group_by(cohort) %>% 
  summarise(n = n())

# which cohorts are the spleens from that don't have data
olivier_spleen_list_df %>% 
  dplyr::filter(experiment == "Cocaine", !rfid %in% selfadmin_df$rfid) %>% 
  group_by(cohort) %>% 
  summarise(n = n())
# spleen that don't have data (test if they overlap with the truncated id's)
olivier_spleen_list_df %>% 
  dplyr::filter(experiment == "Cocaine", !rfid %in% selfadmin_df$rfid) %>% 
  select(rfid) %>% 
  mutate(nchar = nchar(rfid))

# how many and which naive rats have data
WFU_Olivier_co_naive_df %>% 
  dplyr::filter(rfid %in% selfadmin_df$rfid) %>% 
  group_by(cohort) %>% 
  summarise(n = n())

WFU_Olivier_co_naive_df %>% 
  dplyr::filter(rfid %in% selfadmin_df$rfid)

# are all of these found in the experiments 
## extract from the u01_qc file  
## merge the two olivier Rdata files and compare the long rfid 
phenotyped_vs_spleens <- do.call("rbind", list(olivier_spleen_list_df$rfid, WFU_Olivier_ox_test_df$rfid, WFU_Olivier_co_test_df$rfid))
any(duplicated(do.call("rbind", list(olivier_spleen_list_df$rfid, WFU_Olivier_ox_test_df$rfid, WFU_Olivier_co_test_df$rfid))))

olivier_spleen_list_df %>% 
  mutate(Cocaine_Data = ifelse(experiment == "Cocaine" && rfid %in% selfadmin_df$rfid, "Self admin data", "No self admin data")) %>% 
         # ,
         # Oxycodone_Data = ifelse(experiment == "Oxycodone" && rfid %in% WFU_Olivier_ox_naive_test$rfid, "Naive", "Not Naive")) %>% 
  group_by(Cocaine_Data) %>% 
           #, Oxycodone_Data) %>% 
  count() # All 411 spleens have self admin data

# check the number of characters in the rfid
olivier_spleen_list_df %>% mutate(rfid_digits = nchar(rfid)) %>% filter(rfid_digits != 15) # all id's are 15 digits here
WFU_Olivier_ox_test_df %>% mutate(rfid_digits = nchar(rfid)) %>% filter(rfid_digits != 15) # one case isn't 15 digits
WFU_Olivier_co_test_df %>% dplyr::mutate(rfid_digits = nchar(rfid)) %>% dplyr::filter(rfid_digits != 15) # four cases aren't 15 digits

olivier_spleen_list_df %>% 
  dplyr::filter(experiment == "Cocaine", !rfid %in% selfadmin_df$rfid) %>% 
  select(rfid) %>% 
  mutate(nchar = nchar(rfid))

# prepare table for Hannah 
# to create excel workbooks 
library(tidyverse)
library(openxlsx) 

# create dataframes to fill up each sheet
to_genotype_olivier_cocaine <- olivier_spleen_list_df %>% 
  dplyr::filter(experiment == "Cocaine") %>% 
  mutate(to_genotype = ifelse(rfid %in% selfadmin_df$rfid, "yes", 
                              ifelse(is.na(rfid), NA, "no")))

to_genotype_olivier_oxy <- olivier_spleen_list_df %>% 
  dplyr::filter(experiment == "Oxycodone") %>% 
  mutate(to_genotype = ifelse(rfid %in% WFU_Olivier_ox_test_df[which(is.na(WFU_Olivier_ox_test_df$comment)), ]$rfid, "yes", 
                            ifelse(rfid %in% WFU_Olivier_ox_test_df[which(WFU_Olivier_ox_test_df$comment == "Naive"), ]$rfid, "no", NA))) ## XX CHANGE THIS STATEMENT ONCE GIORDANO / OLIVIER GIVE YOU DATA FOR OXY

exp <- olivier_spleen_list_df$experiment %>% unique
wb <- createWorkbook() 
addWorksheet(wb, sheetName = exp[1] )
addWorksheet(wb, sheetName = exp[2] )
writeData(wb, exp[1], to_genotype_olivier_cocaine)
writeData(wb, exp[2], to_genotype_olivier_oxy)
saveWorkbook(wb, file = "olivier_spleen_cocaine_oxy_to_genotype.xlsx", overwrite = TRUE)

# use this function in the future to print the results of same analyses run on diff dataframes into multiple worksheets on excel 
# createSpreadsheets <- function(species,r1,r2){
#   ## Create new workbooks
#   wb <- createWorkbook() 
#   
#   ## Create the worksheets
#   addWorksheet(wb, sheetName = "Results1" )
#   addWorksheet(wb, sheetName = "Results2" )
#   
#   ## Write the data
#   writeData(wb, "Results1", r1)
#   writeData(wb, "Results2", r2)
#   
#   ## Save workbook to working directory 
#   saveWorkbook(wb, file = paste(species,".xlsx", sep=""), overwrite = TRUE)
# }
# 
# ## create spreadsheets by calling our function for each species
# for(s in exp){
#   createSpreadsheets(s,to_genotype_olivier_cocaine,to_genotype_olivier_oxy)
# }

###########################
###### MITCHELL ###########
###########################
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/20190829_wfu_u01_shippingmaster/TissueShipments")

# spleens

mitchell_shipments_files <- list.files(path = ".", pattern = "Shipping_Content_Lists.*")
mitchell_extractspleen <- function(x){
  spleen_shipments <- u01.importxlsx(x)$`Spleen Shipping Sheet` %>% 
    clean_names() %>% 
    rename("rfid" = "id") %>% 
    rename_at(vars(matches("^shipping_box_1_container_1$")), function(x) "shipping_box") %>% 
    rename_at(vars(matches("tissue_collected")), function(x) "tissue") %>% 
    rename_at(vars(matches("microchip_id")), function(x) "microchip") %>% 
    mutate(shipping_box = gsub("[^[:digit:].]", "", shipping_box))
  return(spleen_shipments)
}
mitchell_shipments_spleen <- lapply(mitchell_shipments_files, mitchell_extractspleen) 
names(mitchell_shipments_spleen) <- mitchell_shipments_files

mitchell_shipments_spleen_df <- mitchell_shipments_spleen %>% rbindlist(fill = T, idcol = "sheet")

# since there are two columns, one for barcode_number and another for rfid # do quick check to make sure that these are equal
mitchell_shipments_spleen_df %>% subset(!is.na(barcode_number)) %>% subset(rfid != paste0("933000", barcode_number)) # since barcode_number is empty for shipment1 doc



## XX 6/30/2020 fix the shipment3 spleen ceca original object, doesn't exist

mitchell_extractceca <- function(x){
  spleen_shipments <- u01.importxlsx(x)$`Ceca Shipping Sheet` %>% 
    clean_names() %>% 
    rename_at(vars(matches("^id$")), function(x) "rfid") %>%
    rename_at(vars(matches("^shipping_box_2_container_2$|^box$")), function(x) "shipping_box") %>%
    rename_at(vars(matches("tissue_collected")), function(x) "tissue") %>%
    rename_at(vars(matches("microchip_id")), function(x) "microchip") %>%
    mutate(shipping_box = gsub("[^[:digit:].]", "", shipping_box))
  return(spleen_shipments)
}
mitchell_shipments_ceca <- lapply(mitchell_shipments_files, mitchell_extractceca) 
names(mitchell_shipments_ceca) <- mitchell_shipments_files
mitchell_shipments_ceca_df <- mitchell_shipments_ceca %>% rbindlist(fill = T, idcol = "sheet") %>% 
  dplyr::filter(grepl("^\\d+$", rfid))




# mitchell_spleenceca_toprocess <- plyr::rbind.fill(mitchell_spleen_shipments, mitchell_ceca_shipments)


###########################
###### KALIVAS ############
###########################
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/20190829_wfu_u01_shippingmaster/TissueShipments")
kalivas_spleenceca_original_excel <- u01.importxlsx("Kalivas U grant_Spleen collection_Cohort information.xlsx")[-1] 
kalivas_spleenceca_original_excel_updated <- u01.importxlsx("Kalivas U grant_Spleen collection_Cohort updated 20200214.xlsx")[-1] 

kalivas_spleenceca_original_df <- kalivas_spleenceca_original_excel %>% rbindlist(idcol = "cohort", fill = T) %>% #get rid of the timeline sheet because it gives us many unwanted columns
  clean_names() %>% 
  rename("rfid" = "microchip") %>% 
  subset(grepl("Cohort", cohort)) %>% 
  mutate(cohort = str_pad(str_extract(cohort, "\\d+"), 2, "left", "0"), sex = str_extract(toupper(sex), "\\D{1}")) %>% 
  subset(grepl("^\\d+", rfid)) %>% 
  select(-sex) 

kalivas_spleenceca_original_excel_updated %>% rbindlist(idcol = "cohort", fill = T) %>% #get rid of the timeline sheet because it gives us many unwanted columns
  clean_names() %>% 
  rename("rfid" = "microchip") %>% 
  subset(grepl("Cohort", cohort)) %>% 
  mutate(cohort = str_pad(str_extract(cohort, "\\d+"), 2, "left", "0"), sex = str_extract(toupper(sex), "\\D{1}")) %>% 
  subset(grepl("^\\d+", rfid)) %>% 
  select(-sex) 
# %>% 
  # left_join(., WFU_Kalivas_test_df[, c("rfid", "sex", "cohort")], by = c("rfid", "cohort")) ## rfid and cohort match in the spleens

# show to get the number of dead animals 
# u01.importxlsx("Kalivas U grant_Spleen collection_Cohort information.xlsx")[-1] %>% rbindlist(idcol = "cohort", fill = T) %>% #get rid of the timeline sheet because it gives us many unwanted columns
#   clean_names() %>% 
#   rename("rfid" = "microchip",
#          "status" = "cohort") %>% 
#   subset(grepl("Dead", status)) %>% 
#   select(-sex) %>% 
#   left_join(., WFU_Kalivas_test_df[, c("rfid", "sex", "cohort")], by = c("rfid")) %>% select(cohort) %>% table()


#################################################
###### ALL SPLEENS DELIVERED FOR EXTRACTION #####
#################################################


## for khai's request for a spreadsheet with all delivered spleen 

spleen_extraction_df <- list(
  Jhou = jhou_spleen_test %>% subset(spleen == "Yes") %>% select(rfid, notes),
  Mitchell = mitchell_shipments_spleen_df %>% subset(tissue == "Spleen") %>% select(rfid, notes),
  Kalivas = kalivas_spleenceca_original_df %>% select(rfid, notes),
  Olivier = olivier_spleen_list_df %>% select(rfid)
) %>% rbindlist(fill = T, idcol = "u01")
# quick qc before creating into excel sheet to share with khai
spleen_extraction_df %>% subset(nchar(rfid) != 15|!grepl("^\\d", rfid)) 
spleen_extraction_df %>% get_dupes(rfid) 

# create the excel sheet as requested
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/20190829_wfu_u01_shippingmaster")
write.xlsx(spleen_extraction_df, "spleen_extraction_u01_04242020.xlsx")



### CREATE TISSUES TABLE

