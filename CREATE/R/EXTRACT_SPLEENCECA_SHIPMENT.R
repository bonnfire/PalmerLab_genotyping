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

jhou_tissue_shipments_files <- c("U01 Spleen and Ceca shipment 20190319 (Thomas, Alen).xlsx",
                                 "Jhou_2019- 06-11 U01 Spleen and Ceca shipment .xlsx", 
                                 "2020-02-25 Spleen Shipment.xlsx",
                                 "05-05-2020 Spleen shipment .xlsx",
                                 "05-12-2020 Spleen Shipment.xlsx")
# excluding  because the 06-11 is a file that includes the 20190319 samples
                                 
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
names(jhou_tissue_shipments) <- c("03-19-2019", "06-11-2019", "02-25-2020", "05-05-2020", "05-12-2020") # "U01 Spleen and Ceca shipment 20190319 (Thomas, Alen).xlsx" is included in the 06-11 file
jhou_tissue_shipments_df <- jhou_tissue_shipments %>% 
  rbindlist(fill = T, idcol = "shipment_date") %>% 
  subset(!is.na(rfid))

# qc
# fixing dupe rfids within the shipments date
jhou_tissue_shipments_df %>% get_dupes(rfid, shipment_date) 
# fix dupes # using the Summary All table from Jhou project
jhou_tissue_shipments_df <- jhou_tissue_shipments_df %>% 
  mutate(rfid = replace(rfid, labanimalid == "U93"&sex == "M", "933000320046784"),
         rfid = replace(rfid, labanimalid == "U94"&sex == "M", "933000320046780"),
         rfid = replace(rfid, labanimalid == "U97"&sex == "F", "933000320046788"),
         rfid = replace(rfid, labanimalid == "U98"&sex == "F", "933000320046789"))

# fixing dupe rfids across the shipments
# remove the duplicated cases 
jhou_tissue_shipments_df <- jhou_tissue_shipments_df %>% 
  arrange(shipment_date) %>% 
  group_by(rfid) %>% 
  slice(1) %>% 
  ungroup()

# post fix qc
jhou_tissue_shipments_df %>% get_dupes(rfid) 

# remove unwanted columns
# jhou_tissue_shipments_df <- jhou_tissue_shipments_df %>% 
#   select(-matches("labanimalid|sex"))
  
# after rfid is fixed, fix format to get tissue type column
jhou_tissue_shipments_df <- jhou_tissue_shipments_df %>% 
  rowwise() %>% 
  mutate(tissue_type = ifelse(grepl("Y", cecum, ignore.case = T)&grepl("Y", spleen, ignore.case = T), paste0("cecum,", cecum_box_id, ";", "spleen,", spleen_box_id), "spleen")) %>% 
  # mutate(tissue_box = ifelse(!is.na(cecum_box_id)&!is.na(spleen_box_id), paste0(cecum_box_id, ",", spleen_box_id), NA)) %>% 
  ungroup() %>% 
  select(-matches("cecum|spleen")) %>% 
  separate_rows(tissue_type, sep = ";") %>% 
  separate(tissue_type, into = c("tissue_type", "shipping_box"), sep = ",") %>% 
  rename("comments"="notes") %>% 
  select(rfid, tissue_type, tissue_type, shipping_box, comments) 


anti_join(extraction_log %>% subset(grepl("jhou", project_name)), 
          jhou_tissue_shipments_df %>% subset(tissue_type == "spleen"), by = "rfid") %>% 
  select(rfid) %>% write.csv(file = "missing_jhou_tissue_shipinfo_n67.csv", row.names = F)
## all of jhou spleen shipments


###########################
###### OLIVIER ############
###########################
# both cocaine and oxycodone are included here

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/20190829_wfu_u01_shippingmaster/TissueShipments")
olivier_spleen_raw <- readxl::read_excel(path = "Olivier Spleens Oxy and Coc 91319.xlsx", col_names = F)

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
olivier_spleens_df <- plyr::rbind.fill(olivier_spleen_list_df, olivier_spleen_raw_2) %>% 
  mutate(tissue_type = "spleen",
         comments = NA) %>% 
  select(rfid, tissue_type, experiment)

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
  left_join(.,olivier_spleens_df, by = c("rfid")) %>% # c("rfid", "experiment") results in 508 instead of 517; because of the naive animals
  rename("wfu_cohort" = "cohort.x",
         "shipment_cohort" = "cohort.y") %>% 
  mutate(wfu_cohort = paste0("C", wfu_cohort)) %>% 
  subset(!is.na(shipment_cohort)) %>% dim
olivier_spleens_df %>% dim


# prepare table for Hannah (which ones to start extracting and which ones to hold off on, because no phenotype data)
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

mitchell_shipments_spleen_df <- mitchell_shipments_spleen %>% rbindlist(fill = T, idcol = "sheet") %>% 
  rename("dissection_comments"="disecction_comments")

# since there are two columns, one for barcode_number and another for rfid # do quick check to make sure that these are equal
mitchell_shipments_spleen_df %>% subset(!is.na(barcode_number)) %>% subset(rfid != paste0("933000", barcode_number)) # since barcode_number is empty for shipment1 doc



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




mitchell_tissues_df <- plyr::rbind.fill(mitchell_shipments_spleen_df[, c("rfid", "shipping_box", "tissue", "dissection_comments", "resolutions")], 
                                     mitchell_shipments_ceca_df[, c("rfid", "shipping_box", "tissue", "dissection_comments", "resolutions")]) %>%
  mutate(dissection_comments = gsub("\"", "", dissection_comments)) %>% 
  rename("tissue_type" = "tissue",
         "comments" = "dissection_comments") %>% 
  mutate(tissue_type = tolower(tissue_type)) %>% 
  select(rfid, tissue_type, shipping_box, comments)


###########################
###### KALIVAS ############
###########################
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/20190829_wfu_u01_shippingmaster/TissueShipments")
# kalivas_spleenceca_original_excel <- u01.importxlsx("Kalivas U grant_Spleen collection_Cohort information.xlsx")[-1] 
kalivas_spleen_xl <- u01.importxlsx("Kalivas U grant_Spleen collection_Cohort updated 20200214.xlsx")[-1] 

kalivas_tissues_df <- kalivas_spleen_xl %>% rbindlist(idcol = "cohort", fill = T) %>% #get rid of the timeline sheet because it gives us many unwanted columns
  clean_names() %>% 
  rename("rfid" = "microchip") %>% 
  subset(grepl("Cohort", cohort)) %>% # remove the dead rats 
  mutate(cohort = str_pad(str_extract(cohort, "\\d+"), 2, "left", "0"), 
         sex = str_extract(toupper(sex), "\\D{1}")) %>% 
  subset(grepl("^\\d+", rfid)) %>% # remove comments
  mutate(tissue_type = "spleen",
         tissue_type = replace(tissue_type, grepl("no spleen|tail", notes, ignore.case = T), "tail")) %>% 
  rename("comments" = "notes") %>% 
  select(rfid, tissue_type, comments)
  
# show to get the number of dead animals 
# u01.importxlsx("Kalivas U grant_Spleen collection_Cohort information.xlsx")[-1] %>% rbindlist(idcol = "cohort", fill = T) %>% #get rid of the timeline sheet because it gives us many unwanted columns
#   clean_names() %>% 
#   rename("rfid" = "microchip",
#          "status" = "cohort") %>% 
#   subset(grepl("Dead", status)) %>% 
#   select(-sex) %>% 
#   left_join(., WFU_Kalivas_test_df[, c("rfid", "sex", "cohort")], by = c("rfid")) %>% select(cohort) %>% table()



###########################
###### P50- JERRY #########
###########################
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/P50/Tissues")
jerry_shipments <- read_excel("RatSampleCollection_Batch17_NY-Richards_11-13Mar2020.xlsx", col_names = F)
names(jerry_shipments) <- jerry_shipments[2, ]
jerry_shipments <- jerry_shipments %>% clean_names()

# keep on the side for data dictionary 
jerry_shipments_dictionary <- jerry_shipments[1:2,] %>% t()
names(jerry_shipments_dictionary) <- c("varname_long", "varname_abv")

jerry_shipments <- jerry_shipments[-c(1:2),]

jerry_shipments_df <- jerry_shipments %>%
  rename("rfid" = "rat_rfid") %>% 
  mutate(datetime_dissected = openxlsx::convertToDate(as.numeric(datetime_dissected)) %>% as.character) %>% 
  select(matches("rfid|date|box")) %>% 
  gather(tissue, shipping_box, cecum_box:baculum_box) %>% 
  mutate(tissue_type = gsub("_box", "", tissue))


# extract number of tissues sent to us? 02/04/2021

jerry_shipments_2 <- read_excel("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/P50/Tissues/RatSampleCollection_Batch19_NY-Ishiwari_12-29Jan2021.xlsx", col_names = F)
names(jerry_shipments_2) <- jerry_shipments_2[2, ] %>% unlist() %>% as.character
jerry_shipments_2 <- jerry_shipments_2 %>% clean_names()

# keep on the side for data dictionary 
jerry_shipments_2_dictionary <- jerry_shipments_2[1:2,] %>% t()
names(jerry_shipments_2_dictionary) <- c("varname_long", "varname_abv")

jerry_shipments_2 <- jerry_shipments_2[-c(1:2),]

jerry_shipments_2_df <- jerry_shipments_2 %>%
  rename("rfid" = "rat_rfid") %>% 
  mutate(datetime_dissected = openxlsx::convertToDate(as.numeric(datetime_dissected)) %>% as.character) %>% 
  select(matches("rfid|date|box")) %>% 
  gather(tissue, shipping_box, cecum_box:baculum_box) %>% 
  mutate(tissue_type = gsub("_box", "", tissue))

#################################################
###### ALL SPLEENS DELIVERED FOR EXTRACTION #####
#################################################


### CREATE TISSUES TABLE
tissues <- list(u01_tom_jhou = jhou_tissue_shipments_df, 
                u01_suzanne_mitchell = mitchell_tissues_df,
                u01_olivier_george_cocaine = olivier_spleens_df %>% subset(experiment == "Cocaine") %>% select(-experiment), 
                u01_olivier_george_oxycodone = olivier_spleens_df %>% subset(experiment == "Oxy") %>% select(-experiment), 
                us_peter_kalivas_us = kalivas_tissues_df,
                p50_jerry_richards = jerry_shipments_df
     )
tissue_df <- tissues %>% 
  rbindlist(fill = T, idcol = "project_name") %>% 
  left_join(khai_tissueextraction_df_join[, c("rfid", "storage_box_of_spleen", "freezer_location_of_tissue")], by = "rfid") %>% ## XX when khai provides the link for the ceca storage info, UPDATE THIS LINE
  mutate_at(vars(one_of("storage_box_of_spleen", "freezer_location_of_tissue")), ~replace(., tissue_type == "cecum", NA)) %>% 
  select(rfid, project_name, tissue_type, shipping_box, storage_box_of_spleen, freezer_location_of_tissue, comments) %>% 
  naniar::replace_with_na_all(condition = ~.x %in% c("", "\"\"", "NA", "N/A", "None"))
