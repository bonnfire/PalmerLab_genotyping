## set up chron job 
# 
# devtools::install_github("bnosac/cronR")
# 
# library(cronR)
# cron_rm("extraction")
# cmd <- cron_rscript("/home/bonnie/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/R/extraction_chron.R")
# cron_add(command = cmd, frequency = 'hourly', at = '00:29', id = 'extraction', description = "testing linux scheduler")
# 
### 

library(googledrive)
library(googlesheets4)
library(janitor)
library(tidyverse)
library(data.table)


#find sheet

gs4_auth("palmerlabmeister@gmail.com")


theSheets <- gs4_find()

#find sheetnames of the first one(dummy sheet)
sheetNames <- sheet_names(theSheets$id[grep("Spleen&Fish", theSheets$name, ignore.case = T)])


if (exists("sheetNames1") == FALSE) {
  
  #read the sheets 
  theWorksheets <- lapply(sheetNames[1:20], function(x){
    x <- sheets_read(theSheets$id[grep("Spleen&Fish", theSheets$name, ignore.case = T)], sheet = x)  %>% 
      clean_names() %>% 
      subset(!is.na(dna_plate_code))
    return(x)
  })
  
  #generate a copy of sheet names
  sheetNames1 <- sheetNames[1:20]
  names(theWorksheets) <- sheetNames[1:20]
  
  
  #look through and see if one column has na,
  #if so, remove the sheet from sheetNames1 so next run, it will look at the sheet again
  remove <- c()
  
  for (i in 1:20) {
    if (any(is.na(theWorksheets[[i]]$riptide_plate_number))) {
      
      sheetNames1 <- sheetNames1[-i]
      remove <- append(remove, i)
    } 
  }
  
  #now, remove any sheets that had na in one column from the main list of sheets
  theWorksheets <- theWorksheets[-remove]
  
  ### do everything else now with the remaining sheets, append to database
  
  
} else {
  
  #read the sheets 
  theWorksheets <- lapply(sheetNames, function(x){
    sheets_read(theSheets$id[grep("Spleen&Fish", theSheets$name, ignore.case = T)], sheet = x) } )
  
  names(theWorksheets) <- sheetNames_split
  
  
  #take a look at last round, remove any sheets that were done last round.
  sheetNames1 <- subset(sheetNames, !(sheetNames  %in% sheetNames1))
  
  
  theWorksheets <- theWorksheets[sheetNames1]
  
  
  #do everything with these sheets now
  
  
  #add to database
  
  
}

# clean for df 
extraction_df <- theWorksheets %>% 
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


write.csv(extraction_df, "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/sample_tracking/generated/extraction_chron_test.csv", row.names = F)
