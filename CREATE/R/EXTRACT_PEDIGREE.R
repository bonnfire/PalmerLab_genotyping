## create and qc pedigree
# trying to use db access id's to fix CANNOT 
## db query returned 0 
# select * from (select 'u01_tom_jhou' as schema, sires, dames, accessid, sex from u01_tom_jhou.wfu_master
#                union all
#                select 'u01_suzanne_mitchell' as schema, sires, dames, accessid, sex from u01_suzanne_mitchell.wfu_master
#                union all
#                select 'u01_olivier_george_cocaine' as schema, sires, dames, accessid, sex from u01_olivier_george_cocaine.wfu_master
#                union all
#                select 'u01_olivier_george_oxycodone' as schema, sires, dames, accessid, sex from u01_olivier_george_oxycodone.wfu_master
#                union all
#                select 'u01_peter_kalivas_us' as schema, sires, dames, accessid, sex from u01_peter_kalivas_us.wfu_master
#                union all
#                select 'u01_peter_kalivas_italy' as schema, sires, dames, accessid, sex from u01_peter_kalivas_italy.wfu_master) as derivedTable 
# inner JOIN pedigree_temp ON pedigree_temp.id_f51 = derivedTable.accessid and pedigree_temp.sex = derivedTable.sex

## qc'ing with angela 01/06/2021
pedigree_01062021<- openxlsx::read.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/BREEDING PEDIGREE up to generation 36 01_05_2020.xlsx") %>% 
  mutate_all(as.character) %>% 
  clean_names %>% 
  mutate_all(toupper) %>% 
  mutate_at(vars(matches("(sire|dam)_id"), "id_f51"), ~gsub("(\\d)$", "_\\1", .)) # format access id's to fix our records in wfu sheets

## fixing w angela notes 
pedigree_01062021_fix <- pedigree_01062021 %>% 
  mutate(
    # sire_sw_id = replace(sire_sw_id, id_f51 %in% c("64708_1"), "HSM01117"), # using offspring to fix sire or dam sw_id
    #      sire_sw_id = replace(sire_sw_id, id_f51 %in% c("64713_1"), "HSM01117B"),
    #      sire_sw_id = replace(sire_sw_id, id_f51 %in% c("65075_1"), "HSM01117B"),
    #      sire_id = replace(sire_id, sire_sw_id == "HSM01117", "64556_1"),
    sire_sw_id = replace(sire_sw_id, sire_id == "64556_1", "HSM01117B"), 
    
    sire_sw_id = replace(sire_sw_id, sire_id == "65075_1", "HSM01117B"),
    
    dam_sw_id = replace(dam_sw_id, dam_id == "65144_2", "HSF01201"),
    dam_id = replace(dam_id, dam_sw_id == "HSF01201", "65144_2"),
    dam_id = replace(dam_id, dam_sw_id == "HSF01202", "64660_2"),
    
    dam_sw_id = replace(dam_sw_id, dam_sw_id == "HSF013312", "HSF01331B"),
    
    dam_sw_id = replace(dam_sw_id, dam_sw_id == "HSF01319BB", "HSF01319B"),
    dam_id = replace(dam_id, dam_sw_id == "HSF01319B", "65577_2"), 
    
    dam_id = replace(dam_id, dam_sw_id == "WHSF0513", "73191_2"),
    
    sire_id = replace(sire_id, sire_sw_id == "WHSM0523", "73204_1"),
    
    sire_sw_id = replace(sire_sw_id, sire_id == "75097_1", "WHSM0936"),
    
    dam_sw_id = replace(dam_sw_id, dam_id == "75098_2", "WHSF0903"),
    
    sire_sw_id = replace(sire_sw_id, sire_id == "75691_1", "WHSM1061"),
    
    dam_id = replace(dam_id, dam_sw_id == "WHSF1023B", "75699_2"),
    
    dam_id = replace(dam_id, dam_sw_id == "WHSF1038B", "75735_2"),
    
    ## fixing double sw_id 
    sire_sw_id = replace(sire_sw_id, sire_id == "43873_3", "HSM0112"), 
    sire_sw_id = replace(sire_sw_id, sire_id == "64287_1", "HSM01120"),
    
    dam_sw_id = replace(dam_sw_id, dam_id == "74140_2", "WHSF0707"),   
    dam_sw_id = replace(dam_sw_id, dam_id == "74208_2", "WHSF0707B"), 
    
    dam_sw_id = replace(dam_sw_id, dam_id == "74207_2", "WHSF0751"),   
    dam_sw_id = replace(dam_sw_id, dam_id == "74187_2", "WHSF0751B"),   
    
    sire_sw_id = replace(sire_sw_id, sire_id == "74204_1", "WHSM0716"),   
    sire_sw_id = replace(sire_sw_id, sire_id == "74217_3", "WHSM0734"),
    
    sire_sw_id = replace(sire_sw_id, sire_id == "74142_1", "WHSM0725"),   
    sire_sw_id = replace(sire_sw_id, sire_id == "74134_1", "WHSM0713"),
    
    sire_sw_id = replace(sire_sw_id, sire_id == "43873_3", "HSM0112")
  )

## do the id's as parents match when they're children
pedigree_id_nomatch <- pedigree_01062021_fix %>%
  distinct(sire_id, dam_id, sire_sw_id, dam_sw_id) %>%
  rowwise() %>%
  mutate(sire_id = paste0(sire_id, ",", sire_sw_id),
         dam_id = paste0(dam_id, ",", dam_sw_id)) %>%
  select(-matches("sw_id")) %>%
  gather("parent_sex", "parent_id") %>%
  separate(parent_id, into = c("parent_id", "parent_sw_id"), sep = ",") %>%
  mutate(parent_sw_id = gsub(" ", "", parent_sw_id)) %>%
  distinct() %>%
  mutate_all(~na_if(., "NA")) %>%
  subset(!(is.na(parent_id)&is.na(parent_sw_id))) %>% 
  left_join(pedigree_01062021_fix[, c("id_f51", "sw_id")] %>% rename("sw_id_as_child"="sw_id"), by = c("parent_id" = "id_f51")) %>% 
  subset(parent_sw_id != sw_id_as_child)

pedigree_01062021_fix %>%
  distinct(sire_id, dam_id, sire_sw_id, dam_sw_id) %>%
  rowwise() %>%
  mutate(sire_id = paste0(sire_id, ",", sire_sw_id),
         dam_id = paste0(dam_id, ",", dam_sw_id)) %>%
  select(-matches("sw_id")) %>%
  gather("parent_sex", "parent_id") %>%
  separate(parent_id, into = c("parent_id", "parent_sw_id"), sep = ",") %>%
  mutate(parent_sw_id = gsub(" ", "", parent_sw_id)) %>%
  distinct() %>%
  mutate_all(~na_if(., "NA")) %>%
  subset(!(is.na(parent_id)&is.na(parent_sw_id))) %>% 
  left_join(pedigree_01062021_fix[, c("id_f51", "sw_id")] %>% rename("access_id_as_child"="id_f51"), by = c("parent_sw_id" = "sw_id")) %>% 
  subset(parent_id != access_id_as_child) %>% 
  subset(!is.na(parent_sw_id))

# temp pedigree w NA to riyan 
pedigree_01062021_fix_temp <- pedigree_01062021_fix %>% 
  mutate(comments = case_when(
    id_f51 %in% pedigree_id_nomatch$parent_id | sw_id %in% pedigree_id_nomatch$sw_id_as_child | sire_id %in% pedigree_id_nomatch$parent_id | sire_id %in% pedigree_id_nomatch$parent_id | dam_id %in% pedigree_id_nomatch$parent_id | sire_sw_id %in% pedigree_id_nomatch$parent_sw_id ~ "NA ID as parent and as child do not match",
    TRUE ~ NA_character_
  )) %>% 
  mutate(id_f51 = replace(id_f51, id_f51 %in% pedigree_id_nomatch$parent_id, NA),
         sw_id = replace(sw_id, sw_id %in% pedigree_id_nomatch$sw_id_as_child, NA),
         sire_id = replace(sire_id, sire_id %in% pedigree_id_nomatch$parent_id, NA), 
         dam_id = replace(dam_id, dam_id %in% pedigree_id_nomatch$parent_id, NA), 
         sire_sw_id = replace(sire_sw_id, sire_sw_id %in% pedigree_id_nomatch$parent_sw_id, NA), 
         dam_sw_id = replace(dam_sw_id, dam_sw_id %in% pedigree_id_nomatch$parent_sw_id, NA)) %>% 
  mutate(dob = openxlsx::convertToDate(dob))
pedigree_01062021_fix_temp %>% 
  distinct() %>% 
  write.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/pedigree_01222021_temp_n4655.csv", row.names = F)

## create file with the na's for the db
pedigree_01062021_fix_temp %>% 
  distinct() %>% 
  write.csv("~/Desktop/Database/csv files/sample_tracking/pedigree_temp_n4633.csv", row.names = F)




pedigree_01062021_siredame <- pedigree_01062021 %>%
  distinct(sire_id, dam_id, sire_sw_id, dam_sw_id) %>%
  rowwise() %>%
  mutate(sire_id = paste0(sire_id, ",", sire_sw_id),
         dam_id = paste0(dam_id, ",", dam_sw_id)) %>%
  select(-matches("sw_id")) %>%
  gather("parent_sex", "parent_id") %>%
  separate(parent_id, into = c("parent_id", "parent_sw_id"), sep = ",") %>%
  mutate(parent_sw_id = gsub(" ", "", parent_sw_id)) %>%
  distinct() %>%
  mutate_all(~na_if(., "NA")) %>%
  subset(!is.na(parent_id)&!is.na(parent_sw_id)) %>%
  mutate(parent_id = replace(parent_id, parent_sw_id == "HSF01201", "64659_2"),
         parent_id = replace(parent_id, parent_sw_id == "HSF01202", "64660_2"),
         parent_id = replace(parent_id, parent_sw_id == "WHSF0513", "73191_2"),
         parent_id = replace(parent_id, parent_sw_id == "WHSM0523", "73204_1"),
         parent_id = replace(parent_id, parent_sw_id == "WHSM0937", "75281_1"),
         parent_id = replace(parent_id, parent_sw_id == "WHSF0904", "75067_2"),
         parent_id = replace(parent_id, parent_sw_id == "WHSM1036", "75801_1"),
         parent_id = replace(parent_id, parent_sw_id == "WHSF1023B", "75699_2"),
         parent_id = replace(parent_id, parent_sw_id == "WHSF1038B", "75735_2")) %>%
  mutate(parent_sw_id = replace(parent_sw_id, parent_sw_id == "HSF01319BB", "HSF01319B"),
         parent_sw_id = replace(parent_sw_id, parent_sw_id == "HSF01331B", "HSF013312"),
         parent_sw_id = replace(parent_sw_id, parent_sw_id == "HSM01172", "HSM01117B"),) %>%
  distinct() ## waiting for two more cases

# two different parent_sw_id for parent_id
pedigree_01062021_siredame %>%
  # mutate(parent_id_corrected = parent_id) %>%
  # mutate(parent_id_corrected = replace(parent_id_corrected, parent_sw_id == "WHSF0513", "73191_2"),
  #   parent_id_corrected = replace(parent_id_corrected, parent_sw_id == "WHSM0523", "73204_1"),
  #   parent_id_corrected = replace(parent_id_corrected, parent_sw_id == "WHSM0937", "75281_1"),
  #   parent_id_corrected = replace(parent_id_corrected, parent_sw_id == "WHSF0904", "75067_2"),
  #   parent_id_corrected = replace(parent_id_corrected, parent_sw_id == "WHSM1036", "75801_1"),
  #   parent_id_corrected = replace(parent_id_corrected, parent_sw_id == "WHSF1023B", "75699_2"),
  #   parent_id_corrected = replace(parent_id_corrected, parent_sw_id == "WHSF1038B", "75735_2")) %>%
  # get_dupes(parent_id_corrected) %>%
  get_dupes(parent_id) %>%
  subset(grepl("M", parent_sw_id)&parent_sex=="sire_id"|grepl("F", parent_sw_id)&parent_sex=="dam_id") %>%
  # distinct(parent_id_corrected, parent_sw_id) %>% get_dupes(parent_id_corrected) %>% select(-dupe_count) %>%
  distinct(parent_id, parent_sw_id) %>% get_dupes(parent_id) %>% select(-dupe_count) %>%
  as.data.frame(row.names = NA)







## pedigree as of 12/16/2020

# apurva's copy 
load("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/Riptide_control_pedigree.RData")
ped_map 


# renamed original file   
pedigree_12162020 <- openxlsx::read.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/Breeding Pedigree up to generation 36 12-16-20.xlsx") %>% 
  mutate_all(as.character) %>% 
  clean_names %>% 
  mutate_all(toupper) %>% 
  mutate_at(vars(matches("(sire|dam)_id"), "id_f51"), ~gsub("(\\d)$", "_\\1", .)) 

old_pedigree <- read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/pedigree.csv") %>% 
  mutate_all(as.character)

pedigree_12162020 %>% 
  subset(sire_id %in% c(pedigree_12162020 %>% 
                          subset(id_f51 %in% unique(pedigree_12162020$sire_id) & sex == "F") %>% 
                          select(id_f51) %>% unlist() %>% as.character))

## pedigree as of 12/14/2020
pedigree_12142020 <- openxlsx::read.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/Copy of Breeding Pedigree up to generation 36 12-8-20.xlsx") %>% 
  mutate_all(as.character) %>%  # n = 4657
  clean_names %>% 
  mutate_all(toupper) %>% 
  mutate_at(vars(matches("(sire|dam)_id"), "id_f51"), ~gsub("(\\d)$", "_\\1", .)) 
# animals who had sire listed as a female rat 
pedigree_12142020 %>% 
  subset(sire_id %in% c(pedigree_12142020 %>% 
                          subset(id_f51 %in% unique(pedigree_12082020$sire_id) & sex == "F") %>% 
                          select(id_f51) %>% unlist() %>% as.character)) %>% # rows that list a "FEMALE" sire ("F" designated by individual record)
  tibble::rownames_to_column("excel_row") 

pedigree_12142020 %>% 
  get_dupes(sw_id) %>% 
  write.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/dupe_sw_id_n100.xlsx")

pedigree_12142020 %>% 
  distinct() %>% 
  write.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/pedigree_12152020_temp_n4655.csv", row.names = F)

pedigree_12142020_temp_fix <- pedigree_12142020 %>% 
  distinct() %>% 
  mutate(sire_id = replace(sire_id, id_f51 == "73154_1", "72774_3"),
         sire_id = replace(sire_id, id_f51 == "73192_1", "72905_1"),
         dam_id = replace(dam_id, id_f51 == "73192_1", "72773_4"),
         sire_id = replace(sire_id, id_f51 == "73154_2", "72774_3"),
         sire_id = replace(sire_id, id_f51 == "73192_2", "72905_1"),
         dam_id = replace(dam_id, id_f51 == "73192_2", "72773_4")) %>% 
  mutate_all(str_trim) %>%
  mutate_all(str_squish) %>% 
  rowwise() %>% 
  mutate(sw_id = replace(sw_id, sex == "F"&grepl("M", sw_id), gsub("M", "F",sw_id))) %>% 
  ungroup()

pedigree_12142020_siredame <- pedigree_12142020_temp_fix %>% 
  distinct(sire_id, dam_id, sire_sw_id, dam_sw_id) %>%
  rowwise() %>% 
  mutate(sire_id = paste0(sire_id, ",", sire_sw_id),
         dam_id = paste0(dam_id, ",", dam_sw_id)) %>%
  select(-matches("sw_id")) %>% 
  gather("parent_sex", "parent_id") %>% 
  separate(parent_id, into = c("parent_id", "parent_sw_id"), sep = ",") %>% 
  distinct() %>%
  mutate_all(~na_if(., "NA")) %>% 
  subset(!is.na(parent_id)&!is.na(parent_sw_id))

# two different parent_sw_id for parent_id
pedigree_12142020_siredame %>% get_dupes(parent_id) %>% subset(grepl("M", parent_sw_id)&parent_sex=="sire_id"|grepl("F", parent_sw_id)&parent_sex=="dam_id") %>% select(parent_id, parent_sw_id) %>% get_dupes(parent_id) %>% select(-dupe_count) %>% as.data.frame(row.names = NA)
# correct the list of sire x dame 
pedigree_12142020_siredame_sex <- pedigree_12142020_siredame %>% 
  mutate(sw_id_sex = str_extract(parent_sw_id, "[MF]")) %>% 
  mutate(sd_sex = ifelse(parent_sex == "sire_id", "M", "F")) %>%  # sd = sire x dame
  subset(sw_id_sex == sd_sex) %>% 
  distinct(parent_id, sd_sex)

# join to fix the dame and sire 
pedigree_12142020_temp_fix_2 <- pedigree_12142020_temp_fix %>% 
  left_join(pedigree_12142020_siredame_sex, by = c("sire_id" = "parent_id")) %>% 
  left_join(pedigree_12142020_siredame_sex, by = c("dam_id" = "parent_id")) %>%
  rowwise() %>% 
  mutate(sd_sex.y = replace(sd_sex.y, sd_sex.y == "M", dam_id),
         dam_id = replace(dam_id, sd_sex.x == "F", sire_id),
         sire_id = replace(sire_id, sd_sex.x == "F", sd_sex.y)) %>% 
  ungroup() %>% 
  select(-matches('sd_sex[.][xy]')) %>% 
  mutate_all(str_trim)
write.csv(pedigree_12142020_temp_fix_2, "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/pedigree_12152020_temp_n4655_v2.csv", row.names = F)


  
  




### pedigree as of 12/08/2020
pedigree_12082020 <- openxlsx::read.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/Breeding Pedigree up to generation 36 12-8-20.xlsx") %>% 
  mutate_all(as.character) %>%  # n = 4657
  clean_names %>% 
  mutate_all(toupper) %>% 
  mutate_at(vars(matches("(sire|dam)_id"), "id_f51"), ~gsub("(\\d)$", "_\\1", .)) 
# qc male vs female, sires, dames 
# subset animals who had sire listed as a female rat
pedigree_12082020 %>% 
  subset(sire_id %in% c(pedigree_12082020 %>% 
                         subset(id_f51 %in% unique(pedigree_12082020$sire_id) & sex == "F") %>% 
                         select(id_f51) %>% unlist() %>% as.character)) %>% # rows that list a "FEMALE" sire ("F" designated by individual record)
  tibble::rownames_to_column("excel_row") %>% 
  write.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/rows_with_F_sire.xlsx")

pedigree_12082020 %>% 
  subset(id_f51 %in% c(pedigree_12082020 %>% 
                         subset(id_f51 %in% unique(pedigree_12082020$sire_id) & sex == "F") %>% 
                         select(id_f51) %>% unlist() %>% as.character) | 
           sire_id %in% c(pedigree_12082020 %>% 
                            subset(id_f51 %in% unique(pedigree_12082020$sire_id) & sex == "F") %>% 
                            select(id_f51) %>% unlist() %>% as.character))


# subset animals who had dam listed as a male rat
pedigree_12082020 %>% subset(id_f51 %in% unique(pedigree_12082020$dam_id) & sex == "M") %>% select(id_f51) %>% unlist() %>% as.character # listed as a dam but individual record show it is a male  

pedigree_12082020 %>% 
  subset(dam_id %in% c(pedigree_12082020 %>% 
                         subset(id_f51 %in% unique(pedigree_12082020$dam_id) & sex == "M") %>% 
                         select(id_f51) %>% unlist() %>% as.character)) %>% # rows that list a "MALE" dam ("M" designated by individual record)
  tibble::rownames_to_column("excel_row") %>% 
  write.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/rows_with_M_dam.xlsx")

# repeat sire and dam
pedigree_12082020 %>% subset(sire_id == dam_id)

# dupes sw_id 
pedigree_12082020 %>% get_dupes(sw_id)

## are all sires and dames we have are found in id_f51?
# extract the id's that couldn't be joined 
read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/hs_metadata_n1536_20201125.csv") %>% 
  mutate_all(as.character) %>% distinct(project_name, dames, sires) %>% gather("sex", "id", -project_name) %>% left_join(pedigree_12082020, by = c("id" = "id_f51")) %>% subset(is.na(sex.y)) %>% subset(project_name != "p50_hao_chen_2014")

read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/hs_metadata_n1536_20201125.csv") %>% 
  mutate_all(as.character) %>% distinct(project_name, dames, sires) %>% gather("sex", "id", -project_name) %>% subset(project_name == "p50_hao_chen_2014") %>% 
  left_join(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/pedigree.csv") %>% 
              mutate_all(as.character), by = "id") %>% 
  subset(is.na(sex.y))

read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/hs_metadata_n1536_20201125.csv") %>% 
  mutate_all(as.character) %>% distinct(project_name, dames, sires) %>% gather("sex", "id", -project_name) %>% subset(project_name == "p50_hao_chen_2014") %>% 
  left_join(read.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/pedigree.csv") %>% 
              mutate_all(as.character), by = "id") %>% subset(is.na(sex.y)) %>% distinct(id, sex.x) %>% rename("sex" = "sex.x") %>% mutate(sex = replace(sex, sex == "dames", "F"), sex = replace(sex, sex == "sires", "M")) %>% write.csv("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/missing_pedigree_id_n12.csv", row.names = F)

# temporarily fixing 
pedigree_12082020_temp <- pedigree_12082020 %>% 
  distinct() %>% 
  mutate(sire_id = replace(sire_id, id_f51 == "73154_1", "72774_3"),
         sire_id = replace(sire_id, id_f51 == "73192_1", "72905_1"),
         dam_id = replace(dam_id, id_f51 == "73192_1", "72773_4"),
         sire_id = replace(sire_id, id_f51 == "73154_2", "72774_3"),
         sire_id = replace(sire_id, id_f51 == "73192_2", "72905_1"),
         dam_id = replace(dam_id, id_f51 == "73192_2", "72773_4")) %>% 
  mutate_all(str_trim) %>%
  mutate_all(str_squish) 
write.csv(pedigree_12082020_temp, "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/pedigree_12082020_temp_n4655.csv", row.names = F)
  

