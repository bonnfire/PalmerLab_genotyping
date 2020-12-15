## create and qc pedigree
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
  

