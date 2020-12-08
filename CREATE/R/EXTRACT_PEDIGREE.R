## create and qc pedigree

### pedigree as of 12/08/2020
pedigree_12082020 <- openxlsx::read.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/Breeding Pedigree up to generation 36 12-8-20.xlsx") %>% 
  mutate_all(as.character) %>%  # n = 4657
  clean_names %>% 
  mutate_all(toupper) %>% 
  mutate_at(vars(matches("(sire|dam)_id"), "id_f51"), ~gsub("(\\d)$", "_\\1", .)) 
# qc male vs female, sires, dames 
# subset animals who had sire listed as a female rat
pedigree_12082020 %>% 
  subset(id_f51 %in% c(pedigree_12082020 %>% 
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
  subset(id_f51 %in% c(pedigree_12082020 %>% 
                         subset(id_f51 %in% unique(pedigree_12082020$dam_id) & sex == "M") %>% 
                         select(id_f51) %>% unlist() %>% as.character)) %>% # rows that list a "MALE" dam ("M" designated by individual record)
  tibble::rownames_to_column("excel_row") %>% 
  write.xlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/rows_with_M_dam.xlsx")
