## CREATE PROJECT METADATA TABLE

project_metadata <- data.frame(project_name = unique(sample_metadata$project_name)) %>% 
  mutate(project_title = NA,
         project_description = NA,
         phenotypic_data = NA,
         omics = NA,
         comments = NA) %>% 
  mutate(phenotypic_data = replace(phenotypic_data, project_name != "pcal_brian_trainor","yes"))
