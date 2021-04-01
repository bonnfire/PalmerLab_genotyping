# move all csv files generation here 

## dna extraction 
write.csv(khai_tissueextraction_1_52_df, file = "~/Desktop/Database/csv files/sample_tracking/extraction_log_1_52.csv", row.names = F)
write.csv(khai_tissueextraction_53_96_df_db, file = "~/Desktop/Database/csv files/sample_tracking/extraction_log_53_96.csv", row.names = F)
write.csv(khai_tissueextraction_53_101_df_db, file = "~/Desktop/Database/csv files/sample_tracking/extraction_log_53_101.csv", row.names = F)
write.csv(khai_tissueextraction_53_101_df_db_temp, file = "~/Desktop/Database/csv files/sample_tracking/extraction_log_53_101_temp.csv", row.names = F)

## sample barcode lib (entered into database, once the sequencing data are returned)
write.csv(kn05_db, file = "~/Desktop/Database/csv files/sample_tracking/kn05_sample_barcode_lib.csv", row.names = F)


## pedigree 
write.csv(pedigree_01062021_fix, "~/Desktop/Database/csv files/sample_tracking/pedigree_temp.csv", row.names = F)  
