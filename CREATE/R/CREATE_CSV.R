# move all csv files generation here 

## dna extraction 
write.csv(khai_tissueextraction_1_52_df, file = "~/Desktop/Database/csv files/sample_tracking/extraction_log_1_52.csv", row.names = F)
write.csv(khai_tissueextraction_53_96_df_db, file = "~/Desktop/Database/csv files/sample_tracking/extraction_log_53_96.csv", row.names = F)
write.csv(khai_tissueextraction_53_101_df_db, file = "~/Desktop/Database/csv files/sample_tracking/extraction_log_53_101.csv", row.names = F)
# upload Hao's data
write.csv(khai_tissueextraction_53_101_df_db_upload, file = "~/Desktop/Database/csv files/sample_tracking/extraction_log_53_101_hao_n134.csv", row.names = F)

## sample barcode lib (entered into database, once the sequencing data are returned)
write.csv(kn05_db, file = "~/Desktop/Database/csv files/sample_tracking/kn05_sample_barcode_lib.csv", row.names = F)
write.csv(kn06_db, file = "~/Desktop/Database/csv files/sample_tracking/kn06_sample_barcode_lib.csv", row.names = F)
write.csv(kn07_db, file = "~/Desktop/Database/csv files/sample_tracking/kn07_sample_barcode_lib.csv", row.names = F)


# sample metadata joins to sample barcode library for genotyping team
write.csv(kn07_db_metadata, file = "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/sample_tracking/generated/kn07_fastq_sample_metadata_n931.csv", row.names = F)
write.csv(kn06_db_metadata, file = "~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/sample_tracking/generated/kn06_fastq_sample_metadata_n953.csv", row.names = F)


## pedigree 
write.csv(pedigree_01062021_fix, "~/Desktop/Database/csv files/sample_tracking/pedigree_temp.csv", row.names = F)  
