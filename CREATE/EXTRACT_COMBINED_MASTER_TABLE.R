## create combined_master_table

## have a table of rfid, project_name, and sex

######################## 
## COMBINED_MASTER_TABLE
######################## 

combined <- list(p50 = shipments_p50_df[, c("p50", "rfid", "sex")],
     u01 = shipments_df[, c("u01", "rfid", "sex")], 
     ...) 

# run when you have the information for pcal, zebrafish, and huda
combined_df <- combined %>% 
  rbindlist(fill = T)