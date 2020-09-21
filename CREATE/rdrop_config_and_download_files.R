install.packages('rdrop2')


library(rdrop2)
library(dplyr)
library(purrr)


drop_auth()

token <- drop_auth()
getwd()
saveRDS(token, file = "token.rds")


#on TSCC
setwd("/home/apurva/Documents/genetic_correlation/")

token <- readRDS("token.rds")


# Then pass the token to each drop_ function

#My example


all<-drop_search(query="pheno_processing_summary",dtoken = token)





#drop_search returns a list of 3 elements
#1st element contains all the matches
#This is the syntax to extract matches all[[1]][[1]]$metadata$path_display

to_download<-data.frame(path_display=character(length=length(all[[1]])),stringsAsFactors = F)
for(i in 1:length(all[[1]])){
  
  to_download$path_display[i]<-all[[1]][[i]]$metadata$path_display
  
}

#remove joel missing kidney frok row 1


to_download<-to_download[grepl("P50 NIDA GWAS",to_download$path_display),"path_display",drop=F]

to_download<-to_download[!grepl("kidney_missing",to_download$path_display),"path_display",drop=F]



to_download$path_display[1]


#create path to residual names
#get expname
to_download$expname<-NA
for(i in 1:nrow(to_download)){
to_download$expname[i] <- basename(gsub("/pheno_processing_summary","",to_download$path_display[i]))  
}

to_download<-to_download[!grepl("physiological",to_download$path_display),]
#remove physiological traits

#remove old_baculum
to_download<-to_download[!grepl("old_baculum",to_download$path_display),]

#construct residuals name
to_download$residuals<-NA


setwd("/projects/ps-palmer/apurva/round8/unpruned/genetic_correlation/residuals")

to_download$residuals<-paste0(to_download$path_display,"/residuals/residuals_",to_download$expname,".RData")


for(i in 1:nrow(to_download)){
  

drop_download(to_download$residuals[i],dtoken = token,overwrite = T)
}

for(i in 6:nrow(to_download)){
  
  
  drop_download(to_download$residuals[i],dtoken = token,overwrite = T)
}




