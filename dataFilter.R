library(dplyr)
library(readr)
library(dplyr)


columnas <- c ("id", 
               "scientificName" ,
               "taxonRank",
               "kingdom",
               "family",
               "higherClassification",
               "vernacularName",                    
               "individualCount",
               "lifeStage",                                               
               "longitudeDecimal",
               "latitudeDecimal",                              
               "country",             
               "stateProvince",                
               "locality",
               "habitat",
               "eventDate")


col.names <- colnames(read_csv(paste0("occurence/", list.files("occurence")[1]), 
                             n_max = 1))

df <- c()
for( i in 1:length(list.files("occurence"))){
  
  file <- read_csv(paste0("occurence/", list.files("occurence")[i]), 
                   #n_max = 10,
                   col_names = FALSE, 
                   skip = 1)
  
  names(file) <- col.names
  
  df1 <- file %>%
     select(one_of(columnas)) %>%
     filter(country == "Poland") 
  
  df <- rbind(df,df1)
  print(paste("!!!!!!!!!!!!!!!!!!!!DATA!!!!!!!!!!!!!!!!!!",i))
}



write.csv(df, "occurence_poland.csv")

# multimedia poland ----


df_poland <- read_csv("occurence_poland.csv")

ids <- df_poland$id

col.names <- colnames(read_csv(paste0("multimedia/", list.files("multimedia")[1]), 
                               n_max = 1))

cols <- c("CoreId", "accessURI")

df <- c()
for( i in 1:length(list.files("multimedia"))){
  
  file <- read_csv(paste0("multimedia/", list.files("multimedia")[i]), 
                   #n_max = 10,
                   col_names = FALSE, 
                   skip = 1)
  
  names(file) <- col.names
  
  df1 <- file %>%
    select(one_of(cols)) %>%
    filter(CoreId %in% ids)
  
  df <- rbind(df,df1)
  print(paste("!!!!!!!!!!!!!!!!!!!!DATA!!!!!!!!!!!!!!!!!!",i))
}



write.csv(df, "mutimedia_poland.csv")






