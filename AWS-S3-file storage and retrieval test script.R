#dependencies from 2020 US Ozone file
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(gmodels)
library(knitr)
library(kableExtra)
library(leaflet)
library(geojsonio)
library(data.table)
library(htmlwidgets)
library(htmltools)
library(readxl)
library(aws.s3)

Sys.setenv("AWS_ACCESS_KEY_ID" = "AWS_ACCESS_KEY_ID", 
             "AWS_SECRET_ACCESS_KEY" = "AWS_SECRET_ACCESS_KEY",
             "AWS_DEFAULT_REGION" = "us-east-2")
  
S3_Bucket_name <- "shiny-practice"
bucket_contents <- get_bucket(S3_Bucket_name)

#Enter the year of data you want to analyze
Year_to_analyze <- "2020"
#converts the year to a date at the start of the ozone season
ozone_start_date <- as.Date(paste0(Year_to_analyze,"-05-01"))
Date_Today <- Sys.Date()-1

#create a list of all days to include for analysis
day_list <- str_remove_all(as.character(seq(ozone_start_date,Date_Today,1)),"-")

bucket_names <- vector()
for (i in 1:length(bucket_contents)){
  temp_data <- substr(bucket_contents[[i]][[1]], 1, 8)
  bucket_names <- c(bucket_names, temp_data)
  bucket_names 
}
dates_missing <- anti_join(as.data.frame(day_list), as.data.frame(bucket_names), by = c("day_list" = "bucket_names"))

## To download needed files:
#main url for airnow daily files
airnowtech_files <- "https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/"
fileName <- "daily_data_v2.dat"

#actual download:
for (i in 1:nrow(dates_missing)){
  #only downloads new files which have not already been downloaded
  tmp <- tempfile(pattern = as.character(i))
  on.exit(unlink(tmp))
  temp_url <- paste0(airnowtech_files,Year_to_analyze,"/",dates_missing[i, 1],"/",fileName)
    temp_file <- fread(temp_url, sep = "|", header = F,  stringsAsFactors = F)
    #place file into S3 bucket
    write_delim(temp_file, path = tmp, delim = "|")
    put_object(tmp, object = paste0(dates_missing[i, 1], fileName), bucket = S3_Bucket_name)
  }

bucket_contents <- get_bucket(S3_Bucket_name)

#These lines are successful
#creates list of all file names in bucket, then reads all files into a dataframe

#These lines of code extract all data from the bucket and join into a dataframe
  #list all file names in bucket
  bucket_names <- vector()
  for (i in 1:length(bucket_contents)){
    temp_data <- bucket_contents[[i]][[1]]
    bucket_names <- c(bucket_names, temp_data)
    bucket_names
  }

  #read all files in S3 bucket
  dataset <- data.frame()
  for (i in 1:length(bucket_names)){
    temp_data <- rawToChar(get_object(bucket_names[i], bucket = S3_Bucket_name))
    dataset <- rbind(dataset, fread(temp_data, sep = "|", header = T,  stringsAsFactors = F), use.names = T)
  }

dataset <- dataset %>% 
  filter(V1 != TRUE)

