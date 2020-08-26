# ---
#   title: "2020 US Ozone Nonattainment Analysis"
# authors: "map579 & Microscone"
# date: "7/29/2020"
# output: html_document
# ---
  ## Purpose
  ##This analysis is to estimate the current-to-date 4th max ozone values and 2018-2020 design values in the US.
  ##Daily max 8-hour ozone data was retrieved from the EPA AQS API. 
  ##This file exports a data frame for use with the shiny app.


library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(gmodels)
library(knitr)
library(kableExtra)
library(data.table)
library(readxl)
library(RCurl)

WD <- getwd()

#Enter the year of data you want to analyze
Year_to_analyze <- "2020"

#converts the year to a date at the start of the ozone season
ozone_start_date <- as.Date(paste0(Year_to_analyze,"-05-01"))

#gets the date of "today", which is actually for "yesterday" to ensure that the analysis is current UP TO the current day.
Date_Today <- Sys.Date()-1

#converts the date of "today" to a character value without dashes
Today_characters <- paste0(substr(Date_Today,6,7),substr(Date_Today,9,10))

#creates a list of dates between the start of the ozone season and "today"
#removes the dashes from the dates and converts them all to characters
day_list <- str_remove_all(as.character(seq(ozone_start_date,Date_Today,1)),"-")

#main url for airnow daily files
airnowtech_files <- "https://s3-us-west-1.amazonaws.com//files.airnowtech.org/airnow/"

#name used for daily data files from airnow, which list the max concentrations for pollutants at each monitor each day
fileName <- "daily_data_v2.dat"

#creates a default folder on the computer
airnow_folder <- "c:/airnow/"

#creates a subfolder
ifelse(!dir.exists(file.path(airnow_folder,Year_to_analyze)), dir.create(file.path(airnow_folder,Year_to_analyze)), FALSE)

#creates a variable for the path of the subfolder
YOC_folder <- paste0(airnow_folder,Year_to_analyze,sep = "/")

#new subfolder name
dat_folder_name <- "Daily_Data_Files"

#creates another subfolder
ifelse(!dir.exists(file.path(YOC_folder,dat_folder_name)), dir.create(file.path(YOC_folder,dat_folder_name)), FALSE)

#another variable for the path of the final folder
dat_folder <- paste0(YOC_folder,dat_folder_name,sep = "/")

system.time(
  #downloads the daily dat files for the selected year for the ozone season and places them in the download folder created.
  for (i in day_list){
    destination_file <- paste0(dat_folder,i,fileName)
    #only downloads new files which have not already been downloaded
    if(!file.exists(destination_file)){
      temp_url <- paste0(airnowtech_files,Year_to_analyze,"/",i,"/",fileName)
      download.file(temp_url,destination_file)
    }
  })


#list of dat files in folder
file_list <- list.files(dat_folder)

#empty data frame
dataset <- data.frame()

system.time(
  #read and bind all dat files into the empty data frame
  for (i in 1:length(file_list)){
    setwd(dat_folder)
    temp_data <- fread(file_list[i], sep = "|", header = F,  stringsAsFactors = F) 
    dataset <- rbindlist(list(dataset, temp_data), use.names = T) 
  })

#----------
#ALTHOUGH NOT USED, THIS SECTION READS ALL DAT FILES DIRECTLY INTO A DATA FRAME, AS OPPOSED TO DOWNLOADING THEM 

# #empty data frame
# df_total = data.frame()
# 
# system.time(
# #loop to read the dat files for the selected days and copy into the empty data frame
# for (i in day_list){
#   temp_url <- paste0(airnowtech_files,Year_to_analyze,"/",i,"/",fileName)
#   data <- read.delim(temp_url, header = FALSE, sep="|", as.is=TRUE)
#   df_total <- rbind(df_total,data)
# })

#----------

#list of names for the header of the data frame
headers <- c("Date","Monitor_ID","SiteName","Param_Name","Units","Value","Averaging_Period","Data_Source",
             "AQI_Value","AQI_Category","Latitude","Longitude","AQSID")

#renaming header of data frame
colnames(dataset) <- headers

#copying data frame to a new working data frame
AQ2020 <- dataset

# #converting Date field from a character to a date.
# AQ2020$POSIX.Date <- as.POSIXct(paste0(AQ2020$Date,"20"), format = '%m/%d/%Y')

AQ2020$CountryCode <- substring(AQ2020$AQSID,1,3)

countryCodeList <- unique(AQ2020$CountryCode)

US_Country_Codes <- c('840','021','001','093','113')

#filtering the data frame for monitors in the US and their 8hr daily max values, and renaming field for 8hr ozone average max values
US_daily_max <- AQ2020 %>% 
  filter(CountryCode %in% US_Country_Codes) %>%
  filter(Param_Name == "OZONE-8HR") %>%
  rename("Avg_8hr" = Value)

#filters records for days when monitors exceed 70ppb
num_exceedences <- US_daily_max %>% 
  filter(Avg_8hr > 70)

#calculates how many values are included for each monitor in the US_4thMax df
exceedences_by_mon <- plyr::ddply(US_daily_max,~Monitor_ID,summarise,'days_>70ppb'=sum(Avg_8hr > 70))

#intermediary data frame
US_mon_coords <- US_daily_max %>%
  select(Monitor_ID,SiteName,Longitude,Latitude) %>%
  group_by(Monitor_ID) %>% 
  slice(1) %>% 
  left_join(.,exceedences_by_mon, by = "Monitor_ID", keep = F)


#selecting the 4 highest 8hr ozone max values for each monitor in the Philly NAA
US_4_Highest <- AQ2020 %>% 
  filter(CountryCode %in% US_Country_Codes) %>%
  filter(Param_Name == "OZONE-8HR") %>%
  rename("Avg_8hr_4thMax" = Value) %>%
  group_by(Monitor_ID) %>% 
  arrange(desc(AQI_Value)) %>% 
  slice(1:4)

#calculates how many values are included for each monitor in the US_4thMax df
num_monitors <- plyr::ddply(US_4_Highest,~Monitor_ID,summarise,num_days=length(unique(Date)))

#empty vector
num_vec <- c()

#loop to create a list of values (1 through 4) to represent the n-highest values by monitor
#the values create this list based upon the monitors and numbers in num_monitors
for (i in 1:nrow(num_monitors)){
  z <- seq(1,num_monitors[i,2],1)
  end <- length(num_vec)+1
  num_vec <- append(num_vec,z,end)
}

#the num_vec of values representing the n-highest values are added to the df
US_4_Highest$n_highest <- num_vec

#a "wide" pivot table is created which includes columsn for the 4 highest ozone values
US_Pivot <- US_4_Highest %>%
  select(Monitor_ID,SiteName,Avg_8hr_4thMax,n_highest) %>%
  pivot_wider(names_from = n_highest,values_from = Avg_8hr_4thMax)

#renaming columns of pivot table
colnames(US_Pivot) <- c("Monitor_ID","SiteName","2020_Max","2020_2nd_High","2020_3rd_High","2020_4th_High")

#alternate df of US_Pivot to include coordinates
US_Pivot_Coords <- US_Pivot %>%
  left_join(.,US_mon_coords, by = c("Monitor_ID","SiteName"), keep = F)


#url of official ozone design values for 2019
ozoneDV2019_file <- "https://www.epa.gov/sites/production/files/2020-05/o3_designvalues_2017_2019_final_05_26_20.xlsx"

#temporary location for downloading file
temp_excel <- tempfile(fileext = ".xlsx")

#downloads the excel file to the temporary location
download.file(ozoneDV2019_file,destfile = temp_excel, mode = 'wb')

#reads the temporary excel file into a data frame
ozoneDV2019 <- read_excel(temp_excel, "Table5. Site Status", skip = 3, col_names = T)

#shortened list of header names
headersDVfile <- c("State","County","CBSA","CSA","NAA_Name","EPA_Region","Monitor_ID","SiteName","SiteAddress",
                   "Latitude","Longitude","Valid_17_19_DV","Invalid_17_19_DV","Avg_17_19_Completeness","Completeness_2017",
                   "Completeness_2018","Completeness_2019","2017_4thMax","2018_4thMax","2019_4thMax","2017_Exceedance_Days",
                   "2018_Exceedance_Days","2019_Exceedance_Days")

#applying header names to data frame
colnames(ozoneDV2019) <- headersDVfile

#duplicates data frame from the original
ozondDV2019df <- ozoneDV2019

#converts the ppm values to ppb values
ozondDV2019df$`2018_4thMax` <- ozondDV2019df$`2018_4thMax`*1000
ozondDV2019df$`2019_4thMax` <- ozondDV2019df$`2019_4thMax`*1000

#removes the comma and state abbreviations in the NAA names in order to match those in the NAA polygon file, below.
ozondDV2019df$NAA_Name <- substr(ozondDV2019df$NAA_Name,1,regexpr(",",ozondDV2019df$NAA_Name)-1)

#the names of these two NAAs are adjusted to match the names in the NAA polygon file, below.
ozondDV2019df[ozondDV2019df == 'Dona Ana County (Sunland Park)'] <- 'Dona Ana County (Sunland Park Area)'
ozondDV2019df[ozondDV2019df == 'Pechanga Band of Luiseno Mission Indians'] <- 'Pechanga Band of Luiseno Mission Indians of the Pechanga Reservation'

#filters the 2019 DV data, joins the 2020 4th Max value to the table
US_2020DV <- ozondDV2019df %>%
  select(State,County,Monitor_ID,NAA_Name,Latitude,Longitude,`2018_4thMax`,`2019_4thMax`) %>%
  left_join(.,US_Pivot, by = "Monitor_ID", keep = F)  %>%
  left_join(.,exceedences_by_mon, by = "Monitor_ID", keep = F)

#calculates the draft 2018-2020 design value for ozone for the monitors
US_2020DV$Draft_DV_18_20 <- apply(US_2020DV[,c(7,8,13)], 1, function(x) trunc(mean(x)))

#removes records with no draft DV for 2020
US_2020DV <- US_2020DV %>%
  filter(!is.na(Draft_DV_18_20))

#bins the draft design value data, above and below the 0.070ppm ozone NAAQS
US_2020DV$O3_NAAQS_Attainment <- cut(US_2020DV$Draft_DV_18_20,c(0,60,65,70,75,80,200), include.lowest = T,labels = c('60 or below','61-65','66-70','71-75','76-80','Above 80'))

#bins the draft design value data, above and below the 0.070ppm ozone NAAQS
US_2020DV$O3_2020_4thMax <- cut(US_2020DV$`2020_4th_High`,c(0,60,65,70,75,80,150), include.lowest = T,labels = c('60 or below','61-65','66-70','71-75','76-80','Above 80'))

setwd(WD)
save(US_2020DV, file = "US_2020DV.RData")

