library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(httr)
library(tidycensus)
#create a function that takes the url of a zip file as an argument and reads in the first file
download_and_unzip<-function(url){
  #create temp file to store zip
  zip_file<-tempfile()
  #download zip file into temp file
  download.file(url,zip_file)
  #find the name of the first file in the zip
  file_list<-unzip(zip_file,list=TRUE)
  #limit to just text or csv files, and just the first one
  file_name<-file_list[grepl("(*.txt)|(*.csv)",file_list$Name),][1,]$Name
  #read in the text file using the appropriate function based on whether its txt or csv
  if(grepl(".csv",file_name)){
    read_csv(unzip(zip_file,files=file_name))
  }else{
    read_tsv(unzip(zip_file,files=file_name))
  }
}

#declare the url where the 2013 school data is located
url<-"https://nces.ed.gov/ccd/Data/zip/sc132a_txt.zip"
#create a temporary file where the zip will go
zip_file<-tempfile()
#retrieve the zip file containing the data
download.file(url,zip_file)

#unzip the zip file, list files
files<-unzip(zip_file,list=TRUE)
#extract the 2013-2014 data from zip file
dat13_14<-read_tsv(unzip(zip_file,files="sc132a.txt"))

############## Grab School Data from each successive year to determine which schools dropped ##########
############### The data we need is located in the "directory" file for each year, 
################### 2014 - 2015 #####################################
closed14_15<-download_and_unzip("https://nces.ed.gov/ccd/Data/zip/ccd_sch_029_1415_w_0216601a_txt.zip")%>%
  #filter for schools with status 2 ("closed since last report") or 6 ("close but may reopen)
  filter(UPDATED_STATUS%in%c(2,6))%>%
  #select the school number
  select(NCESSCH)
################### 2015 - 2016 #####################################
closed15_16<-download_and_unzip("https://nces.ed.gov/ccd/Data/zip/ccd_sch_029_1516_w_2a_011717_csv.zip")%>%
  #filter for schools with status 2 ("closed since last report") or 6 ("close but may reopen)
  filter(UPDATED_STATUS%in%c(2,6))%>%
  #select the school number
  select(NCESSCH)
################### 2016 - 2017 #####################################

closed16_17<-download_and_unzip("https://nces.ed.gov/ccd/Data/zip/ccd_sch_029_1617_w_1a_11212017.zip")%>%
  #filter for schools with status 2 ("closed since last report") or 6 ("close but may reopen)
  filter(UPDATED_STATUS%in%c(2,6))%>%
  #select the school number
  select(NCESSCH)
################### 2017 - 2018 #####################################

closed17_18<-download_and_unzip("https://nces.ed.gov/ccd/Data/zip/ccd_sch_029_1718_w_1a_083118.zip")%>%
  #filter for schools with status 2 ("closed since last report") or 6 ("close but may reopen)
  filter(UPDATED_STATUS%in%c(2,6))%>%
  #select the school number
  select(NCESSCH)

#add closure flags to dat13_14
with_flags<-dat13_14%>%
  #for each year, check if the school's unique identifier is in the list of closed schools for that year
  mutate(closed14_15=ifelse(NCESSCH %in% closed14_15$NCESSCH,1,0),
         closed15_16=ifelse(NCESSCH %in% closed15_16$NCESSCH,1,0),
         closed16_17=ifelse(NCESSCH %in% closed16_17$NCESSCH,1,0),
         closed17_18=ifelse(NCESSCH %in% closed17_18$NCESSCH,1,0),
         closed_any=ifelse(closed14_15+closed15_16+closed16_17+closed17_18>0,1,0))
#save with flags as rdata
save(with_flags,file="2013-14 school data with closure flags.RData")

#check out flag counts
# sum(with_flags$closed14_15)
# sum(with_flags$closed15_16)
# sum(with_flags$closed16_17)
# sum(with_flags$closed17_18)
# sum(with_flags$closed_any)/nrow(with_flags)
#bla bla bla
