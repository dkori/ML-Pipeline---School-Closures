#Set up data for modeling
rm(list=ls())
library(dplyr)
library(tidyr)
library(mice)
library(tidycensus)

# census_vars<-load_variables(year=2014,dataset="acs5")
# income_vars<-census_vars%>%filter(grepl("income",label))
# property_vars<-census_vars%>%filter(grepl("value",label,ignore.case=TRUE))
# poverty_vars<-census_vars%>%filter(grepl("poverty",concept,ignore.case=TRUE))%>%
#   arrange(name)
#create list of relevant census variables
census_var_list<-c("median_family_income"="B19113_001","median_home_value" = "B25107_001",
                   "poverty_denominator"="B16009_001","poverty_pop"="B16009_002")

census_data<-get_acs(geography="zcta",variables=census_var_list,year=2014,output="wide")%>%
  #create poverty rate
  mutate(poverty_rate=poverty_popE/poverty_denominatorE)%>%
  #extract five-digit zip code from ZCTA
  mutate(MZIP=gsub("ZCTA5 ","",NAME))%>%
  #select only needed estimates
  select(MZIP,median_family_incomeE,median_home_valueE,poverty_rate)

#load school closure data
load("2013-14 school data with closure flags.RData")


#select only variables we chose for model inclusion
to_include<-c("state" = "MSTATE",
              "cd" = "CDCODE",
              "school_type" = "TYPE",
              #"supervisory_union" = "UNION",
              "urban_locale_type" = "ULOCAL",
              #not including BIE schools because there are so few of them
              #bies = bies,
              "reconstituted" = "RECONSTF",
              "teachers" = "FTE",
              #"pre_k" = "PKOFFRD",
              "grade_level" = "LEVEL",
              "title1_status" = "TITLEISTAT",
              "magnet" = "MAGNET",
              "charter" = "CHARTR",
              "shared" = "SHARED",
              "free_lunch_students" = "FRELCH",
              "reduced_lunch_students" = "REDLCH",
              # "pre_k_enrol" = "PK",
              # "kinder_enrol" = "KG",
              # "grade1_enrol" = "G01",
              # "grade2_enrol" = "G02",
              # "grade3_enrol" = "G03",
              # "grade4_enrol" = "G04",
              # "grade5_enrol" = "G05",
              # "grade6_enrol" = "G06",
              # "grade7_enrol" = "G07",
              # "grade8_enrol" = "G08", 
              # "grade9_enrol" = "G09", 
              # "grade10_enrol" = "G10",
              # "grade11_enrol" = "G11",
              # "grade12_enrol" = "G12",
              "total_students" = "MEMBER",
              "ungraded" = "UG",
              "american_indian" = "AM",
              "asian" = "ASIAN",
              "hisp" = "HISP",
              "black" = "BLACK",
              "white" = "WHITE",
              "pacific_islander" = "PACIFIC",
              "multi_racial" = "TR",
              "nslp_status" = "NSLPSTATUS",
              #"closed14_15","closed15_16","closed16_17","closed17_18",
              "closed_any")
#limit just to needed features
needed_features<-with_flags%>%
  #limit to relevant variables
  select("MZIP",to_include)%>%
  #make MZIP character
  mutate(MZIP=as.character(MZIP))%>%
  #merge in census data
  left_join(census_data,by="MZIP")%>%
  #remove zip
  select(-MZIP)%>%
  #exclude PR and territories
  filter(!state%in%c("AS", "AE", "AP", "GU", "MP", "VI","PR"))

#######################Create 2 Datasets #########################################
#there are a few kinds of data missing in the dataset
#M and -1 indicate that a measurement is expected but not available
#N and -2 indicate that a measurement is not applicable (ex. number of reduced lunch students in ineligible schools)

############# 1.) Drop NAs / missing data   
############# 2.) Conditional imputation for missing values
##### 1.) Drop rows with NA values
dropped_nas<-needed_features%>%
  #remove rows where its NAs
  filter(!teachers%in%c(0,-1,-2) &
           !total_students%in%c(0,-1,-2))%>%
  #create percentage variables
  mutate(stud_teacher_ratio = total_students/teachers,
         ln_stud_teacher_ratio=log(total_students/teachers),
         white_perc = white/total_students,
         asian_perc = asian/total_students,
         black_perc = black/total_students,
         amerind_perc = american_indian/total_students,
         hisp_perc = hisp/total_students,
         pacif_perc = pacific_islander/total_students,
         multirac_perc =multi_racial/total_students,
         freelunch_perc = free_lunch_students/total_students,
         reducedlunch_perc = reduced_lunch_students/total_students)%>%
  #replace NAs for census variables by imputing means
  mutate(median_family_incomeE=ifelse(is.na(median_family_incomeE),
                                      mean(median_family_incomeE,na.rm=TRUE),
                                      median_family_incomeE),
         median_home_valueE=ifelse(is.na(median_home_valueE),
                                   mean(median_home_valueE,na.rm=TRUE),
                                   median_home_valueE),
         poverty_rate=ifelse(is.na(poverty_rate),
                             mean(poverty_rate,na.rm=TRUE),
                             poverty_rate))%>%
  # #impute means for census variables missing
  # replace_na(list("median_family_incomeE"=mean(median_family_incomeE,na.rm=TRUE),
  #            "median_home_valueE"=mean(median_home_valueE,na.rm=TRUE),
  #            "poverty_rate"=mean(poverty_rate,na.rm=TRUE)))%>%
  #make categorical variables factors so R doesn't think they're continous
  mutate(state=factor(state),
         school_type=factor(school_type),
         urban_locale_type=factor(urban_locale_type),
         reconstituted=factor(reconstituted),
         grade_level=factor(grade_level))

##### 2.) impute 0s percentages for student/ teacher counts, impute averages student counts
  ## If teacher counts are missing or NA, add missing indicators and impute 0s
  ## If NSLP status is 4, impute 0 for Free lunch and reduced price lunch
#replace (-1) and (-2) with 0 for numeric variables in the dataset
with_imputations<-needed_features%>%
  #create indicator flags for missing teacher and student data
  mutate(missing_teacher=ifelse(teachers%in%c(0,-1),1,0),
         na_teacher=ifelse(teachers==(-2),1,0),
         #since there are few missing students that aren't also missing teacher info, just combine categories
         missing_students=ifelse(total_students%in%c(0,-1,-2),1,0))


#create list of vectors to have 0, -1, and -2 replaced by 1
to_impute<-names(with_imputations)[!names(with_imputations)%in%c("missing_teacher","na_teacher","missing_students","closed_any")]

for(var in to_impute){
  #create a temporary vector of given variable
  temp_vector<-with_imputations[,var]
  #rename temporary vector
  names(temp_vector)<-"temp_var"
  
  #replace (-1) and (-2) with NA 
  temp_vector[temp_vector$temp_var%in%c(0,-1,-2),]<-1
  
  #replace given var with temp_vector in original dataset
  with_imputations[,var]<-temp_vector
}

#final transformations
with_imputations<-with_imputations%>%
  #create percentage variables
  mutate(stud_teacher_ratio = total_students/teachers,
         ln_stud_teacher_ratio=log(total_students/teachers),
         white_perc = white/total_students,
         asian_perc = asian/total_students,
         black_perc = black/total_students,
         amerind_perc = american_indian/total_students,
         hisp_perc = hisp/total_students,
         pacif_perc = pacific_islander/total_students,
         multirac_perc =multi_racial/total_students,
         freelunch_perc = free_lunch_students/total_students,
         reducedlunch_perc = reduced_lunch_students/total_students)%>%
  mutate(state=factor(state),
       school_type=factor(school_type),
       urban_locale_type=factor(urban_locale_type),
       reconstituted=factor(reconstituted),
       grade_level=factor(grade_level))%>%
  #replace NAs in census variables with means
  mutate(median_family_incomeE=ifelse(is.na(median_family_incomeE),
                                      mean(median_family_incomeE,na.rm=TRUE),
                                      median_family_incomeE),
         median_home_valueE=ifelse(is.na(median_home_valueE),
                                   mean(median_home_valueE,na.rm=TRUE),
                                   median_home_valueE),
         poverty_rate=ifelse(is.na(poverty_rate),
                             mean(poverty_rate,na.rm=TRUE),
                             poverty_rate))

# rm(needed_features)
# rm(with_flags)
summary(dropped_nas)
summary(with_imputations)
#add the for_modeling dataset to the Rdata
save(dropped_nas,with_imputations,file="for modeling.RData")
