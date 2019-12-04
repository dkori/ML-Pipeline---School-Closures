#Set up data for modeling
rm(list=ls())
library(dplyr)
library(tidyr)

load("2013-14 school data with closure flags.RData")
#summary(for_modeling$zip)
names(with_flags)
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
  select(to_include)

#######################Create 3 Datasets #########################################
#there are a few kinds of data missing in the dataset
#M and -1 indicate that a measurement is expected but not available
#N and -2 indicate that a measurement is not applicable (ex. number of reduced lunch students in ineligible schools)

############# 1.) Drop NAs / missing data   
############# 2.) 0s imputed for missing student counts, dataset average for
#############     student/teacher ratio
############# 3.) Conditional imputation for missing values

##### 1.) Drop rows with NA values
dropped_nas<-needed_features%>%
  #remove rows where its NAs
  filter(!teachers%in%c(0,-1,-2) &
           !total_students%in%c(0,-1,-2))%>%
  #create percentage variables
  mutate(stud_teacher_ratio = total_students/teachers,
         white_perc = white/total_students,
         asian_perc = asian/total_students,
         black_perc = black/total_students,
         amerind_perc = american_indian/total_students,
         hisp_perc = hisp/total_students,
         pacif_perc = pacific_islander/total_students,
         multirac_perc =multi_racial/total_students,
         freelunch_perc = free_lunch_students/total_students,
         reducedlunch_perc = reduced_lunch_students/total_students)

##### 2.) impute 0s percentages for student/ teacher counts, impute averages student counts

#replace (-1) and (-2) with 0 for numeric variables in the dataset
for(var in names(needed_features)){
  #create a temporary vector of given variable
  temp_vector<-needed_features[,var]
  #rename temporary vector
  names(temp_vector)<-"temp_var"
  
  #replace (-1) and (-2) with NA 
  temp_vector[temp_vector$temp_var%in%c(-1,-2),]<-0
  
  #replace given var with temp_vector in original dataset
  needed_features[,var]<-temp_vector
}

#make categorical variables factors so R doesn't think they're continuous
for_modeling<-needed_features%>%
  mutate(state=factor(state),
       school_type=factor(school_type),
       urban_locale_type=factor(urban_locale_type),
       reconstituted=factor(reconstituted),
       grade_level=factor(grade_level))


# rm(needed_features)
# rm(with_flags)

#add the for_modeling dataset to the Rdata
save(for_modeling,file="for modeling.RData")
