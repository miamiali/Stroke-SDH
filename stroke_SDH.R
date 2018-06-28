<<<<<<< HEAD
# Date: Tuesday, June 26th, 2018
# By: Mia Li
# Description: Stroke&SDH Project
# Version of R Studio used: 1.0.136

#install related packages####
#data cleaning packages
install.packages(c("ggplot2","dplyr","stringr","scales","xlsx","data.table","tidyr","ggpubr","purrr", "gridExtra"))
r1<-c("ggplot2","dplyr","stringr","scales","xlsx","data.table","tidyr","ggpubr","purrr", "gridExtra")

lapply(r1,require,character.only=TRUE)

#read raw dataset into R and prepare subsets for the next steps####
raw<-read.xlsx2("H:/EHR project/Stroke-SDH/FIPS_GWTG_DY.xlsx",sheetIndex = 1, stringsAsFactors = FALSE) #set stringsAsFactors argument to prevent the function automatically convert all columns to factors

#SUBSET 1: individual level record##
ind<-raw[1:691,2:20]
#SUBSET 2: patient unique identifier and matching FIPS##
key<-raw[1:644,22:23]
#SUBSET 3: neiboughorhood characteristics of interest: poverty rate, unemployment rate, uninsured rate, GINI (measure of wealth inequality), crime rate, housing violation rate, and walk score##
SDH<-raw[,c(25:55,63,66,67,70,71)]

#clean subset 1####
#convert number to date and time in ind df##
ind$DOB<-as.Date(as.numeric(as.character(ind$DOB)), origin = "1899-12-30") #convert date 
ind$Arrival<-as.POSIXct(as.numeric(as.character(ind$Arrival))*(60*60*24), origin = "1899-12-30", tz = "GMT") #convert date and time ??
ind$Discharge<-as.POSIXct(as.numeric(as.character(ind$Discharge))*(60*60*24), origin = "1899-12-30", tz = "GMT")

#calculate the age at the visit##
ind$Arrival_date<-as.Date(ind$Arrival)
ind$Discharge_date<-as.Date(ind$Discharge)
ind$age_at_visit<-round((ind$Arrival_date-ind$DOB)/365,digits = 0)

#examine the number of patients who have repeated visits##
ind %>%
  group_by(MRN) %>%
  summarise(n=n()) %>%
  filter(n>1) %>%
  count(n)  # 42 patients had 2 visits, 1 had 3 visits, 1 had 4 visits,  47 repeated visits out of 691 visits

#Join subset 1 and subset 2 and subset 3####
#use inner join to examine the MRNs that appear in both dataset##
ind_overlap<-ind %>%
  inner_join(key, by = c("MRN" = "Primary.MRN")) # according to the returned dataset, every patient has a matching FIPS

#merge ind dataset with SDH dataset##
merged<-ind_overlap %>%
  left_join(SDH, by = c("FIPS.1" = "FIPS.2"))

#Working with merged data - dealing with missing data####
#examine missing data##
sapply(merged, function(x) sum(is.na(x))) #note: the majority of the empty cells are not read into R as NA, but as empty

####NOTE: there's a trade-off of setting stringsAsFactors = FALSE when reading the file into R. 
####                                            Benefit: Convenient When replacing empty cells with NA or 0, as it cannot be done if the data type is factor, because NA and 0 are not defined factor level
####                                            Drawback: Inconvenient When checking the distribution of empty cells using summary, as the summary won't show the level of empty values; Also, factor variables are easier in some analyses
####                                                      However, can use glimpse to check if empty value exists
####                                            Another option: don't use read.xlsx but read.csv to set na.strings=c(""," ",...)
glimpse(merged)  

#check missings of SDH variables##
SDH_missing %>%
  group_by(FIPS.1) %>%
  summarise(n=n())      # 7 FIPS representing 11 records don't have neighborhood characteristics

#create a duplicated dataset to work on##
merged_working<-merged #if want to replace all values in the dataset, can use merged_working[merged_working == ""] <-NA

#write a function to replace empty cells with zeros as some empty cells don't represent missing value##
replace_blank_zero <- function (data, col){
    is_blank <- grepl("^\\s*$", data[, col]) #way to refer to column of a dataframe in function
    data[is_blank, col] <- 0
    data #don't know why I have to print this to be able to show changes????????
}

#write a function to replace empty cells with NAs for the real missing cells##
replace_blank_na <- function (data, col){
  is_blank <- grepl("^\\s*$", data[, col]) 
  data[is_blank, col] <- NA
  data
}

#create a list of variables whose empty values to be replaced by 0, and a list to be replaced by NA
non_missing<-c(5:14)

for (i in 5 : 14){
  merged_working<-replace_blank_zero(merged_working,i)
} 

for (i in 1:4) {
  merged_working<-replace_blank_na(merged_working,i)
}
for (i in 15 : length(merged_working)) {
  merged_working<-replace_blank_na(merged_working,i)
}
#examine missing again
sapply(merged_working, function(x) sum(is.na(x))) # Adm.NIHSS.nmiss=7, DC.mRS.nmiss=1, X90.mRS.nmiss=632

merged_working %>%
  group_by(MRN) %>%
  summarize(missings = sum(is.na(Poverty_Rate))) %>%
  filter(missings>0)

  sapply(function(x) sum(is.na(x)))

#bivariate plotting####
glimpse(merged_working)

#outcome variables: Adm.NIHSS, DC.mRS

#create a list of variables need to be converted into numeric
merged_working[c(17,22,25:58)] <- sapply(merged_working[c(17,22,25:58)], as.numeric) 

#create a new variable combine race and hispanic
merged_working<-merged_working %>%
  mutate(Ethnicity = ifelse(Race == "White" & Hispanic == "0", "White", 
                            ifelse(Race == "African American (Black)" & Hispanic == "0", "Black",
                                   ifelse(Race == "Asian" & Hispanic == "0", "Asian",
                                          ifelse(Hispanic == "1", "Hispanic","Other")))))

#create a histogram for age
merged_working %>%
  summarise("0.30" = quantile(age_at_visit, probs = 0.30),
            "0.60" = quantile(age_at_visit, probs = 0.60),
            "0.90" = quantile(age_at_visit, probs = 0.90),
            min_age = min(age_at_visit),
            max_age = max(age_at_visit),
            mean_age = mean(age_at_visit),
            sd_age = sd(age_at_visit))
#create a new variable of age group
merged_working <- merged_working %>%
  mutate(age_group = ifelse(age_at_visit < 60, "<60", 
                            ifelse(age_at_visit >60 & age_at_visit < 75, "60-74",">=75" )))

#create a function to plot outcome variable - DC.mRS and predictors
plot_factor <- function (df, x, y){
  ggplot(df, aes_string(x=x, y=y))+
    geom_point(position = position_jitter(0.1))
}

#create a list of names of predictor variables
Colnames_merged <- colnames(merged_working)
predictors <- Colnames_merged[c(3, 59, 22, 35, 37, 45, 46, 48, 49, 54:58)]

#keep the df and y unchanged, loop through a list of x values 
DC_mRS_plots <- lapply(predictors, plot_factor, df=merged_working, y="DC.mRS")

Adm_NIHSS_plots <- lapply(predictors, plot_factor, df=merged_working, y="Adm.NIHSS")

do.call(grid.arrange, DC_mRS_plots)
do.call(grid.arrange, Adm_NIHSS_plots)

DC_mRS <- merged_working %>%
      ggplot(aes(x = DC.mRS)) +
      geom_histogram(stat = "count")
Adm_NIHSS <- merged_working %>%
      ggplot(aes(x = Adm.NIHSS)) +
      geom_density(fill = "lightgrey")
g <- c(DC_mRS, Adm_NIHSS)



=======
# Date: Tuesday, June 26th, 2018
# By: Mia Li
# Description: Stroke&SDH Project
# Version of R Studio used: 1.0.136

#install related packages####
#data cleaning packages
install.packages(c("ggplot2","dplyr","stringr","scales","xlsx","data.table","tidyr","ggpubr","purrr"))
r1<-c("ggplot2","dplyr","stringr","scales","xlsx","data.table","tidyr","ggpubr","purrr")

lapply(r1,require,character.only=TRUE)

#read raw dataset into R and prepare subsets for the next steps####
raw<-read.xlsx2("H:/EHR project/Stroke-SDH/FIPS_GWTG_DY.xlsx",sheetIndex = 1, stringsAsFactors = FALSE) #set stringsAsFactors argument to prevent the function automatically convert all columns to factors

#SUBSET 1: individual level record##
ind<-raw[1:691,2:20]
#SUBSET 2: patient unique identifier and matching FIPS##
key<-raw[1:644,22:23]
#SUBSET 3: neiboughorhood characteristics of interest: poverty rate, unemployment rate, uninsured rate, GINI (measure of wealth inequality), crime rate, housing violation rate, and walk score##
SDH<-raw[,c(25:55,63,66,67,70,71)]

#clean subset 1####
#convert number to date and time in ind df##
ind$DOB<-as.Date(as.numeric(as.character(ind$DOB)), origin = "1899-12-30") #convert date 
ind$Arrival<-as.POSIXct(as.numeric(as.character(ind$Arrival))*(60*60*24), origin = "1899-12-30", tz = "GMT") #convert date and time ??
ind$Discharge<-as.POSIXct(as.numeric(as.character(ind$Discharge))*(60*60*24), origin = "1899-12-30", tz = "GMT")

#calculate the age at the visit##
ind$Arrival_date<-as.Date(ind$Arrival)
ind$Discharge_date<-as.Date(ind$Discharge)
ind$age_at_visit<-round((ind$Arrival_date-ind$DOB)/365,digits = 0)

#examine the number of patients who have repeated visits##
ind %>%
  group_by(MRN) %>%
  summarise(n=n()) %>%
  filter(n>1) %>%
  count(n)  # 42 patients had 2 visits, 1 had 3 visits, 1 had 4 visits,  41 repeated visits out of 691 visits

#Join subset 1 and subset 2 and subset 3####
#use inner join to examine the MRNs that appear in both dataset##
ind_overlap<-ind %>%
  inner_join(key, by = c("MRN" = "Primary.MRN")) # according to the returned dataset, every patient has a matching FIPS

#merge ind dataset with SDH dataset##
merged<-ind_overlap %>%
  left_join(SDH, by = c("FIPS.1" = "FIPS.2"))

#Working with merged data - dealing with missing data####
#examine missing data##
sapply(merged, function(x) sum(is.na(x))) #note: the majority of the empty cells are not read into R as NA, but as empty

####NOTE: there's a trade-off of setting stringsAsFactors = FALSE when reading the file into R. 
####                                            Benefit: Convenient When replacing empty cells with NA or 0, as it cannot be done if the data type is factor, because NA and 0 are not defined factor level
####                                            Drawback: Inconvenient When checking the distribution of empty cells using summary, as the summary won't show the level of empty values; Also, factor variables are easier in some analyses
####                                                      However, can use glimpse to check if empty value exists
####                                            Another option: don't use read.xlsx but read.csv to set na.strings=c(""," ",...)
glimpse(merged)  

#check missings of SDH variables##
SDH_missing %>%
  group_by(FIPS.1) %>%
  summarise(n=n())      # 7 FIPS representing 11 records don't have neighborhood characteristics

#create a duplicated dataset to work on##
merged_working<-merged #if want to replace all values in the dataset, can use merged_working[merged_working == ""] <-NA

#write a function to replace empty cells with zeros as some empty cells don't represent missing value##
replace_blank_zero <- function (data, col){
    is_blank <- grepl("^\\s*$", data[, col]) #way to refer to column of a dataframe in function
    data[is_blank, col] <- 0
    data #don't know why I have to print this to be able to show changes????????
}

#write a function to replace empty cells with NAs for the real missing cells##
replace_blank_na <- function (data, col){
  is_blank <- grepl("^\\s*$", data[, col]) 
  data[is_blank, col] <- NA
  data
}

#create a list of variables whose empty values to be replaced by 0, and a list to be replaced by NA
non_missing<-c(5:14)

for (i in 5 : 14){
  merged_working<-replace_blank_zero(merged_working,i)
} 

for (i in 1:4) {
  merged_working<-replace_blank_na(merged_working,i)
}
for (i in 15 : length(merged_working)) {
  merged_working<-replace_blank_na(merged_working,i)
}
#examine missing again
sapply(merged_working, function(x) sum(is.na(x))) # Adm.NIHSS.nmiss=7, DC.mRS.nmiss=1, X90.mRS.nmiss=632

#bivariate plotting####
#outcome variables: Adm.NIHSS, DC.mRS

merged_working %>%
  ggplot(aes(x = DC.mRS, y=as.numeric(Poverty_Rate)))+
  geom_point(position=position_jitter(0.1))

>>>>>>> 0a450f15bdf18c4052d255bbfee94b7fa13e5cf0
