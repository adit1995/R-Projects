### Installing the libraries required for the analysis###

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggalluvial)
library(lubridate)
library(stringr)

### Reading in the CSV files### 


patient_diagnosis <- read.csv(file.choose())
patient_treatment <- read.csv(file.choose())


### Questions to be Answered ###

### 1). What cancers does the clinic see patients for? ###
### 2). How many patients does the clinic have for each cancer diagnosis?###
  
## Answer: The clinic see's patients primarily for colon and breast cancer. 
## The number of patients with breast cancer is 26 and the number of patients with colon cancer is 12.


cancer_type <- patient_diagnosis %>%
  group_by(Diagnosis) %>%
  summarise(Cancer_Type_Count=n())

### 3). How long after being diagnosed does it take for a patient to start treatment? Do any patients start treatment before being diagnosed?

## Answer: The treatment start date varies from patient to patient with the least amount of time being 2 days and the most amount of time being 70 days. 
## Patient 4374 and 6877 have started their treatments before the diagnosis.
## Average treatment start time for patients taking Drug 201 is 24 days and for patients taking Drug 202 it is 31 days.

### 4). Are there any patients which are diagnosed but not treated at the practice?

## Answer: Yes, there are 10 Patients who are diagnosed but are not treated at practice.These patients are represented by N/A values in the Date Difference column.


Date_diagnosis <- patient_diagnosis %>% select(Patient_ID,Diagnosis_Date)

Diagnosis_of_Patients <- Date_diagnosis %>% ### Used to identify unique diagnosis dates for each patient
                                      ### Patient ID - 3757 has two diagnosis dates for both colon and breast cancer of the same day. 
  group_by(Patient_ID) %>% 
  fill(Diagnosis_Date) %>% 
  distinct()  %>% select(Patient_ID,Treatment_Date)

treatment_date <- patient_treatment %>% ### Used to select the date on which each patient first starts their treatments.
  group_by(Patient_ID) %>%
  filter(row_number()==1) %>% select(Patient_ID,Treatment_Date, Drug_Code)


Date_Difference <- left_join(Diagnosis_of_Patients,treatment_date,by="Patient_ID") ## The join command combines both the tables based on the Patient ID

Date_Difference$Diagnosis_Date <- as.Date(Date_Diff$Diagnosis_Date , format = "%m/%d/%y") ### Used to convert numeric columns to date fields
Date_Difference$Treatment_Date <- as.Date(Date_Diff$Treatment_Date , format = "%m/%d/%y")

Date_Difference$Difference_in_Dates <- Date_Diff$Treatment_Date - Date_Diff$Diagnosis_Date ### Provides the difference between in the treatment date and the diagnosis dates negative values indicate patients who start the treatment before the diagnosis


Average_Diagnosis_Time <- Date_Difference%>% ### Provides the average number of days for the treatment to start from the date of the diagnosis. 
  group_by(Drug_Code)%>% 
  summarise(Avg_Duration = mean(Difference_in_Dates)) %>% drop_na()


### 5).After being treated with a first line of treatment (a drug or combination of drugs), what proportion of all cancer patients go on to be treated with a second line of treatment?

## Assumption : "second line of treatment" refers to patients who have received a treatment more than once i.e. they have multiple treatment dates. 

## Answer : 88.88% of all patients go on to the second line of treatment i.e. 24 out of 27 patients who are receiving treatment. 

first_treatment_date <- patient_treatment %>%
  group_by(Patient_ID) %>%
  filter(row_number()==1) %>% select(Patient_ID,Treatment_Date)


second_treatment_date <- patient_treatment %>%
group_by(Patient_ID) %>%
  filter(row_number()==2) %>% select(Patient_ID,Treatment_Date)

patient_difference_dates <- left_join(first_treatment_date,second_treatment_date,by="Patient_ID")

NROW(patient_difference_dates$Treatment_Date.x) ### Number of patients who receive their First Treatment 

NROW((patient_difference_dates$Treatment_Date.y)) ### Number of patients who receive their Second treatment

NROW (na.omit(patient_diff$Treatment_Date.y))   ### Number of people who go for the second treatment excluding the N/A values which represent patients who have not received their second treatments

Proportion_of_Patients <- (NROW(na.omit(patient_differnce_dates$Treatment_Date.y))/NROW((patient_difference_dates$Treatment_Date.y))) * 100

### 6). How does each drug used at the clinic compare in terms of its duration of therapy?

##Assumption: 

##Answer: Drug Code 201 takes more time than drug code 202 for treatment.
## The maximum duration taken by drug code 201 is 126 and the minimum duration is 28.
## The maximum duration taken by drug code 202 is 21 and the minimum duration is 7.
## Drug code 201 takes about 72 days on average for treatment and drug code 202 takes 10.76 days on average for treatment.
## The average time mentioned above is the time taken once the treatment has started and is not be confused with the average time taken for the treatment to actually start as described for question 03.

patient_treatment$Diagnosis_Date <- as.Date(Date_Diff$Diagnosis_Date , format = "%m/%d/%y")
patient_treatment$Treatment_Date <- as.Date(Date_Diff$Treatment_Date , format = "%m/%d/%y")


treatment_date <- patient_treatment %>%
  group_by(Patient_ID) %>%
  filter(row_number()==1) %>% select(Patient_ID,Treatment_Date)

first_treatment_date <- patient_treatment %>%
  group_by(Patient_ID) %>%
  filter(row_number()==1) 

last_treatment_date <- patient_treatment %>%
  group_by(Patient_ID) %>%
  filter(row_number()==n()) 

duration_time <- cbind(first_treatment_date,last_treatment_date)

duration_final <- duration_time %>% select(Patient_ID...1,Treatment_Date...2,Treatment_Date...5,Drug_Code...6) ### Total diffrenece in duration times for each drug type for each patient


names(duration_final)[1] <- "Patient_ID"
names(duration_final)[2] <- "First_Treatment_Date"
names(duration_final)[3] <- "Laast_Treatment_Date"
names(duration_final)[4] <- "Drug_Code"


duration_final$First_Treatment_Date <- as.Date(duration_final$First_Treatment_Date , format = "%m/%d/%y")
duration_final$Laast_Treatment_Date <- as.Date(duration_final$Laast_Treatment_Date , format = "%m/%d/%y")


duration_final$Difference_in_Dates <- (duration_final$Laast_Treatment_Date - duration_final$First_Treatment_Date) 

duration_comparision <- duration_final %>%  ### Used to compare the difference between the first and last date for each patient based on drug type
  arrange(desc(Difference_in_Dates))

duration_average <- duration_comparision %>%  ### Used to find the average duration for each drug code
  group_by(Drug_Code)%>% 
  summarise(Avg_Duration = mean(Difference_in_Dates))






  