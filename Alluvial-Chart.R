library(ggplot2)
library(dplyr)
library(tidyr)
library(ggalluvial)
library(date)

install.packages("ggplot2")
install.packages("dplyr")
install.packages("date")
install.packages("ggalluvial")


wage <- read.csv(file.choose())

Wages_flight_attendants<- wage %>%
  group_by(Gender,Year) %>%
  summarise(count=sum(Total.Population,na.rm=TRUE),PUMS.Occupation="Flight Attendants")

Wages_mechanics<- wage %>%
  group_by(Gender,Year) %>%
  summarise(count=sum(Total.Population,na.rm=TRUE),PUMS.Occupation="Aircraft mechanics & service technicians")


Wages_pilots<- wage %>%
  group_by(Gender,Year) %>%
  summarise(count=sum(Total.Population,na.rm=TRUE),PUMS.Occupation="Aircraft pilots & flight engineers")

Wages_customerservice<- wage %>%
  group_by(Gender,Year) %>%
  summarise(count=sum(Total.Population,na.rm=TRUE),PUMS.Occupation="Customer service representatives")

Wages_agents<- wage %>%
  group_by(Gender,Year) %>%
  summarise(count=sum(Total.Population,na.rm=TRUE),PUMS.Occupation="Reservation & transportation ticket agents & travel clerks")



wages_total <- rbind(Wages_flight_attendants,Wages_mechanics,Wages_pilots,Wages_customerservice,Wages_agents)



ggplot(wages_total,
       aes(y = count, axis1 = PUMS.Occupation, axis2 = Year)) +
  geom_alluvium(aes(fill = Gender), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  ggtitle("Alluvial Plot")


workers <- read.csv(file.choose())



workers1<- workers %>%
  group_by(Major.Occupation.Group,Detailed.Occupation) %>%
  summarise(count=sum(Total.Population,na.rm=TRUE))

install.packages("treemap")
library(treemap)

treemap(workers1,index = c("Detailed.Occupation"),vSize =c("count"),vColor = "Major.Occupation.Group",type="categorical",title = "jobs")


