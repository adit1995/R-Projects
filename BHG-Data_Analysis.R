##Installations of the required Libraries


library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(lubridate)
library(ggplot2)
library(maps)
library(plotly)
library(lubridate)

## Loading the file onto R-Studio

Sales_pullthrough1 <- read.csv(file.choose())

summary(Sales_pullthrough)

##Question 01

## total lead count by lead type

lead_type_count<- Sales_pullthrough %>%
  group_by(LeadType) %>%
  summarise(Lead_Type_Count=n())

## Prequalified Rate

prequalified_rate <- Sales_pullthrough %>%
  filter(Prequalified == "1") %>%
  group_by(LeadType) %>% 
  summarise(loan_counts=sum(Prequalified, na.rm=TRUE)) 

prequalified_rate['Rate']<- (prequalified_rate$loan_counts/nrow(Sales_pullthrough))*100

## Submitted Rate

submitted_rate <- Sales_pullthrough %>%
  filter(Submitted == "1") %>%
  group_by(LeadType) %>% 
  summarise(Loan_Counts=sum(Submitted, na.rm=TRUE)) 

submitted_rate['Rate']<- (submitted_rate$Loan_Counts/nrow(Sales_pullthrough))*100

## Approval Rate

Approval_Rate <- Sales_pullthrough %>%
  filter(Approved == "1") %>%
  group_by(LeadType) %>% 
  summarise(Loan_Counts=sum(Approved, na.rm=TRUE)) 

Approval_Rate['Rate']<- (Approval_Rate$Loan_Counts/nrow(Sales_pullthrough))*100

## DocsIn Rate

DocsIn_Rate <- Sales_pullthrough %>%
  filter(DocsIn == "1") %>%  
  group_by(LeadType) %>% 
  summarise(Loan_Counts=sum(DocsIn, na.rm=TRUE)) 

DocsIn_Rate['Rate']<- (DocsIn_Rate$Loan_Counts/nrow(Sales_pullthrough)) * 100   

## Pull Through 


Pull_Through_Rate <- Sales_pullthrough %>%
  filter(Funded == "1") %>%
  group_by(LeadType) %>% 
  summarise(Loan_Counts=sum(Funded, na.rm=TRUE)) 

Pull_Through_Rate['Rate']<- (Pull_Through_Rate$Loan_Counts/nrow(Sales_pullthrough))*100    

## Average Loan Financed Amount

Avg_fin_amt <- Sales_pullthrough %>%
  filter(LoanFinancedAmount != "0") %>%
  group_by(LeadType) %>%
  summarise(Average_Financed_Amount=mean(LoanFinancedAmount, na.rm=TRUE)) 


## Average Margin per loan

Avg_margin_amt <- Sales_pullthrough %>%
  filter(GrossMargin != "0") %>%
  group_by(LeadType) %>%
  summarise(Average_Margin_Amount = mean(abs(GrossMargin), na.rm=TRUE)) 


## Gross Margin Rate

gross_margin_rate <- Sales_pullthrough %>%
  filter(GrossMargin != "0") %>%
  group_by(LeadType) %>% 
  summarise(Margin_totals=sum(abs(GrossMargin), na.rm=TRUE)) 

gross_margin_rate['Rate']<- (gross_margin_rate$Margin_totals/sum(gross_margin_rate$Margin_totals))*100    


## Average Margin per lead based on lead type

Avg_lead_margin_amt <- Sales_pullthrough %>%
  filter(GrossMargin != "0") %>%
  group_by(LeadType) %>%
  summarise(Average_lead_Margin_Amount = mean(abs(GrossMargin), na.rm=TRUE)) %>%
  arrange((Average_lead_Margin_Amount)) %>% top_n(-2)


## Average Margin by specific specialities


Avg_lead_margin_amt_specialities <- Sales_pullthrough %>%
  filter(GrossMargin != "0") %>%
  filter(LeadType != "INBOUND") %>%
  group_by(Specialty,LeadType) %>%
  summarise(Average_lead_Margin_Specialty_Amount = mean(abs(GrossMargin), na.rm=TRUE)) %>%
  arrange((Average_lead_Margin_Amount)) 


plot <- Avg_lead_margin_amt_specialities %>% ggplot(aes(fill=LeadType, y= Average_lead_Margin_Amount, x=Specialty)) + 
  geom_bar(position="dodge", stat="identity") +scale_color_manual(values=c(rep("white", 17)))+theme(legend.position="none")
ggplotly(plot)

## The average lead margin for each different lead type is almost exactly the same when compared with the specialty type
## Primary specialties had the highest average margin with No Action leads actually having a higher margin than warm leads 
## Secondary specialties had the lowest average margin.Profunding was the only specialty type where warm leads outperformed no action leads.


## Average Margin by Marketing


Avg_lead_margin_amt_marketing <- Sales_pullthrough %>%
  filter(GrossMargin != "0") %>%
  filter(LeadType != "INBOUND") %>%
  group_by(MarketingMethod,LeadType) %>%
  summarise(Average_lead_Margin_Marketing_Amount = mean(abs(GrossMargin), na.rm=TRUE)) %>%
  arrange(desc(MarketingMethod)) 


plot_marketing <- Avg_lead_margin_amt_marketing %>% ggplot(aes(fill=LeadType, y= Average_lead_Margin_Marketing_Amount, x=MarketingMethod)) + 
  geom_bar(position="dodge", stat="identity") +scale_color_manual(values=c(rep("white", 17)))+theme(legend.position="none")
ggplotly(plot_marketing)

## PPC marketing for no action leads clearly has the highest margin with social and mail marketing techniques having 
## similar margins. However, mail marketing slightly outperformed social media marketing. Digital ads interestingly did not 
## feature any no action leads.No action leads had a higher margin across all the marketing channels.

##Which marketing method yields the highest pullthrough?


Pull_Through_marketing <- Sales_pullthrough %>%
  filter(Funded == "1") %>%
  group_by(MarketingMethod) %>% 
  summarise(Pull_Through_Count=sum(Funded, na.rm=TRUE)) 

## Mail had the height pull through with 4184 leads being converted to a sale.


### Month Over Month Analysis

Sales_pullthrough <- read.csv("C:/Users/adith/Downloads/Salespullthrough.csv", stringsAsFactors = FALSE)


Sales_pullthrough$ResponseDate <- as.Date(Sales_pullthrough1$ResponseDate , format = "%m/%d/%Y")

## Gives the total lead count
mom_lead_count <- Sales_pullthrough %>%
  mutate(
    YearMonth = format(ResponseDate, "%Y-%m")
  ) %>%
  filter(LeadType == "INBOUND") %>%
  group_by(YearMonth,LeadType) %>%
  summarise(montly_lead_count=n()) %>%
   arrange(YearMonth)

## Gives the total conversion count
  
  mom_conversion_stats <- Sales_pullthrough %>%
  mutate(
    YearMonth = format(ResponseDate, "%Y-%m")
  ) %>%
  filter(LeadType == "INBOUND") %>%
  filter(AppCount == "1") %>%
  group_by(YearMonth,LeadType) %>%
  summarise(conversion_count=n()) %>%
  arrange(YearMonth)

## Creating a conversion funnel to identify number of lead, number of conversions, conversion rate, 
## change in number of conversions and percentage change.

mom_conversion_funnel <- left_join(mom_lead_count,mom_conversion_stats,by=c("YearMonth","LeadType"))

## Calculation of conversion rate

mom_conversion_funnel<-mom_conversion_funnel%>%
  mutate(Conversion_rate= (conversion_count/montly_lead_count)*100)

## Calculation of change in conversion volumes
conversion_change_volumes <- mom_conversion_funnel %>%
  group_by(LeadType) %>%
  arrange(YearMonth) %>%
  mutate(change=(conversion_count - lag(conversion_count, default = first(conversion_count))))

conversion_percent_change <- mom_conversion_funnel %>%
  group_by(LeadType) %>%
  arrange(YearMonth) %>%
mutate(percentage_change=((conversion_count - lag(conversion_count, default = first(conversion_count)))/conversion_count)*100)

mom_conversion_funnel <- left_join(conversion_change_volumes,conversion_percent_change,by=c("YearMonth","LeadType","montly_lead_count","conversion_count","Conversion_rate"))

write.table(mom_conversion_funnel,file = "Conversion_funnel.csv",sep =",")

### Question 02

closing_pricing <- read.csv(file.choose())

## Total number of records
Number_of_records <- closing_pricing %>%
  group_by(GuarantorPricingGroup) %>%
  summarise(Number_of_records = n()) 

## DocsIn Rate calculation
Docs_In_Count <- closing_pricing %>%
  group_by(GuarantorPricingGroup, DocsInCount) %>%
  summarise(Number_of_records = n()) 


DocsIn_Rate <- closing_pricing %>%
  group_by(GuarantorPricingGroup) %>% 
  summarise(Sum_DocsIn=sum(DocsInCount, na.rm=TRUE)) 

DocsIn_Rate['Rate'] <- ((DocsIn_Rate$Sum_DocsIn/DocsIn_Records$Number_of_records))*100

##closed rate at start rate and as percentage of total number of records

closed_Rate_at_Start_Rate <- closing_pricing %>%
  group_by(GuarantorPricingGroup,StartRate,ClosedRate) %>% 
  summarise(Total_records=n())

closed_Rate_at_Start_Rate['percentage_of_total']<-((closed_Rate_at_Start_Rate$Total_records/sum(Number_of_records$Number_of_records)))*100

##closed rate at min_rate and as percentage of total number of records

closed_Rate_at_min_rate <- closing_pricing %>%
  group_by(GuarantorPricingGroup,ApprovedDate,MinRate,ClosedRate) %>% 
  summarise(Total_records=n())

closed_Rate_at_min_Rate['percentage_of_total']<-((closed_Rate_at_min_Rate$Total_records/sum(Number_of_records$Number_of_records)))*100

  
### Negotiation

closing_pricing <- read.csv(file.choose())

Negotiation <- closing_pricing %>% filter(ClosedRate!="NULL")

## Defining negotiation as the difference between start rate and closed rate
Negotiation['Diff_In_Rates'] <- as.numeric(Negotiation$StartRate) - as.numeric(Negotiation$ClosedRate)

## This step is done in order to analyze the buffer in negotiations for each group
Negotiated_rates <- Negotiation %>%
  filter(Negotiation != "0.000") %>%
  group_by(GuarantorPricingGroup) %>% 
   tally()

colnames(Negotiated_rates)[2] <- "Buffer_Counts"

## Data exported to be loaded onto Power BI

Buffer <- subset(Negotiation, select = -c(MinRate,RiskGrade,DocsInCount) )

write.csv(Buffer,file = "Buffers.CSV", row.names = FALSE)

