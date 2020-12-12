### The first step involves loading the required libraries to clean up the data set as shown below.

library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(lubridate)

library(data.table)


### The data set is read into R and the head function is used to see the 
###first few rows of the data for exploratory analysis.
UN_Data <- read.csv(file.choose())

head(UN_Data)

### Since the country names are to be reprasented as row values for the 
###visualization the data frame is read as a matrix and transposed 
###excluding the year and the destination values.

UNData_Transposed <- as.data.frame(as.matrix(t(UN_Data[,-2][,-1])))

### The first column of the transposed matrix gives the count for each country
###for the year 1990, it is passed as a data frame and the respective year 
###and the destination column are attached to it.

Count<- UNData_Transposed[,1]
un_1990 <- as.data.frame(Count)
un_1990["Year"]<- '1990'
un_1990["Destination"] <- 'United States'

Count<- UNData_Transposed[,2]

un_1995 <- as.data.frame(Count)
un_1995["Year"]<- '1995'
un_1995["Destination"] <- 'United States'


Count<- UNData_Transposed[,3]
un_2000 <- as.data.frame(Count)
un_2000["Year"]<- '2000'
un_2000["Destination"] <- 'United States'


Count<- UNData_Transposed[,4]
un_2005 <- as.data.frame(Count)
un_2005["Year"]<- '2005'
un_2005["Destination"] <- 'United States'


Count<- UNData_Transposed[,5]
un_2010 <- as.data.frame(Count)
un_2010["Year"]<- '2010'
un_2010["Destination"] <- 'United States'


Count<- UNData_Transposed[,6]
un_2015 <- as.data.frame(Count)
un_2015["Year"]<- '2015'
un_2015["Destination"] <- 'United States'


Count<- UNData_Transposed[,7]
un_2019 <- as.data.frame(Count)
un_2019["Year"]<- '2019'
un_2019["Destination"] <- 'United States'

### The same step is repeaded to reach year from 1990 to 2019.
### Since the matrix was transposed the country names are shown as row names
### The data frames created above are binded together and the rownames are assigned to the country column.


rownames(un_1990)

UN_Data_Flourish <- rbind(un_1990,un_1995,un_2000,un_2005,un_2010,un_2015,un_2019)
UN_Data_Flourish["Country"] <- rownames(un_1990)

### It was found that countries with two names such as New Zealand were seprated with a Dot
### The dot is replaced by a space for all such country names and the names of 
### some other countries have been formatted in the right way based on ease of reprasentation in the visualization.


UN_Data_Flourish$Country <- gsub('\\.', " ",UN_Data_Flourish$Country)


UN_Data_Flourish$Country <- str_replace_all(UN_Data_Flourish$Country, "Bolivia  Plurinational State of", "Bolivia")

UN_Data_Flourish$Country <- str_replace_all(UN_Data_Flourish$Country, "Iran  Islamic Republic of", "Iran")

UN_Data_Flourish$Country <- str_replace_all(UN_Data_Flourish$Country, "Dem  People s Republic of Korea", "Dem.People's Republic of Korea")

UN_Data_Flourish$Country <- str_replace_all(UN_Data_Flourish$Country, "China  Hong Kong SAR", "China,Hong Kong SAR")

UN_Data_Flourish$Country <- str_replace_all(UN_Data_Flourish$Country, "China  Macao SAR", "China,Macao SAR")

UN_Data_Flourish$Country <- str_replace_all(UN_Data_Flourish$Country, "Côte d Ivoire", "Côte d'Ivoire")

UN_Data_Flourish$Country <- str_replace_all(UN_Data_Flourish$Country, "Lao People s Democratic Republic", "Lao People's Democratic Republic")

UN_Data_Flourish$Country <- str_replace_all(UN_Data_Flourish$Country, "Venezuela  Bolivarian Republic of", "Venezuela")

UN_Data_Flourish$Country <- str_replace_all(UN_Data_Flourish$Country, "Viet Nam", "Vietnam")

UN_Data_Flourish$Country <- str_replace_all(UN_Data_Flourish$Country, "Bonaire  Sint Eustatius and Saba", "Bonaire Sint Eustatius and Saba")

UN_Data_Flourish$Country <- str_replace_all(UN_Data_Flourish$Country, "Micronesia  Fed  States of", "Micronesia")

### Finally the row names are given based on the row count and the destination code is attached as a column.

rownames(UN_Data_Flourish) <- 1:nrow(UN_Data_Flourish)

UN_Data_Flourish["Code"] <- '840'

UN_Data_Flourish <- UN_Data_Flourish %>% select(Year, Destination, Code,Country,Count)

### Comma values in the count column are replaced with space as it was found that the visualization provides incorrect values if this is not done.

UN_Data_Flourish$Count <- as.numeric(gsub(",","",UN_Data_Flourish$Count))

### The data set is exported making sure that the NA values are replaced with 0.

write.csv(UN_Data_Flourish, "UN_Migration_data.csv", na = "0")

