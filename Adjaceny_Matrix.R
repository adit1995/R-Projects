install.packages("igraph")
install.packages("ggplot2")
install.packages("tm")

library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(lubridate)
library(igraph)
library(ggplot2)
library(tm)
library(readxl)

### The first step involves loading the required libraries and packages for the analysis

### The data set is read and the null values are dropped and it is stacked into two coulmns.
### Unique values are selected and some long string values which have also been selected 
### as unique are dropped and the data set is stroed as a new data frame.

new_data = read_excel(file.choose())
newvariable <- stack(new_data) %>%
  drop_na()  
test <- unique(newvariable$values)
t1<- as.data.frame(test)
t2 <- t1[-c(1:49),]
t3<- as.data.frame(t2)


### A new matrix is created for the values and the column and row names are assigned.
### The weights for keywords are caluclated in the for loop as shown below.
trial <- matrix(0,254,254)
colnames(trial) <- t2
rownames(trial) <- t2
t4 <- as.data.frame(trial)

for (i in 1:12) {
  matrix_new <- unlist(new(i,))
  matrix_new <-matrix_new[!is.na(matrix_new)]
  combinelist <- combn(matrix_new,2)
  for(j in 1:length(combinelist[1,])) {
    row_var <- which(rownames(trial) == combinelist[1,j])
    col_var <- which(colnames(trial) == combinelist[2,j])
    trial[row_var,col_var] <- trial[row_var,col_var] + 1
    trial[col_var,row_var] <- trial[col_var,row_var] + 1
  }
}
network1 <- graph_from_adjacency_matrix(
  trial,
  mode = c("undirected"),
  weighted = TRUE
)

### from the igraph library the graph adjacency matrix is created and is stored as a dataframe.

from_adjacency( )
answer=get.data.frame(network1)
answer <- as.data.frame(answer)
write.csv(answer,"flourish_link.csv")
write.csv(data.frame(t3),"flourish_nodes.csv")

### It is made sure that the network is undirected and weighted and the different csv files for node,link and degree and exported.
network <- graph_from_adjacency_matrix(trial,mode = "undirected",weighted = TRUE)
Flourish_Deg <- degree(network)
Flourish_Deg <- as.data.frame(deg)
write.csv(Flourish_Deg,"flourish_degree.csv")



