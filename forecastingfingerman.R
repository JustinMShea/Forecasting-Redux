# Although most of this will be done in markdown, 
# this r script file allows me to test and buiild without compiling the document.

## install R markdown Tufte theme
install.packages("tufte", type = "source")

## Import data from table 6.1 text file
Table_61_path <- path.expand("~/R/forecasting/Table 6.1.txt")
table_61 <- read.delim(Table_61_path, header = FALSE, sep = " ", skip = 1)
 # add column names
colnames(table_61) <- c("Period", "Year 1", "Period", "Year 2", "Period", "Year 3", "Period", "Year 4")
 # review structure
str(table_61)

## Page 3, create plot
 # First, gather the time series data table into one column.
library(tidyr)
table_61_series <- gather(table_61, Period)
table_61_series <- table_61_series[,-(2:5)]
table_61_series$Period <- as.numeric(rownames(table_61_series))
colnames(table_61_series) <- c("Period", "Sales Volume")
str(table_61_series)


 #Creat chart
library(ggplot2)
plot_61 <- ggplot(data = table_61_series, aes(y = `Sales Volume`, x = Period)) +
           geom_line() 


## page 6, create table 2
 # Import data from table 6.2 text file
Table_62_path <- path.expand("~/R/forecasting/Table 6.2.txt")
table_62 <- read.delim(Table_62_path, header = FALSE, sep = " ", skip = 1)
 # add column names
colnames(table_62) <- c("Period", "Actual", "Period", "Actual")
 # review structure
str(table_62)

## Page 3, create plot
# First, gather the time series data table into one column.
library(tidyr)
table_62_series <- gather(table_62[,c(2,4)])
table_62_series$key<- as.numeric(rownames(table_62_series))
colnames(table_62_series) <- c("Period", "Horizontal Data")
str(table_62_series)


#Creat chart
library(ggplot2)
plot_62 <- ggplot(data = table_62_series, aes(y = `Horizontal Data`, x = Period)) +
        geom_line() 

plot_62

mean_62 <- round(mean(table_62_series[,2]), digits = 1)

plot_62 +  geom_hline(aes(yintercept=356.5), color = "red", linetype="dashed", size = 1.3)

## Smoothing moving average, pg 8.

period_3_example <- table_62_series[6:8,]
smooth <- round(mean(table_62_series[6:8,2]), digits = 2)
period_3_example$Smoothing <- c("...",smooth,"...")


