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


 #Creat chjart
library(ggplot2)
ggplot(data = table_61_series, aes(y = `Sales Volume`, x = Period)) +
        geom_line()

