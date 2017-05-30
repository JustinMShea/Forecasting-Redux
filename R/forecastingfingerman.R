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
           geom_line() +
           ylim(0,800)
          

plot_61 + scale_x_continuous(breaks=seq(0,54,6), limits = c(0,54))
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
library(cowplot)
plot_62 <- ggplot(data = table_62_series, aes(y = `Horizontal Data`, x = Period)) +
        geom_line() 

plot_62

mean_62 <- round(mean(table_62_series[,2]), digits = 1)

plot_62 +  geom_hline(aes(yintercept=356.5), color = "red", linetype="dashed", size = 1.3)

## Smoothing moving average, pg 8.

period_3_example <- table_62_series[6:8,]
smooth <- round(mean(table_62_series[6:8,2]), digits = 2)
period_3_example$Smoothing <- c("...",smooth,"...")
colnames(period_3_example) <- c("Period", "Actual", "Smoothing")


library(forecast)
smooth_63 <- round(ma(table_62_series[,2], order = 3, centre = TRUE), digits = 1)
table_63_series <- data.frame(table_62_series, "CMA(3)" = smooth_63)
colnames(table_63_series) <- c("Period", "Actual", "Centered Moving Average (3)")
table_63 <- data.frame(table_63_series[1:15,], table_63_series[16:30,])
colnames(table_63) <- c("Period", "Actual", "CMA(3)", "Period", "Actual", "CMA(3)")

# tablef for chart
table_63_chart <- table_63_series
colnames(table_63_chart) <- c("Period", "Actual", "CMA(3)")
table_63_chart <- gather(table_63_chart, Period)
colnames(table_63_chart) <- c("Period", "Series", "Value")

chart_63 <- ggplot(data = table_63_chart, aes(y = Value, x = Period, col = Series)) +
            geom_line(position = "jitter", size = 1) +
            ggtitle("3-Period Centered Moving Average") 



ggdraw(switch_axis_position(chart_63 + theme_grey(), axis = 'y'))

## creating table 6.4
library(forecast)
smooth_64 <- round(ma(table_62_series[,2], order = 5, centre = TRUE), digits = 1)
table_64_series <- data.frame(table_62_series, "CMA(3)" = smooth_63, "CMA(5)" = smooth_64)
colnames(table_64_series) <- c("Period", "Actual", "CMA(3)", "CMA(5)")

table_64 <- data.frame(table_64_series[1:15,], table_64_series[16:30,])
colnames(table_64) <- c("Period", "Actual", "CMA(3)","CMA(5)","Period", "Actual", "CMA(3)", "CMA(5)")

table_64_series

# first chart
table_641_chart <- table_64_series[,c(1,2,4)]
table_641_chart <- gather(table_641_chart, Period)
colnames(table_641_chart) <- c("Period", "Series", "Value")

chart_641 <- ggplot(data = table_641_chart, aes(y = Value, x = Period, col = Series)) +
  geom_line(position = "jitter") +
  ggtitle("Actual ~ CMA(5)")

#figure_641 <- ggplot(data = table_64, aes(y = `Actual`, x = Period)) +
 # geom_line(color = "blue", size = 1) +
  #ggtitle("5-Period Centered Moving Average") +
  #geom_line(aes(y = `CMA(5)`), color = "red", linetype="dashed", size = 1.4)

ggdraw(switch_axis_position(figure_641 + theme_grey(), axis = 'y'))


# second chart
table_642_chart <- table_64_series
table_642_chart <- gather(table_642_chart, Period)
colnames(table_642_chart) <- c("Period", "Series", "Value")

chart_642 <- ggplot(data = table_642_chart, aes(y = Value, x = Period, col = Series)) +
  geom_line(position = "jitter") +
  ggtitle("Actual ~ CMA(3,5)")


library(ggthemes)
chart_642 <- ggplot(data = table_642_chart, aes(y = Value, x = Period, col = Series)) +
  geom_line(position = "jitter") +
theme_tufte() +
  xlab("Period") + ylab("Value") + 
  theme(axis.title.x = element_text(vjust=-0.5), axis.title.y = element_text(vjust=1.5))


#figure_642 <- ggplot(data = table_64, aes(y = `Actual`, x = Period))+ geom_line(color = "darkgreen", size = 1) +
 # geom_line(aes(y = `CMA(3)`), color = "red", linetype="dashed", size = 1.4) +
  #geom_line(aes(y = `CMA(5)`), color = "blue", linetype="dashed", size = 1.4) + 
  #ggtitle("3-Period and 5-Period CMA") 

ggdraw(switch_axis_position(chart_642 + theme_grey(), axis = 'y'))

ggplot(data = table_63_series, aes(y = `Actual`, x = Period)) +
  geom_line(color="blue", size = 1) +
  ggtitle(`3-Period Centered Moving Average`) +
  geom_line(aes(y = `Centered Moving Average (3)`))

library(forecast)
smooth_6 <- round(ma(table_62_series[,2], order = 6, centre = TRUE), digits = 1)
smooth_12 <- round(ma(table_62_series[,2], order = 12, centre = TRUE), digits = 1)

table_66_series <- data.frame(table_62_series, "CMA(6)" = smooth_6, "CMA(12)" = smooth_12)
colnames(table_66_series) <- c("Period", "Actual", "CMA(6)", "CMA(12)")

# chart 2
table_66_chart <- gather(table_66_series, Period)
colnames(table_66_chart) <- c("Period", "Series", "Value")

chart_66 <- ggplot(data = table_66_chart, aes(y = Value, x = Period, col = Series)) +
  geom_line() +
  ggtitle("Actual ~ Centered Moving Average (6) and (12)") +
  geom_line() + 
  ylim(320,400) + 
  scale_x_continuous(breaks=seq(0,35,5), limits = c(0,35)) 


## Center Weighted Moving Average 3-Period
smooth_67 <- filter(table_62_series[,2], c(0.6,0.3,0.1), method = "convolution", sides = 2)
table_67_series <- data.frame(table_62_series, "CWMA(3)" = smooth_67)
colnames(table_67_series) <- c("Period", "Actual", "CWMA(3)")
table_67 <- data.frame(table_67_series[1:15,], table_67_series[16:30,])
colnames(table_67) <- c("Period", "Actual", "CWMA(3)", "Period", "Actual", "CWMA(3)")


table_67_chart <- gather(table_67_series, Period)
colnames(table_67_chart) <- c("Period", "Series", "Value")

chart_67 <- ggplot(data = table_67_chart, aes(y = Value, x = Period, col = Series)) +
  geom_line() +
  geom_line() + 
  ylim(320,400) + 
  scale_x_continuous(breaks=seq(0,35,5), limits = c(0,35)) 

ggdraw(switch_axis_position(chart_67 + theme_tufte() +
                              theme(legend.position = "top"), axis = 'y'))


# trend/cylical components
# Create trend line, by regressing Sales volume against time steps.
trend <- lm(`Sales Volume` ~ Period, table_61_series)
smooth_12 <- round(ma(table_61_series[,2], order = 12, centre = TRUE), digits = 1)
table_610 <- data.frame(table_61_series, Trend = trend$fitted.values)
table_610_series <- gather(table_610, Period)
colnames(table_610_series) <- c("Period", "Series", "Value")

figure_610 <- ggplot(data = table_610_series, aes(y = Value, x = Period, col = Series)) +
  geom_line() + 
  ylim(0,800) + 
  scale_x_continuous(breaks=seq(0,54,6), limits = c(0,54))

ggdraw(switch_axis_position(figure_610 + theme_tufte(), axis = 'y'))

## Trend chart, with cycle component
table_611 <- data.frame(table_61_series, Trend = trend$fitted.values, Cycle = trend$residuals)
table_611_series <- gather(table_611, Period)
colnames(table_611_series) <- c("Period", "Series", "Value")

figure_611 <- ggplot(data = table_611_series, aes(y = Value, x = Period, col = Series)) +
  geom_line() + 
  ylim(-200,800) + 
  scale_x_continuous(breaks=seq(0,54,6), limits = c(0,54))

ggdraw(switch_axis_position(figure_611 + theme_tufte(), axis = 'y'))


# Seasonal Component

library(forecast)
# Create centered 12 period Moving Average
smooth_12_2 <- ma(table_61_series[,2], order = 12, centre = TRUE)
# Create new data frame with CMA(12) and monthly number
table_68_series <- data.frame("Period"=table_61_series[,1], "Month"=rep(month.abb), "Sales"=table_61_series[,2], "CMA12"=round(smooth_12_2, digits=2))
table_68_series <- data.frame(table_68_series, "Ratio" = round(table_68_series$Sales/smooth_12_2, digits = 2))

table_68_wide <- data.frame(table_68_series[1:24,],table_68_series[25:48,])
names_68 <- colnames(table_68_series)
colnames(table_68_wide) <- c(names_68, names_68)

# Create wide table, columns by month
table_69 <- matrix(table_68_series$Ratio, ncol=12, byrow = TRUE)
colnames(table_69) <- rep(month.abb)
table_69_ratio <- data.frame(table_69)

# Create Unadjusted Seasonal Index - Median values of ratio by month
table_69_med <- matrix(table_68_series$Sales/smooth_12_2 , ncol=12, byrow=TRUE)
colnames(table_69_med) <- rep(month.abb)
table_69_med <- data.frame(table_69_med)

table_69_median <- apply(table_69_med, MARGIN = 2, FUN = median, na.rm=TRUE)

table_69_median_tbl <- matrix(table_69_median, ncol=12, byrow=TRUE)
colnames(table_69_median_tbl) <- rep(month.abb)
table_69_median_tbl <- data.frame(table_69_median)

# Create Adjusted Seasonal Index
table_69_adjusted <- (12/sum(table_69_median))*table_69_median

table_69_adjusted_tbl <- matrix(table_69_adjusted, ncol=12, byrow=TRUE)
colnames(table_69_adjusted) <- rep(month.abb)
table_69_adjusted <- data.frame(table_69_adjusted)
sum(table_69_adjusted)

display_table_69 <- rbind(table_69_med, table_69_median)
row.names(c("Year 1, Year 2, Year 3, Year 4, Median"))


#Chart seasonal index 
table_69_series <- data.frame("Period"=seq(1,12,1),"Month"=rep(month.abb),"Index"=table_69_adjusted[1:12])

figure_69 <- ggplot(data = table_69_series, aes(y = Index, x = Period, col = Index)) +
  geom_line() + 
  geom_hline(yintercept = 1, linetype="dashed") +
  scale_y_continuous(breaks=seq(.9,1.3,.05), limit = c(.9,1.3)) +
  scale_x_continuous(breaks=seq(1,12,1), limits = c(0,12), labels = rep(month.abb))

ggdraw(switch_axis_position(figure_69 + theme_tufte(), axis = 'y'))


# Add to table_68 series and calculate seasonally adjusted values
table_614_seasonal <- data.frame(table_68_series, table_69_adjusted, table_68_series$Sales/table_69_adjusted)
colnames(table_614_seasonal) <- c(colnames(table_68_series), "Seasonal", "DeSeasonalized")

# Create Series for chart
table_614_series <- data.frame(table_614_seasonal, "ds.Cycle" = table_614_seasonal$Sales - table_614_seasonal$DeSeasonalized)

table_614_series2 <- gather(table_614_series[,c(1,3,7,8)], Period)
colnames(table_614_series2) <- c("Period", "Series", "Value")

# Create chart object
figure_614 <- ggplot(data = table_614_series2, aes(y = Value, x = Period, col = Series)) +
  geom_line() + 
  geom_hline(yintercept = 1, linetype="dashed") +
  scale_y_continuous(breaks=seq(-50,800,200), limits = c(-50,800)) + 
  scale_x_continuous(breaks=seq(0,54,6), limits = c(0,54))

# Call graphics Device to illustrate chart
ggdraw(switch_axis_position(figure_614 + theme_tufte(), axis = 'y'))


figure_615 <- ggplot(data = table_614_series, aes(y = ds.Cycle, x = Period, col = ds.Cycle)) +
  geom_line() + 
  geom_hline(yintercept = 0, linetype="dashed") +
  ylim(-50,150) + 
  scale_x_continuous(breaks=c(11,12,23,24,35,36,47,48), limits = c(0,54))

ggdraw(switch_axis_position(figure_615 + theme_tufte(), axis = 'y'))

# Decomposition
table_61_time_series <- ts(data = table_61_series$`Sales Volume`, frequency = 12)
table_61_decompose <- decompose(table_61_time_series)
plot(stl(table_61_time_series, s.window = 12, t.window = 12))

# Irregular values
#Add to table_614_series series and calculate irregular values.
table_615_Irregular <- data.frame(table_614_seasonal, "Irregular" = table_614_seasonal$Sales/(table_614_seasonal$CMA12*table_614_seasonal$Seasonal))

# Create Series for chart
table_Irregular_series <- gather(table_615_Irregular[,c(1,3,7,8)], Period)
colnames(table_Irregular_series) <- c("Period", "Series", "Value")

# Create chart object
figure_Irregular_series <- ggplot(data = table_615_Irregular, aes(y = Irregular, x = Period, col = Irregular)) +
  geom_line() + 
  geom_hline(yintercept = 0, linetype="dashed") +
  scale_x_continuous(breaks=seq(0,54,6), limits = c(0,54))

# Call graphics Device to illustrate chart
ggdraw(switch_axis_position(figure_Irregular_series + theme_tufte(), axis = 'y'))

#####################################################################
##### Problems and Questions Section ################################
#####################################################################

## Import data from problem 1 text file
problem_1_path <- path.expand("~/R/forecasting/problem table 1.txt")
problem_1 <- read.delim(problem_1_path, skip = 1, sep = " ")
# add column names
colnames(problem_1) <- c("Period", "Observation", "Period", "Observation")

problem_1

## Import data from problem 4 text file
problem_4_path <- path.expand("~/R/forecasting/problem 4 table.txt")
problem_4 <- read.delim(problem_4_path, sep = " ", header = TRUE)
# add column names
colnames(problem_1) <- c("Period", "Observation", "Period", "Observation")

problem_4
