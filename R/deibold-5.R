
library(hexView)
# C:\Users\skate_more\Documents\R\forecasting\data\Data Sets\fcst4-05
data_path <- paste0(getwd(),"/data/Data Sets/fcst4-05/fcst5input.wf1")
fcst5input.wf1 <- readEViews(data_path)


data_path_1 <- paste0(getwd(),"/data/Data Sets/fcst4-05/fcst5input.dat")
fcst5input.dat <- readEViews(data_path_1)

data_path_2 <- paste0(getwd(),"/data/Data Sets/fcst4-05/fcst5finalized.wf1")
fcst5finalized.wf1 <- readEViews(data_path_2)
