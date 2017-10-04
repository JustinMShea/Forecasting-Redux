
# Data download

library(hexView)
# C:\Users\skate_more\Documents\R\forecasting\data\Data Sets\fcst4-05
data_path <- paste0(getwd(),"/data/Data Sets/fcst4-05/fcst5input.wf1")
fcst5input.wf1 <- readEViews(data_path)


library(readr)
fcst5input.dat <- read_delim("~/R/forecasting/data/Data Sets/fcst4-05/fcst5input.dat", 
                         "\t", escape_double = FALSE, col_types = cols(RTRR = col_integer()), 
                         trim_ws = TRUE)

PC5_07_EUROstar <- read_csv("~/R/forecasting/data/Data Sets/fcst4-05/PC5_07_EUROstar.dat")

data_path_2 <- paste0(getwd(),"/data/Data Sets/fcst4-05/fcst5finalized.wf1")
fcst5finalized.wf1 <- readEViews(data_path_2)

## Models



