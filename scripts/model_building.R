library(plyr)
library(dplyr)
library(tidyverse)
library(nnet)

usa_data <- data.frame(read.csv("data/usa_parsed_fatalities.csv"))
can_data <- data.frame(read.csv("data/canada_parsed_fatalities.csv"))



usa_data$cod <- ifelse(usa_data$Cause.of.Death=="Gunshot", "Gunshot", "Other")
can_data$cod <- ifelse(can_data$Cause.of.Death=="Gunshot", "Gunshot", "Other")


can_mod <- nnet::multinom(cod ~ Race+Age + Gender + Prov.State,
                          data = can_data)
usa_mod <- nnet::multinom(cod ~ Race+Age + Gender + Prov.State,
                          data = usa_data)

saveRDS(can_mod, file = "model/can_mod.rds")
saveRDS(usa_mod, file = "model/usa_mod.rds")

# 
# predict(can_mod, data.frame(Race = c("South Asian"), Age=c(22), Gender="Male", Prov.State="YT"), type="probs")
# predict(usa_mod, data.frame(Race = c("South Asian"), Age=c(22), Gender="Male", Prov.State="NY"), type="probs")
