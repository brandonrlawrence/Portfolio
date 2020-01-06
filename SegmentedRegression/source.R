library(tidyverse)
library(ggplot2)
library(lubridate)
library(segmented)
library(plyr)

data <- read_csv("SegmentedRegressionData.csv")

sapply(data,class)

#Price is character vector with $, convert to decimal
#Sales is character vector with $ and ,, convert to decimal
#Standardize data
data$price_num <- as.numeric(gsub('[$,]','', data$Price))
data$sales_num <- as.numeric(gsub('[$,]','', data$`Sales ($s)`))
data$volume <- as.numeric(data$`Volume (units)`)
data$Date <- as.Date(data$Date, format = '%m/%d/%Y')
data <- data[,c(1,2,6,7,8)]

mean_p <- mean(data$price_num)
mean_q <- mean(data$volume)

el_coeff <- mean_p/mean_q

store_list <- matrix(unique(data$Store))

for (store in store_list){
  
                  data_store <- data[which(data$Store == store),]
                  data_store <- data_store[order(data_store$Date),]
                  el_coeff <- mean(data_store$price_num)/mean(data_store$volume)
                  elas <- (diff(data_store$price_num, lag = 1)/diff(data_store$volume, lag = 1)) * el_coeff
                  data_store <- data_store[c(1:nrow(data_store)-1),]
                  data_store$elas <- elas
                  
                  data_store <- data_store[which(is.na(data_store$elas) == FALSE),]
                  data_store <- data_store[which(data_store$elas < 10),]
                  data_store <- data_store[which(data_store$elas > -10),]
                  data_store <- data_store[which(data_store$price_num > 9),]
                  data_store <- data_store[which(data_store$price_num < 15),]
                  
                  data_store <- data_store[,c(3,6)]
                  
                  if (exists('elas_agg')){
                    elas_agg <- rbind(elas_agg, data_store)
                  } else {
                    elas_agg <- data_store
                  }
                  
}

ggplot(data = elas_agg, aes(x = elas_agg$price_num, y = elas_agg$elas)) + 
        geom_point() + 
        scale_x_continuous(trans = 'log2') +
        scale_y_continuous(trans = 'log2')


l_model <- lm(elas~price_num, data = elas_agg)

segmented(l_model, seg.Z = ~price_num, psi = c(12))
