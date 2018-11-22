library(data.table)
library(ggplot2)
library(tibble)
library(dplyr)
library(foreach)
library(doParallel)
library(parallel)
library(magrittr)

data<- fread("C:/Disk for Data Science/datasets/IMS/Barnaul/f532median_no_flag/Z-norm.csv", data.table = FALSE, header = TRUE)



data$Class <- as.factor(data$Class)


Control<-subset(x=data,subset =Class==0) 

Rak<-subset(x=data,subset =Class==1) 

cl <- makePSOCKcluster(10) 
registerDoParallel(cl)

system.time ({
  
  h <- foreach(i = 2:ncol(data), .combine = 'c' ) %dopar% {
    
  t.test(Control[,i], Rak[,i])[3]
    
  }
  
})


stopCluster(cl)


 
dt <- data.frame(peptide=as.character(colnames(data[-1])),value=as.numeric(h)) %>%

arrange(value) %>%

slice(1:1000)


T_stat_data <-data.frame(Class=data$Class, data[,as.character(dt$peptide)])



write.csv(T_stat_data,  "C://Disk for Data Science/datasets/IMS/Barnaul/f532median_no_flag/T-stat(1000).csv", row.names = FALSE)


