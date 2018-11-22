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

#subset to 2 datasets.
Control<-subset(x=data,subset =Class==0) 

Rak<-subset(x=data,subset =Class==1) 

#clusters for parallel

cl <- makePSOCKcluster(10) 
registerDoParallel(cl)




#take from ks.test only p.value

system.time ({
  
  h <- foreach(i = 2:ncol(data), .combine = 'c' ) %dopar% {
    
    ks.test(Control[,i], Rak[,i])[2]
    
  }
  
})


stopCluster(cl)


#make dataframe with name of peptide and pvalue? and slice first 1000

dt <- data.frame(peptide=as.character(colnames(data[-1])),value=as.numeric(h)) %>%
  
  arrange(value) %>%
  
  slice(1:1000)


KS_stat_data <-data.frame(Class=data$Class, data[,as.character(dt$peptide)])

#safe on disk
write.csv(KS_stat_data,  "C://Disk for Data Science/datasets/IMS/Barnaul/f532median_no_flag/T-stat(1000).csv", row.names = FALSE)


