library(spatialEco)
library(ggplot2)
library(data.table)
library(tibble)
library(dplyr)
library(foreach)
library(doParallel)
library(parallel)
library(magrittr)

data <- fread("C://Disk for Data Science/datasets/IMS/Barnaul/f532median_no_flag/Z-norm.csv", data.table = FALSE, header = TRUE)


cl <- makePSOCKcluster(32) 
registerDoParallel(cl)


Control <- subset(x=data,subset =Class==0) 
Rak <- subset(x=data,subset =Class==1)

system.time ({
  
h <- foreach(i = 2:ncol(data), .combine = rbind, .packages = c("spatialEco")) %dopar% {
   x <- separability(Control[,i], Rak[,i])[3] 
    
}

})

rownames(h) <- colnames(data)[2:ncol(data)]
  
stopCluster(cl)

h <- 
rownames_to_column(h, var = "Peptide")%>%

  arrange(desc(M))%>%
  slice(1:1000)


M_stat_data <-data.frame(Class=data$Class, data[,as.character(h$Peptide)])



write.csv(M_stat_data,  "C://Disk for Data Science/datasets/IMS/Barnaul/f532median_no_flag/M-stat(1000).csv", row.names = FALSE)



