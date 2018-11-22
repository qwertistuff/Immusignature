library(caret)
library(data.table)
library(tibble)
library(magrittr)



source <- fread("F:/BARNAUL/report/635/M-stat(1000).csv",data.table = FALSE)

name_for_graph <- "M-stat(1000)(635)_tsne.csv"

x <- source[,1] %>%
  as.factor()

source[,1] <- NULL


PCA <- preProcess(source, method = c("center", "scale", "pca"), pcaComp = 2) %>%

  predict(source) %>%
  as.data.frame() %>%
  
  add_column(x, .before=1)

  names(PCA)[1] <- "Class"

  
graph <- ggplot(PCA, aes(x =PCA$PC1,y=PCA$PC2, colour= PCA$Class))+geom_point()+
  xlab("1 компонента")+ylab("2 компонента")+labs(colour = "Class") +ggtitle(name_for_graph)

print(graph)

#ggsave(filename = name_for_graph, dpi = 300)
#write.csv(x = PCA,"F:/BARNAUL/report/635/TSNE_M_STAT_1000peptide.csv", row.names = FALSE)








