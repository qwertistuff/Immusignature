library(Rtsne)
library(caret)
library(data.table)
library(tibble)
library(magrittr)



source <- fread("F:/BARNAUL/report/635/M-stat(1000).csv",data.table = FALSE)

name_for_graph <- "M-stat(1000)(635)_tsne.csv"

x <- source[,1] %>%
  as.factor()

source[,1] <- NULL

source <- source %>%  
  
unique() %>%
  
as.matrix()

tsne_out <- Rtsne(source,dims = 2 )[5] %>%

as.data.frame() %>%

add_column( x, .before = 1) 

names(tsne_out)[1] <- "Class"


graph <- ggplot(tsne_out, aes(x =tsne_out$Y.1,y=tsne_out$Y.2, colour= tsne_out$Class))+geom_point()+
  xlab("1 компонента")+ylab("2 компонента")+labs(colour = "Class") +ggtitle(name_for_graph)

print(graph)

#ggsave(filename = name_for_graph, dpi = 300)
#write.csv(x = tsne_out,"F:/BARNAUL/report/635/TSNE_M_STAT_1000peptide.csv", row.names = FALSE)



