#Receiver Operating Characteristic (ROC) curve, AUC 구하기
install.packages("pROC")
library(pROC)
library(ggplot2)
library(magrittr)

data<- read.csv("example_basic.csv")
summary(data)
names<- colnames(data)[1:10][-3][-5]




diag_DF<- data.frame(Attribute=c(names), AUC=NA)

for (i in 1:nrow(diag_DF)){
  roc_result <- roc(data$status, data[, as.character(diag_DF$Attribute[i])])
  diag_DF[i,'AUC']<-roc_result$auc
}

diag_DF<-diag_DF[order(-diag_DF$AUC),]




time_roc<- roc(data$status, data$time)

plot.roc(time_roc, col="red", print.auc=TRUE, max.auc.polygon = TRUE, print.thres = TRUE, print.thres.pch = 19, print.thres.col = "red", auc.polygon = T, auc.polygon.col = "#D1F2EB" )



#two(or more) 비교하는 ROC curve, ggroc로 그려보기

bi_roc<- roc(data$status,data$ph.karno)
plot.roc(time_roc, col="red", print.auc=TRUE, print.auc.adj=c(2.5,-8), max.auc.polygon=T, print.thres=T, print.thres.pch=19, print.thres.col="red", print.thres.adj=c(0.3,-1.2), auc.polygon=T, auc.polygon.col="#D1F2EB")

plot.roc(bi_roc, add=T, col="blue", print.auc=T, print.auc.adj =c(1.11,1.2), print.thres=T, print.thres.pch=19, print.thres.col="blue", print.thres.adj=c(-0.085,1.1) )

legend("bottomright",   # legend의 위치를 우측 하단으로 설정합니다.
       legend=c("time", "ph.karno"),   # legend의 명칭을 설정합니다.
       col=c("red", "blue"), lwd=2)



inst_roc<-roc(data$status,data$inst)
time_roc<- roc(data$status, data$time)
bi_roc<- roc(data$status,data$ph.karno)


ggroc(time_roc)
ggroc(bi_roc)

ggroc(inst_roc)

g2<-ggroc(list(time=time_roc, wfns= inst_roc))

roc.list<- roc(status~ inst+ age, data= data)
g.list<-ggroc(roc.list)


g.list + 
  geom_abline(intercept= 1, slope=1, linetype="dashed")

?geom_abline
