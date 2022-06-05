library(dplyr)
library(mice)
library(ggthemes)
library(gridExtra)
library(ggplot2)
library(GGally)
library(VIM)
library(data.table)
library(magrittr)
library(ClustOfVar)
library(dummies)
library(fastDummies)
library(corrplot)
library(randomForest)
library(leaps)
library(ggfortify)
library(car)
library(Metrics)
library(nortest)
library(jtools)
library(sandwich)
library(forcats)
library(factoextra)
library(caret)
#library(Tejapi)
par(mfrow=c(1,1)) 
options(scipen = 9999)
setwd("D:/User/Desktop/Program/R/ho")

#myapi = "2RCY7gpsnNpaWIzRmAFXWfN4mmHlzH" #到8-23
#Tejapi.api_key(myapi)
#priceData <- Tejapi('TRAIL/TAAPRTRAN',coid='E', mdate.gt='2019-07-01',mdate.lt='2021-06-30', paginate=TRUE)

data <- fread("data.csv",stringsAsFactors=FALSE)
data = data %>% filter(交易別 == '不動產' & 備註資訊 == '') %>% filter(`單價(萬/坪)` > 0) %>% filter(交易標的 == '房地(土地+建物)' | 交易標的 == '房地(土地+建物)+車位') %>% filter(經度>=120.1032 & 經度<=121.0115) %>% filter(緯度>=22.28 & 緯度<=23.28) %>% select(c(1:45,-交易別,-交易別註記,-備註資訊))
data[data == "",] <- NA
data[data == "NA",] <- NA
data[data == "N.A.",] <- NA

NAS <-which(colSums(is.na(data))>0)
NASdata <- data[,c(16,17,18,25,26,27,28,29,30,37,38)]

NMiss<-function(x){sum(is.na(x))}
apply(NASdata,2,NMiss)

aggr_plot<-aggr(NASdata,numbers=TRUE,sortVars=TRUE,labels=names(NASdata),cex.axis=.5,
                gap=3,ylab=c("Histogram of missing data","Pattern"))

#刪除遺失率10%以上欄位
data1<-data[,-c("車位類別","車位類別註記","主要用途","主要用途註記")]
#刪除含有遺失資料列4233筆，約占10% 剩35860筆 原40093
data1 <- na.omit(data1)
#******************************************
names(data1)[4] = "星期"
data1$`門牌/地號` <- as.character(data1$`門牌/地號`)
data1$緯度 <- as.numeric(data1$緯度)
data1$經度 <- as.numeric(data1$經度)
data1$`屋齡(年)` <- as.numeric(data1$`屋齡(年)`)
data1$建築完成日期 <- as.character(data1$建築完成日期)
data1$星期 <- as.factor(data1$星期)
data1$鄉鎮市區 <- as.factor(data1$鄉鎮市區)
data1$交易標的 <- as.factor(data1$交易標的)
data1$'交易標的註記' <- as.factor(data1$'交易標的註記')
data1$'臨路(Y/N)' <- as.factor(data1$'臨路(Y/N)')
data1$都市土地使用分區 <- as.factor(data1$都市土地使用分區)
data1$都市土地使用分區註記 <- as.factor(data1$都市土地使用分區註記)
data1$移轉樓層 <- as.factor(data1$移轉樓層)
data1$移轉地上樓層 <- as.numeric(data1$移轉地上樓層)
data1$'移轉單一地上樓層(棟)(Y/N)' <- as.factor(data1$'移轉單一地上樓層(棟)(Y/N)')
data1$總樓層 <- as.numeric(data1$總樓層)
data1$'頂樓註記(Y/N)' <- as.factor(data1$'頂樓註記(Y/N)')
data1$建物型態 <- as.factor(data1$建物型態)
data1$建物型態註記 <- as.factor(data1$建物型態註記)
data1$主要建材  <- as.factor(data1$主要建材)
data1$主要建材註記  <- as.factor(data1$主要建材註記)
data1$'隔間(Y/N)'  <- as.factor(data1$'隔間(Y/N)')
data1$'管理組織(Y/N)'  <- as.factor(data1$'管理組織(Y/N)')

colnames(data1) <- c("年","月","日","星期","鄉鎮市區","交易標的","交易標的註記"
,"門牌","臨路","緯度","經度","土地數量","建物數量","車位數量","土地移轉坪數"
,"使用分區","使用分區註記","移轉樓層","移轉地上樓層","移轉單一地上樓層","總樓層","頂樓註記","建物型態"
,"建物型態註記","主要建材","主要建材註記","建築完成日","屋齡","建物移轉坪數","房間","客廳","衛浴","隔間","管理組織","車位總面積"
,"總價","單價","車位總價")

F_data<-data1[,c(4,5,6,9,16,18,20,22,23,25,33,34)]
N_data<-data1[,c(1,2,3,12,13,14,15,19,21,28,29,30,31,32,35,36,37,38)]

#write.csv(data1, file = "cleanhousing.csv", row.names=FALSE)

#**********************************

#公寓(5樓含以下無電梯)3771 住宅大樓(11層含以上有電梯)20806 透天厝 7662 華廈(10層含以下有電梯) 2811
住宅大樓 <- data1 %>% filter(建物型態 == '住宅大樓(11層含以上有電梯)')
#write.csv(住宅大樓, file = "住宅大樓.csv", row.names=FALSE)

F_data_住宅大樓<-住宅大樓[,c(5,9,16,20,22,25,33,34)]
N_data_住宅大樓<-住宅大樓[,c(12,13,14,15,19,21,28,29,30,31,32,35,36,37,38)]
cor_numVar_住宅大樓 <- cor(scale(N_data_住宅大樓))
corrplot.mixed(cor_numVar_住宅大樓, tl.col="black", tl.pos = "lt")

#######################################################################
#part1 

X.quanti1 <- 住宅大樓[,c(12,13,14,19,21,28,30,31,32,37)]
X.quali1 <- 住宅大樓[,c(5,9,16,20,22,25,33,34)]


#住1 <- 住宅大樓[,c(5,9,16,20,22,25,33,34,12,13,14,19,21,28,30,31,32,37)]
#住1 <- model.matrix(單價~.,data=住1)[,-1]
#住1 <- data.frame(單價=住宅大樓$單價,住1)


#fviz_nbclust(住1, 
#             FUNcluster = kmeans,  # hierarchical clustering
#             method = "silhouette") # total within sum of square
 



tree1_住宅大樓 <- hclustvar(X.quanti1,X.quali1)
summary(tree1_住宅大樓)

plot(tree1_住宅大樓, main = '住宅大樓')
#plot(tree1_住宅大樓,type = "index")
rect.hclust(tree1_住宅大樓 , k = 5,border = 2:6)
#avg_dend_obj <- as.dendrogram(tree1_住宅大樓)
#avg_col_dend <- color_branches(avg_dend_obj, k = 5)
#plot(avg_col_dend,main = '住宅大樓')
part1 <- cutreevar(tree1_住宅大樓,5)
summary(part1)
part1$var
part1$E
#43.9064
part1$wss
stab1_住宅大樓 <- stability(tree1_住宅大樓, B = 10)
stab1_住宅大樓$matCR
stab1_住宅大樓$meanCR
plot(stab1_住宅大樓,nmax=6)
boxplot(stab1_住宅大樓$matCR[,1:5])

#part2     
X.quanti2 <- 住宅大樓[,c(12,13,14,19,21,28,37)]
X.quali2 <- 住宅大樓[,c(5,9,16,20,22,25,34)]
tree2_住宅大樓 <- hclustvar(X.quanti2,X.quali2)
plot(tree2_住宅大樓, main = '住宅大樓')
rect.hclust(tree2_住宅大樓 , k = 5,border = 2:6)
part2 <- cutreevar(tree2_住宅大樓,5)
summary(part2)
part2$var
part2$E
#44.64638
part2$wss

stab2_住宅大樓 <- stability(tree2_住宅大樓, B = 10)
stab2_住宅大樓$matCR
stab2_住宅大樓$meanCR
plot(stab2_住宅大樓,nmax=10)
boxplot(stab2_住宅大樓$matCR[,1:10])
#part3 
X.quanti3 <- 住宅大樓[,c(12,13,14,21,28,37)]
X.quali3 <- 住宅大樓[,c(5,9,16,22,25,34)]
tree3_住宅大樓 <- hclustvar(X.quanti3,X.quali3)

plot(tree3_住宅大樓, main = '住宅大樓') 
rect.hclust(tree3_住宅大樓 , k = 5,border = 2:6)

part3 <- cutreevar(tree3_住宅大樓,5)
summary(part3)
part3$var
part3$E
#43.91259
part3$wss
stab3_住宅大樓 <- stability(tree3_住宅大樓, B = 10)
stab3_住宅大樓$matCR
stab3_住宅大樓$meanCR
plot(stab3_住宅大樓,nmax=10)
boxplot(stab3_住宅大樓$matCR[,1:10])

#part4 
X.quanti4 <- 住宅大樓[,c(12,13,14,21,28,37)]
X.quali4 <- 住宅大樓[,c(5,22,25,34)]
tree4_住宅大樓 <- hclustvar(X.quanti4,X.quali4)
plot(tree4_住宅大樓, main = '住宅大樓')
rect.hclust(tree4_住宅大樓 , k = 5,border = 2:6)

part4 <- cutreevar(tree4_住宅大樓,5)
summary(part4)
part4$var
part4$E

part4$wss
stab4_住宅大樓 <- stability(tree4_住宅大樓, B = 10)
stab4_住宅大樓$matCR
stab4_住宅大樓$meanCR
plot(stab4_住宅大樓,nmax=10)
boxplot(stab4_住宅大樓$matCR[,1:8])

#part5 排除part4的第5群
X.quanti5 <- 住宅大樓[,c(12,13,14,21,28,37)]
X.quali5 <- 住宅大樓[,c(22,34)]
tree5_住宅大樓 <- hclustvar(X.quanti5,X.quali5)
plot(tree5_住宅大樓, main = '住宅大樓')
rect.hclust(tree5_住宅大樓 , k = 5,border = 2:6)
part5 <- cutreevar(tree5_住宅大樓,5)

summary(part5)
part5$E
part5$var
part5$wss
stab5_住宅大樓 <- stability(tree5_住宅大樓, B = 10)
stab5_住宅大樓$matCR
stab5_住宅大樓$meanCR
plot(stab5_住宅大樓,nmax=10)
boxplot(stab5_住宅大樓$matCR[,1:5])
###########################################
#5群全部 依據summary(part1)

new1_住宅大樓 <- 住宅大樓[,c(12,13,14,19,21,28,30,31,32,37,5,9,16,20,22,25,33,34)]

set.seed(1000)

index <- createDataPartition(
  住宅大樓$單價,
  p = 0.7,
  list = FALSE
)

x1_住宅大樓 <- model.matrix(單價~.,data=new1_住宅大樓)[,-1]
x1_住宅大樓[1:5,]

xtrain1_住宅大樓 <- x1_住宅大樓[index,]

xtest1_住宅大樓 <- x1_住宅大樓[-index,]

ytrain1_住宅大樓 <- new1_住宅大樓$單價[index]

ytest1_住宅大樓 <- new1_住宅大樓$單價[-index]        

a1_住宅大樓 <- data.frame(單價=ytrain1_住宅大樓,xtrain1_住宅大樓)

ad.test(a1_住宅大樓$單價)


data_ctrl <- trainControl(method = "cv", number = 10)


ml1_住宅大樓 <- train(單價~.,data = a1_住宅大樓,trControl = data_ctrl,method ="lm")
summ(ml1_住宅大樓$finalModel,robust = T)
summary(ml1_住宅大樓$finalModel)
plot(ml1_住宅大樓$finalModel)
durbinWatsonTest(ml1_住宅大樓$finalModel)
ncvTest(ml1_住宅大樓$finalModel)
plot(ecdf(abs(ml1_住宅大樓$finalModel$residuals/sd(ytest1_住宅大樓))),main="ASD chart",xlab="Residual/STD",ylab="Cumulative distrubtion",col="darkblue")

predict1_住宅大樓 <- predict(ml1_住宅大樓$finalModel,newdata=data.frame(xtest1_住宅大樓))
b1_住宅大樓 <-data.frame(cbind(ytest1_住宅大樓,round(predict1_住宅大樓,1)))
colnames(b1_住宅大樓) <- c("真實價格_住宅大樓","預測價格_住宅大樓")
b1_住宅大樓$誤差 <- ((b1_住宅大樓$真實價格_住宅大樓-b1_住宅大樓$預測價格_住宅大樓)^2)^0.5
apply(b1_住宅大樓,2,mean)
apply(b1_住宅大樓,2,sd)
mean(b1_住宅大樓$誤差)
sd(b1_住宅大樓$誤差)
b1_住宅大樓$左預測區間 <- round(b1_住宅大樓$預測價格_住宅大樓 - 1.96*sd(b1_住宅大樓$誤差),1)
b1_住宅大樓$右預測區間 <- round(b1_住宅大樓$預測價格_住宅大樓 + 1.96*sd(b1_住宅大樓$誤差),1)
b1_住宅大樓[1:10,]
confidence1_預測 <- predict(ml1_住宅大樓$finalModel, newdata=data.frame(xtest1_住宅大樓), interval = 'confidence')
prediction1_預測 <- predict(ml1_住宅大樓$finalModel, newdata=data.frame(xtest1_住宅大樓), interval = 'prediction')

套合_1 <- data.frame(fitted(ml1_住宅大樓$finalModel))
套合_1[-index,]
cbind(ytest1_住宅大樓,confidence1_預測,套合_1[-index,])

#前4群 依據summary(part1)
new2_住宅大樓 <- 住宅大樓[,c(12,13,14,19,21,28,37,5,9,16,20,22,25,34)]
x2_住宅大樓 <- model.matrix(單價~.,data=new2_住宅大樓)[,-1]
x2_住宅大樓[1:5,]

xtrain2_住宅大樓 <- x2_住宅大樓[index,]
xtest2_住宅大樓 <- x2_住宅大樓[-index,]
ytrain2_住宅大樓 <- new2_住宅大樓$單價[index]
ytest2_住宅大樓 <- new2_住宅大樓$單價[-index]        
a2_住宅大樓 <- data.frame(單價=ytrain2_住宅大樓,xtrain2_住宅大樓)
ad.test(a2_住宅大樓$單價)


ml2_住宅大樓 <- train(單價~.,data = a2_住宅大樓,trControl = data_ctrl,method ="lm")

summ(ml2_住宅大樓$finalModel,robust = T)
summary(ml2_住宅大樓$finalModel)
plot(ml2_住宅大樓$finalModel)
durbinWatsonTest(ml2_住宅大樓$finalModel)
ncvTest(ml2_住宅大樓$finalModel)
plot(ecdf(abs(ml2_住宅大樓$finalModel$residuals/sd(ytest2_住宅大樓))),main="ASD chart",xlab="Residual/STD",ylab="Cumulative distrubtion",col="darkblue")

predict2_住宅大樓 <- predict(ml2_住宅大樓$finalModel,newdata=data.frame(xtest2_住宅大樓))

b2_住宅大樓 <-data.frame(cbind(ytest2_住宅大樓,round(predict2_住宅大樓,1)))
colnames(b2_住宅大樓) <- c("真實價格_住宅大樓","預測價格_住宅大樓")
b2_住宅大樓$誤差 <- ((b2_住宅大樓$真實價格_住宅大樓-b2_住宅大樓$預測價格_住宅大樓)^2)^0.5
apply(b2_住宅大樓,2,mean)
apply(b2_住宅大樓,2,sd)
mean(b2_住宅大樓$誤差)
sd(b2_住宅大樓$誤差)
b2_住宅大樓$左預測區間 <- round(b2_住宅大樓$預測價格_住宅大樓 - 1.96*sd(b2_住宅大樓$誤差),1)
b2_住宅大樓$右預測區間 <- round(b2_住宅大樓$預測價格_住宅大樓 + 1.96*sd(b2_住宅大樓$誤差),1)
b2_住宅大樓[1:10,]
confidence2_預測 <- predict(ml2_住宅大樓$finalModel, newdata=data.frame(xtest2_住宅大樓), interval = 'confidence')
prediction2_預測 <- predict(ml2_住宅大樓$finalModel, newdata=data.frame(xtest2_住宅大樓), interval = 'prediction')

套合_2 <- data.frame(fitted(ml2_住宅大樓$finalModel))
套合_2[-index,]
cbind(ytest2_住宅大樓,confidence2_預測,套合_2[-index,])

#前3群 依據summary(part1)
new3_住宅大樓 <- 住宅大樓[,c(12,13,14,21,28,37,5,9,16,22,25,34)]
#smp3_siz = floor(0.7*nrow(new3_住宅大樓))
x3_住宅大樓 <- model.matrix(單價~.,data=new3_住宅大樓)[,-1]
x3_住宅大樓[1:5,]

xtrain3_住宅大樓 <- x3_住宅大樓[index,]
xtest3_住宅大樓 <- x3_住宅大樓[-index,]
ytrain3_住宅大樓 <- new3_住宅大樓$單價[index]
ytest3_住宅大樓 <- new3_住宅大樓$單價[-index]        
a3_住宅大樓 <- data.frame(單價=ytrain3_住宅大樓,xtrain3_住宅大樓)
ad.test(a3_住宅大樓$單價)

ml3_住宅大樓 <- train(單價~.,data = a3_住宅大樓,trControl = data_ctrl,method ="lm")

summ(ml3_住宅大樓$finalModel,robust = T)
summary(ml3_住宅大樓$finalModel)

plot(ml3_住宅大樓$finalModel)
durbinWatsonTest(ml3_住宅大樓$finalModel)
ncvTest(ml3_住宅大樓$finalModel)
plot(ecdf(abs(ml3_住宅大樓$finalModel$residuals/sd(ytest3_住宅大樓))),main="ASD chart",xlab="Residual/STD",ylab="Cumulative distrubtion",col="darkblue")

predict3_住宅大樓 <- predict(ml3_住宅大樓$finalModel,newdata=data.frame(xtest3_住宅大樓))

b3_住宅大樓 <-data.frame(cbind(ytest3_住宅大樓,round(predict3_住宅大樓,1)))
colnames(b3_住宅大樓) <- c("真實價格_住宅大樓","預測價格_住宅大樓")
b3_住宅大樓$誤差 <- ((b3_住宅大樓$真實價格_住宅大樓-b3_住宅大樓$預測價格_住宅大樓)^2)^0.5
apply(b3_住宅大樓,2,mean)
apply(b3_住宅大樓,2,sd)
mean(b3_住宅大樓$誤差)
sd(b3_住宅大樓$誤差)
b3_住宅大樓$左預測區間 <- round(b3_住宅大樓$預測價格_住宅大樓 - 1.96*sd(b3_住宅大樓$誤差),1)
b3_住宅大樓$右預測區間 <- round(b3_住宅大樓$預測價格_住宅大樓 + 1.96*sd(b3_住宅大樓$誤差),1)
b3_住宅大樓[1:10,]
confidence3_預測 <- predict(ml3_住宅大樓$finalModel, newdata=data.frame(xtest3_住宅大樓), interval = 'confidence')
prediction3_預測 <- predict(ml3_住宅大樓$finalModel, newdata=data.frame(xtest3_住宅大樓), interval = 'prediction')

套合_3 <- data.frame(fitted(ml3_住宅大樓$finalModel))
套合_3[-index,]
cbind(ytest3_住宅大樓,confidence3_預測,套合_3[-index,])

#前2群 依據summary(part1)
new4_住宅大樓 <- 住宅大樓[,c(12,13,14,21,28,37,5,22,25,34)]

x4_住宅大樓 <- model.matrix(單價~.,data=new4_住宅大樓)[,-1]
x4_住宅大樓[1:5,]

xtrain4_住宅大樓 <- x4_住宅大樓[index,]
xtest4_住宅大樓 <- x4_住宅大樓[-index,]
ytrain4_住宅大樓 <- new4_住宅大樓$單價[index]
ytest4_住宅大樓 <- new4_住宅大樓$單價[-index]        
a4_住宅大樓 <- data.frame(單價=ytrain4_住宅大樓,xtrain4_住宅大樓)
ad.test(a4_住宅大樓$單價)

ml4_住宅大樓 <- train(單價~.,data = a4_住宅大樓,trControl = data_ctrl,method ="lm")


summ(ml4_住宅大樓$finalModel,robust = T)
summary(ml4_住宅大樓$finalModel)

plot(ml4_住宅大樓$finalModel)
durbinWatsonTest(ml4_住宅大樓$finalModel)
ncvTest(ml4_住宅大樓$finalModel)
plot(ecdf(abs(ml4_住宅大樓$finalModel$residuals/sd(ytest4_住宅大樓))),main="ASD chart",xlab="Residual/STD",ylab="Cumulative distrubtion",col="darkblue")

predict4_住宅大樓 <- predict(ml4_住宅大樓$finalModel,newdata=data.frame(xtest4_住宅大樓))

b4_住宅大樓 <-data.frame(cbind(ytest4_住宅大樓,round(predict4_住宅大樓,1)))
colnames(b4_住宅大樓) <- c("真實價格_住宅大樓","預測價格_住宅大樓")
b4_住宅大樓$誤差 <- ((b4_住宅大樓$真實價格_住宅大樓-b4_住宅大樓$預測價格_住宅大樓)^2)^0.5
apply(b4_住宅大樓,2,mean)
apply(b4_住宅大樓,2,sd)
mean(b4_住宅大樓$誤差)
sd(b4_住宅大樓$誤差)
b4_住宅大樓$左預測區間 <- round(b4_住宅大樓$預測價格_住宅大樓 - 1.96*sd(b4_住宅大樓$誤差),1)
b4_住宅大樓$右預測區間 <- round(b4_住宅大樓$預測價格_住宅大樓 + 1.96*sd(b4_住宅大樓$誤差),1)
b4_住宅大樓[1:10,]
confidence4_預測 <- predict(ml4_住宅大樓$finalModel, newdata=data.frame(xtest4_住宅大樓), interval = 'confidence')
prediction4_預測 <- predict(ml4_住宅大樓$finalModel, newdata=data.frame(xtest4_住宅大樓), interval = 'prediction')

套合_4 <- data.frame(fitted(ml4_住宅大樓$finalModel))
套合_4[-index,]
cbind(ytest4_住宅大樓,confidence4_預測,套合_4[-index,])

#第1群 依據summary(part1)
new5_住宅大樓 <- 住宅大樓[,c(12,13,14,21,28,37,22,34)]

x5_住宅大樓 <- model.matrix(單價~.,data=new5_住宅大樓)[,-1]
x5_住宅大樓[1:5,]

xtrain5_住宅大樓 <- x5_住宅大樓[index,]
xtest5_住宅大樓 <- x5_住宅大樓[-index,]
ytrain5_住宅大樓 <- new5_住宅大樓$單價[index]
ytest5_住宅大樓 <- new5_住宅大樓$單價[-index]        
a5_住宅大樓 <- data.frame(單價=ytrain5_住宅大樓,xtrain5_住宅大樓)
ad.test(a5_住宅大樓$單價)

ml5_住宅大樓 <- train(單價~.,data = a5_住宅大樓,trControl = data_ctrl,method ="lm")

summ(ml5_住宅大樓$finalModel,robust = T)
summary(ml5_住宅大樓$finalModel)

plot(ml5_住宅大樓$finalModel)
durbinWatsonTest(ml5_住宅大樓$finalModel)
ncvTest(ml5_住宅大樓$finalModel)
plot(ecdf(abs(ml5_住宅大樓$finalModel$residuals/sd(ytest5_住宅大樓))),main="ASD chart",xlab="Residual/STD",ylab="Cumulative distrubtion",col="darkblue")

predict5_住宅大樓 <- predict(ml5_住宅大樓$finalModel,newdata=data.frame(xtest5_住宅大樓))

b5_住宅大樓 <-data.frame(cbind(ytest5_住宅大樓,round(predict5_住宅大樓,1)))
colnames(b5_住宅大樓) <- c("真實價格_住宅大樓","預測價格_住宅大樓")
b5_住宅大樓$誤差 <- ((b5_住宅大樓$真實價格_住宅大樓-b5_住宅大樓$預測價格_住宅大樓)^2)^0.5
apply(b5_住宅大樓,2,mean)
apply(b5_住宅大樓,2,sd)
mean(b5_住宅大樓$誤差)
sd(b5_住宅大樓$誤差)
b5_住宅大樓$左預測區間 <- round(b5_住宅大樓$預測價格_住宅大樓 - 1.96*sd(b5_住宅大樓$誤差),1)
b5_住宅大樓$右預測區間 <- round(b5_住宅大樓$預測價格_住宅大樓 + 1.96*sd(b5_住宅大樓$誤差),1)
b5_住宅大樓[1:10,]
confidence5_預測 <- predict(ml5_住宅大樓$finalModel, newdata=data.frame(xtest5_住宅大樓), interval = 'confidence')
prediction5_預測 <- predict(ml5_住宅大樓$finalModel, newdata=data.frame(xtest5_住宅大樓), interval = 'prediction')

套合_5 <- data.frame(fitted(ml5_住宅大樓$finalModel))
套合_5[-index,]
cbind(ytest5_住宅大樓,confidence5_預測,套合_5[-index,])





































                                                       
