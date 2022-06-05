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
par(mfrow=c(1,1)) 
options(scipen = 9999)
#setwd("D:/User/Desktop/Program/R/ho")
data <- fread("data.csv",stringsAsFactors=FALSE)
data = data %>% filter(交易別 == '不動產' & 備註資訊 == '') %>% filter(`單價(萬/坪)` > 0) %>% filter(交易標的 == '房地(土地+建物)' | 交易標的 == '房地(土地+建物)+車位') %>% filter(經度>120.1032 & 經度<121.0115) %>% filter (緯度>22.28 & 緯度<23.28) %>% select(c(1:45,-交易別,-交易別註記,-備註資訊))
data[data == "",] <- NA
data[data == "NA",] <- NA
data[data == "N.A.",] <- NA

NAS <-which(colSums(is.na(data))>0)
NASdata <- data[,c(16,17,18,25,26,27,28,29,30,37,38)]

NMiss<-function(x){sum(is.na(x))}
apply(NASdata,2,NMiss)

#aggr_plot<-aggr(NASdata,numbers=TRUE,sortVars=TRUE,labels=names(NASdata),cex.axis=.5,gap=3,ylab=c("Histogram of missing data","Pattern"))

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
N_data_住宅大樓<-住宅大樓[,c(12,13,14,15,19,21,28,30,31,32,35,36,37,38)]
cor_numVar_住宅大樓 <- cor(scale(N_data_住宅大樓))
#corrplot.mixed(cor_numVar_住宅大樓, tl.col="black", tl.pos = "lt")




















                                                       
