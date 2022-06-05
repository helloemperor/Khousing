library(rio)
library(shinythemes)
library(plotly)
library(tidyverse)
library(data.table)
library(dplyr)
library(mice)
library(ggthemes)
library(gridExtra)
library(GGally)
library(VIM)
library(magrittr)
library(dummies)
library(fastDummies)
library(corrplot)
library(leaps)
library(ggfortify)
library(car)
library(Metrics)
library(nortest)
library(jtools)
library(sandwich)
library(forcats)
library(ClustOfVar)
library(shiny)
library(leaflet)
library(markdown)
library(caret)
library(ggplot2)
setwd("D:/User/Desktop/Program/R/ho/Khousing")
#資料先前處理
#source("reclean.R", encoding = "UTF-8")
#########################################################
#recleanR的部分
par(mfrow=c(1,1)) 
options(scipen = 9999)
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

############################################################################


#原資料
KHousing <- 住宅大樓
#畫地圖用
house_da <-dplyr::select(KHousing,門牌,使用分區,屋齡,總價,單價,
                         建物移轉坪數,經度,緯度,鄉鎮市區,管理組織,移轉地上樓層)
#設定selectinput的值
鄉鎮市區 <- distinct(KHousing, 鄉鎮市區)
使用分區 <- distinct(KHousing, 使用分區)
管理組織 <- distinct(KHousing, 管理組織)
鄉鎮市區 <- rbind("全部",鄉鎮市區,use.names=FALSE)
colnames(鄉鎮市區) <- "鄉鎮市區"
使用分區 <- rbind("全部",使用分區,use.names=FALSE)
colnames(使用分區) <- "使用分區"
管理組織 <- rbind("全部",管理組織,use.names=FALSE)
colnames(管理組織) <- "管理組織"
房屋屋齡 <- data.frame(c("全部","5年內新屋","5-10年內成屋","10-20年內成屋","20-30年內成屋","30年以上老屋"))
房屋坪數 <- data.frame(c("全部","40坪以內","40坪到60坪內","60坪到80坪內","80坪到100坪內","100坪以上"))
names(房屋屋齡)<-"屋齡"
names(房屋坪數)<-"坪數"
#集群用
KHousingcl <- as.data.frame(KHousing)
#模型用
cldata <- KHousingcl
#####################################################
ui =
shinyUI(
    fluidPage(
#########################################
#大標    
theme = "www/style.css",
list(tags$head(HTML('<link rel="icon", href="www/logo2.png", 
                                   type="png" />'))),
div(style="padding: 1px 0px; width: '100%'",
    titlePanel(
      title="", windowTitle="高雄房價分析系統"
    )),


navbarPage( inverse = TRUE,
            id = "tabs",
            title = div(
              img(src = "logo2.png", height = 25, width = 30),
              "　高雄房價分析系統"),
#######################################################################
#首頁
           tabPanel("首頁",
                    img(class="img-kggle",
                        src="kao.jpg",height = 180  , width = 270 
                    ),
                    p(),
                    includeMarkdown("home.md")
           ),
####################################################################  
#數據資集         
           tabPanel("數據資料集",
                    h4("數據資料集"),
                    DT::dataTableOutput("dataTable"),
                    downloadButton("download_data", "Save data"),
           ),
####################################################################
 #數據探索       
 navbarMenu("數據探索",
            tabPanel("各區平均單價",
                mainPanel(width = 12,plotlyOutput("cost",width="100%",height = "600px"))),
            tabPanel("各區平均屋齡",
                mainPanel(width = 12,plotlyOutput("old",width="100%",height = "600px"))),
            tabPanel("房屋使用分區",
                mainPanel(width = 12,plotlyOutput("rank",width="100%",height = "600px"))),                           
            tabPanel("各區平均坪數",
                mainPanel(width = 12,plotlyOutput("big",width="100%",height = "600px"))),                              
            tabPanel("各區平均成交量",
                mainPanel(width = 12,plotlyOutput("sell",width="100%",height = "600px")))
 ),
            
###################################################################           
   #集群分析        
           tabPanel("集群分析",
                    sidebarLayout(
                        sidebarPanel(
                            checkboxGroupInput("showVars1", "請選擇數值變數",
                                               names(KHousingcl[,c(12,13,14,15,19,21,28,29,30,31,32,35,36,37,38)]),selected = c("單價")),
                            checkboxGroupInput("showVars2", "請選擇類別變數",
                                               names(KHousingcl[,c(5,9,16,20,22,25,33,34)]),selected = c("鄉鎮市區","管理組織")),
                            hr(),
                        #    includeMarkdown("variables.md")
                        ),
                        mainPanel(
                            uiOutput('clmath'),
                            verbatimTextOutput("clinfo"),
                            plotOutput("Plot"),
                            downloadButton("download_plot", "Save image"),
                            h4("Data"),
                            DT::dataTableOutput("clData"),
                        downloadButton("download_clData", "Save data")
                     )
                  )
          ),
 #########################################################################################   
 
#地圖
        tabPanel("地圖",value="地圖",icon=icon("globe-asia"),
             #可選取區域,建物型態,屋齡,坪數,有無車位
             fluidRow(div(style = "height:0px"),
               column(width=1),
               column(width=2, 
                      selectInput("block","鄉鎮市區",c(鄉鎮市區),selectize=F)),
               column(width=2, 
                     selectInput("structure","使用分區",c(使用分區),selectize=F)),
               column(width=2, 
                      selectInput("age","屋齡",c(房屋屋齡),selectize=F)),
               column(width=2, 
                      selectInput("size","坪數",c(房屋坪數),selectize=F)),
               column(width=2, 
                      selectInput("manage","管理組織",c(管理組織),selectize=F)),
            
             mainPanel(width = 12,div(style = "height:0px"),
                       #輸出map
                       column(width=12, 
                              leafletOutput("map",width = "100%",height = 600))
                       ) )
                     ),
##################################################################################
#模型
#小物件   
           tabPanel("迴歸模型",
                    sidebarLayout(
                        sidebarPanel(
                            conditionalPanel(condition="input.action=='原始數據集'", 
                                            sliderInput("selectPercent", "請分割資料集與測試集: ", 
                                                        min = 0.6, max = 1, value = 0.8, step = 0.1),
                                            hr(),
                                            checkboxGroupInput("inVars1", "請選擇數值變數",
                                               names(KHousingcl[,c(12,13,14,15,19,21,28,29,30,31,32,35,36,38)]),selected = c("建物數量","總價")),
                            checkboxGroupInput("inVars2", "請選擇類別變數",
                                               names(KHousingcl[,c(5,9,16,20,22,25,33,34)]),selected = c("鄉鎮市區","管理組織")),
                                            hr()
                                   #         includeMarkdown("variables.md")
                            ),
#####################################################################
#面板                            
                            conditionalPanel(condition="input.action=='使用者自訂'",
                                             conditionalPanel(condition="input.inputVars.includes('土地數量')",
                                                              sliderInput("select土地數量", "土地數量:", 
                                                                          min = 0., max = 50, 
                                                                          value = 4, step = 1)
                                             ),
                                             conditionalPanel(condition="input.inputVars.includes('建物數量')",
                                                              sliderInput("select建物數量", "建物數量:", 
                                                                          min = 0., max = 50, 
                                                                          value = 10, step = 1)
                                             ),
                                             conditionalPanel(condition="input.inputVars.includes('移轉地上樓層')",
                                                              sliderInput("select移轉地上樓層", "移轉地上樓層:", 
                                                                          min = 0., max = 85, 
                                                                          value = 11, step = 1)
                                             ),
                                             conditionalPanel(condition="input.inputVars.includes('車位數量')",
                                                              sliderInput("select車位數量", "車位數量:", 
                                                                          min = 0., max = 20, 
                                                                          value = 1, step = 1),
              
                                             ),
                                             uiOutput("ui")
                            )
                        ),
############################################################################################
#原始數據集
                        mainPanel(
                            tabsetPanel(
                                id = "action",
                                tabPanel("原始數據集", 
                                         h4("迴歸模型表現(訓練集):"),
                                         verbatimTextOutput("trainingSummary"),
                                         h6("雙擊放大畫面"),

                                         plotOutput("trainPlot",  
                                                    dblclick = "trainplot_dblclick",
                                                    brush = brushOpts(
                                                      id = "trainplot_brush",
                                                      resetOnNew = TRUE)),

                                         downloadButton("download_trainplot", "Save image"),

                                         h4("迴歸模型表現(測試集):"),
                                         verbatimTextOutput("testingSummary"),
                                         h6("雙擊放大畫面"),

                                         plotOutput("testPlot",  
                                                    dblclick = "testplot_dblclick",
                                                    brush = brushOpts(
                                                      id = "testplot_brush",
                                                      resetOnNew = TRUE)),

                                         downloadButton("download_testplot", "Save image")
                                         ),

#####################################################################################################
#自訂                       
                                tabPanel("使用者自訂", 
                                         h4("您的輸入"),
                                         tableOutput("userInputinfo"),
                                         h4("預測單價為:"),
                                         verbatimTextOutput("userInputresult")
                                         )
                            )
                        )
                    )
           )



    )
  )
)

 #####################################################################################


server = function(input, output, session) {
    #數據資料集
         # Download data
     output$download_data <- downloadHandler(
         filename = "KHousing.csv",
         content = function(file) {
             write.csv(KHousing, file, row.names = FALSE)
         })
     
     # Output entire dataset
     output$dataTable <- DT::renderDataTable({
         DT::datatable(KHousing)
     })
########################################################
   #地圖

     #讀取selectinput建物和使用分區的內容
  store1<-reactive({
   
    if(input$block=="全部"|input$block==""){
      if(input$structure=="全部"|input$structure==""){
        a<-house_da
      }
      else{
        a<-filter(house_da,使用分區==input$structure)
      }
    }
    else{
      if(input$structure=="全部"|input$structure==""){
        a<-filter(house_da,鄉鎮市區==input$block)
      }
      else{
        a<-filter(house_da,鄉鎮市區==input$block,使用分區==input$structure)
      }
    }

  })

  #讀取selectinput屋齡區間的內容
  store2<-reactive({
    
    if(input$age=="全部"){
        a<-house_da
    }
    else{
        if(input$age=="5年內新屋"){
          a<-filter(house_da,屋齡<5)
        }
        else if(input$age=="5-10年內成屋"){
          a<-filter(house_da,5<=屋齡&屋齡<10)
        }
        else if(input$age=="10-20年內成屋"){
          a<-filter(house_da,屋齡<20&屋齡>=10)
        }
        else if(input$age=="20-30年內成屋"){
          a<-filter(house_da,屋齡<30&屋齡>=20)
        }
        else if(input$age=="30年以上老屋"){
          a<-filter(house_da,屋齡>=30)
        }
       }
    })
  #讀取selectinput坪數區間的內容
  store3<-reactive({
    if(input$size=="全部"){
      a<-house_da
    }
    else{
      if(input$size=="40坪以內"){
        a<-filter(house_da,建物移轉坪數<40)
      }
      else if(input$size=="40坪到60坪內"){
        a<-filter(house_da,建物移轉坪數<60&建物移轉坪數>=40)
      }
      else if(input$size=="60坪到80坪內"){
        a<-filter(house_da,建物移轉坪數<80&建物移轉坪數>=60)
      }
      else if(input$size=="80坪到100坪內"){
        a<-filter(house_da,建物移轉坪數<100&建物移轉坪數>=80)
      }
      else if(input$size=="100坪以上"){
        a<-filter(house_da,建物移轉坪數>=100)
      }
    }
  })
  
  #讀取selectinput管理組織的內容
  store4<-reactive({
    if(input$manage=="全部"){
      a<-house_da
    }
    else{
      if(input$manage=="Y"){
        a<-filter(house_da,管理組織=="Y")
      }
      else if(input$manage=="N"){
        a<-filter(house_da,管理組織=="N")
      }
    }
  })
    
  ##畫地圖
  output$map <-renderLeaflet({
    x1<-store1()
    x2<-store2()
    x3<-store3()
    x4<-store4()
    
    a<-Reduce(intersect, list(x1,x2,x3,x4))
    
    a$rank_money<- cut(
      a$總價,
      breaks = c(0,500,1000,1500,2000,Inf),
      labels = c('500萬以內','500萬以上1000萬以下','1000萬以上1500萬以下','1500萬以上2000萬以下','2000萬以上'))
    a$color<- cut(a$總價,
                  breaks = c(0,500,1000,1500,2000,Inf),
                  labels = c('black','purple','green','blue','red'))
    #延續上一個步驟，房價限分割完區間，共分了5組，所以用5種顏色來分別標示不同的房價
    pal <- colorFactor(palette = c('black','purple','green',
                                   'blue','red'),domain = a$rank_money)
    rank_list<- split(a,a$rank_money)
    # house_da<-filter(house_da,鄉鎮市區==input$block,使用分區==input$structure)
    map<- leaflet()%>%
      addTiles()%>%                 #addTiles()加上一個預設的地圖資料
      #fitBounds()用來設定地圖的位置，讓畫面可呈現指定的方形區域，這兩組經緯度是高雄市的經緯度位置
      fitBounds(120.1032,22.28,121.0115,23.28)
    names(rank_list) %>% purrr::walk( function(df) {
      map <<-map %>% addAwesomeMarkers(data=rank_list[[df]],icon=awesomeIcons(icon = 'home',markerColor = rank_list[[df]]$color),
                                       clusterOptions = markerClusterOptions(removeOutsideVisibleBounds=F),
                                       lng = rank_list[[df]]$經度,lat =rank_list[[df]]$緯度,popup =paste(sep='<br/>',
                                                                                                     '地址',rank_list[[df]]$門牌,'總價',rank_list[[df]]$總價,'屋齡',
                                                                                                     rank_list[[df]]$屋齡,'坪數',rank_list[[df]]$建物移轉坪數,'單價',rank_list[[df]]$單價,
                                                                                                     '使用分區',rank_list[[df]]$使用分區,'樓層',rank_list[[df]]$移轉地上樓層,'管理組織',rank_list[[df]]$管理組織) ,group= df)
    })
    map<- map%>% addLayersControl(overlayGroups =names(rank_list),
                                  options = layersControlOptions(collapsed = F))%>%
      addLegend(position = 'bottomright',pal=pal,values = a$rank_money,opacity = 1)
    map
  })
############################################################
#數據探索
    #畫平均各區單坪房價
  output$cost <- renderPlotly({
    
     data1<-KHousing
     統計<-group_by(data1,鄉鎮市區)%>%
     summarise(
        平均單價=round(sum(單價)/n(),1 ),
        每區購屋占比_百分比= round( n()/20750*100,2 ),
        每區購買件數=n(),
        每區平均屋齡=round(sum(屋齡)/n(),1))%>%arrange(desc(平均單價))%>%filter(每區購買件數>=100)
     windowsFonts(A=windowsFont("微軟正黑體"))  
     ggplotly(ggplot(data=統計[], aes(x=reorder(鄉鎮市區,-平均單價),y=平均單價,fill=鄉鎮市區)) +
      geom_bar(stat="identity",color="black")+
      ggtitle("各行政區單坪價格")+ 
        labs(x = "高雄各鄉鎮市區", y = "平均單價")+
        theme_minimal(base_size = 20)+
      theme(
            axis.line = element_line(color = "orange", size = 2),
            axis.title = element_text(color = "red", face = "bold",family = "A"),
            axis.ticks = element_line(color = "purple", size = 3),
            axis.text = element_text(color = "blue",face = "bold",family = "A")
            ))
  })
  #畫平均各區販售數
  output$sell <- renderPlotly({
    
     data1<-KHousing
     統計<-group_by(data1,鄉鎮市區)%>%
     summarise(
        平均單價=round(sum(單價)/n(),1 ),
        每區購屋占比_百分比= round( n()/20750*100,2 ),
        每區購買件數=n(),
        每區平均屋齡=round(sum(屋齡)/n(),1))%>%arrange(desc(每區購買件數))%>%filter(每區購買件數>=100)
     windowsFonts(A=windowsFont("微軟正黑體"))  
    
    barplot_zone3<-ggplot(data=統計, aes(x=reorder(鄉鎮市區,-每區購買件數),y=每區購買件數,fill=鄉鎮市區)) +
      geom_bar(stat="identity",color="black")+
      ggtitle("各行政區成交量")+
      labs(x = "鄉鎮市區", y = "成交數量")+
      theme_minimal(base_size = 20)+
      theme(
        axis.line = element_line(color = "orange", size = 2),
        axis.title = element_text(color = "red", face = "bold",family = "A"),
        axis.ticks = element_line(color = "purple", size = 3),
        axis.text = element_text(color = "blue",face = "bold",family = "A")
      )
    
    ggplotly(barplot_zone3)
    
    
  })
  
  #畫平均各區屋齡
  output$old <- renderPlotly({
    
     data1<-KHousing
     統計<-group_by(data1,鄉鎮市區)%>%
     summarise(
        平均單價=round(sum(單價)/n(),1 ),
        每區購屋占比_百分比= round( n()/20750*100,2 ),
        每區購買件數=n(),
        每區平均屋齡=round(sum(屋齡)/n(),1))%>%arrange(desc(每區平均屋齡))%>%filter(每區購買件數>=100)
     windowsFonts(A=windowsFont("微軟正黑體"))  
    
    ggplotly(ggplot(data=統計[], aes(x=reorder(鄉鎮市區,-每區平均屋齡),y=每區平均屋齡,fill=鄉鎮市區)) +
               geom_bar(stat="identity",color="black")+
               ggtitle("各行政區屋齡")+ 
               labs(x = "高雄各鄉鎮市區", y = "每區平均屋齡(年)")+
               theme_minimal(base_size = 20)+
               theme(
                 axis.line = element_line(color = "orange", size = 2),
                 axis.title = element_text(color = "red", face = "bold",family = "A"),
                 axis.ticks = element_line(color = "purple", size = 3),
                 axis.text = element_text(color = "blue",face = "bold",family = "A")
               ))
  })
  #畫各使用分區占比
  output$rank <- renderPlotly({
    
     data1<-KHousing
     統計<-group_by(data1,使用分區)%>%
     summarise(
        平均單價=round(sum(單價)/n(),1 ),
        每區購屋占比_百分比= round( n()/20750*100,2 ),
        每區購買件數=n(),
        每區平均屋齡=round(sum(屋齡)/n(),1))%>%arrange(desc(每區購屋占比_百分比))
     windowsFonts(A=windowsFont("微軟正黑體"))  
    
    barplot_zone3<-ggplot(data=統計, aes(x=reorder(使用分區,-每區購屋占比_百分比),y=每區購屋占比_百分比,fill=使用分區)) +
      geom_bar(stat="identity",color="black")+
      ggtitle("各使用分區占比")+
      labs(x = "使用分區", y = "成交房屋百分比(%)")+
      theme_minimal(base_size = 18)+
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
             axis.line = element_line(color = "orange", size = 2),
             axis.title = element_text(color = "red", face = "bold",family = "A"),
             axis.ticks = element_line(color = "purple", size = 3),
             axis.text = element_text(color = "blue",face = "bold",family = "A")
                                  )
    ggplotly(barplot_zone3)
    
  })

  #畫平均各區坪數
    output$big <- renderPlotly({
    data1<-KHousing
    統計<-group_by(data1,鄉鎮市區)%>%
      summarise(
        單價=round(sum(單價)/n(),1 ),
        每區購屋占比_百分比= round( n()/6957*100,2 ),
        每區購買件數=n(),
        每區平均屋齡=round(sum(屋齡)/n(),1),
        每區平均坪數=round(sum(建物移轉坪數)/n(),1)) %>%arrange(desc(每區平均坪數))
    
    barplot_zone<-ggplot(data=統計, aes(x=reorder(鄉鎮市區,-每區平均坪數),y=每區平均坪數,fill=鄉鎮市區)) +
      geom_bar(colour="black",stat="identity")+
      ggtitle("高雄各區平均坪數")+
      labs(x = "高雄各鄉鎮市區", y = "平均坪數")+
      theme_minimal(base_size = 20)+
      theme(
        axis.line = element_line(color = "orange", size = 2),
        axis.title = element_text(color = "red", face = "bold",family = "A"),
        axis.ticks = element_line(color = "purple", size = 3),
        axis.text = element_text(color = "blue",face = "bold",family = "A")
      )
    ggplotly(barplot_zone)  

  })
##################################################################################################
#集群分析
     # Output 集群 math using MathJax
     output$clmath <- renderUI({
         withMathJax(
             helpText('本研究使用的集群方法是以PCA為基底所開發的ClustOfVar套件，
                      透過套件中的hclustvar函數與cutreevar函數進行本研究變數集群分析，
                      來探討各個集群對於不動產價格能涵蓋與解釋多少訊息量與各集群內變數的變化。')
             
         )
     })
     
    # Output 集群 information
     output$clinfo <- renderText({
         if (length(input$showVars1) < 2 & length(input$showVars2) < 2){
             print("請選取至少兩個變數!")
         }
     })
     
     clplot <- function(){
         if (length(input$showVars1) > 1 | length(input$showVars2) > 1){
            tree <- hclustvar(select(KHousingcl, input$showVars1),select(KHousingcl, input$showVars2))
            plot(tree,main="變數集群")
            } else if(length(input$showVars1) < 1 | length(input$showVars2) >1){
            tree <- hclustvar(select(KHousingcl, input$showVars2))
            plot(tree,main="變數集群")
            } else if(length(input$showVars1) == 1 | length(input$showVars2) == 1 ){
            tree <- hclustvar(select(KHousingcl, input$showVars1),select(KHousingcl, input$showVars2))
            plot(tree,main="變數集群")
            } else if(length(input$showVars1) > 1 | length(input$showVars2) < 1){
            tree <- hclustvar(select(KHousingcl, input$showVars1))
            plot(tree,main="變數集群")
            }

         
     }
     
    # Output plot for 集群
     output$Plot <- renderPlot({
         if (length(input$showVars1) > 1 | length(input$showVars2) > 1){
            tree <- hclustvar(select(KHousingcl, input$showVars1),select(KHousingcl, input$showVars2))
            plot(tree,main="變數集群")
            } else if(length(input$showVars1) < 1 | length(input$showVars2) >1){
            tree <- hclustvar(select(KHousingcl, input$showVars2))
            plot(tree,main="變數集群")
            } else if(length(input$showVars1) == 1 | length(input$showVars2) == 1 ){
            tree <- hclustvar(select(KHousingcl, input$showVars1),select(KHousingcl, input$showVars2))
            plot(tree,main="變數集群")
            } else if(length(input$showVars1) > 1 | length(input$showVars2) < 1){
            tree <- hclustvar(select(KHousingcl, input$showVars1))
            plot(tree,main="變數集群")
            }
     })
     
     # Download cl plot
     output$download_plot <- downloadHandler(
        filename = "cl.png",
         content = function(file) {
             png(file)
             clplot()
             dev.off()
             }
         )
     
    # Output data for selected variable
     output$clData <- DT::renderDataTable({
         tmp <- as.data.frame(KHousingcl[, input$showVars1])
         tmp2 <- as.data.frame(KHousingcl[, input$showVars2])
         colnames(tmp) <- input$showVars1
         colnames(tmp2) <- input$showVars2
         tmpall <- cbind(tmp,tmp2)
         DT::datatable(tmpall)
     })
     
    # # Download selected data
     output$download_clData <- downloadHandler(
         filename = "cl_data.csv",
         content = function(file) {
             tmp <- as.data.frame(KHousingcl[, input$showVars1])
             tmp2 <- as.data.frame(KHousingcl[, input$showVars2])
             colnames(tmp) <- input$showVars1
             colnames(tmp2) <- input$showVars2
             tmpall <- cbind(tmp,tmp2)
             write.csv(tmpall, file, row.names = FALSE)
         })
#########################################################################
#模型     
     
     # 分割數據集跟測試集
      trainIndex <- reactive({ 
         createDataPartition(KHousingcl$單價, 
                              p =input$selectPercent, 
                              list = FALSE, 
                              times = 1)
      })
     
#      # Train model
    trainResult <- reactive({ 
        
        style <- isolate(input$style)
        
        withProgress(message = "Computing...", style = style, value = 0.1, {
            # Get training data
            cltrain <- cldata[trainIndex(),]
            
            # Set training control
            control <- trainControl(method="repeatedcv", number=5, repeats=10)
            seed <- 1000
            set.seed(seed)
            
            incProgress(0.8)
            # Train the model
            if (!is.null(input$inVars1)) {

                train(cltrain[, input$inVars1], cltrain[, "單價"], 
                          method="lm", trControl=control)
                
            }
        })
    })


    testResult <- reactive({ 
        cltest  <- cldata[-trainIndex(),]
        prediction <- predict(trainResult(), newdata = cltest)
        postResample(pred = prediction, obs = cltest$單價)
    })



    output$trainingSummary <- renderPrint({
        if (is.null(input$inVars1)) {
            return("請選擇兩個變數以上")
        } else {
            trainResult()
        }
    })


    # Single zoomable plot
    ranges <- reactiveValues(x = NULL, y = NULL)



    output$trainPlot <- renderPlot({
        if (is.null(input$inVars1)) {
            return("請選擇兩個變數以上")
        } else {
        tmp <- trainResult()
            cltrain <- cldata[trainIndex(),]
            prediction <- predict(tmp, newdata = cltrain)
            observation <- cltrain$單價
            df <- tibble(prediction, observation)
            ggplot(df, aes(x=prediction, y=observation)) + 
                geom_point() + 
                geom_smooth(method=lm) +
                coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
        }
    })




    #
    observeEvent(input$trainplot_dblclick, {
        brush <- input$trainplot_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })




    savetrainplot <- function() {
        if (is.null(input$inVars1)) {
            return("請選擇兩個變數以上")
        } else {
            tmp <- trainResult()
            cltrain <- cldata[trainIndex(),]
            prediction <- predict(tmp, newdata = cltrain)
            observation <- cltrain$單價
            df <- tibble(prediction, observation)
            ggplot(df, aes(x=prediction, y=observation)) + 
                geom_point() + 
                geom_smooth(method=lm) +
                coord_equal()
                
        }
    }

        output$download_trainplot <- downloadHandler(
        filename = "train.png",
        content = function(file) {
            ggsave(savetrainplot(), filename = file)
        })


        output$testingSummary <- renderPrint({
        if (is.null(input$inVars1)) {
            return("請選擇兩個變數以上")
        } else {
            testResult()
        }
    })


        ranges <- reactiveValues(x = NULL, y = NULL)


        output$testPlot <- renderPlot({
        if (is.null(input$inVars1)) {
            return("請選擇兩個變數以上")
        } else {
            cltest  <- cldata[-trainIndex(),]
            prediction <- predict(trainResult(), newdata = cltest)
            observation <- cltest$單價
            df <- tibble(prediction, observation)
            ggplot(df, aes(x=prediction, y=observation)) + 
                geom_point() + 
                geom_smooth(method=lm) +
                coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
        }
    })




        observeEvent(input$testplot_dblclick, {
        brush <- input$testplot_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })




        savetestplot <- function(){
        if (is.null(input$inVars1)) {
            return("請選擇兩個變數以上")
        } else {
            cltest  <- cldata[-trainIndex(),]
            prediction <- predict(trainResult(), newdata = cltest)
            observation <- cltest$單價
            df <- tibble(prediction, observation)
            ggplot(df, aes(x=prediction, y=observation)) + 
                geom_point() + 
                geom_smooth(method=lm) +
                coord_equal()
        }
    }


        output$download_testplot <- downloadHandler(
        filename = "test.png",
        content = function(file) {
            ggsave(savetestplot(), filename = file)
        })

#########################################################################
        #可能調整
        output$ui <- renderUI({
        # Depending on input$inVars1, we'll generate a different
        # UI component and send it to the client.
        if (is.null(input$inVars1)) {
            includeMarkdown("variables_hint.md")
        } else {
            includeMarkdown("variables.md")
        }
    })


        userInput <- reactive({
        # Assign values to corresponding variables
        i <- 0
        tmp_values <- c()
        for (x in input$inVars1) {
            i <- i+1
            tmp_values[i] <- eval(parse(text=paste0("input$select", 
                                                    toupper(substr(x, 1, 1)), 
                                                    substr(x, 2, nchar(x))
            ))
            )
        }
       
        tmp_values
        
    })


        output$userInputinfo <- renderTable({
        # Put them into dataframe
        data.frame(
            Name = input$inVars1,
            Value = as.character(userInput()),
            stringsAsFactors = FALSE)
    })
    
    # Output user input summary
    output$userInputresult <- renderPrint({
        if (is.null(input$inVars1)) {
            return("請選擇兩個變數以上")
        } else {
            df <- as.data.frame(t(userInput()))
            colnames(df) <- input$inVars1
            prediction <- predict(trainResult(), newdata = df)
            cat(prediction)
        }
    })











              # Get training data
#               rtrain <- cldata[trainIndex(),]
#               c1train <- rtrain[, input$inputVars1]
#               c2train <- rtrain[, input$inputVars2]
#               cbtrain <- cbind(c1train,c2train)
#               trainmatrix <- model.matrix(單價~.,data=cbtrain)[,-1]
#               xtrain <- trainmatrix[rtrain,]
#               ytrain <- cbtrain$單價[rtrain]
#               a1ltrain <- data.frame(單價=ytrain,xtrain)
#              # Set training control
#              control <- trainControl(method="repeatedcv", number=5, repeats=10)
#              seed <- 1000
#              set.seed(seed)
#              incProgress(0.8)
#              # Train the model
#              if (!is.null(input$inputVars1) | !is.null(input$inputVars2))  {
# 
#                  
#               train(a1ltrain$單價~., data=a1ltrain, method="lm", trControl=control)
# 
#              }
# 
#          })
#      })
# 
#     # Test model
#      testResult <- reactive({ 
#          rtest  <- cldata[-trainIndex(),]
#          c1test <- rtest[, input$inputVars1]
#          c2test <- rtest[, input$inputVars2]
#          cbtest <- cbind(c1test,c2test)     
#          testmatrix <- model.matrix(單價~.,data=cbtest)[,-1]
#          xtest <- trainmatrix[rtest,]
#          ytest <- cbtest$單價[rtest]
#          a1ltest <- data.frame(單價=ytest,xtest)    
# 
#          prediction <- predict(trainResult(), newdata = a1ltest)
#          postResample(pred = prediction, obs = a1ltest$單價)
#      })
#      
#      # Output train summary
#      output$trainingSummary <- renderPrint({
#          if (!is.null(input$inputVars1) | !is.null(input$inputVars2)) {
#              return("Please select at least one predictor.")
#          } else {
#              trainResult()
#          }
#      })
#      
#      # Single zoomable plot
#      ranges <- reactiveValues(x = NULL, y = NULL)
#      
#     # Output train plot
#      output$trainPlot <- renderPlot({
#          if (!is.null(input$inputVars1) | !is.null(input$inputVars2)) {
#              return("Please select at least one predictor.")
#          } else {
#          tmp <- trainResult()
#              rtrain <- cldata[trainIndex(),]
#               c1train <- rtrain[, input$inputVars1]
#               c2train <- rtrain[, input$inputVars2]
#               cbtrain <- cbind(c1train,c2train)
#               trainmatrix <- model.matrix(單價~.,data=cbtrain)[,-1]
#               xtrain <- trainmatrix[rtrain,]
#               ytrain <- cbtrain$單價[rtrain]
#               a1ltrain <- data.frame(單價=ytrain,xtrain)
# 
#              prediction <- predict(tmp, newdata = a1ltrain)
#              observation <- a1ltrain$單價
#              df <- tibble(prediction, observation)
#              ggplot(df, aes(x=prediction, y=observation)) + 
#                  geom_point() + 
#                  geom_smooth(method=lm) +
#                  coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
#          }
#      })
#     # 
#     # # When a double-click happens, check if there's a brush on the plot.
#     # # If so, zoom to the brush bounds; if not, reset the zoom.
#      observeEvent(input$trainplot_dblclick, {
#          brush <- input$trainplot_brush
#          if (!is.null(brush)) {
#              ranges$x <- c(brush$xmin, brush$xmax)
#              ranges$y <- c(brush$ymin, brush$ymax)
#              
#          } else {
#              ranges$x <- NULL
#              ranges$y <- NULL
#          }
#      })
#      
#     # For downloading train plot
#      savetrainplot <- function() {
#          if (!is.null(input$inputVars1) | !is.null(input$inputVars2)) {
#              return("Please select at least one predictor.")
#          } else {
#              rtrain <- cldata[trainIndex(),]
#               c1train <- rtrain[, input$inputVars1]
#               c2train <- rtrain[, input$inputVars2]
#               cbtrain <- cbind(c1train,c2train)
#               trainmatrix <- model.matrix(單價~.,data=cbtrain)[,-1]
#               xtrain <- trainmatrix[rtrain,]
#               ytrain <- cbtrain$單價[rtrain]
#               a1ltrain <- data.frame(單價=ytrain,xtrain)
#              prediction <- predict(tmp, newdata = a1ltrain)
#              observation <- a1ltrain$單價
#              df <- tibble(prediction, observation)
#              ggplot(df, aes(x=prediction, y=observation)) + 
#                  geom_point() + 
#                  geom_smooth(method=lm) +
#                  coord_equal()
#                  
#          }
#      }
#     
#         
#     # Download train plot
#      output$download_trainplot <- downloadHandler(
#          filename = "train.png",
#          content = function(file) {
#              ggsave(savetrainplot(), filename = file)
#          })
#      
#     # # Output test summary
#      output$testingSummary <- renderPrint({
#          if (!is.null(input$inputVars1) | !is.null(input$inputVars2)) {
#              return("Please select at least one predictor.")
#          } else {
#              testResult()
#          }
#      })
#     
#     # # Single zoomable plot
#      ranges <- reactiveValues(x = NULL, y = NULL)
#      
#     # # Output test plot
#      output$testPlot <- renderPlot({
#          if (!is.null(input$inputVars1) | !is.null(input$inputVars2)) {
#              return("Please select at least one predictor.")
#          } else {
#          rtest  <- cldata[-trainIndex(),]
#          c1test <- rtest[, input$inputVars1]
#          c2test <- rtest[, input$inputVars2]
#          cbtest <- cbind(c1test,c2test)     
#          testmatrix <- model.matrix(單價~.,data=cbtest)[,-1]
#          xtest <- trainmatrix[rtest,]
#          ytest <- cbtest$單價[rtest]
#          a1ltest <- data.frame(單價=ytest,xtest)   
#         prediction <- predict(trainResult(), newdata = a1ltest)
#         observation <- a1ltest$單價 
# 
#              df <- tibble(prediction, observation)
#              ggplot(df, aes(x=prediction, y=observation)) + 
#                  geom_point() + 
#                  geom_smooth(method=lm) +
#                  coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
#          }
#      })
#     # # When a double-click happens, check if there's a brush on the plot.
#     # # If so, zoom to the brush bounds; if not, reset the zoom.
#      observeEvent(input$testplot_dblclick, {
#          brush <- input$testplot_brush
#          if (!is.null(brush)) {
#              ranges$x <- c(brush$xmin, brush$xmax)
#              ranges$y <- c(brush$ymin, brush$ymax)
#              
#          } else {
#              ranges$x <- NULL
#              ranges$y <- NULL
#          }
#      })
#      
#      savetestplot <- function(){
#          if ((!is.null(input$inputVars1) | !is.null(input$inputVars2))) {
#              return("Please select at least one predictor.")
#          } else {
#              rtest  <- cldata[-trainIndex(),]
#              c1test <- rtest[, input$inputVars1]
#              c2test <- rtest[, input$inputVars2]
#              cbtest <- cbind(c1test,c2test)     
#              testmatrix <- model.matrix(單價~.,data=cbtest)[,-1]
#              xtest <- trainmatrix[rtest,]
#              ytest <- cbtest$單價[rtest]
#              a1ltest <- data.frame(單價=ytest,xtest)   
#              prediction <- predict(trainResult(), newdata = a1ltest)
#              observation <- a1ltest$單價 
# 
#              df <- tibble(prediction, observation)
#              ggplot(df, aes(x=prediction, y=observation)) + 
#                  geom_point() + 
#                  geom_smooth(method=lm) +
#                  coord_equal()
#          }
#      }
#          
#     # Download test plot
#      output$download_testplot <- downloadHandler(
#          filename = "test.png",
#          content = function(file) {
#              ggsave(savetestplot(), filename = file)
#          })
# #################################################################################
# #使用者自訂     
#     # Dynamic UI for user input hint
#      output$ui <- renderUI({
#          # Depending on input$inputVars, we'll generate a different
#          # UI component and send it to the client.
#          if ((!is.null(input$inputVars1) | !is.null(input$inputVars2))) {
#              includeMarkdown("variables_hint.md")
#          } else {
#              includeMarkdown("variables.md")
#          }
#      })
#      
#      userInput1 <- reactive({
#          # Assign values to corresponding variables
#          i <- 0
#          
#          tmp_values1 <- c()
#          for (x in input$inputVars1) {
# 
#              i <- i+1
#              tmp_values1[i] <- eval(parse(text=paste0("input$select", 
#                                                      toupper(substr(x, 1, 1)), 
#                                                      substr(x, 2, nchar(x))
#              ))
#              )
#          }
# 
#          tmp_values1
#      })
# 
#     userInput2 <- reactive({
#          # Assign values to corresponding variables
#          i <- 0
#          tmp_values1 <- c()
#          for (x in input$inputVars2) {
#              i <- i+1
#              tmp_values1[i] <- eval(parse(text=paste0("input$select", 
#                                                      toupper(substr(x, 1, 1)), 
#                                                      substr(x, 2, nchar(x))
#              ))
#              )
#          }
#          tmp_values1
#      })
# 
# 
# 
#     
#     # Show the values in an HTML table ----
#      output$userInputinfo1 <- renderTable({
#          # Put them into dataframe
#          data.frame(
#              Name = input$inputVars1,
#              Value = as.character(userInput1()),
#              stringsAsFactors = FALSE)
#      })
#      output$userInputinfo2 <- renderTable({
#          # Put them into dataframe
#          data.frame(
#              Name = input$inputVars2,
#              Value = as.character(userInput2()),
#              stringsAsFactors = FALSE)
#      })
# 
# 
# 
#      
#     # # Output user input summary
#      output$userInputresult <- renderPrint({
#          if (!is.null(input$inputVars1) | !is.null(input$inputVars2)) {
#              return("請選擇至少兩個變數")
#          } else {
#              df1 <- as.data.frame(t(userInput1()))
#              df2 <- as.data.frame(t(userInput2()))
#              colnames(df1) <- input$inputVars1
#              colnames(df2) <- input$inputVars2
#              cbdf <- cbind(df1,df2)
#              dfmatrix <- model.matrix(單價~.,data = cbdf)
#              prediction <- predict(trainResult(), newdata = dfmatrix)
#              cat(prediction)
#          }
#      })
#      

    
}

shinyApp(ui = ui, server = server)