##此部分已經在shiny上
##https://davidwebb.shinyapps.io/movie/
library(shiny)
library(timeDate)
library(xml2)
library(xmlview)
library(ggplot2)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("台北周末電影票房"),
  sidebarLayout(
    sidebarPanel(
      # Sidebar with a slider input for number of bins 
      # Application title
      
      selectInput("select1",label = h3("日期:"),choices= list("2017年"="2017")
                  ,selected = sub(Sys.Date(),1,4)),
      selectInput("select2", label=NULL,
                  choices = list("一月" = "01", "二月" = "02", "三月" = "03",
                                 "四月" = "04", "五月" = "05", "六月" = 06,
                                 "七月" = "07", "八月" = "08", "九月" = 09,
                                 "十月" = 10, "十一月" = 11, "十二月"=12), 
                  selected = substr(Sys.Date(),6,7)),
      selectInput("select3",label=NULL,choices= list("第一周"=1,"第二周"=2,
                                                     "第三周"=3,"第四周"=4,
                                                     "第五周"=5)
                  ,selected = 2)
    ),
    
    # Show a plot of the generated distribution
    
    tabsetPanel(
      id = 'dataset',
      tabPanel('表',  h3(strong(textOutput("text"))),"單位:百萬元",tableOutput('table')
               ,"來源:開眼電影網"),
      tabPanel('圖', h3(strong(textOutput("text1"))),plotOutput('plot1'))
    )
  )
))
shinyServer(function(input, output) {
  code<- reactive({
    tempdate1<-paste(input$select1,input$select2,sep="-")
    tempdate1<-paste(tempdate1,"01",sep="-")
    tempdate1<-timeNthNdayInMonth(tempdate1, 5, as.numeric(input$select3))
    tempdate1<-as.character(tempdate1)
    ##資料處理
    ##日期轉換
    #輸入幾月第幾周查出日期
    ##tempDate1<-timeNthNdayInMonth("2017-03-01", 5, 3)
    url   <- "http://app2.atmovies.com.tw/boxoffice/twweekend/jjjj/"
    url<-sub("jjjj",replacement =tempdate1,url)
    ##加入除錯機制
    tryCatch({
      read_html(url)
    }, warning = function(w) {
      
    }, error = function(e) {
      
    })
  })
  ##統計時間
  date1<-reactive({
    doc<-code()  
    date1 <- "//*[@class='boDate']"
    if(!is.null(doc)){
      xml_text(xml_find_all(doc, date1))
    }else{
      ""
    }
  })
  ##資料明細
  selected1 <- reactive({
    doc<-NULL
    doc<-code()
    
    if(!is.null(doc)){ 
      xpathmoviename <- "//*[@id='main']//table[2]//td[2]"
      moviename<-xml_text(xml_find_all(doc, xpathmoviename))
      
      moviename<-moviename[(1:20)*2-1]
      ##本周票房
      xpathmoviemoney <- "//*[@id='main']//table[2]//td[3]"
      moviemoney<-xml_text(xml_find_all(doc, xpathmoviemoney))
      ##取消錢字號
      moviemoney<-gsub("(\\$)","",moviemoney)
      moviemoney<-gsub(",","",moviemoney)
      ##累積票房
      xpathmoviemoney2 <- "//*[@id='main']//table[2]//td[4]"
      moviemoney2<-xml_text(xml_find_all(doc, xpathmoviemoney2))
      ##取消錢字號
      moviemoney2<-gsub("(\\$)","",moviemoney2)
      moviemoney2<-gsub(",","",moviemoney2)
      ##上週排行
      xpathlastweek <- "//*[@id='main']//table[2]//td[5]"
      LastweekRank<-xml_text(xml_find_all(doc, xpathlastweek))
      LastweekRank<-gsub(" ","",LastweekRank)
      ##上榜週數
      xpathallweek <- "//*[@id='main']//table[2]//td[6]"
      Allweek<-xml_text(xml_find_all(doc, xpathallweek))
      Allweek<-gsub(" ","",Allweek)
      Rank<-1:20
      Data<-data.frame("Rank"=Rank,"moviename"=moviename,"BoxOffice"=as.numeric(moviemoney)
                       ,"AllBox-office"=as.numeric(moviemoney2),"LastweekRank"=LastweekRank,"RankofTimes"=Allweek)
      
    }else{
      Data<-data.frame("Rank"=c(""),"moviename"=c("No Data"),"BoxOffice"=c("")
                       ,"AllBox-office"=c(""),"LastweekRank"=c(""),"RankofTimes"=c(""))
    }
  })
  output$text<-renderText({
    date1()
  })
  output$text1<-renderText({
    date1()
  })
  output$table <- renderTable({
    selected1()
  })
  output$plot1 <- renderPlot({
    if(dim(selected1()[1])!=1){
      ggplot( selected1()[1:5,], title="Box-office",aes(x="",y=BoxOffice, fill=moviename))+
        geom_bar(stat="identity")+coord_polar("y")+
        scale_fill_brewer(palette="Dark2")+
        scale_fill_discrete(name="Rank1:5")
    }else{
      ggplot( selected1(), title="Box-office")+
        
        scale_fill_discrete(name="")
    }   
  })
  
  
})
