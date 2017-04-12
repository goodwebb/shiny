##台北電影票房進階版
##未上shiny DT不能用
library(shiny)
library(DT)
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
      tabPanel('表',  h3(strong(textOutput("text"))),"單位:百萬元",dataTableOutput('table')
               ,"來源:開眼電影網"),
      tabPanel('圖', h3(strong(textOutput("text1"))),plotOutput('plot1'))    )
  )
))
library(shiny)
library(timeDate)
library(xml2)
library(xmlview)
library(DT)
library(ggplot2)
# Define server logic required to draw a histogram
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
  ##抓取排名資料
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
      Data<-data.frame("排行"=Rank,"片名"=moviename,"本周票房"=as.numeric(moviemoney)
                       ,"累積票房"=as.numeric(moviemoney2),"上週排名"=LastweekRank,"上榜週數"=Allweek)
      
    }else{
      Data<-data.frame("排行"=c(""),"moviename"=c("查無資料"),"本周票房"=c("")
                       ,"累積票房"=c(""),"上週排名"=c(""),"上榜週數"=c(""))
      
    }
  })
  
  
  ##日期處理 
  ## date1<-paste(selected1(),selected2(),sep="-")
  ##date1<-paste(date1,selected3(),sep="-")
  
  output$table <- renderDataTable(datatable(
    selected1(),
    options = list(rowCallback = JS(
      'function(row, data) {
      // Bold cells for those >= 5 in the first column
      if (parseFloat(data[1]) <=5.0){
      $("td:eq(1)", row).css("font-weight", "bold");
      $("td:eq(1)", row).css("color", "red");
      $("td:eq(2)", row).css("font-weight", "bold");
      $("td:eq(2)", row).css("color", "red");
      $("td:eq(3)", row).css("font-weight", "bold");
      $("td:eq(3)", row).css("color", "red");
      }
}'
    ))
    ))  
  output$text<-renderText({
    date1()
  })
  output$text1<-renderText({
    date1()
  })
  output$plot1 <- renderPlot({
    if(dim(selected1()[1])!=1){
      ggplot( selected1()[1:5,], title="本周票房前五名",aes(x="",y=本周票房, fill=片名))+
        geom_bar(stat="identity")+coord_polar("y")+
        scale_fill_brewer(palette="Dark2")+
        scale_fill_discrete(name="本周票房前五名")
    }else{
      ggplot( selected1(), title="本周票房前五名")+
        
        scale_fill_discrete(name="")
    }
  })
  })

