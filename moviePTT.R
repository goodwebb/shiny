#已經上版網址如下: https://davidwebb.shinyapps.io/moviePTT/
#爬完比較慢，要有點耐心

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("PTT:movie版電影評價"),
  h1("請先到https://www.ptt.cc/bbs/movie/index.html"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h2("進入PTT網頁版後，請按'上頁'鈕後，網址後面有頁數碼"),
      # Sidebar with a slider input for number of bins 
      # Application title
      numericInput("num", label = h3("輸入頁數碼:"), value = 
                     5180+(120*(as.numeric(substr(Sys.Date(),6,7))-3))),
      textInput("text", label = h3("片名:"), value = ""),
      submitButton("提交")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel('圖',  plotOutput('plot1')),
        tabPanel('表',  tableOutput('table1')),
        tabPanel('明細',  tableOutput('table2'))
      )
    )
  )
))
library(shiny)
library(xml2)
library(xmlview)
library(ggplot2)
library(dplyr)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  selected1 <- reactive({
    score<-NULL
    cc<-NULL
    dd<-NULL
    Alldate <-NULL
    Allauthor <-NULL
    c<-as.numeric(input$num)
    b<-5150
    a<-input$text
    if(a!=""){
      for(k in 1:(c-b)){
        url   <- "https://www.ptt.cc/bbs/movie/indexjjjj.html"
        page<-b+k
        url<-sub("jjjj",replacement = page,url)
        tryCatch({
          doc   <- read_html(url)
          xpathdate <- "//*[@class='date']"
          xpathtitle<- "//*[@class='title']"
          xpathauthor<- "//*[@class='author']"
          title<-xml_text(xml_find_all(doc, xpathtitle))
          title<-gsub("\t","",title)
          title<-gsub("\n","",title)
          author<-xml_text(xml_find_all(doc, xpathauthor))
          date<-xml_text(xml_find_all(doc, xpathdate))
          length(title)
          ## a=="收尋名字"
          serl<-grepl(a,title)
          ##xxxx<-strsplit(title[serl==TRUE],split=" ")
          xxxx<-strsplit(title[serl==TRUE],split="]")
          if(length(xxxx)!=0){
            for(i in 1:length(xxxx)){
              tempcc<-as.character(xxxx[[i]][1])
              tempcc<-gsub(" ","",tempcc)
              tempcc<-gsub("(\\[)","",tempcc)
              #tempcc<-gsub("Re","回文",tempcc) ##回文置換
              ##重新評分
              #if(grepl("回文",tempcc)){
              #  score<-c(score,tempcc)
              #}else if(grepl("好",tempcc)){
              #  score<-c(score,"好雷")
              #}else if(grepl("負",tempcc)){
              #  score<-c(score,"負雷")
              #}else{
              #  score<-c(score,tempcc)
              #}
              score<-c(score,tempcc)
              cc<-c(cc,tempcc)
              dd<-c(dd,as.character(xxxx[[i]][2]))
            }
          }  
          Alldate <- c(Alldate,date[serl==TRUE])
          Allauthor <- c(Allauthor,author[serl==TRUE])
        }, warning = function(w) {
          
        }, error = function(e) {
          
        })
        
        
        
        
      }
      data <- data.frame("Score"=score,"title"=dd,
                         "Author"=Allauthor,"Date"=Alldate,"O.Score"=cc)
      
    }else{
      data <- NULL
    }
  })
  data2 <- reactive({
    ##為了上shiny選出前五排名
    D<-selected1()
    if(!is.null(D)){
      selected1()%>%
        group_by(Score)%>%
        summarise(Freq=n())%>%
        arrange(desc(Freq))
    }else{
      NULL
    }  
  })
  
  
  output$table2 <- renderTable({
    selected1()
  })
  output$table1 <- renderTable({
    data2()
  })
  output$plot1 <- renderPlot({
    D<-data2()
    if(!is.null(D)){
      ggplot( D[1:7,], title="moviemoney",aes(x="",y=Freq, fill=Score))+
        geom_bar(stat="identity")+coord_polar("y")+
        scale_fill_brewer(palette="Reds")+
        theme_minimal()
    }  
    
  })
  
})
