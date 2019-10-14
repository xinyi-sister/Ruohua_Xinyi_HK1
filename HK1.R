
library(RMySQL)
library(tidyverse)
library(lubridate)
library(readxl)
library(highcharter)
library(tidyquant)
library(timetk)
library(tibbletime)
library(quantmod)
library(PerformanceAnalytics)
library(scales)


####数据库导出数据####

#清空所有object
rm(list=ls())

#连接数据库
mydb <- dbConnect(MySQL(),user='ktruc002', password='35442fed', dbname='global_index',host='172.19.3.250')

#按要求查询，获得数据表(海外指数日行情)
#[S&P500] 001006 [伦敦FTSE100] 001003 [东京日经] 002003 [香港恒生] 002001
sql_statement <- "select index_code, substr(trade_date,1,10) as date_chr, close
                  from daily_quote
                  where index_code in ('001006','001003','002003','002001')
                  order by index_code, trade_date"

rawdata <- dbGetQuery(mydb,sql_statement)
str(rawdata)

#关闭数据库
dbDisconnect(mydb)



####数据预处理####

#字符格式的时间转化为日期时间格式
rawdata$date <- ymd(rawdata$date_chr)

#将dataframe格式的长数据转化为xts格式的宽数据
price_daily_alltime <- rawdata %>%
  select(-date_chr) %>%
  spread(index_code, close) %>%
  tk_xts(date_var = date)

#截取某时间范围的数据
price_daily <- price_daily_alltime["2008-01-01/2018-01-01"]
head(price_daily)
tail(price_daily)



####计算资产收益率####

#计算资产日收益率
#(连续)
log_return_daily <- 
  Return.calculate(price_daily,method = "log") %>%
  na.omit()
head(log_return_daily,3)
#(离散)
discrete_return_daily <-
  Return.calculate(price_daily,method = "discrete") %>%
  na.omit()
head(discrete_return_daily,3)

#计算资产月收益率
#(日收盘价转为月收盘价)
price_monthly <-
  to.monthly(price_daily, indexAt = "lastof", OHLC = FALSE)
head(price_monthly,3)
#(连续)
log_return_monthly <- 
  Return.calculate(price_monthly,method = "log") %>%
  na.omit()
head(log_return_monthly,3)
#(离散)
discrete_return_monthly <-
  Return.calculate(price_monthly,method = "discrete") %>%
  na.omit()
head(discrete_return_monthly,3)

#计算资产季度收益率
#(日收盘价转为季度收盘价)
price_quarterly <- to.quarterly(price_daily, indexAt = "lastof", OHLC = FALSE)
head(price_quarterly,3)
#(连续)
log_return_quarterly <- 
  Return.calculate(price_quarterly,method = "log") %>%
  na.omit()
head(log_return_quarterly,3)
#(离散)
discrete_return_quarterly <-
  Return.calculate(price_quarterly,method = "discrete") %>%
  na.omit()
head(discrete_return_quarterly,3)



####计算组合收益率(连续收益率为例)####
w <- c(0.25, 0.15, 0.3, 0.3)

#计算组合日连续收益率+日度再平衡
portfolio_LogReturnDaily <-
  Return.portfolio(log_return_daily,
                   weights = w,
                   rebalance_on = "days") %>%
  `colnames<-`("returns")
head(portfolio_LogReturnDaily,3)
tail(portfolio_LogReturnDaily,3)

#计算组合月连续收益率+月度再平衡
portfolio_LogReturnMonthly <-
  Return.portfolio(log_return_monthly,
                   weights = w,
                   rebalance_on = "months") %>%
  `colnames<-`("returns")
head(portfolio_LogReturnMonthly,3)

#计算组合季度连续收益率+季度再平衡
portfolio_LogReturnQuartly <-
  Return.portfolio(log_return_quarterly,
                   weights = w,
                   rebalance_on = "quarters") %>%
  `colnames<-`("returns")
head(portfolio_LogReturnQuartly,3)



####收益率可视化(月度连续收益率为例)####

#xts数据时间序列线图
highchart(type = "stock") %>%
  hc_title(text = "Portfolio Monthly Returns") %>%
  hc_add_series(portfolio_LogReturnMonthly$returns,
                name = "Rebalanced Monthly",
                color = "cornflowerblue") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)

#dataframe格式绘制直方图+概率密度函数图

p_LogReturnMonthly <- 
  portfolio_LogReturnMonthly %>%
  data.frame(date = index(.)) %>%
  remove_rownames()

p_LogReturnMonthly %>%
  ggplot(aes(x = returns)) +
  geom_histogram(binwidth = .01,
                 fill = "cornflowerblue",
                 color = "cornflowerblue") +
  geom_density(alpha = 1, color = "red") +
  xlab("monthly returns") +
  ylab("distribution") +
  ggtitle("Portfolio Returns Distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))


####收益率简单统计指标计算(月度连续收益率为例)####
stat.desc(portfolio_LogReturnMonthly, norm=T)



#####ShinyApp可视化#####
library(shiny)

server <- function(input, output) {
  
  output$distPlot_d <- renderPlot({
    
    w <- c(input$w1/100, input$w2/100, input$w3/100, input$w4/100)
    
    portfolio_return_daily <- price_daily %>%
      Return.calculate(method = input$cal_method) %>%
      na.omit() %>%
      Return.portfolio(weights = w, 
                       rebalance_on = "days") %>%
      `colnames<-`("returns")
    
    p_return_daily <- portfolio_return_daily %>%
      data.frame(date = index(.)) %>%
      remove_rownames()
    
    ggplot(p_return_daily, aes(x = returns)) +
      geom_histogram(binwidth = .01,
                     fill = "cornflowerblue",
                     color = "cornflowerblue") +
      geom_density(alpha = 1, color = "red") +
      labs(title = "Daily Return")
    
  })
  
  output$distPlot_m <- renderPlot({
    
    w <- c(input$w1/100, input$w2/100, input$w3/100, input$w4/100)
    
    portfolio_return_monthly <- price_monthly %>%
      Return.calculate(method = input$cal_method) %>%
      na.omit() %>%
      Return.portfolio(weights = w, 
                       rebalance_on = "months") %>%
      `colnames<-`("returns")
    
    p_return_monthly <- portfolio_return_monthly %>%
      data.frame(date = index(.)) %>%
      remove_rownames()
    
    ggplot(p_return_monthly, aes(x = returns)) +
      geom_histogram(binwidth = .01,
                     fill = "cornflowerblue",
                     color = "cornflowerblue") +
      geom_density(alpha = 1, color = "red") +
      labs(title = "Monthly Return")
    
  })
  
  output$distPlot_q <- renderPlot({
    
    w <- c(input$w1/100, input$w2/100, input$w3/100, input$w4/100)
    
    portfolio_return_quarterly <- price_quarterly %>%
      Return.calculate(method = input$cal_method) %>%
      na.omit() %>%
      Return.portfolio(weights = w, 
                       rebalance_on = "quarters") %>%
      `colnames<-`("returns")
    
    p_return_quarterly <- portfolio_return_quarterly %>%
      data.frame(date = index(.)) %>%
      remove_rownames()
    
    ggplot(p_return_quarterly, aes(x = returns)) +
      geom_histogram(binwidth = .01,
                     fill = "cornflowerblue",
                     color = "cornflowerblue") +
      geom_density(alpha = 1, color = "red") +
      labs(title = "Quarterly Return")
    
  })
}

ui <- fluidPage(
  
  titlePanel("Portfolio Return Calculation"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "cal_method",
                  label = "Method of Calculation on Return",
                  choices =c("log", "discrete")),
      numericInput(inputId = "w1",
                   label = "Weight of 伦敦FTSE100. %",
                   min = 0,
                   max = 100,
                   value =25),
      numericInput(inputId = "w2",
                   label = "Wight of S&P500. %",
                   min = 0,
                   max = 100,
                   value =15),
      numericInput(inputId = "w3",
                   label = "Weight of 香港恒生. %",
                   min = 0,
                   max = 100,
                   value =20),
      numericInput(inputId = "w4",
                   label = "Weight of 东京日经. %",
                   min = 0,
                   max = 100,
                   value =30)
    ),
    mainPanel(
      plotOutput("distPlot_d"),
      plotOutput("distPlot_m"),
      plotOutput("distPlot_q")
    )
  )
)

shinyApp(ui = ui, server = server)

