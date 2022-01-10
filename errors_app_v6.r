# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Find out more about building applications with Shiny here:
# http://shiny.rstudio.com/

# î÷èùàåì êýø
rm(list=ls())
# î÷èùàåì êýø ãðàôèêîâ
graphics.off()

library(shiny)
library(dygraphs)
library(lubridate)
library(ggplot2)
library(slider)
library(dplyr)
library(dygraphs)
library(openxlsx)
library(utils)
library(stringr)
library(plotly)
library(xts)
library(zoo)

setwd(dir = "C:/Users/sofja/Documents/R/diplom/app/app_v4")
source(file = "error_types_function_v4_1.R")
setwd(dir = "C:/Users/sofja/Documents/R/diplom/app/app_v4")
loger_data = read.xlsx(xlsxFile = "chanelTable.xlsx",colNames = T)
list_lab=unique(loger_data$labor)
list_room=unique(loger_data$ruum)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Temperature and humidity monitoring system"),
  fluidPage(
    
    tabsetPanel(
      tabPanel("Monitoring system setups", # èìÿ ïåðâîé çàêëàäêè
               fluidRow(
                 column(4,  
                        selectInput("lab", "Laboratory name", choices = unique(loger_data$labor)), 
                        selectInput("room", "Room number", choices = NULL), 
                        selectInput("device_t", "Temperature sensor", choices = NULL), 
                        selectInput("device_h", "Humidity sensor", choices = NULL),
                        p("Update data first, then update timeline"), 
                        p("Update data everytime you changed inputs"),
                        actionButton("update", "Update!"),
                        p("Don't forget update timeline once"),
                        actionButton("time_periond", "Update timeline once!"),   
                        sliderInput("time",
                                    "Time period (sec)",
                                    value = c(as.POSIXct("2020-01-01 00:00:07"),
                                              as.POSIXct("2021-01-01 00:00:07")),
                                    min = as.POSIXct("2020-01-01 00:00:07"),
                                    max = as.POSIXct("2021-01-01 00:00:07"),
                                    timeFormat = "%Y-%m-%d %H:%M:%S"),
                        sliderInput("t_limit", "Temperature limit (C)", value = c(20, 60), min = -50, max = 100),
                        sliderInput("h_limit", "Humidity limit (%)", value = c(20, 80), min = 0, max = 100),
                        sliderInput("t_poss", "Possible temperature (C)", value = c(-10, 100), min = -50, max = 200),
                        numericInput("rosa", "Surface temperature (C)", value = 18, min = -50, max = 200),
                        # numericInput("sec_diff_time", "Time limit between signals (sec)", value = 306, min = 0),
                        p("Push Plot once, after updating timeline"),
                        actionButton("plot_", "Plot!"),
                        
                        textOutput("try")
                        
                 ),
                 column(8, 
                        dygraphOutput("dygraph_t"),
                        dygraphOutput("dygraph_h"),
                        dygraphOutput("dygraph_r")
                 )
               )
      ),
      tabPanel("Plots for errors", 
               fluidRow(
                 column(6,
                        plotlyOutput("plot_t")
                 ),
                 column(6, 
                        plotlyOutput("plot_h")
                 )
               ),
               fluidRow( 
                 column(6, # 
                        dataTableOutput("SUMMARY_out_T")
                 )
                 ,
                 column(6, #
                        dataTableOutput("SUMMARY_out_H")
                 )
               )
      )
    )
    
    
  )
)

server <- function(input, output) {
  # áåðåì íèæíèé ëèìèò äëÿ òåìïåðàòóðû èç áåãóíêîâ â èíòåðôåéñå
  t_low_limit <- eventReactive(input$update, {
    min(input$t_limit)
  })
  # áåðåì âåðõíèé ëèìèò äëÿ òåìïåðàòóðû èç áåãóíêîâ â èíòåðôåéñå
  t_top_limit <- eventReactive(input$update, {
    max(input$t_limit)
  })
  # áåðåì íèæíèé ëèìèò äëÿ âëàæíîñòè èç áåãóíêîâ â èíòåðôåéñå
  h_low_limit <- eventReactive(input$update, {
    min(input$h_limit)
  })
  # áåðåì âåðõíèé ëèìèò äëÿ âëàæíîñòè èç áåãóíêîâ â èíòåðôåéñå
  h_top_limit <- eventReactive(input$update, {
    max(input$h_limit)
  })
  # áåðåì ëåâûé ëèìèò äëÿ âðåìåíè èç áåãóíêîâ â èíòåðôåéñå
  time_limit_left <- eventReactive(input$update, {
    min(input$time)
  })
  # áåðåì ïðàâûé ëèìèò äëÿ âðåìåíè èç áåãóíêîâ â èíòåðôåéñå
  time_limit_right <- eventReactive(input$update, {
    max(input$time)
  })
  # áåðåì íèæíèé ëèìèò äëÿ âîçìîæíîé òåìïåðàòóðû èç áåãóíêîâ â èíòåðôåéñå
  t_low_filter <- eventReactive(input$update, {
    min(input$t_poss)
  })
  # áåðåì âåðõíèé ëèìèò äëÿ âîçìîæíîé òåìïåðàòóðû èç áåãóíêîâ â èíòåðôåéñå
  t_top_filter <- eventReactive(input$update, {
    max(input$t_poss)
  })
  # áåðåì òåìïåðàòóðó ïîâåðõíîñòè äëÿ òî÷êè ðîñû èç èíòåðôåéñà
  t_rosa <- eventReactive(input$update, {
    max(input$rosa)
  })
  # áåðåì òåìïåðàòóðó ïîâåðõíîñòè äëÿ òî÷êè ðîñû èç èíòåðôåéñà
  diff_time <- eventReactive(input$update, {
    
    temp=read.csv(file = T_sensor(),header = T,sep = ";")
    temp <- temp %>% 
      mutate_at(c("val"), COMMA) %>% 
      mutate_at(c("dtime"), TIME) %>%
      mutate_at(c("dtime"), as.POSIXct)
    temperature <- zoo(x=temp$val, order.by=temp$dtime)
    per=periodicity(temperature,scale="hourly")
    per$frequency*60+30
    
  })
  
  
  observeEvent(input$update,{
    output$try <- renderText(
      paste("Time difference limit fo 2nd error -",round(diff_time(),digits = 2),"sec",sep = " ")
    )
  })
  
  
  # àêòèâèðóåì íàøó ðàíåå íàïèñàíóþ ôóíêöèþ ïî ðàñ÷åòó îøèáîê 
  # êîãäà íàæèìàåì àïäåéò è ðàíåå çàäàííûå ïàðìåòðû èçèíðåôåéñà ïîäãðóæàþòñÿ â ôóíêöèþ
  
  DATA <- eventReactive(input$update,{
    
    data_update <- errors(
      temperature=T_sensor(),
      humidity=H_sensor(),
      temperature_low_limit = t_low_limit(),
      temperature_top_limit = t_top_limit(),
      humidity_low_limit = h_low_limit(),
      humidity_top_limit = h_top_limit(),
      temperature_low_filter = t_low_filter(),
      temperature_top_filter = t_top_filter(),
      surface_t = t_rosa(),
      sec_diff_time = diff_time())
  })
  
  
  
  DATA_plot <- reactive({
    input$plot_
    DATA_time <- DATA() %>%
      filter(time_t >= as.POSIXct(time_limit_left()) &
               time_t <= as.POSIXct(time_limit_right()),
             .preserve=T)
  })
  
  DATA_plot_T <- reactive({
    input$plot_
    DATA_time <- DATA() %>%
      filter(time_t >= as.POSIXct(time_limit_left()) &
               time_t <= as.POSIXct(time_limit_right()) &
               all_error_for_temperature != "clear data",
             .preserve=T)
  })
  
  DATA_plot_H <- reactive({
    input$plot_
    DATA_time <- DATA() %>%
      filter(time_t >= as.POSIXct(time_limit_left()) &
               time_t <= as.POSIXct(time_limit_right()) &
               all_error_for_humidity != "clear data",
             .preserve=T)
  })
  # îáíîâäëåíèå âðåìåííîãî ïåðèîäà, òàê êà ìû íå çàíåì êàêîé ôàéñ ñ äàííûìè ïðèõîäèò
  # è êàêîå òàì âðåìÿ, îíî ïîäãðóçèòñÿ èç ôàéëà ïðè íàæàòèè îáíîâëåíèÿ âðåìåííîãî ëèìèòà
  observeEvent(input$time_periond,
               updateSliderInput(inputId = "time", 
                                 value = c(min(DATA()$time_t),
                                           max(DATA()$time_t)),
                                 min = min(DATA()$time_t),
                                 max = max(DATA()$time_t))
  )
  # âûáîð ëàáû
  Laboratory <- reactive({
    filter(loger_data, labor == input$lab)
  })
  # âûáîð êîìíàò â ëàáå
  observeEvent(Laboratory(), {
    choices <- unique(Laboratory()$ruum)
    updateSelectInput(inputId = "room", choices = choices) 
  })
  # ïåðåìåííàÿ äëÿ êîìíàòû
  Room <- reactive({
    req(input$room)
    filter(Laboratory(), ruum == input$room)
  })
  # âûáîð êîìíàòû
  observeEvent(Room(), {
    choices_t <- grep(glob2rx("T*"), unique(Room()$DEVICE_ID), value = TRUE)
    choices_h <- grep(glob2rx("H*"), unique(Room()$DEVICE_ID), value = TRUE)
    updateSelectInput(inputId = "device_t", choices = choices_t)
    updateSelectInput(inputId = "device_h", choices = choices_h)
  })
  # ïåðåìåííàÿ äëÿ äàò÷èêà òåìïåðàòóðû 
  T_sensor <- eventReactive(input$update,{
    req(input$device_t)
    t_sensor=str_sub(string = input$device_t, 3, nchar(input$device_t))
    paste(t_sensor,"csv",sep = ".")
  })
  # ïåðåìåííàÿ äëÿ äàò÷èêà âëàæíîñòè
  H_sensor <- eventReactive(input$update,{
    req(input$device_h)
    t_sensor=str_sub(string = input$device_t, 3, nchar(input$device_t))
    paste("Ruum_",t_sensor,"_-_H",".csv",sep = "")
  })
  # îòðèñîâêà ãðàôèêà ñ îøèáêàìè äëÿ òåìïåðàòóðû ïðè íàæàòèèè ïëîò
  # è îáíîâëåíèå ïðè íàæàòèè àïäåéò
  observeEvent(input$plot_,{
    output$plot_t <- renderPlotly({
      ggplot(DATA_plot_T(), aes(x = time_t, y = temperature)) +
        geom_point(aes(colour = all_error_for_temperature), size = 0.5) +
        xlab("Date")+ ylab("Temperature C")+
        scale_y_continuous(breaks = seq(0, 33, 1))+
        guides(colour=guide_legend("Error type"))+
        labs(title = "Temperature and errors")
    })
  })
  # îòðèñîâêà ãðàôèêà ñ îøèáêàìè äëÿ âëàæíîñòè ïðè íàæàòèèè ïëîò
  # è îáíîâëåíèå ïðè íàæàòèè àïäåéò
  observeEvent(input$plot_,{
    output$plot_h <- renderPlotly({
      base <- ggplot(DATA_plot_H(), aes(x = time_h, y = humidity)) +
        geom_point(aes(colour = all_error_for_humidity), size = 0.5) +
        xlab("Date")+ ylab("Humidity %")+
        scale_y_continuous(breaks = seq(0, 100, 10))+
        guides(colour=guide_legend("Error type"))+
        labs(title = "Humidity and errors")
      ggplotly(base)
    })
  })
  
  # ïåðåìåííàÿ ñ òàáëèöåé äëÿ ñàììàðè
  DATA_SUMMARY <- reactive({
    input$plot_
    SUMMARY=data.frame(Temperature=as.factor(DATA_plot()$all_error_for_temperature),
                       Humidity=as.factor(DATA_plot()$all_error_for_humidity))
    summary(SUMMARY)
  })
  
  DATA_SUMMARY_T <- reactive({
    input$plot_
    SUMMARY=data.frame(Temperature=as.factor(DATA_plot_T()$all_error_for_temperature))
    summary(SUMMARY)
  })
  
  DATA_SUMMARY_H <- reactive({
    input$plot_
    SUMMARY=data.frame(Humidity=as.factor(DATA_plot_H()$all_error_for_humidity))
    summary(SUMMARY)
  })
  # ðåíäåð òàáëèöû ïðè íàæàòèèè íà ïëîò
  # îáíîâëåíèå ïðè íàæàòèè íà àïäåéò
  # observeEvent(input$plot_,{
  #   output$SUMMARY_out <- renderDataTable(DATA_SUMMARY()
  #   )
  # })
  observeEvent(input$plot_,{
    output$SUMMARY_out_T <- renderDataTable(DATA_SUMMARY_T()
    )
  })
  observeEvent(input$plot_,{
    output$SUMMARY_out_H <- renderDataTable(DATA_SUMMARY_H()
    )
  })
  # äàííûå äëÿ ãðàôèêà äëÿ òåìïåðàòóðû çà ïîñëåäíèé äåíü ñ ëèìèòàìè
  DATA_dygraph_t <- reactive({
    input$plot_
    xts_t <- DATA() %>%
      filter(time_t >= (as.POSIXct(max(time_t))-days(1)) &
               time_t <= as.POSIXct(max(time_t)),.preserve = T)
    zoo_t <- zoo(x=xts_t$temperature, order.by=xts_t$time_t)
    zoo_t
  })
  # ãðàôèê äëÿ òåìïåðàòóðû çà ïîñëåäíèé äåíü ñ ëèìèòàìè
  observeEvent(input$plot_,{
    output$dygraph_t <- renderDygraph(
      
      dygraph(DATA_dygraph_t(),main = "Temperature for last day",
              xlab = "Time",ylab = "Temperature C") %>%
        dySeries("V1", stepPlot = F, color = "red",label = "Temperature") %>%
        dyLimit(limit = t_top_limit(), "Temperature max limit",
                strokePattern = "solid", color = "red")%>% 
        dyLimit(limit = t_low_limit(), "Temperature min limit",
                strokePattern = "solid", color = "red")
    )
  })
  # äàííûå äëÿ ãðàôèêà äëÿ âëàæíîñòè çà ïîñëåäíèé äåíü ñ ëèìèòàìè
  DATA_dygraph_h <- reactive({
    input$plot_
    xts_h <- DATA() %>%
      filter(time_t >= (as.POSIXct(max(time_t))-days(1)) &
               time_t <= as.POSIXct(max(time_t)),.preserve = T)
    zoo_h <- zoo(x=xts_h$humidity, order.by=xts_h$time_t)
    zoo_h
  })
  # ãðàôèê äëÿ âëàæíîñòè çà ïîñëåäíèé äåíü ñ ëèìèòàìè
  observeEvent(input$plot_,{
    output$dygraph_h <- renderDygraph(
      
      dygraph(DATA_dygraph_h(),main = "Humidity for last day",
              xlab = "Time",ylab = "Humidity %") %>%
        dySeries("V1", stepPlot = F, color = "blue",label = "Humidity") %>%
        dyLimit(limit = h_top_limit(), "Humidity max limit",
                strokePattern = "solid", color = "red")%>% 
        dyLimit(limit = h_low_limit(), "Humidity min limit",
                strokePattern = "solid", color = "red")
    )
  })
  # äàííûå äëÿ ãðàôèêà äëÿ òåìïåðàòóðà, âëàæíîñòè è òî÷êè ðîñû
  DATA_dygraph_r <- reactive({
    input$plot_
    xts <- DATA() %>%
      filter(time_t >= (as.POSIXct(max(time_t))-days(1)) &
               time_t <= as.POSIXct(max(time_t)),.preserve = T)
    
    temperature <- zoo(x=xts$temperature, order.by=xts$time_t)
    humidity <- zoo(x=xts$humidity, order.by=xts$time_t)
    dew_point <- zoo(x=xts$t_rosa, order.by=xts$time_t)
    xts_all=merge(temperature,humidity)
    xts_all=merge(xts_all,dew_point)
    
    xts_all
  })
  #  ãðàôèê äëÿ òåìïåðàòóðà, âëàæíîñòè è òî÷êè ðîñû çà ïîñëåäíèé äåíü
  observeEvent(input$plot_,{
    output$dygraph_r <- renderDygraph(
      
      dygraph(DATA_dygraph_r(), main = "Temperature & Humidity & Dew point for last day")%>%
        dyAxis("y", label = "Temperature C", 
               # valueRange = c(0, 1.0),
               independentTicks = TRUE)%>%
        dyAxis("y2", label = "Humidity % ", 
               # valueRange = c(0, 25.0), 
               independentTicks = TRUE) %>%
        dySeries("humidity", axis=('y2'))
      
    )
  })
}
shinyApp(ui = ui, server = server)
