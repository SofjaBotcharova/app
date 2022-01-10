

# install.packages('dygraphs')
library(dygraphs)
library(lubridate)
library(ggplot2)
library(slider)
library(dplyr)



# empty dataframe with all times

errors <- function(temperature="D3030.csv",
                   humidity="Ruum_D3030_-_H.csv",
                   temperature_low_limit=20, 
                   temperature_top_limit=60, 
                   humidity_low_limit=20, 
                   humidity_top_limit=80, 
                   temperature_low_filter=-10, 
                   temperature_top_filter=100, 
                   surface_t=18, 
                   sec_diff_time=330 
){
  TIME <- function(x, na.rm = FALSE) +
    (strptime(x,"%Y-%m-%d %H:%M:%OS"))
  COMMA <- function(x, na.rm = FALSE) +
    (as.numeric(gsub(",", ".", gsub("\\.", "", x))))
  ROSA <- function(t,
                   h,
                   a=17.27,
                   b=237.7,
                   c=100,
                   na.rm = FALSE)+
    {round((b*((a*t)/(b+t)+log10(h/c)))/(a-((a*t)/(b+t)+log10(h/c))),digits = 2)}
  filter_t <- function(x)(
    all(x >= temperature_top_limit | x <= temperature_low_limit)
  )
  filter_h <- function(x)(
    all(x >= humidity_top_limit | x <= humidity_low_limit)
  )
  temperature=read.csv(temperature,header = T,sep = ";")
  humidity=read.csv(humidity,header = T,sep = ";")
  data <- data.frame(temperature=temperature$val,
                     humidity=humidity$val,
                     time_t=temperature$dtime,
                     time_h=humidity$dtime)
  
  
  data <- data %>% 
    mutate_at(c("temperature", "humidity"), COMMA) %>% 
    mutate_at(c("time_t","time_h"), TIME) %>%
    mutate_at(c("time_t","time_h"), as.POSIXct)
  # ïðîïóñê äàííûõ â ðåàëüíîì âðåìåíè ïðè çàãðóçêå ôàéëà
  SYS_time=data.frame(NA,NA,Sys.time(),Sys.time())
  colnames(SYS_time) <- colnames(data)
  data <- rbind(data, SYS_time)
  ######### Èùåì ïðîïóñêè è ñ÷èòàåì èõ êîëè÷åñòâî
  data <- data %>%
    mutate(time_diff_t = difftime(time_t, lag(time_t),units = "sec")) %>%
    mutate(time_diff_h = difftime(time_h, lag(time_h),units = "sec")) %>%
    mutate(n_2nd_t=floor(as.numeric(time_diff_t/sec_diff_time))) %>%
    mutate(n_2nd_h=floor(as.numeric(time_diff_h/sec_diff_time))) %>%
    mutate_at(c("time_diff_t","n_2nd_t","time_diff_h","n_2nd_h"), funs(lead), n = 1 )
  sum(data$n_2nd_t,na.rm = T) 
  sum(data$n_2nd_h,na.rm = T) 
  ###### ôèëüòð ëîæíûõ äàííûõ 4 òèï îøèáêè
  
  data <- data %>% 
    mutate(Fail_T = ifelse((temperature >= temperature_top_filter | 
                              temperature <= temperature_low_filter), 1, 0)) %>%
    mutate(Fail_H = ifelse((humidity >= 100 | 
                              humidity <= 0), 1, 0))
  
  
  ##### Èùåì òî÷êó ðîñû ########## 3 òèï îøèáêè
  
  data <- data %>% 
    mutate(t_rosa = ifelse((Fail_T == 0 & Fail_H == 0), 
                           ROSA(t=temperature,h=humidity), NA)) 
  
  
  #  îøèáêà ïåðâîãî òèïà ,åñëè 3 çíà÷åíèÿ ïîäðÿä áîëüøå èëè ìåíüøå ëèìèòà
  
  
  
  data <- data %>% 
    mutate(t_err_1 = slide_dbl(temperature, filter_t, .before = 2,.complete = T)) %>%
    mutate(h_err_1 = slide_dbl(humidity, filter_h, .before = 2,.complete = T))
  summary(data)
  
  # äåëàåì ñòîëáåö ñ òèïàìè îøèáêè
  # òåìïåðàòóðà
  data$all_error_for_temperature="clear data"
  #   îøèáêà ïåðâîãî òèïà
  data[(data$t_err_1==1 & is.na(data$t_err_1)==F), "all_error_for_temperature"] <- "error 1st type"
  #   îøèáêà âòîðîãî òèïà
  data[(data$n_2nd_t>=1 & is.na(data$n_2nd_t)==F), "all_error_for_temperature"] <- "error 2nd type"
  #  4 òèï îøèáêè
  data[(data$Fail_T==1& is.na(data$Fail_T)==F), "all_error_for_temperature"] <- "error 4 type"
  
  # òåïåðü äåëàåì äëÿ âëàæíîñòè
  data$all_error_for_humidity="clear data"
  #  îøèáêà ïåðâîãî òèïà
  data[(data$h_err_1==1 & is.na(data$h_err_1)==F), "all_error_for_humidity"] <- "error 1st type"
  #  îøèáêà âòîðîãî òèïà
  data[(data$n_2nd_h>=1 & is.na(data$n_2nd_h)==F), "all_error_for_humidity"] <- "error 2nd type"
  #  îøèáêà òðåòüåãî òèïà
  data[(data$t_rosa>=surface_t & is.na(data$t_rosa)==F), "all_error_for_humidity"] <- "error 3rd type"
  #  4 òèï îøèáêè
  data[(data$Fail_H==1 & is.na(data$Fail_H)==F), "all_error_for_humidity"] <- "error 4 type"
  data <- data[-nrow(data),]
  return (data)
}
TIME <- function(x, na.rm = FALSE) +
  (strptime(x,"%Y-%m-%d %H:%M:%OS"))
COMMA <- function(x, na.rm = FALSE) +
  (as.numeric(gsub(",", ".", gsub("\\.", "", x))))

# âèçóàëèçèðóåì òåìïåðàòóðó è òèïû îøèáîê

# ggplot(ERRORS, aes(x = time_t, y = temperature)) +
#   geom_point(aes(colour = all_error_for_temperature), size = 0.5) +
#   xlab("Date")+ ylab("Temperature C")+
#   scale_y_continuous(breaks = seq(0, 33, 1))+
#   guides(colour=guide_legend("Error type"))+
#   labs(title = "Teperature and errors")
# 
# # âèçóàëèçèðóåì âëàæíîñòü è òèïû îøèáîê
# 
# ggplot(data, aes(x = time_h, y = humidity)) +
#   geom_point(aes(colour = all_error_for_humidity), size = 0.5) +
#   xlab("Date")+ ylab("Humidity %")+
#   scale_y_continuous(breaks = seq(0, 100, 10))+
#   guides(colour=guide_legend("Error type"))+
#   labs(title = "Humidity and errors")
