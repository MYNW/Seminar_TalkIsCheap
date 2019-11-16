# Load libraries ------------------------
library(readr)
library(readxl)
library(dplyr)
library(magrittr)
library(xts)
library(zoo)
library(lubridate)
library(data.table)
require(EventStudy)
library(ggplot2)
library(scales)
library(tidyr)
library(tm)
library(stringr)
library(tidyverse)
library(gridExtra)
library(highfrequency)
library(rugarch)
library(dyn)
library(dynlm)
library(knitr)
library(car)
library(lawstat)
library(PairedData)
library(fastDummies)
library(sandwich)
library(lmtest)
library(vars)

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/Cand.Polit/Seminar - Exchange Rates")

#Create times ------------
ts  <- seq(as.POSIXct('2018/01/01 00:00:00'), as.POSIXct('2019/09/30 23:59'), 'mins')
dtc <- data.frame(Datetime=strftime(ts, format='%Y-%m-%d %H:%M:%SS'))
dtc$Datetime <- as.POSIXct(dtc$Datetime, tz = "Europe/Copenhagen")

## Load Data ---------------------
USD_2018 <- read_excel("DXY2018.xlsm", col_types = c("text", "text", "text", 
                                                     "numeric", "numeric", 
                                                     "numeric", "numeric", 
                                                     "numeric", "numeric", 
                                                     "text"))
USD_2019 <- read_excel("DXY2019.xlsx", col_types = c("text", "text", "text", 
                                                     "numeric", "numeric", 
                                                     "numeric", "numeric", 
                                                     "numeric", "numeric", 
                                                     "text"))

USD_df_core <- bind_rows(USD_2018,USD_2019)
rm(USD_2018)
rm(USD_2019)


VIX_2018 <- read_excel("VIX 2018 + 2019 (CLEAN).xlsx",
                       sheet = "VIX 2018", col_types = c("date",
                                                         "text", "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "text"))
VIX_2019 <- read_excel("VIX 2018 + 2019 (CLEAN).xlsx",
                       sheet = "VIX 2019", col_types = c("date",
                                                         "text", "numeric", "numeric", "numeric", 
                                                         "numeric", "numeric", "numeric", 
                                                         "text"))

VIX_df_core <- bind_rows(VIX_2018,VIX_2019)
rm(VIX_2018)
rm(VIX_2019)

Macro_df_core <- read_excel("Macro data releases (CLEAN) (15-10-2019).xlsx", 
                            col_types = c("text", "date", "numeric", 
                                          "numeric", "numeric", "text", "text",   
                                          "text", "text", "text", "text", "text",     
                                          "numeric", "text", "numeric", 
                                          "numeric", "numeric"))

FedSpeak_df_core <- read_excel("Communication_Data.xlsx", sheet = "Fed Speak", 
                               col_types = c("date", "text", "numeric", "text", "text",
                                             "numeric", "numeric", "numeric",
                                             "numeric", "numeric", "numeric",
                                             "numeric", "text", "text", "text", 
                                             "text", "text", "text", "numeric", 
                                             "text", "text", "text", "date", "numeric", 
                                             "numeric", "text"))

Trump_df_core <- read_excel("Communication_Data.xlsx", sheet = "Trump Tweets", 
                            col_types = c("text","text", "text", "text", "text", "text", 
                                          "date", "numeric", "numeric", "numeric",
                                          "numeric", "date", "numeric", "numeric",
                                          "numeric", "text", "numeric", "numeric", "numeric", 
                                          "date", "numeric", "text", "numeric", "text", "text"))
Trump_df_core <- Trump_df_core %>% filter(is.na(Trump_df_core$Delete)==1)

## Clean data frames and prepare timestamps -------------
VIX_df1 <- dplyr::select(VIX_df_core, one_of("Date (DD/MM/YY)", "Time Interval", "Close"))
VIX_df1$Datetime <- as.POSIXct(paste(VIX_df1$`Date (DD/MM/YY)`,
                                     substr(VIX_df1$`Time Interval`, 1, 5)), 
                               tz = "Europe/Copenhagen")
VIX_df1 <- dplyr::select(VIX_df1, one_of("Datetime", "Close"))

VIX_df <- dtc %>%
  left_join(VIX_df1, by = "Datetime") %>%
  arrange(Datetime) %>%
  fill(Close, .direction = "down") %>%
  filter(is.na(Close)==0)

names(VIX_df) <- c("Datetime","VIX")


USD_df1 <- dplyr::select(USD_df_core, one_of("Dato", "Tid", "Close"))
USD_df1$Datetime <- as.POSIXct(paste(USD_df1$Dato,USD_df1$Tid), tz = "Europe/Copenhagen")
USD_df1 <- dplyr::select(USD_df1, one_of("Datetime", "Close"))

USD_df <- dtc %>%
  left_join(USD_df1, by = "Datetime") %>%
  arrange(Datetime) %>%
  fill(Close, .direction = "down") %>%
  filter(is.na(Close)==0) %>%
  left_join(VIX_df, by = "Datetime")

USD_df <- USD_df %>%
  mutate(return5min = log(Close/lag(Close,5))) %>%
  #mutate(return10min = log(Close/lag(Close,10))) %>%
  #mutate(return24hr = log(Close/lag(Close,60*24))) %>%
  mutate(net_change = Close-lag(Close,5)) %>%
  mutate(weekday = weekdays(Datetime))

USD_df$weekday <- str_replace_all(USD_df$weekday, "Mandag","Monday") 
USD_df$weekday <- str_replace_all(USD_df$weekday, "Tirsdag","Tuesday") 
USD_df$weekday <- str_replace_all(USD_df$weekday, "Onsdag","Wednesday") 
USD_df$weekday <- str_replace_all(USD_df$weekday, "Torsdag","Thursday") 
USD_df$weekday <- str_replace_all(USD_df$weekday, "Fredag","Friday") 
USD_df$weekday <- str_replace_all(USD_df$weekday, "Lørdag","Saturday") 
USD_df$weekday <- str_replace_all(USD_df$weekday, "Søndag","Sunday") 

plot(density(USD_df_core$return24hr, na.rm = TRUE), main = "5-Minute Returns of the US Dollar Index")

Macro_df_core$Datetime <- (as.POSIXct(paste(Macro_df_core$`Publication date (DA)`,
                                            Macro_df_core$`Publication time (DA)`), tz="Europe/Copenhagen"))
Macro_df <- dplyr::select(Macro_df_core, one_of("Datetime", "Event", "Survey", "Actual", "Revised", "Ticker", "Standardized Surprise"))
Macro_df$Survey <- as.numeric(sub("%", "", Macro_df$Survey))
Macro_df$Actual <- as.numeric(sub("%", "", Macro_df$Actual))
Macro_df$Revised <- as.numeric(sub("%", "", Macro_df$Revised))
Macro_df$Surprise <- Macro_df$Actual - Macro_df$Survey

FedSpeak_df_core$Datetime <- as.POSIXct(FedSpeak_df_core$Column3, tz = "Europe/Copenhagen")
FedSpeak_df_core$Datetime =  ceiling_date(FedSpeak_df_core$Datetime, unit = "minute")
FedSpeak_df_core$Type <- FedSpeak_df_core$`Overordnet type`
FedSpeak_df_core$Coding <- FedSpeak_df_core$`Classifier \r\n(-1 for negative,\r\n0 for neutral or ambiguous,\r\n1 for positive)`
FedSpeak_df_core$Objective <- FedSpeak_df_core$`Communication in practice \r\n(Objectives and Strategy, Policy Decisions, Economic Outlook and Path of Future Policy Rates )`
FedSpeak_df <- dplyr::select(FedSpeak_df_core, 
                             one_of("Datetime", "Speaker", "Comment", "Type",
                                    "Coding", "Objective", "Subcategory", "Category"))
FedSpeak_df$isMedia = if_else(FedSpeak_df$Type == "Media",1,0)
FedSpeak_df <-  filter(FedSpeak_df, FedSpeak_df$isMedia!=1 )
FedSpeak_df <-  filter(FedSpeak_df, FedSpeak_df$Subcategory != "US Monetary Policy - Balance Sheet")

Trump_df_core$Timestamp = paste0(Trump_df_core$Hour,":",Trump_df_core$`Time (EST, -0500 from GMT)` )
Trump_df_core$Datetime =  as.POSIXct(paste(Trump_df_core$Date,Trump_df_core$Timestamp), tz = "EST")
Trump_df_core$Datetime =  ceiling_date(Trump_df_core$Datetime, unit = "minute")
Trump_df <- dplyr::select(Trump_df_core,one_of("Datetime", "text", "Subject", "Score", 
                                        "retweet_count", "favorite_count", "is_retweet"))
attributes(Trump_df$Datetime)$tzone <- "Europe/Copenhagen"


## Join data and create event table ----------------------
final_df = left_join(USD_df, Trump_df, by = "Datetime") %>%
  arrange(Datetime) %>%
  #mutate(leading_dummy = lag((text),1)) %>%
  arrange(Datetime, text)
final_df$Trump_speak_dummy = ifelse(is.na((final_df$text))==0  , 1, 0) #+ ifelse(is.na((final_df$leading_dummy))==0  , 1, 0)
final_df$Score[final_df$Score == "NO"] <- 1
final_df$Score[final_df$Score == "?"] <- 0

# Remove two duplicate dates & time zone change
#final_df<-final_df[!(final_df$Trump_speak_dummy==2),]
#final_df <- final_df[!duplicated(final_df$Datetime), ]
attributes(final_df$Datetime)$tzone <- "Europe/Copenhagen"

# Add Fed statements
final_df_test <- left_join(final_df, FedSpeak_df, by = "Datetime")
final_df_test <- final_df_test[!duplicated(final_df_test$Datetime), ]
final_df_test$Fed_speak_dummy = ifelse(is.na((final_df_test$Type))==0, 1, 0)
final_df_test <- final_df_test[!duplicated(final_df_test$Datetime), ]

# Add macro releases
final_df_all <- left_join(final_df_test, Macro_df, by = "Datetime")
final_df_all$Macro_dummy = ifelse(is.na((final_df_all$Event))==1, 0, 
                                  ifelse(final_df_all$Event=="FOMC Rate Decision (Upper Bound)",0,1))
final_df_all$FOMC_dummy = ifelse(final_df_all$Event=="FOMC Rate Decision (Upper Bound)",1,0)
final_df_all$FOMC_dummy[is.na(final_df_all$FOMC_dummy)] <- 0

# Remove non-trading days
#final_df_all <- filter(final_df_all, 
#                       weekdays(final_df_all$Datetime) != "Lørdag")
#final_df_all <- filter(final_df_all, 
#                       weekdays(final_df_all$Datetime) != "Søndag")
#names(final_df_all)[names(final_df_all)=="Close"] <- "PRICE"

#final_df_all <- filter(final_df_all, weekday != "Friday") ||(weekday == "Friday" & (hour(Datetime) < 22) ==1 ))

final_df_all_new <- final_df_all %>%
  mutate(DayOnly = as.Date((Datetime)) )%>%
  group_by(DayOnly) %>%
  mutate(Macro_Events = cumsum(Macro_dummy)) %>%
  mutate(Fed_Events = cumsum(Fed_speak_dummy)) %>%
  mutate(Trump_Events = cumsum(Trump_speak_dummy)) %>%
  mutate(EventDay = if_else((Macro_Events + Fed_Events + Trump_Events)>0,1,0) ) %>%
  ungroup() %>%
  arrange(Datetime) %>%
  mutate(IDA = cumsum(FOMC_dummy)) %>%
  group_by(IDA) %>%
  mutate(post_FOMC = seq_along(return5min)) %>%
  ungroup() %>%
  arrange(Datetime) %>%
  mutate(ID = cumsum(Macro_dummy)) %>%
  group_by(ID) %>%
  mutate(post_macro = seq_along(return5min)) %>%
  ungroup()


control_days_df <- final_df_all %>%
  mutate(DayOnly = as.Date((Datetime)) )%>%
  group_by(DayOnly) %>%
  mutate(Macro_Events = cumsum(Macro_dummy)) %>%
  mutate(Fed_Events = cumsum(Fed_speak_dummy)) %>%
  mutate(Trump_Events = cumsum(Trump_speak_dummy)) %>%
  mutate(EventDay = if_else((Macro_Events + Fed_Events + Trump_Events)>0,1,0) ) %>%
  filter(EventDay == 0)

#max_control <- control_days_df[order(control_days_df$return5min), ]
#by(max_control, max_control["return5min"], head, n=20)

Event_days_df <- final_df_all %>%
  mutate(DayOnly = as.Date((Datetime)) )%>%
  group_by(DayOnly) %>%
  mutate(Macro_Events = cumsum(Macro_dummy)) %>%
  mutate(Fed_Events = cumsum(Fed_speak_dummy)) %>%
  mutate(Trump_Events = cumsum(Trump_speak_dummy)) %>%
  mutate(EventDay = if_else((Macro_Events + Fed_Events + Trump_Events)>0,1,0) ) %>%
  filter(EventDay == 1)

## Descriptive statistics ---------------
# FOMC meetings
event_fig_data = Fed_reg_data %>%
  filter(DayOnly == '2018-09-26' |  DayOnly == '2019-06-19' | DayOnly == '2019-07-31' | DayOnly == '2019-09-18' ) %>%
  filter(timeofday >= '19:30:00' & timeofday <= '21:00:00')
event_fig_data$timeofday = as.POSIXct(sprintf('%s %s:%s', Sys.Date(), 
                                              format(event_fig_data$Datetime, '%H:%M'), '00')
                                      , tz = "Atlantic/South_Georgia")
event_fig_data$DayOnly = factor(as.Date(event_fig_data$Datetime))
p = ggplot(event_fig_data, aes(x = timeofday, y = return5min, 
                               group = DayOnly, color = DayOnly)) + 
  geom_line() + 
  theme(legend.position = "bottom") + scale_x_datetime(breaks=date_breaks('30 min'), 
                                                       labels=date_format('%H:%M')) + 
  geom_vline(xintercept=as.integer(as.POSIXct("2019-11-12 21:00:00")), 
             linetype=1, colour="black") +
  geom_vline(xintercept=as.integer(as.POSIXct("2019-11-12 21:30:00")), 
             linetype="dashed", colour="black") + labs(y = "5-minute DXY Index Returns", 
                                                       x= "Time(CEST)", 
                                                       colour = "FOMC meeting days") +
  scale_color_brewer(palette="Dark2") 
p


# Jackson_Hole
event_fig_data = Fed_reg_data %>%
  filter(DayOnly == '2018-08-24' |  DayOnly == '2019-08-23') %>%
  filter(timeofday >= '14:00:00' & timeofday <= '18:00:00')
event_fig_data$timeofday = as.POSIXct(sprintf('%s %s:%s', Sys.Date(), 
                                              format(event_fig_data$Datetime, '%H:%M'), '00')
                                      , tz = "Atlantic/South_Georgia")
event_fig_data$DayOnly = factor(as.Date(event_fig_data$Datetime))
p = ggplot(event_fig_data, aes(x = timeofday, y = return5min, group = DayOnly, color = DayOnly)) + 
  geom_line() + 
  theme(legend.position = "bottom") + scale_x_datetime(breaks=date_breaks('30 min'), 
                                                       labels=date_format('%H:%M')) + 
  geom_vline(xintercept=as.integer(as.POSIXct("2019-11-12 17:00:00")), 
             linetype=1, colour="black") +
  scale_color_brewer(palette="Dark2") + labs(y = "5-minute DXY Index Returns", 
                                             x= "Time(CEST)", 
                                             colour = "Jackson Hole speech by Powell")
p

# Beige Book
event_fig_data = Fed_reg_data %>%
  filter(DayOnly == '2018-09-12' |  DayOnly == '2019-07-17') %>%
  filter(timeofday >= '18:00:00' & timeofday <= '22:00:00')
event_fig_data$timeofday = as.POSIXct(sprintf('%s %s:%s', Sys.Date(), 
                                              format(event_fig_data$Datetime, '%H:%M'), '00')
                                      , tz = "Atlantic/South_Georgia")
event_fig_data$DayOnly = factor(as.Date(event_fig_data$Datetime))
p = ggplot(event_fig_data, aes(x = timeofday, y = return5min, 
                               group = DayOnly, color = DayOnly)) + 
  geom_line() + 
  theme(legend.position = "bottom") + scale_x_datetime(breaks=date_breaks('30 min'), 
                                                       labels=date_format('%H:%M')) + 
  geom_vline(xintercept=as.integer(as.POSIXct("2019-11-13 21:00:00")), 
             linetype=1, colour="black") +
  scale_color_brewer(palette="Dark2") + labs(y = "5-minute DXY Index Returns", 
                                             x= "Time(CEST)", 
                                             colour = "Beige Book publication")
p

# Beige Book
event_fig_data = Fed_reg_data %>%
  filter(DayOnly == '2018-09-12' |  DayOnly == '2019-07-17') %>%
  filter(timeofday >= '16:00:00' & timeofday <= '20:00:00')
event_fig_data$timeofday = as.POSIXct(sprintf('%s %s:%s', Sys.Date(), 
                                              format(event_fig_data$Datetime, '%H:%M'), '00')
                                      , tz = "Atlantic/South_Georgia")
event_fig_data$DayOnly = factor(as.Date(event_fig_data$Datetime))
p = ggplot(event_fig_data, aes(x = timeofday, y = return5min, 
                               group = DayOnly, color = DayOnly)) + 
  geom_line() + 
  theme(legend.position = "bottom") + scale_x_datetime(breaks=date_breaks('30 min'), 
                                                       labels=date_format('%H:%M')) + 
  geom_vline(xintercept=as.integer(as.POSIXct("2019-11-12 19:00:00")), 
             linetype=1, colour="black") +
  scale_color_brewer(palette="Dark2") + labs(y = "5-minute DXY Index Returns", 
                                             x= "Time(CEST)", 
                                             colour = "Beige Book publication")
p

# Testimony
event_fig_data = Fed_reg_data %>%
  filter(DayOnly == '2018-07-17' |  DayOnly == '2019-07-10') %>%
  filter(timeofday >= '15:00:00' & timeofday <= '19:00:00')
event_fig_data$timeofday = as.POSIXct(sprintf('%s %s:%s', Sys.Date(), 
                                              format(event_fig_data$Datetime, '%H:%M'), '00')
                                      , tz = "Atlantic/South_Georgia")
event_fig_data$DayOnly = factor(as.Date(event_fig_data$Datetime))
p = ggplot(event_fig_data, aes(x = timeofday, y = return5min, 
                               group = DayOnly, color = DayOnly)) + 
  geom_line() + 
  theme(legend.position = "bottom") + scale_x_datetime(breaks=date_breaks('30 min'), 
                                                       labels=date_format('%H:%M')) + 
  geom_vline(xintercept=as.integer(as.POSIXct("2019-11-12 18:00:00")), 
             linetype=1, colour="black") +
  scale_color_brewer(palette="Dark2") + labs(y = "5-minute DXY Index Returns", 
                                             x= "Time(CEST)", 
                                             colour = "Congressional Testimonials")
p

# Trump
event_fig_data = Trump_reg_data %>%
  filter(DayOnly == '2019-09-18')  %>%
  filter(timeofday >= '18:15:00' & timeofday <= '22:15:00')
event_fig_data$timeofday = as.POSIXct(sprintf('%s %s:%s', Sys.Date(), 
                                              format(event_fig_data$Datetime, '%H:%M'), '00')
                                      , tz = "Atlantic/South_Georgia")
event_fig_data$DayOnly = factor(as.Date(event_fig_data$Datetime))
p = ggplot(event_fig_data, aes(x = timeofday, y = return5min, 
                               group = DayOnly, color = DayOnly)) + 
  geom_line() + 
  theme(legend.position = "none", axis.text=element_text(size=10), axis.title=element_text(size=12),
        plot.title = element_text(hjust = 0.5)) + 
  scale_x_datetime(breaks=date_breaks('30 min'), labels=date_format('%H:%M')) + 
  geom_vline(xintercept=as.integer(as.POSIXct("2019-11-12 21:25:00")), 
             linetype=1, colour="black") +
  geom_vline(xintercept=as.integer(as.POSIXct("2019-11-12 21:00:00")), 
             linetype=1, colour="blue") +
  geom_vline(xintercept=as.integer(as.POSIXct("2019-11-12 21:30:00")), 
             linetype=2, colour="blue") +
  scale_color_brewer(palette="Dark2") + labs(y = "5-minute DXY Index Returns", 
                                             x= "Time(CEST)", 
                                             colour = "Trump Tweet") + 
  labs(title = "Jay Powell and the Federal Reserve Fail Again. 
       No “guts,” no sense, no vision!  A terrible communicator!")
p

event_fig_data = Trump_reg_data %>%
  filter(DayOnly == '2019-08-23')  %>%
  filter(timeofday >= '14:00:00' & timeofday <= '18:00:00')
event_fig_data$timeofday = as.POSIXct(sprintf('%s %s:%s', Sys.Date(), 
                                              format(event_fig_data$Datetime, '%H:%M'), '00')
                                      , tz = "Atlantic/South_Georgia")
event_fig_data$DayOnly = factor(as.Date(event_fig_data$Datetime))
q = ggplot(event_fig_data, aes(x = timeofday, y = return5min, 
                               group = DayOnly, color = DayOnly)) + 
  geom_line() + 
  theme(legend.position = "none", axis.text=element_text(size=10), axis.title=element_text(size=12),
        title = element_text(hjust = 0.5)) + scale_x_datetime(breaks=date_breaks('30 min'), 
                                                              labels=date_format('%H:%M')) + 
  geom_vline(xintercept=as.integer(as.POSIXct("2019-11-12 16:40:00")), 
             linetype=1, colour="black") +
  geom_vline(xintercept=as.integer(as.POSIXct("2019-11-12 17:00:00")), 
             linetype=1, colour="blue") +
  scale_color_brewer(palette="Dark2") + labs(y = "5-minute DXY Index Returns", 
                                             x= "Time(CEST)", 
                                             colour = "Trump Tweet") + 
  labs(title = "As usual, the Fed did NOTHING! It is incredible that they can “speak” without knowing or asking what I am doing, 
  which will be announced shortly. We have a very strong dollar and a very weak Fed. 
  I will work “brilliantly” with both, and the U.S. will do great
       ...My only question is, who is our bigger enemy, Jay Powell or Chairman Xi?")
q

ggsave("Trump_graphics.pdf", arrangeGrob(p, q), width = 25, height = 15, units = "cm")


q = ggplot(final_df_all_new, aes(x = Datetime, y = return5min)) 
q = q+ geom_line()
q= q+ylab("5-min log return")

q
grid.arrange(p,q, ncol=2)
# Beige Book - 2018
event_data <- Fed_reg_data %>%
  filter(DayOnly == '2018-09-12')
p = ggplot(event_data, aes(x = Datetime, y = Close)) 
p = p+ geom_line() +
  geom_vline(xintercept=as.integer(as.POSIXct("2018-09-12 20:00:00")), 
             linetype=4, colour="black")  +
  scale_x_datetime(limits = c(
    as.POSIXct("2018-09-12 18:00:00"),
    as.POSIXct("2018-09-12 22:00:00")))
p

# Beige Book - 2019
event_data <- Fed_reg_data %>%
  filter(DayOnly == '2019-07-17')
p = ggplot(event_data, aes(x = Datetime, y = Close)) 
p = p+ geom_line() +
  geom_vline(xintercept=as.integer(as.POSIXct("2019-07-17 20:00:00")), 
             linetype=4, colour="black")  +
  scale_x_datetime(limits = c(
    as.POSIXct("2019-07-17 18:00:00"),
    as.POSIXct("2019-07-17 22:00:00")))
p

# Jackson Hole - 2018
event_data <- Fed_reg_data %>%
  filter(DayOnly == '2018-08-24')
p = ggplot(event_data, aes(x = Datetime, y = Close)) 
p = p+ geom_line() +
  geom_vline(xintercept=as.integer(as.POSIXct("2018-08-24 16:00:00")), 
             linetype=4, colour="black")  +
  scale_x_datetime(limits = c(
    as.POSIXct("2018-08-24 14:00:00"),
    as.POSIXct("2018-08-24 18:00:00")))

# Jackson Hole 2019
event_data <- Fed_reg_data %>%
  filter(DayOnly == '2019-08-23')
p = ggplot(event_data, aes(x = Datetime, y = Close)) 
p = p+ geom_line() +
  geom_vline(xintercept=as.integer(as.POSIXct("2019-08-23 16:09:00")), 
             linetype=4, colour="black")  +
  scale_x_datetime(limits = c(
    as.POSIXct("2019-08-23 14:09:00"),
    as.POSIXct("2019-08-23 18:09:00")))

# Fed Interest Rate - 2019
event_data <- Fed_reg_data %>%
  filter(DayOnly == '2019-07-31')
p = ggplot(event_data, aes(x = Datetime, y = Close)) 
p = p+ geom_line() +
  geom_vline(xintercept=as.integer(as.POSIXct("2019-07-31 20:00:00")), 
             linetype=1, colour="black")  +
  scale_x_datetime(limits = c(
    as.POSIXct("2019-07-31 18:00:00"),
    as.POSIXct("2019-07-31 22:30:00"))) +
  geom_vline(xintercept=as.integer(as.POSIXct("2019-07-31 20:30:00")), 
             linetype=4, colour="black") 
p

# Fed Interest Rate - 2018
event_data <- Fed_reg_data %>%
  filter(DayOnly == '2018-09-26')
p = ggplot(event_data, aes(x = Datetime, y = Close)) 
p = p+ geom_line() +
  geom_vline(xintercept=as.integer(as.POSIXct("2018-09-26 20:00:00")), 
             linetype=1, colour="black")  +
  scale_x_datetime(limits = c(
    as.POSIXct("2018-09-26 18:00:00"),
    as.POSIXct("2018-09-26 22:30:00"))) +
  geom_vline(xintercept=as.integer(as.POSIXct("2018-09-26 20:30:00")), 
             linetype=4, colour="black") 
p

## FED Sample ----------------
Fed_sample <- final_df_all_new %>%
  dplyr::select(one_of("Datetime", "DayOnly","weekday","Close", "VIX",
                       "Type", "Coding", "Objective",
                       "Subcategory", "Category", 
                       "return5min", 
                       "net_change", "Fed_speak_dummy", "FOMC_dummy", "post_FOMC",
                       "Macro_dummy", "post_macro","Standardized Surprise")) 
Fed_sample$Score <- as.numeric(as.character(Fed_sample$Coding))
Fed_sample$weekday <- factor(Fed_sample$weekday)
Fed_sample$weekday <- relevel(Fed_sample$weekday, "Sunday")


Fed_df_window <- Fed_sample %>%
  arrange(Datetime) %>%
  mutate(cumulative_tweet = cumsum(Fed_speak_dummy)) %>% 
  group_by(cumulative_tweet) %>%
  arrange(cumulative_tweet) %>%
  mutate(time_to_next_tweet = (max(Datetime)-min(Datetime))/60) %>%
  mutate(timeofday = strftime(Datetime, format="%H:%M:%S")) %>%
  ungroup() %>%
  mutate(New_Fed_dummy = ifelse(time_to_next_tweet<=180 | is.na(Coding) == 1,
                                0,Fed_speak_dummy))
Fed_df_window$Objective <- str_replace_all(Fed_df_window$Objective, "Objectives and strategy","Objectives and Strategy") 
Fed_df_window$Objective <- str_replace_all(Fed_df_window$Objective, "Economic outlook","Economic Outlook") 

Fed_df_clean <- Fed_df_window %>%
  ungroup() %>%
  arrange(Datetime) %>%
  mutate(ID = cumsum(New_Fed_dummy)) %>%
  group_by(ID) %>%
  mutate(post_tweet = seq_along(return5min)) %>%
  arrange(desc(Datetime)) %>%
  mutate(pre_tweet = seq_along(return5min)) %>%
  ungroup() %>%
  arrange(Datetime) %>%
  mutate(SentimentGroup_Post = ifelse(New_Fed_dummy == 1,Coding, NA)) %>%
  mutate(SentimentGroup_Pre = ifelse(New_Fed_dummy == 1,Coding, NA)) %>%
  mutate(Subject_Post = ifelse(New_Fed_dummy == 1,Category, NA)) %>%
  mutate(Subject_Pre = ifelse(New_Fed_dummy == 1,Category, NA)) %>%
  mutate(Subcat_Post = ifelse(New_Fed_dummy == 1,Subcategory, NA)) %>%
  mutate(Subcat_Pre = ifelse(New_Fed_dummy == 1,Subcategory, NA)) %>%
  mutate(Type_Post = ifelse(New_Fed_dummy == 1,Type, NA)) %>%
  mutate(Type_Pre = ifelse(New_Fed_dummy == 1,Type, NA)) %>%
  mutate(Objective_Post = ifelse(New_Fed_dummy == 1,Objective, NA)) %>%
  mutate(Objective_Pre = ifelse(New_Fed_dummy == 1,Objective, NA)) %>%
  mutate(pre_tweet = ifelse(lag(pre_tweet,1)==1,1,pre_tweet+1)) %>%
  fill(SentimentGroup_Post, .direction = "down") %>%
  fill(SentimentGroup_Pre, .direction = "up") %>%
  fill(Subject_Post, .direction = "down") %>%
  fill(Subject_Pre, .direction = "up") %>%
  fill(Subcat_Post, .direction = "down") %>%
  fill(Subcat_Pre, .direction = "up") %>%
  fill(Type_Post, .direction = "down") %>%
  fill(Type_Pre, .direction = "up") %>%
  fill(Objective_Post, .direction = "down") %>%
  fill(Objective_Pre, .direction = "up") 
Fed_df_clean$FOMC_dummy[is.na(Fed_df_clean$FOMC_dummy)] <- 0


Fed_reg_data <- Fed_df_clean %>%
  mutate(postwindow = ifelse((ID!=0 & post_tweet <= 120), 1, 0)) %>%
  mutate(prewindow = ifelse((ID!=max(ID) & pre_tweet <= 60), 1, 0)) %>%
  mutate(macrorelease_30min = ifelse((ID!=0 & post_macro <= 30), 1, 0)) %>%
  mutate(FOMC_30min = ifelse((ID!=0 & post_FOMC <= 30), 1, 0)) %>%
  mutate(postwindow_sentiment = ifelse((ID!=0 & post_tweet <= 120), 
                                       SentimentGroup_Post, 0)) %>%
  mutate(prewindow_sentiment = ifelse((ID!=0 & pre_tweet <= 120), 
                                      SentimentGroup_Post, 0)) %>%
  mutate(netchange_5min = Close-lag(Close,5))
Fed_reg_data$MacroSurprise <- Fed_reg_data$`Standardized Surprise`
Fed_reg_data$MacroSurprise[is.na(Fed_reg_data$MacroSurprise)] <- 0

Fed_reg_split_data <- Fed_reg_data %>%
  mutate(post_dummy = ifelse((post_tweet<=120 & ID != 0), post_tweet, 0)) %>%
  mutate(pre_dummy = ifelse((pre_tweet<=60 & ID != max(ID) & pre_tweet !=2), pre_tweet, 0)) 
Fed_reg_split_data$Fed_post_dummies <- factor(Fed_reg_split_data$post_dummy)
#Fed_reg_split_data$Fed_post_dummies <- relevel(Fed_reg_split_data$Fed_post_dummies , 0)
Fed_reg_split_data$Fed_pre_dummies <- factor(Fed_reg_split_data$pre_dummy)
#Fed_reg_split_data$Fed_pre_dummies <- relevel(Fed_reg_split_data$Fed_pre_dummies , 0)


Fed_reg_data$Objective <- str_replace_all(Fed_reg_data$Objective, "Objectives and strategy","Objectives and Strategy") 
Fed_reg_data$Objective <- str_replace_all(Fed_reg_data$Objective, "Economic outlook","Economic Outlook") 
Fed_reg_objective <- Fed_reg_data %>%
  arrange(Datetime) %>%
  mutate(Objective_Post = ifelse(New_Fed_dummy == 1, Objective, NA)) %>%
  fill(Objective_Post, .direction = "down") %>%
  ungroup() %>%
  spread(Objective_Post, postwindow_sentiment, fill = 0) 

Fed_reg_cat <- Fed_reg_data %>%
  spread(Subject_Post, postwindow_sentiment, fill = 0)

Fed_reg_subcat <- Fed_reg_data %>%
  arrange(Datetime) %>%
  mutate(Subcategory_Post = ifelse(New_Fed_dummy == 1, Subcategory, NA)) %>%
  fill(Subcategory_Post, .direction = "down") %>%
  ungroup() %>%
  spread(Subcategory_Post, postwindow_sentiment, fill = 0) 

Fed_reg_type <- Fed_reg_data %>%
  arrange(Datetime) %>%
  mutate(Type_Post = ifelse(New_Fed_dummy == 1, Type, NA)) %>%
  fill(Type_Post, .direction = "down") %>%
  ungroup() %>%
  spread(Type_Post, postwindow_sentiment, fill = 0) 

Fed_reg_sentiment <- Fed_reg_data %>%
  mutate(post_sentiment_dummy = ifelse((post_tweet<=120 & ID != 0), SentimentGroup_Post, "None")) %>%
  mutate(pre_sentiment_dummy = ifelse((pre_tweet<=60 & ID != 0), SentimentGroup_Pre, "None")) 
Fed_reg_sentiment$Fed_post_sentiment <- factor(Fed_reg_sentiment$post_sentiment_dummy)
Fed_reg_sentiment$Fed_post_sentiment <- relevel(Fed_reg_sentiment$Fed_post_sentiment, "None")
Fed_reg_sentiment$Fed_pre_sentiment <- factor(Fed_reg_sentiment$pre_sentiment_dummy)
Fed_reg_sentiment$Fed_pre_sentiment <- relevel(Fed_reg_sentiment$Fed_pre_sentiment, "None")

## FED matched sample test -------------------------
Fed_df_window_120min <- Fed_df_clean %>% filter((post_tweet <= 120 & cumulative_tweet != 0 
                                                 #& (SentimentGroup_Post) == -1
                                                 #&(Objective_Post == "Policy Decisions"
                                                   #| Subject_Post == "US Monetary Policy - Fed Institution"
                                                  #)
                                                 )
                                                | (pre_tweet <= 60  
                                                   #& (SentimentGroup_Pre) == -1
                                                   & cumulative_tweet != as.numeric(max(Fed_df_window$cumulative_tweet))
                                                   #& (Objective_Pre == "Policy Decisions"
                                                      #| Subject_Pre == "US Monetary Policy - Fed Institution"
                                                   #)
                                                   )) %>%
  mutate(Window = ifelse(post_tweet<=120, "After", ifelse(pre_tweet<=60, "Before", NA)))

Boxplot_data <- Fed_df_window_120min %>% group_by(ID, Window) %>% summarise(avg = mean(return5min))

before <- subset(Boxplot_data,  Window == "Before", avg, drop = TRUE)
after <- subset(Boxplot_data,  Window == "After", avg,drop = TRUE)
length(before)
mean(before)
mean(after)
ttest <- t.test(before, after, paired = TRUE, alternative = "two.sided")
ttest$statistic
ttest$p.value




sample_test <- FedSpeak_df_core %>%
  filter(Type == "FOMC meeting related")
group_by(Window, ID) %>%
  summarise(avg = mean(return5min),
            n= n())

var_data <- Trump_df_window_30min %>%
  group_by(Window, ID) %>%
  summarise(var = var(return5min))

## Matched sample test ----------
library("ggpubr")
ggboxplot(Boxplot_data, x = "Window", y = "avg", 
          color = "Window", palette = c("#00AFBB", "#E7B800"),
          order = c("Before", "After"),
          ylab = "5-minute DXY Index returns", xlab = "Groups")

# Subset weight data before treatment
before <- subset(Boxplot_data,  Window == "Before", avg,
                 drop = TRUE)
# subset weight data after treatment
after <- subset(Boxplot_data,  Window == "After", avg,
                drop = TRUE)
length(before)
mean(before)
mean(after)
ttest <- t.test(before, after, paired = TRUE, alternative = "two.sided")
ttest$statistic
ttest$p.value


before_var <- subset(var_data,  Window == "Before", var,
                     drop = TRUE)
after_var <- subset(var_data,  Window == "After", var,
                    drop = TRUE)
length(before_var)
mean(before_var)
mean(after_var)
# F-test - Very sensitive to non-normality
var.test(before, after, alternative = c("two.sided"))

# Levene test
levene.test(Trump_df_window_30min$return5min, 
            as.factor(Trump_df_window_30min$Window), location = "mean")

# Fligner test
fligner.test(Trump_df_window_30min$return5min, 
             Trump_df_window_30min$Window)


plot(density(Trump_df_window$return5min, na.rm = TRUE), 
     main = "5-Minute Returns of the US Dollar Index")
Fed_differences <- before-after
plot(Fed_differences)
hist(Fed_differences)

t.test(differences, mu=0)
t.test(before, after, paired = TRUE, alternative = "two.sided")


pd <- paired(before, after)
plot(pd, type= "profile") + theme_bw()

## Simple OLS regression of AR(1) model with Fed dummies ----------
F1 <- lm(formula = dyn(return5min 
                       ~ lag(return5min, 1) + Fed_post_dummies + VIX),
         data = Fed_reg_split_data)

F2 <- lm(formula = dyn(return5min 
                       ~ lag(return5min, 1) + Fed_post_dummies + VIX 
                       + macrorelease_30min + MacroSurprise),
         data = Fed_reg_split_data)

F3 <- lm(formula = (dyn(return5min 
                        ~ lag(return5min, 1) + `Path of Future Policy Rates` + 
                          `Objectives and Strategy` + `Policy Decisions` +
                          `Current Economic Stance` + `Economic Outlook` + VIX)), 
         data = Fed_reg_objective)

F4 <- lm(formula = (dyn(return5min 
                        ~ lag(return5min, 1) + `Path of Future Policy Rates` + 
                          `Objectives and Strategy` + `Policy Decisions` +
                          `Current Economic Stance` + `Economic Outlook` + VIX
                        + macrorelease_30min + MacroSurprise)), 
         data = Fed_reg_objective)

F5 <- lm(dyn(return5min ~ lag(return5min, 1) + `US Monetary Policy` + 
               `US Economy` + VIX), 
         data = Fed_reg_cat)
F6 <- lm(dyn(return5min ~ lag(return5min, 1) + `US Monetary Policy` + 
               `US Economy` + VIX + macrorelease_30min + MacroSurprise), 
         data = Fed_reg_cat)

F7 <- lm(dyn(return5min ~ lag(return5min, 1) + `US Monetary Policy - Policy Rate` + 
               `US Monetary Policy - Dual Mandate` + `US Monetary Policy - Balance Sheet` +
               `US Economy - General` + `US Economy - Trade` + VIX), 
         data = Fed_reg_subcat)
F8 <- lm(dyn(return5min ~ lag(return5min, 1) + `US Monetary Policy - Policy Rate` + 
               `US Monetary Policy - Dual Mandate` + `US Monetary Policy - Balance Sheet` +
               `US Economy - General` + `US Economy - Trade` + VIX + macrorelease_30min + MacroSurprise), 
         data = Fed_reg_subcat)

F9 <- lm(dyn(return5min ~ lag(return5min, 1) + `FOMC meeting related` + Speech +
               Statement + Testimony + VIX), 
         data = Fed_reg_type)
F10 <- lm(dyn(return5min ~ lag(return5min, 1) + `FOMC meeting related` + Speech +
                Statement + Testimony + VIX + macrorelease_30min + MacroSurprise), 
          data = Fed_reg_type)

F11 <- lm(formula = dyn(return5min 
                        ~ lag(return5min, 1) + postwindow_sentiment + VIX),
          data = Fed_reg_data)

F12 <- lm(formula = dyn(return5min 
                        ~ lag(return5min, 1) + postwindow_sentiment + VIX 
                        + macrorelease_30min + MacroSurprise),
          data = Fed_reg_data)

## Fed regression w/o lagged return as variable -------------------
#FUNCTION to write output to text file
#supply fxn with title of model (in quotes) and model name
writeOutput.F <- function(txt_name,output.title, modelname) {
  sink(file=txt_name, append=TRUE) #creates an empty file; if TRUE, it will append to the file
  print("##########################", row.names=FALSE)
  print(output.title, row.names=FALSE)
  print("OLS Regression results",  row.names=FALSE)
  print("##########################",  row.names=FALSE)
  print("Adj.R2",  row.names=FALSE)
  print(summary(modelname)$adj.r.squared)
  print("BP-test for homoskedasticity",  row.names=FALSE)
  print(bptest(modelname))
  print("BG-test for first order serial correlation",  row.names=FALSE)
  print(bptest(modelname))
  print("Heteroscesticity-Robust Standard Errors",  row.names=FALSE)
  print(coeftest(modelname, vcov = vcovHC(modelname)))
}

FF1 <- lm(formula = (return5min 
                     ~ postwindow + VIX  + FOMC_30min 
                     + macrorelease_30min + MacroSurprise + weekday),
          data = Fed_reg_data)

FF2 <- lm(formula = (return5min  ~ Fed_post_dummies + VIX  
                      + FOMC_dummy + macrorelease_30min + MacroSurprise),
           data = Fed_reg_split_data)

FF3 <- lm(formula = ((return5min  ~  `Path of Future Policy Rates` + 
                        `Objectives and Strategy` + `Policy Decisions` +
                        `Current Economic Stance` + `Economic Outlook` + VIX
                      + FOMC_dummy + macrorelease_30min + MacroSurprise + weekday)), 
          data = Fed_reg_objective)

FF4 <- lm(formula = (return5min ~  `US Monetary Policy` + 
                       `US Economy` + VIX + FOMC_dummy + macrorelease_30min + MacroSurprise + weekday), 
          data = Fed_reg_cat)

FF5 <- lm(formula = (return5min ~  `US Monetary Policy - Policy Rate` + 
                       `US Monetary Policy - Dual Mandate` +
                       `US Economy - General` + `US Economy - Trade` + VIX 
                     + FOMC_dummy + macrorelease_30min + MacroSurprise + weekday), 
          data = Fed_reg_subcat)

FF6 <- lm(formula = (return5min ~ `FOMC meeting related` + Speech +
                        Statement + Testimony + VIX + FOMC_dummy 
                      + macrorelease_30min + MacroSurprise + weekday), 
           data = Fed_reg_type)

FF7 <- lm(formula = (return5min  ~ postwindow_sentiment + VIX 
                      + FOMC_dummy + macrorelease_30min + MacroSurprise + weekday),
           data = Fed_reg_data)

FF8 <- lm(formula = (return5min  ~ Fed_post_sentiment + VIX 
                      + FOMC_dummy + macrorelease_30min + MacroSurprise + weekday),
           data = Fed_reg_sentiment)


txt_name = "Fed_regression_results_HAC_with_weekdays.txt"
writeOutput.F(txt_name, "Fed - all", FF1)
#writeOutput.F(txt_name, "Fed - minute dummies", FF2)
writeOutput.F(txt_name, "Fed - objective", FF3)
writeOutput.F(txt_name, "Fed - subject", FF4)
writeOutput.F(txt_name, "Fed - sub-subject", FF5)
writeOutput.F(txt_name, "Fed - communication type", FF6)
writeOutput.F(txt_name, "Fed - sentiment", FF7)
writeOutput.F(txt_name, "Fed - grouped sentiment", FF8)
closeAllConnections()

FF1 <- lm(formula = (return5min 
                     ~ postwindow + VIX  + FOMC_30min 
                     + macrorelease_30min + MacroSurprise),
          data = Fed_reg_data)

FF2 <- lm(formula = (return5min 
                     ~ postwindow + VIX  + FOMC_30min 
                     + macrorelease_30min + MacroSurprise + weekday),
          data = Fed_reg_data)
txt_name = "Fed_regression_results_HAC.txt"
writeOutput.F(txt_name, "Fed - all - Pre60min", FF1)
writeOutput.F(txt_name, "Fed - all - Pre60min - with Weekdays", FF2)
closeAllConnections()

FF1 <- lm(formula = (return5min 
                     ~ Fed_pre_dummies + VIX  + FOMC_30min 
                     + macrorelease_30min + MacroSurprise),
          data = Fed_reg_split_data)

FF2 <- lm(formula = (return5min 
                     ~ Fed_pre_dummies + VIX  + FOMC_30min 
                     + macrorelease_30min + MacroSurprise + weekday),
          data = Fed_reg_split_data)
txt_name = "Fed_regression_results_pre_minutes_without_t-1.txt"
writeOutput.F(txt_name, "Fed - all - Pre60min", FF1)
writeOutput.F(txt_name, "Fed - all - Pre60min - with Weekdays", FF2)
closeAllConnections()

## Fed panel data -----------------------

Fed_df_panel <- Fed_reg_data %>%
  filter((post_tweet <= 120 & cumulative_tweet != 0) |
           (pre_tweet <= 60 & 
              cumulative_tweet != as.numeric(max(Fed_reg_data$cumulative_tweet)) ))

Fed_df_panel$eventindex <- c(0, rep(1:(nrow(Fed_df_panel)-1)%/%180))

Fed_df_panel_reg <-group_by(Fed_df_panel, eventindex) %>%
  summarise(DXY_change = log(last(Close)/first(Close)),
            VIX_change = log(last(VIX)/first(VIX)),
            Sentiment = nth(Score, 61),
            MS = max(macrorelease_30min),
            Type = nth(Type, 61),
            Objective = nth(Objective, 61),
            Category = nth(Category, 61),
            Subcategory = nth(Subcategory, 61))

Fed_df_panel_reg$Objective <- str_replace_all(Fed_df_panel_reg$Objective, 
                                              "Objectives and strategy","Objectives and Strategy") 
Fed_df_panel_reg$Objective <- str_replace_all(Fed_df_panel_reg$Objective, 
                                              "Economic outlook","Economic Outlook") 
Fed_df_panel_Type <- Fed_df_panel_reg %>%
  spread(Type, Sentiment, fill = 0)

Fed_df_panel_Objective <- Fed_df_panel_reg %>%
  spread(Objective, Sentiment, fill = 0)

Fed_df_panel_Category <- Fed_df_panel_reg %>%
  spread(Category, Sentiment, fill=0)

Fed_df_panel_Subcategory <- Fed_df_panel_reg %>%
  spread(Subcategory, Sentiment, fill=0)

F1 <- lm(formula = (DXY_change ~ Sentiment + VIX_change + MS), 
         data = Fed_df_panel_reg)
summary(F1)

F2 <- lm(formula = (DXY_change ~ `FOMC meeting related` + Speech +
                      Statement + Testimony + + VIX_change + MS), 
         data = Fed_df_panel_Type)
summary(F2)

F3 <- lm(formula = (DXY_change ~ `Path of Future Policy Rates` + 
                      `Objectives and Strategy` + `Policy Decisions` +
                      `Current Economic Stance` + `Economic Outlook` + VIX_change + MS), 
         data = Fed_df_panel_Objective)
summary(F3)

F4 <- lm(formula = (DXY_change ~ `US Monetary Policy` + 
                      `US Economy` + VIX_change + MS), 
         data = Fed_df_panel_Category)
summary(F4)

F5 <- lm(formula = (DXY_change ~ `US Monetary Policy - Policy Rate` + 
                      `US Monetary Policy - Dual Mandate` + `US Monetary Policy - Balance Sheet` +
                      `US Economy - General` + `US Economy - Trade` + VIX_change + MS), 
         data = Fed_df_panel_Subcategory)
summary(F5)


F2 <- lm(formula = dyn(return5min 
                       ~ lag(return5min, 1) + postwindow + VIX 
                       + macrorelease_30min + `Standardized Surprise`), 
         data = Fed_reg_data)

F3 <- lm(formula = dyn(return5min 
                       ~ lag(return5min, 1) + postwindow_sentiment + VIX), 
         data = Fed_reg_data)

F4 <- lm(formula = dyn(return5min 
                       ~ lag(return5min, 1) + postwindow_sentiment + VIX 
                       + macrorelease_30min + `Standardized Surprise`), 
         data = Fed_reg_data)

F5 <- lm(formula = dyn(return5min 
                       ~ lag(return5min, 1) + `US Economy`+ `US Monetary Policy` + VIX), 
         data = Fed_reg_cat)

F6 <- lm(formula = dyn(return5min 
                       ~ lag(return5min, 1) + `US Economy`+ `US Monetary Policy` + VIX 
                       + macrorelease_30min + `Standardized Surprise`) , 
         data = Fed_reg_cat)

T7 <- lm(formula = dyn(return5min 
                       ~ lag(return5min, 1) + `US Economy`+
                         `US Economy - Boast` + `US Economy - Trade` +
                         `US Monetary Policy - Policy Rate` +
                         `US Monetary Policy - Fed Institution` + VIX), 
         data = Trump_test)

T8 <- lm(formula = dyn(return5min 
                       ~ lag(return5min, 1) + `US Economy` +
                         `US Economy - Boast` + `US Economy - Trade` +
                         `US Monetary Policy - Policy Rate` +
                         `US Monetary Policy - Fed Institution` + VIX 
                       + macrorelease_30min + `Standardized Surprise`), 
         data = Trump_test)


summary(F1)
summary(F2)
summary(F3)
summary(F4)
summary(F5)
summary(F6)

summary(T7)
summary(T8)
## TRUMP Sample --------------
Trump_sample <- final_df_all_new %>%
  dplyr::select(one_of("Datetime", "DayOnly","weekday", "Close", "VIX",
                       "Subject", "Score","return5min", 
                       "net_change", "Trump_speak_dummy", 
                       "FOMC_dummy", "post_FOMC",
                       "Macro_dummy", "post_macro","Standardized Surprise")) 
Trump_sample$Score <- as.numeric(as.character(Trump_sample$Score))
Trump_sample$Score[Trump_sample$Score == "NO"] <- 1
Trump_sample$Score[Trump_sample$Score == "?"] <- 0
Trump_sample$weekday <- factor(Trump_sample$weekday)
Trump_sample$weekday <- relevel(Trump_sample$weekday, "Sunday")

Trump_df_window <- Trump_sample %>%
  arrange(Datetime) %>%
  mutate(cumulative_tweet = cumsum(Trump_speak_dummy)) %>% 
  group_by(cumulative_tweet) %>%
  arrange(cumulative_tweet) %>%
  mutate(time_to_next_tweet = (max(Datetime)-min(Datetime))/60) %>%
  mutate(timeofday = strftime(Datetime, format="%H:%M:%S")) %>%
  ungroup() %>%
  mutate(New_Trump_dummy = ifelse(time_to_next_tweet<=60,0,Trump_speak_dummy))

Trump_df_clean <- Trump_df_window %>%
  ungroup() %>%
  arrange(Datetime) %>%
  mutate(ID = cumsum(New_Trump_dummy)) %>%
  group_by(ID) %>%
  mutate(post_tweet = seq_along(return5min)) %>%
  arrange(desc(Datetime)) %>%
  mutate(pre_tweet = seq_along(return5min)) %>%
  ungroup() %>%
  arrange(Datetime) %>%
  mutate(SentimentGroup_Post = ifelse(New_Trump_dummy == 1,Score, NA)) %>%
  mutate(SentimentGroup_Pre = ifelse(New_Trump_dummy == 1,Score, NA)) %>%
  mutate(Subject_Post = ifelse(New_Trump_dummy == 1,Subject, NA)) %>%
  mutate(Subject_Pre = ifelse(New_Trump_dummy == 1,Subject, NA)) %>%
  fill(SentimentGroup_Post, .direction = "down") %>%
  fill(SentimentGroup_Pre, .direction = "up") %>%
  fill(Subject_Post, .direction = "down") %>%
  fill(Subject_Pre, .direction = "up") 

Trump_reg_data <- Trump_df_clean %>%
  mutate(postwindow = ifelse((ID!=0 & post_tweet <= 30), 1, 0)) %>%
  mutate(prewindow = ifelse((ID!=max(ID) & pre_tweet <= 30), 1, 0)) %>%
  mutate(macrorelease_30min = ifelse((ID!=0 & post_macro <= 30), 1, 0)) %>%
  mutate(FOMC_30min = ifelse((ID!=0 & post_FOMC <= 30), 1, 0)) %>%
  mutate(postwindow_sentiment = ifelse((ID!=0 & post_tweet <= 30), 
                                       SentimentGroup_Post, 0)) %>%
  mutate(prewindow_sentiment = ifelse((ID!=0 & pre_tweet <= 30), 
                                      SentimentGroup_Post, 0)) %>%
  mutate(netchange_5min = Close-lag(Close,5))
Trump_reg_data$MacroSurprise <- Trump_reg_data$`Standardized Surprise`
Trump_reg_data$MacroSurprise[is.na(Trump_reg_data$MacroSurprise)] <- 0

Trump_reg_subcat <- Trump_reg_data %>%
  spread(Subject_Post, postwindow, fill = 0) 

Trump_reg_cat <- Trump_reg_subcat %>%
  mutate(Econ = if_else((`US Economy` == 1 |
                           `US Economy - Boast` == 1 |
                           `US Economy - Trade`== 1), 1, 0)) %>%
  mutate(MonPol = if_else((`US Monetary Policy - Policy Rate` 
                           + `US Monetary Policy - Fed Institution`)>=1, 1, 0)) 

Trump_reg_split_data <- Trump_reg_data %>%
  mutate(post_dummy = ifelse((post_tweet<=30 & ID != 0), post_tweet, 0))%>%
  mutate(pre_dummy = ifelse((pre_tweet<=30 & ID != max(ID)), pre_tweet, 0)) 
Trump_reg_split_data$Trump_post_dummies <- factor(Trump_reg_split_data$post_dummy)
Trump_reg_split_data$Trump_pre_dummies <- factor(Trump_reg_split_data$pre_dummy)

Trump_reg_sentiment <- Trump_reg_data %>%
  mutate(post_sentiment_dummy = ifelse((post_tweet<=30 & ID != 0), SentimentGroup_Post, "None")) 
Trump_reg_sentiment$Trump_post_sentiment <- factor(Trump_reg_sentiment$post_sentiment_dummy)
Trump_reg_sentiment$Trump_post_sentiment <- relevel(Trump_reg_sentiment$Trump_post_sentiment, "None")

## Simple OLS w/o lagged returns for Trump tweets -----------
T1 <- lm(formula = (return5min ~ postwindow + VIX),
         data = Trump_reg_data)

T2 <- lm(formula = (return5min 
                    ~ postwindow + VIX  + FOMC_30min + macrorelease_30min + MacroSurprise + weekday),
         data = Trump_reg_data)

T1A <- lm(formula = (return5min ~  Trump_post_dummies + VIX),
          data = Trump_reg_split_data)

T2A <- lm(formula = (return5min  ~ Trump_post_dummies + VIX  + FOMC_30min + macrorelease_30min + MacroSurprise),
          data = Trump_reg_split_data)

T3 <- lm(formula = (return5min  ~ postwindow_sentiment + VIX), 
         data = Trump_reg_data)

T4 <- lm(formula = (return5min ~  postwindow_sentiment + VIX  + FOMC_30min + macrorelease_30min + MacroSurprise + weekday), 
         data = Trump_reg_data)

T3A <- lm(formula = (return5min  ~ Trump_post_sentiment + VIX), 
          data = Trump_reg_sentiment)

T4A <- lm(formula = (return5min ~  Trump_post_sentiment + VIX  + FOMC_30min + macrorelease_30min + MacroSurprise), 
          data = Trump_reg_sentiment)

T5 <- lm(formula = (return5min ~ Econ + MonPol + VIX), 
         data = Trump_reg_cat)

T6 <- lm(formula = (return5min ~  Econ + MonPol + VIX 
                    + FOMC_30min + macrorelease_30min + MacroSurprise + weekday) , 
         data = Trump_reg_cat)

T7 <- lm(formula = (return5min ~  `US Economy`+
                      `US Economy - Boast` + `US Economy - Trade` +
                      `US Monetary Policy - Policy Rate` +
                      `US Monetary Policy - Fed Institution` + VIX), 
         data = Trump_reg_cat)

T8 <- lm(formula = (return5min ~ `US Economy` +
                      `US Economy - Boast` + `US Economy - Trade` +
                      `US Monetary Policy - Policy Rate` +
                      `US Monetary Policy - Fed Institution` + VIX 
                    + FOMC_30min + macrorelease_30min + MacroSurprise + weekday), 
         data = Trump_reg_cat)


txt_name = "Trump_regression_results_30min_with_weekdays.txt"
#writeOutput.F(txt_name, "Trump - all - No macro control", T1)
writeOutput.F(txt_name, "Trump - all - With macro control", T2)
#writeOutput.F(txt_name, "Trump - minute dummies - No macro control", T1A)
#writeOutput.F(txt_name, "Trump - minute dummies - With macro control", T2A)
#writeOutput.F(txt_name, "Trump - sentiment - No macro control", T3)
writeOutput.F(txt_name, "Trump - sentiment - With macro control", T4)
#writeOutput.F(txt_name, "Trump - postsentiment minutes - No macro control", T3A)
#writeOutput.F(txt_name, "Trump - postsentiment minutes - With macro control", T4A)
#writeOutput.F(txt_name, "Trump - category - No macro control", T5)
writeOutput.F(txt_name, "Trump - category - With macro control", T6)
#writeOutput.F(txt_name, "Trump - subcategory - No macro control", T7)
writeOutput.F(txt_name, "Trump - subcategory - With macro control", T8)
closeAllConnections()


txt_name = "Trump_regression_results_pre_minutes_NEW.txt"
T1 <- lm(formula = (return5min 
                    ~ Trump_pre_dummies + VIX  + FOMC_30min + macrorelease_30min + MacroSurprise),
         data = Trump_reg_split_data)
T2 <- lm(formula = (return5min 
                    ~ Trump_pre_dummies + VIX  + FOMC_30min + macrorelease_30min 
                    + MacroSurprise + weekday),
         data = Trump_reg_split_data)

writeOutput.F(txt_name, "Trump - all - Post window", T1)
writeOutput.F(txt_name, "Trump - all - Pre window", T2)
closeAllConnections()

summary(T8)

par(mfrow = c(2,2))
plot(T8)

## Trump etc ------------
#%>% spread(post_dummy, postwindow, fill = 0)

#Trump_post_dummies <- Trump_reg_split_data[,(ncol(Trump_reg_split_data)-30+1):ncol(Trump_reg_split_data)]


Trump_test <- Trump_reg_cat %>%
  mutate(Econ = if_else((`US Economy` == 1 |
                           `US Economy - Boast` == 1 |
                           `US Economy - Trade`== 1), 1, 0)) %>%
  mutate(MonPol = if_else((`US Monetary Policy - Policy Rate` 
                           + `US Monetary Policy - Fed Institution`)>=1, 1, 0)) 
Trump_reg_cat$`US Economy`
#test <- filter(Trump_df_window, Datetime >= '2018-06-22 01:00:00')
Trump_df_window_30min <- Trump_df_clean %>%
  filter((post_tweet <= 30 & 
            cumulative_tweet != 0  
            #&(SentimentGroup_Post) == 0 &
            #(Subject_Post == "US Monetary Policy - Policy Rate"
           #   | Subject_Post == "US Monetary Policy - Fed Institution"
          #  )
          ) 
         | (pre_tweet <= 30 & 
              #(SentimentGroup_Pre) == 0 &
              cumulative_tweet != as.numeric(max(Trump_df_window$cumulative_tweet)) 
              # & (  Subject_Pre == "US Monetary Policy - Policy Rate"
              #  | Subject_Pre == "US Monetary Policy - Fed Institution"
              #)
         )) %>%
  mutate(Window = ifelse(post_tweet<=30, "After", 
                         ifelse(pre_tweet<=30, "Before", NA)))

Trump_df_event_fig <- Trump_df_window_30min %>%
  mutate(timing = ifelse(pre_tweet<=30, -pre_tweet, 
                         ifelse(post_tweet<=30, post_tweet, NA))) %>%
  group_by(timing) %>%
  summarize(
    min = min(return5min)*100,
    mean = mean(return5min)*100,
    max = max(return5min)*100
  )

plot = ggplot(data = Trump_df_event_fig, aes(x=timing, y=mean)) +
  geom_ribbon( aes(ymin = min, ymax = max, color = NULL), alpha = .15) +
  geom_line( aes(y = mean), size = 1)
plot

Boxplot_data <- Trump_df_window_30min %>%
  group_by(ID, Window) %>%
  summarise(avg = mean(return5min))

sample_test <- Trump_df_window_30min %>%
  group_by(Window, ID) %>%
  summarise(avg = mean(return5min),
            n= n())

var_data <- Trump_df_window_30min %>%
  group_by(Window, ID) %>%
  summarise(var = var(return5min))

## Matched sample test ----------
library("ggpubr")
ggboxplot(Boxplot_data, x = "Window", y = "avg", 
          color = "Window", palette = c("#00AFBB", "#E7B800"),
          order = c("Before", "After"),
          ylab = "5-minute DXY Index returns", xlab = "Groups")

# Subset weight data before treatment
before <- subset(Boxplot_data,  Window == "Before", avg,
                 drop = TRUE)
# subset weight data after treatment
after <- subset(Boxplot_data,  Window == "After", avg,
                drop = TRUE)
length(before)
mean(before)
mean(after)
ttest <- t.test(before, after, paired = TRUE, alternative = "two.sided")
ttest$statistic
ttest$p.value


before_var <- subset(var_data,  Window == "Before", var,
                     drop = TRUE)
after_var <- subset(var_data,  Window == "After", var,
                    drop = TRUE)
length(before_var)
mean(before_var)
mean(after_var)
# F-test - Very sensitive to non-normality
var.test(before, after, alternative = c("two.sided"))

# Levene test
levene.test(Trump_df_window_30min$return5min, 
            as.factor(Trump_df_window_30min$Window), location = "mean")

# Fligner test
fligner.test(Trump_df_window_30min$return5min, 
             Trump_df_window_30min$Window)


plot(density(Trump_df_window$return5min, na.rm = TRUE), 
     main = "5-Minute Returns of the US Dollar Index")
Trump_differences <- before-after
plot(Trump_differences)
hist(Trump_differences)

par(mfrow=c(1,1))
plot(density(Fed_differences), type='l', xlab="Post-Event Abnormal Return", main="Federal Reserve Statements")
plot(density(Trump_differences), xlab="Post-Event Abnormal Return", main="Donald Trump Tweets")


t.test(differences, mu=0)
t.test(before, after, paired = TRUE, alternative = "two.sided")


pd <- paired(before, after)
plot(pd, type= "profile") + theme_bw()




## Simple OLS regression of AR(1) model with Trump dummies ----------
T1 <- lm(formula = dyn(return5min 
                       ~ lag(return5min, 1) + Trump_post_dummies + VIX), 
         data = Trump_reg_split_data)

T2 <- lm(formula = dyn(return5min ~ lag(return5min, 1) + Trump_post_dummies + VIX 
                       + macrorelease_30min + MacroSurprise), 
         data = Trump_reg_split_data)

T3 <- lm(formula = dyn(return5min 
                       ~ lag(return5min, 1) + postwindow_sentiment + VIX), 
         data = Trump_reg_data)

T4 <- lm(formula = dyn(return5min 
                       ~ lag(return5min, 1) + postwindow_sentiment + VIX 
                       + macrorelease_30min + `Standardized Surprise`), 
         data = Trump_reg_data)

T5 <- lm(formula = dyn(return5min 
                       ~ lag(return5min, 1) + Econ + MonPol + VIX), 
         data = Trump_test)

T6 <- lm(formula = dyn(return5min 
                       ~ lag(return5min, 1) + Econ + MonPol + VIX 
                       + macrorelease_30min + `Standardized Surprise`) , 
         data = Trump_test)

T7 <- lm(formula = dyn(return5min 
                       ~ lag(return5min, 1) + `US Economy`+
                         `US Economy - Boast` + `US Economy - Trade` +
                         `US Monetary Policy - Policy Rate` +
                         `US Monetary Policy - Fed Institution` + VIX), 
         data = Trump_test)

T8 <- lm(formula = dyn(return5min 
                       ~ lag(return5min, 1) + `US Economy` +
                         `US Economy - Boast` + `US Economy - Trade` +
                         `US Monetary Policy - Policy Rate` +
                         `US Monetary Policy - Fed Institution` + VIX 
                       + macrorelease_30min + `Standardized Surprise`), 
         data = Trump_test)


summary(T1)
summary(T2)
summary(T3)
summary(T4)
summary(T5)
summary(T6)
summary(T7)
summary(T8)








test <- final_df_all_new %>%
  group_by(date(Datetime), EventDay) %>%
  summarise(max = mean(return5min),
            min = min(return5min))

## Preparation for intraday FFF regression ------------
fff <- spotvol()

# make complete time sequence
final_df_5min <- final_df_all[-(1:3), , drop = FALSE]%>%
  mutate(Time = ymd_hms(Datetime)) %>%
  group_by(Time = cut(Time, breaks="5 min")) %>% 
  summarise(RETURN = (log(last(PRICE))-log(first(PRICE)))*100,
            TRUMP = max(Trump_speak_dummy),
            FED = max(Fed_speak_dummy),
            Macro = max(Macro_dummy))
final_df_5min <- subset(final_df_5min, 
                        as.POSIXct(final_df_5min$Time) >= "2018-01-02 00:00:00" 
                        &  as.POSIXct(final_df_5min$Time) <= "2019-09-29 23:55:00") 

## GARCH ---------------
Trump_garch <- Trump_reg_data %>%
  filter(!is.na(VIX)) 

Trump_garch_external <- Trump_garch %>%
  dplyr::select(one_of("VIX","postwindow", "macrorelease_30min", "MacroSurprise"))

spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1), 
                                        external.regressors =  Trump_garch_external), 
                  mean.model = list(armaOrder = c(1,1), 
                                    external.regressors =  Trump_garch_external),
                  distribution.model = "norm")
fit = ugarchfit(spec = spec, data = Trump_garch$return5min*100)
show(fit)
plot(fit)


test <- final_df_5min
test$ID <- seq.int(nrow(final_df_5min))

test <- test %>% 
  mutate(Time = ymd_hms(Time)) %>%
  group_by(Time = cut(Time, breaks="1 day")) %>% 
  summarise(count = n())

## Estimation ---------
final_df_5min_level <- final_df_all[-(1:3), , drop = FALSE]%>%
  mutate(Time = ymd_hms(Datetime, tz = "CET")) %>%
  group_by(Time = cut(Time, breaks="5 min")) %>% 
  summarise(PRICE = (last(Close)+first(Close))/2)

attributes(final_df_5min_level$Time)$tzone <- "GMT"

final_df_5min_level <- subset(final_df_5min_level, 
                              as.POSIXct(final_df_5min_level$Time) >= "2018-03-02 00:00:00" 
                              &  as.POSIXct(final_df_5min_level$Time) <= "2019-09-27 23:55:00") 

data_new <- as.xts(final_df_5min_level[,-1], 
                   order.by=as.POSIXct(final_df_5min_level$Time, tz = "GMT"),
                   tz = "GMT")
data_table_5min <- as.data.table(data_new)
names(data_table_5min)[names(data_table_5min)=="index"] <- "DT"


# Default method, deterministic periodicity
vol <- spotvol(data_new, method = "garch", model = "sGARCH",
               on = "minutes", k = 5,
               marketopen = "00:00:00", marketclose = "23:55:00", 
               tz = "GMT")
plot(vol)







vol1 <- spotvol(data_new, method = "garch")
plot(vol1)

, method = "detper",
on = "minutes", k = 5,
marketopen = "00:00:00", marketclose = "23:55:00")
plot(vol1)


on = "minutes" 
k = 5 
delta <- k*60 
marketopen = "00:00:00" 
marketclose = "23:55:00"
tz = "GMT"
data <- xts(data_new, order.by = as.POSIXct(time(data_new), tz = tz), tzone = tz)
dates <- unique(format(time(data), "%Y-%m-%d"))
cDays <- length(dates)
rdata <- mR <- c()
intraday <- seq(from = chron::times(marketopen), 
                to = chron::times(marketclose), 
                by = chron::times(delta/(24*3600))) 
if (as.character(tail(intraday, 1)) != marketclose) 
  intraday <- c(intraday, marketclose)
intraday <- intraday[2:length(intraday)]
for (d in 1:cDays) {
  datad <- data[as.character(dates[d])]
  if (!all(format(time(datad), format = "%Z") == tz)) 
    stop(paste("Not all data on ", dates[d], " is in time zone \"", tz,
               "\". This may be due to daylight saving time. Try using a",
               " time zone without daylight saving, such as GMT.", 
               sep = ""))
  datad <- aggregatePrice(datad, on = on, k = k , marketopen = marketopen,
                          marketclose = marketclose, tz = tz)
  z <- xts(rep(1, length(intraday)), tzone = tz, 
           order.by = as.POSIXct(paste(dates[d], as.character(intraday), 
                                       sep=" "), tz = tz))
  datad <- merge.xts(z, datad)$datad
  datad <- na.locf(datad)
  rdatad <- makeReturns(datad)
  rdatad <- rdatad[time(rdatad) > min(time(rdatad))]
  rdata <- rbind(rdata, rdatad)
  mR <- rbind(mR, as.numeric(rdatad))
  
}

detper(mR)

par(mfrow=c(1,1))
plot(vol1$spot)


# Compare to stochastic periodicity
init <- list(sigma = 0.03, sigma_mu = 0.005, sigma_h = 0.007,
             sigma_k = 0.06, phi = 0.194, rho = 0.986, mu = c(1.87,-0.42),
             delta_c = c(0.25, -0.05, -0.2, 0.13, 0.02),
             delta_s = c(-1.2, 0.11, 0.26, -0.03, 0.08))
# next method will take around 110 iterations
vol2 <- spotvol(sample_real5minprices, method = "stochper", init = init)
plot(as.numeric(vol1$spot[1:780]), type="l")
lines(as.numeric(vol2$spot[1:780]), col="red")

## SpotVol functions -----------------------------
spotvol <- function(data, method = "detper", ..., on = "minutes", k = 5,
                    marketopen = "09:30:00", marketclose = "16:00:00",
                    tz = "GMT")  
{
  if (on == "seconds" | on == "secs") 
    delta <- k 
  if (on == "minutes" | on == "mins") 
    delta <- k*60  
  if (on == "hours") 
    delta <- k*3600 
  
  if (inherits(data, what = "xts")) {
    data <- xts(data, order.by = as.POSIXct(time(data), tz = tz), tzone = tz)
    dates <- unique(format(time(data), "%Y-%m-%d"))
    cDays <- length(dates)
    rdata <- mR <- c()
    intraday <- seq(from = chron::times(marketopen), 
                    to = chron::times(marketclose), 
                    by = chron::times(delta/(24*3600))) 
    if (as.character(tail(intraday, 1)) != marketclose) 
      intraday <- c(intraday, marketclose)
    intraday <- intraday[2:length(intraday)]
    for (d in 1:cDays) {
      datad <- data[as.character(dates[d])]
      if (!all(format(time(datad), format = "%Z") == tz)) 
        stop(paste("Not all data on ", dates[d], " is in time zone \"", tz,
                   "\". This may be due to daylight saving time. Try using a",
                   " time zone without daylight saving, such as GMT.", 
                   sep = ""))
      datad <- aggregatePrice(datad, on = on, k = k , marketopen = marketopen,
                              marketclose = marketclose, tz = tz)
      z <- xts(rep(1, length(intraday)), tzone = tz, 
               order.by = as.POSIXct(paste(dates[d], as.character(intraday), 
                                           sep=" "), tz = tz))
      datad <- merge.xts(z, datad)$datad
      datad <- na.locf(datad)
      rdatad <- makeReturns(datad)
      rdatad <- rdatad[time(rdatad) > min(time(rdatad))]
      rdata <- rbind(rdata, rdatad)
      mR <- rbind(mR, as.numeric(rdatad))
    }
  } else if (class(data) == "matrix") {
    mR <- data
    rdata <- NULL
  } else stop("Input data has to consist of either of the following: 
            1. An xts object containing price data
            2. A matrix containing return data")
  
  options <- list(...)
  out <- switch(method, 
                detper = detper(mR, rdata = rdata, options = options), 
                stochper = stochper(mR, rdata = rdata, options = options),
                kernel = kernelestim(mR, rdata = rdata, delta, options = options),
                piecewise = piecewise(mR, rdata = rdata, options = options),
                garch = garch_s(mR, rdata = rdata, options = options))  
  return(out)
}

# Deterministic periodicity model
# 
# Modified spotVol function from highfrequency package
detper <- function(mR, rdata = NULL, options = list()) 
{
  # default options, replace if user-specified
  op <- list(dailyvol = "bipower", periodicvol = "TML", dummies = FALSE, 
             P1 = 5, P2 = 5)
  op[names(options)] <- options 
  
  cDays <- nrow(mR)
  M <- ncol(mR)
  if (cDays == 1 & is.null(rdata)) { 
    mR <- as.numeric(mR)
    estimdailyvol <- switch(op$dailyvol, 
                            bipower = rBPCov(mR), 
                            medrv = medRV(mR), 
                            rv = rCov(mR))
  } else {
    if (is.null(rdata)) {
      estimdailyvol <- switch(op$dailyvol, 
                              bipower = apply(mR, 1, "rBPCov"),
                              medrv = apply(mR, 1, "medRV"),
                              rv = apply(mR, 1, "rCov"))
    } else {
      estimdailyvol <- switch(op$dailyvol, 
                              bipower = apply.daily(rdata, rBPCov),
                              medrv = apply.daily(rdata, medRV),
                              rv = apply.daily(rdata, rCov))
      dates = time(estimdailyvol)
    }
  }  
  if (cDays <= 50) {
    print("Periodicity estimation requires at least 50 observations. 
          Periodic component set to unity")
    estimperiodicvol = rep(1, M)
  } else {
    mstdR <- mR/sqrt(as.numeric(estimdailyvol) * (1/M))
    selection <- c(1:M)[ (nrow(mR)-apply(mR,2,'countzeroes')) >=20] 
    # preferably no na is between
    selection <- c( min(selection) : max(selection) )
    mstdR <- mstdR[,selection]
    estimperiodicvol_temp <- diurnal(stddata = mstdR, method = op$periodicvol, 
                                     dummies = op$dummies, P1 = op$P1, 
                                     P2 = op$P2)[[1]]
    estimperiodicvol <- rep(1,M)
    estimperiodicvol[selection] <- estimperiodicvol_temp
    mfilteredR <- mR/matrix(rep(estimperiodicvol, cDays), byrow = T, 
                            nrow = cDays)
    estimdailyvol <- switch(op$dailyvol, 
                            bipower = apply(mfilteredR, 1, "rBPCov"),
                            medrv = apply(mfilteredR, 1, "medRV"), 
                            rv = apply(mfilteredR, 1, "rCov"))
    spot <- rep(sqrt(as.numeric(estimdailyvol) * (1/M)), each = M) * 
      rep(estimperiodicvol, cDays)
    if (is.null(rdata)) {
      spot <- matrix(spot, nrow = cDays, ncol = M, byrow = TRUE)
    } else {
      spot <- xts(spot, order.by = time(rdata))
      estimdailyvol <- xts(estimdailyvol, order.by = dates)
      estimperiodicvol <- xts(estimperiodicvol, order.by = time(rdata[1:M]))
    }
    out <- list(spot = spot, daily = estimdailyvol, periodic = estimperiodicvol)
    class(out) <- "spotvol"
    return(out)
  }
}

# Stochastic periodicity model
# 
# This function estimates the spot volatility by using the stochastic periodcity
# model of Beltratti & Morana (2001)
stochper <- function(mR, rdata = NULL, options = list()) 
{
  #require(FKF)
  # default options, replace if user-specified
  op <- list(init = list(), P1 = 5, P2 = 5, control = list(trace=1, maxit=500))
  op[names(options)] <- options 
  
  N <- ncol(mR)
  days <- nrow(mR)
  mR[mR == 0] <- NA
  logr2 <- log(mR^2)
  rvector <- as.vector(t(logr2)) 
  lambda <- (2*pi)/N;
  
  # default starting values of parameters
  sp <- list(sigma = 0.03,
             sigma_mu = 0.005,
             sigma_h = 0.005,
             sigma_k = 0.05,
             phi = 0.2,
             rho = 0.98,
             mu = c(2, -0.5),
             delta_c = rep(0, max(1,op$P1)),
             delta_s = rep(0, max(1,op$P2)))
  
  # replace if user has specified different values
  sp[names(op$init)] <- op$init
  
  # check input
  for (i in c("sigma", "sigma_mu", "sigma_h", "sigma_k", "phi", "rho")) {
    if (sapply(sp, length)[i] != 1) stop(paste(i, " must be a scalar"))  
  }
  if (length(sp$mu) != 2) 
    stop("mu must have length 2")
  if (length(sp$delta_c) != op$P1 & op$P1 > 0) 
    stop("delta_c must have length equal to P1")
  if (length(sp$delta_s) != op$P2 & op$P2 > 0) 
    stop("delta_s must have length equal to P2")
  if (length(sp$delta_c) < 1) 
    stop("delta_c must at least have length 1")
  if (length(sp$delta_s) < 1) 
    stop("delta_s must at least have length 1")
  
  # transform parameters to allow for unrestricted optimization 
  # (domain -Inf to Inf)
  par_t <- c(sigma = log(sp$sigma), sigma_mu = log(sp$sigma_mu), 
             sigma_h = log(sp$sigma_h), sigma_k = log(sp$sigma_k), 
             phi = log(sp$phi/(1-sp$phi)), rho = log(sp$rho/(1-sp$rho)),
             mu = sp$mu, delta_c = sp$delta_c, delta_s = sp$delta_s) 
  
  opt <- optim(par_t, loglikBM, yt = rvector, N = N, days = days, P1 = op$P1, 
               P2 = op$P2, method="BFGS", control = op$control)
  
  # recreate model to obtain volatility estimates
  ss <- ssmodel(opt$par, days, N, P1 = op$P1, P2 = op$P2)
  kf <- FKF::fkf(a0 = ss$a0, P0 = ss$P0, dt = ss$dt, ct = ss$ct, Tt = ss$Tt, 
                 Zt = ss$Zt, HHt = ss$HHt, GGt = ss$GGt, 
                 yt = matrix(rvector, ncol = length(rvector)))
  sigmahat <- as.vector(exp((ss$Zt%*%kf$at[,1:(N*days)] + ss$ct + 1.27)/2))
  
  # transform parameter estimates back
  estimates <- c(exp(opt$par["sigma"]), exp(opt$par["sigma_mu"]), 
                 exp(opt$par["sigma_h"]), exp(opt$par["sigma_k"]),
                 exp(opt$par["phi"])/(1+exp(opt$par["phi"])), 
                 exp(opt$par["rho"])/(1+exp(opt$par["rho"])), opt$par[-(1:6)])
  
  if (is.null(rdata)) {
    spot <- matrix(sigmahat, nrow = days, ncol = N, byrow = TRUE)
  } else {
    spot <- xts(sigmahat, order.by = time(rdata))
  }
  out <- list(spot = spot, par = estimates)
  class(out) <- "spotvol"
  return(out)
}

# Calculate log likelihood using Kalman Filter
# 
# This function returns the average log likehood value of the stochastic 
# periodicity model, given the input parameters.
loglikBM <- function(par_t, yt, days, N = 288, P1 = 5, P2 = 5)
{
  ss <- ssmodel(par_t, days, N, P1 = P1, P2 = P2)
  yt <- matrix(yt, ncol = length(yt))
  kf <- FKF::fkf(a0 = ss$a0, P0 = ss$P0, dt = ss$dt, ct = ss$ct, Tt = ss$Tt, 
                 Zt = ss$Zt, HHt = ss$HHt, GGt = ss$GGt, yt = yt)
  return(-kf$logLik/length(yt))
}

# Generate state space model
# 
# This function creates the state space matrices from the input parameters.
# The output is in the format used by the FKF package.
ssmodel <- function(par_t, days, N = 288, P1 = 5, P2 = 5)
{
  par <- c(exp(par_t["sigma"]), exp(par_t["sigma_mu"]), exp(par_t["sigma_h"]), 
           exp(par_t["sigma_k"]), exp(par_t["phi"])/(1+exp(par_t["phi"])), 
           exp(par_t["rho"])/(1+exp(par_t["rho"])), par_t[-(1:6)])
  lambda <- (2*pi)/288
  a0 <- c(0, 0, par["delta_c1"], par["delta_s1"])
  if (P1 == 0) 
    a0[3] <- par["delta_c"]
  if (P2 == 0) 
    a0[4] <- par["delta_s"]   
  m <- length(a0)
  P0 <- Tt <- Ht <- matrix(0, m, m)
  diag(Tt) <- c(1, par["phi"], rep(par["rho"]*cos(lambda), 2))
  Tt[3,4] <- par["rho"]*sin(lambda)
  Tt[4,3] <- par["rho"]*-sin(lambda)
  Zt <- matrix(c(1, 1, 1, 0), ncol = m)
  Gt <- sqrt(0.5*pi^2)
  GGt <- Gt %*% t(Gt)
  diag(Ht) <- c(par["sigma_mu"], par["sigma_h"], rep(par["sigma_k"], 2))
  HHt <- Ht %*% t(Ht)
  dt <- matrix(0, nrow = m)
  ct <- log(par["sigma"]^2) - 1.270363
  
  # calculate deterministic part c2, add to ct
  n <- 1:N
  M1 <- (2*n)/(N+1)
  M2 <- (6*n^2)/((N+1)*(N+2))
  c2 <- par["mu1"]*M1 + par["mu2"]*M2
  if (P1 > 1) {
    for (k in 2:P1) {
      c2 <- c2 + par[paste("delta_c", k, sep="")]*cos(k*lambda*n) 
    }  
  }
  if (P2 > 1) {
    for (p in 2:P2) {
      c2 <- c2 + par[paste("delta_s", p, sep="")]*sin(p*lambda*n)
    }  
  }
  ct <- matrix(ct + c2, ncol = N*days)
  
  return(list(a0 = a0, P0 = P0, Tt = Tt, Zt = Zt, GGt = GGt, HHt = HHt, 
              dt = dt, ct = ct))
}

# Kernel estimation method
# 
# See Kristensen (2010)
kernelestim <- function(mR, rdata = NULL, delta = 300, options = list())
{
  # default options, replace if user-specified
  op <- list(type = "gaussian", h = NULL, est = "cv", lower = NULL, 
             upper = NULL)
  op[names(options)] <- options
  
  D <- nrow(mR)
  N <- ncol(mR)
  if (N < 100 & op$est == "cv") 
    warning("Cross-validation may not return optimal results in small samples.")
  if (op$type == "beta" & op$est == "quarticity" ) {
    warning("No standard estimator available for Beta kernel bandwidth.
                Cross-validation will be used instead.")
    op$est = "cv" 
  }
  t <- (1:N)*delta
  S <- N*delta
  if (is.null(op$h)) { 
    h <- numeric(D) 
  } else {
    h <- rep(op$h, length.out = D)
  }
  sigma2hat <- matrix(NA, nrow = D, ncol = N)
  for(d in 1:D) {
    if (is.null(op$h)) {
      quarticity <- (N/3)*rowSums(mR^4)
      qscale <- quarticity^0.2
      qmult <- qscale/sqrt((1/D)*sum(qscale^2))
      if (op$est == "cv") 
        cat(paste("Estimating optimal bandwidth for day", d, "of", D, "...\n"))
      h[d] <- estbandwidth(mR[d, ], delta = delta, qmult = qmult[d], 
                           type = op$type, est = op$est, lower = op$lower, 
                           upper = op$upper)
    }
    for(n in 1:N) {
      if (op$type == "beta") {
        K <- kernelk(t/S, type = op$type, b = h[d], y = t[n]/S)
      } else {
        K <- kernelk((t-t[n])/h[d], type = op$type)/h[d]
      }
      K <- K/sum(K)
      sigma2hat[d, n] <- K %*% (mR[d, ]^2)
    }
  }
  spot <- as.vector(t(sqrt(sigma2hat)))
  if (is.null(rdata)) {
    spot <- matrix(spot, nrow = D, ncol = N, byrow = TRUE)
  } else {
    spot <- xts(spot, order.by = time(rdata))
  }
  out <- list(spot = spot, par = list(h = h))
  class(out) <- "spotvol"
  return(out)
}

# calculate values of certain kernels
# arguments b and y only needed for type == "beta"
kernelk <- function(x, type = "gaussian", b = 1, y = 1)
{
  if (type == "gaussian") 
    return(dnorm(x))  
  if (type == "epanechnikov") {
    z <- (3/4)*(1-x^2)
    z[abs(x) > 1] <- 0
    return(z)
  }
  if (type == "beta") 
    return(dbeta(x, y/b + 1, (1-y)/b + 1))
}

# estimate optimal bandwidth paramater h
# by default, this is done through crossvalidation (cv)
# else the formula for h_opt in Kristensen(2010) is approximated
estbandwidth <- function(x, delta = 300, qmult = 1, type = "gaussian", 
                         est = "cv", lower = NULL, upper = NULL)
{
  N <- length(x)
  S <- N*delta
  default <- bw.nrd0((1:N)*delta)
  if (type == "epanechnikov") 
    default <- default*2.34
  if (est == "quarticity")  
    h <- default*qmult 
  if (est == "cv") {
    if (type == "beta") {
      if (is.null(lower)) 
        lower <- 0.0001
      if (is.null(upper)) 
        upper <- 1
    } else {
      if (is.null(lower)) 
        lower <- default/3
      if (is.null(upper)) 
        upper <- default*3
    }
    opt <- optimize(ISE, c(lower, upper), x = x, type = type, delta = delta)
    h <- opt$minimum
  }
  return(h)
}

# calculate Integrated Square Error, given bandwidth h
ISE <- function(h, x, delta = 300, type = "gaussian")
{
  N <- length(x)
  t <- (1:N)*delta
  S <- N*delta
  sigma2hat <- rep(NA, N)
  for(n in 1:N) {
    if (type == "beta") {
      K <- kernelk(t/S, type = type, b = h, y = t[n]/S)
    } else {
      K <- kernelk((t - t[n])/h, type = type)/h
    }
    K[n] <- 0
    K <- K/sum(K)
    sigma2hat[n] <- K %*% (x^2) 
  }    
  tl <- 5
  tu <- N-5
  ISE <- sum(((x[tl:tu]^2) - sigma2hat[tl:tu])^2)
  return(ISE)
}

# Piecewise constant volatility method
# See Fried (2012)
piecewise <- function(mR, rdata = NULL, options = list())
{
  # default options, replace if user-specified
  op <- list(type = "MDa", m = 40, n = 20, alpha = 0.005, volest = "bipower",
             online = TRUE)
  op[names(options)] <- options
  
  N <- ncol(mR)
  D <- nrow(mR)
  vR <- as.numeric(t(mR))
  spot <- rep(NA, N*D)
  cp <- changePoints(vR, type = op$type, alpha = op$alpha, m = op$m, n = op$n)  
  for (i in 1:(N*D)) {
    if (op$online) {
      if (i > op$n) {
        lastchange <- max(which(cp + op$n < i))
      } else {
        lastchange = 1
      } 
      lastchange <- cp[lastchange]
      spot[i] = switch(op$volest, 
                       bipower = sqrt((1/(i - lastchange + 1)) * 
                                        (rBPCov(vR[(lastchange + 1):i]))),
                       medrv = sqrt((1/(i - lastchange + 1)) * 
                                      (medRV(vR[(lastchange+1):i]))),
                       rv = sqrt((1/(i - lastchange + 1)) * 
                                   (rCov(vR[(lastchange + 1):i]))),
                       sd = sd(vR[(lastchange + 1):i]),
                       tau = robustbase::scaleTau2(vR[(lastchange + 1):i]))
    } else {
      from <- cp[max(which(cp < i))]
      to <- min(c(N*D, cp[which(cp >= i)]))
      len <- to - from
      spot[i] <- switch(op$volest, 
                        bipower = sqrt((1/len)*(rBPCov(vR[from:to]))),
                        medrv = sqrt((1/len)*(medRV(vR[from:to]))),
                        rv = sqrt((1/len)*(rCov(vR[from:to]))),
                        sd = sd(vR[from:to]),
                        tau = robustbase::scaleTau2(vR[from:to]))
    }
  } 
  if (is.null(rdata)) {
    spot <- matrix(spot, nrow = D, ncol = N, byrow = TRUE)
  } else {
    spot <- xts(spot, order.by = time(rdata))
  }
  out <- list(spot = spot, cp = cp)
  class(out) <- "spotvol"
  return(out) 
}

# Detect points on which the volatility level changes
# Input vR should be vector of returns
# Returns vector of indices after which the volatility level in vR changed
changePoints <- function(vR, type = "MDa", alpha = 0.005, m = 40, n = 20)
{
  logR <- log((vR - mean(vR))^2)
  L <- length(logR)
  points <- 0
  np <- length(points)
  N <- n + m
  cat("Detecting change points...\n")
  for (t in 1:L) { 
    if (t - points[np] >= N) {
      reference <- logR[(t - N + 1):(t - n)]
      testperiod <- logR[(t - n + 1):t]  
      if(switch(type,
                MDa = MDtest(reference, testperiod, type = type, alpha = alpha),
                MDb = MDtest(reference, testperiod, type = type, alpha = alpha),
                DM = DMtest(reference, testperiod, alpha = alpha))) {
        points <- c(points, t - n)     
        np <- np + 1
        cat(paste("Change detected at observation", points[np], "...\n"))
      }    
    }
  }
  return(points)
}

# Difference of medians test
# See Fried (2012)
# Returns TRUE if H0 is rejected
DMtest <- function(x, y, alpha = 0.005)
{
  m <- length(x)
  n <- length(y)
  xmed <- median(x)
  ymed <- median(y)
  xcor <- x - xmed
  ycor <- y - ymed
  delta1 <- ymed - xmed
  out <- density(c(xcor, ycor), kernel = "epanechnikov")
  fmed <- as.numeric(BMS::quantile.density(out, probs = 0.5))
  fmedvalue <- (out$y[max(which(out$x < fmed))] + 
                  out$y[max(which(out$x < fmed))+1])/2
  test <- sqrt((m*n)/(m + n))*2*fmedvalue*delta1
  return(abs(test) > qnorm(1-alpha/2))
}

# Median difference test
# See Fried (2012)
# Returns TRUE if H0 is rejected
MDtest <- function(x, y, alpha = 0.005, type = "MDa")
{
  m <- length(x)
  n <- length(y)
  N <- m + n
  lambda <- m/N
  yrep <- rep(y, each = m)
  delta2 <- median(yrep - x)
  if (type == "MDa") {
    z <- rep(0, N)
    z[1:m] <- x
    z[(m+1):N] <- y
    dif <- rep(z, each = length(z))
    dif <- dif - z
    dif[which(dif == 0)] <- NA
  } else if (type == "MDb") {
    difx <- rep(x, each = length(x))
    difx <- difx - x
    dify <- rep(y, each = length(y))
    dify <- dify - y
    dif <- rep(0, length(difx) + length(dify))
    dif[1:length(difx)] <- difx
    dif[(length(difx) + 1):(length(difx) + length(dify))] <- dify
    dif[which(dif == 0)] <- NA
  } else stop(paste("Type", type, "not found."))
  out <- density(dif, na.rm = TRUE, kernel = "epanechnikov")
  g0 <- (out$y[max(which(out$x < 0))] + out$y[max(which(out$x < 0)) + 1])/2
  test <- sqrt(12*lambda*(1 - lambda)*N)*g0*delta2
  return(abs(test) > qnorm(1 - alpha/2))
}

# GARCH with seasonality (external regressors)
garch_s <- function(mR, rdata = NULL, options = list())
{
  # default options, replace if user-specified
  op <- list(model = "eGARCH", order = c(1,1), dist = "norm", P1 = 5, 
             P2 = 5, solver.control = list())
  op[names(options)] <- options
  
  D <- nrow(mR)
  N <- ncol(mR)
  mR <- mR - mean(mR)
  X <- intraday_regressors(D, N = N, order = 2, almond = FALSE, P1 = op$P1,
                           P2 = op$P2)
  spec <- rugarch::ugarchspec(variance.model = list(model = op$model, 
                                                    external.regressors = X,
                                                    garchOrder = op$order),                    
                              mean.model = list(include.mean = FALSE),
                              distribution.model = op$dist)
  if (is.null(rdata)) {
    cat(paste("Fitting", op$model, "model..."))
    fit <- tryCatch(rugarch::ugarchfit(spec = spec, data = as.numeric(t(mR)), 
                                       solver = "nloptr", 
                                       solver.control = op$solver.control),
                    error = function(e) e,
                    warning = function(w) w)
    if (inherits(fit, what = c("error", "warning"))) {
      stop(paste("GARCH optimization routine did not converge.\n", 
                 "Message returned by ugarchfit:\n", fit))
    }
    spot <- as.numeric(rugarch::sigma(fit))
  } else {
    cat(paste("Fitting", op$model, "model..."))
    fit <- tryCatch(rugarch::ugarchfit(spec = spec, data = rdata, 
                                       solver = "nloptr",
                                       solver.control = op$solver.control), 
                    error = function(e) e,
                    warning = function(w) w)
    if (inherits(fit, what = c("error", "warning"))) {
      stop(paste("GARCH optimization routine did not converge.\n", 
                 "Message returned by ugarchfit:\n", fit))
    } 
    spot <- rugarch::sigma(fit)
  }
  out <- list(spot = spot, ugarchfit = fit)
  class(out) <- "spotvol"
  return(out)
}

plot.spotvol <- function(x, ...)
{
  options <- list(...)
  plottable <- c("spot", "periodic", "daily")
  elements <- names(x)
  nplots <- sum(is.element(plottable, elements))
  
  if (nplots == 3) {
    par(mar = c(3, 3, 3, 1))
    layout(matrix(c(1,2,1,3), nrow = 2))
  }
  spot <- as.numeric(t(x$spot))
  
  if(is.element("length", names(options))) {
    length = options$length
  } else {
    length = length(spot)
  }
  
  plot(spot[1:length], type = "l", xlab = "", ylab = "")
  title(main = "Spot volatility")
  if ("cp" %in% elements)
    abline(v = x$cp[-1], lty = 3, col = "gray70")
  if ("periodic" %in% elements) {
    periodic <- as.numeric(t(x$periodic))
    if (inherits(data, what = "xts")) {
      intraday <- time(x$periodic)
      plot(x = intraday, y = periodic, type = "l", xlab = "", ylab = "")
    } else {
      plot(periodic, type = "l", xlab = "", ylab = "") 
    }
    title(main = "Intraday periodicity")
  }
  if ("daily" %in% elements) {
    daily <- as.numeric(t(x$daily))
    if (inherits(data, what = "xts")) {
      dates <- as.Date(time(x$daily))
      plot(x = dates, y = daily, type = "l", xlab = "", ylab = "")
    } else {
      plot(daily, type = "l", xlab = "", ylab = "")
    }
    title(main = "Daily volatility")
  } 
}

intraday_regressors <- function(D, N = 288, order = 1, almond = TRUE, 
                                dummies = FALSE, P1 = 5, P2 = 5)
{  
  if (order == 1) {
    vi <- rep(c(1:N), each = D)
  } else {
    vi <- rep(c(1:N), D)
  }
  X <- c()
  if (!dummies) {
    if (P1 > 0) {
      for (j in 1:P1) {
        X <- cbind(X, cos(2 * pi * j * vi/N))
      }
    }
    M1 <- (N + 1)/2
    M2 <- (2 * N^2 + 3 * N + 1)/6
    ADD <- (vi/M1)
    X <- cbind(X, ADD)
    ADD <- (vi^2/M2)
    X <- cbind(X, ADD)
    if (P2 > 0) {
      ADD <- c()
      for (j in 1:P2) {
        ADD <- cbind(ADD, sin(2 * pi * j * vi/N))
      }
    }
    X <- cbind(X, ADD)
    if (almond) {
      opening <- vi - 0
      stdopening <- (vi - 0)/80
      almond1_opening <- (1 - (stdopening)^3)
      almond2_opening <- (1 - (stdopening)^2) * (opening)
      almond3_opening <- (1 - (stdopening)) * (opening^2)
      X <- cbind(X, almond1_opening, almond2_opening, almond3_opening)
      closing <- max(vi) - vi
      stdclosing <- (max(vi) - vi)/max(vi)
      almond1_closing <- (1 - (stdclosing)^3)
      almond2_closing <- (1 - (stdclosing)^2) * (closing)
      almond3_closing <- (1 - (stdclosing)) * (closing^2)
      X <- cbind(X, almond1_closing, almond2_closing, almond3_closing)
    }
  } else {
    for (d in 1:N) {
      dummy <- rep(0, N)
      dummy[d] <- 1
      dummy <- rep(dummy, each = D)
      X <- cbind(X, dummy)
    }
  }
  return(X)
}

### auxiliary internal functions copied from highfrequency package

countzeroes = function( series )
{
  return( sum( 1*(series==0) ) )
}

HRweight = function( d,k){
  # Hard rejection weight function
  w = 1*(d<=k); return(w)
}

shorthscale = function( data )
{
  sorteddata = sort(data);
  n = length(data);
  h = floor(n/2)+1;
  M = matrix( rep(0,2*(n-h+1) ) , nrow= 2 );
  for( i in 1:(n-h+1) ){
    M[,i] = c( sorteddata[ i ], sorteddata[ i+h-1 ] )
  }
  return( 0.7413*min( M[2,]-M[1,] ) );
}

diurnal = 
  function (stddata, method = "TML", dummies = F, P1 = 6, P2 = 4) 
  {
    cDays = dim(stddata)[1]
    intraT = dim(stddata)[2]
    meannozero = function(series) {
      return(mean(series[series != 0]))
    }
    shorthscalenozero = function(series) {
      return(shorthscale(series[series != 0]))
    }
    WSDnozero = function(weights, series) {
      out = sum((weights * series^2)[series != 0])/sum(weights[series != 
                                                                 0])
      return(sqrt(1.081 * out))
    }
    if (method == "SD" | method == "OLS") {
      seas = sqrt(apply(stddata^2, 2, "meannozero"))
    }
    if (method == "WSD" | method == "TML") {
      seas = apply(stddata, 2, "shorthscalenozero")
      shorthseas = seas/sqrt(mean(seas^2))
      shorthseas[shorthseas == 0] = 1
      weights = matrix(HRweight(as.vector(t(stddata^2)/rep(shorthseas, 
                                                           cDays)^2), qchisq(0.99, df = 1)), ncol = dim(stddata)[2], 
                       byrow = T)
      for (c in 1:intraT) {
        seas[c] = WSDnozero(weights[, c], stddata[, c])
      }
    }
    seas = na.locf(seas,na.rm=F) #do not remove leading NA
    seas = na.locf(seas,fromLast=T)
    seas = seas/sqrt(mean(seas^2))
    if (method == "OLS" | method == "TML") {
      c = center()
      vstddata = as.vector(stddata)
      nobs = length(vstddata)
      vi = rep(c(1:intraT), each = cDays)
      if (method == "TML") {
        if( length(vstddata)!= length(seas)*cDays ){ print(length(vstddata)); print(length(seas)); print(cDays)}
        firststepresids = log(abs(vstddata)) - c - log(rep(seas, 
                                                           each = cDays))
      }
      X = intraday_regressors(cDays, N = intraT, dummies = dummies, P1 = P1, P2 = P2)
      selection = c(1:nobs)[vstddata != 0]
      vstddata = vstddata[selection]
      X = X[selection, ]
      if (method == "TML") {
        firststepresids = firststepresids[selection]
      }
      vy = matrix(log(abs(vstddata)), ncol = 1) - c
      if (method == "OLS") {
        Z = try(solve(t(X) %*% X), silent = T)
        if (inherits(Z, "try-error")) {
          print("X'X is not invertible. Switch to TML")
        }
        else {
          theta = solve(t(X) %*% X) %*% t(X) %*% vy
          rm(X)
          rm(vy)
        }
      }
      if (method == "TML") {
        inittheta = rep(0, dim(X)[2])
        l = -2.272
        u = 1.6675
        nonoutliers = c(1:length(vy))[(firststepresids > 
                                         l) & (firststepresids < u)]
        truncvy = vy[nonoutliers]
        rm(vy)
        truncX = X[nonoutliers, ]
        rm(X)
        negtruncLLH = function(theta) {
          res = truncvy - truncX %*% matrix(theta, ncol = 1)
          return(mean(-res - c + exp(2 * (res + c))/2))
        }
        grnegtruncLLH = function(theta) {
          res = truncvy - truncX %*% matrix(theta, ncol = 1)
          dres = -truncX
          return(apply(-dres + as.vector(exp(2 * (res + 
                                                    c))) * dres, 2, "mean"))
        }
        est = optim(par = inittheta, fn = negtruncLLH, gr = grnegtruncLLH, 
                    method = "BFGS")
        theta = est$par
        rm(truncX)
        rm(truncvy)
      }
      # disable plot for now      
      #       plot(seas, main = "Non-parametric and parametric periodicity estimates", 
      #            xlab = "intraday period", type = "l", lty = 3)
      #       legend("topright", c("Parametric", "Non-parametric"), cex = 1.1,
      #              lty = c(1,3), lwd = 1, bty = "n")
      seas = diurnalfit(theta = theta, P1 = P1, P2 = P2, intraT = intraT, 
                        dummies = dummies)
      #       lines(seas, lty = 1)
      return(list(seas, theta))
    }
    else {
      return(list(seas))
    }
  }

diurnalfit = function( theta , P1 , P2 , intraT , dummies=F )
{
  vi = c(1:intraT) ;  
  M1 = (intraT+1)/2 ; M2 = (2*intraT^2 + 3*intraT + 1)/6;
  
  # Regressors that do not depend on Day of Week:
  X = c()
  if(!dummies){
    if ( P1 > 0 ){ for( j in 1:P1 ){ X = cbind( X , cos(2*pi*j*vi/intraT) )   }  } 
    
    ADD = (vi/M1 ) ; X = cbind(X,ADD);
    ADD = (vi^2/M2); X = cbind(X,ADD);
    if ( P2 > 0 ){ ADD= c(); for( j in 1:P2 ){  ADD = cbind( ADD , sin(2*pi*j*vi/intraT)  ) }}; X = cbind( X , ADD ) ; 
    
    #openingeffect
    opening = vi-0 ; stdopening = (vi-0)/80 ;
    almond1_opening   = ( 1 - (stdopening)^3 );
    almond2_opening   = ( 1 - (stdopening)^2 )*( opening);
    almond3_opening   = ( 1 - (stdopening)   )*( opening^2);   
    X = cbind(  X, almond1_opening , almond2_opening , almond3_opening   )  ;
    
    #closing effect
    closing = max(vi)-vi ; stdclosing = (max(vi)-vi)/max(vi) ;
    almond1_closing   = ( 1 - (stdclosing)^3 );
    almond2_closing   = ( 1 - (stdclosing)^2 )*( closing);
    almond3_closing   = ( 1 - (stdclosing)   )*( closing^2);   
    X = cbind(  X, almond1_closing , almond2_closing , almond3_closing   )  ;
    
  }else{
    for( d in 1:intraT){
      dummy = rep(0,intraT); dummy[d]=1; 
      X = cbind(X,dummy); 
    }
  }
  # Compute fit
  seas = exp( X%*%matrix(theta,ncol=1) );
  seas = seas/sqrt(mean( seas^2) )    
  return( seas )          
}

center = function()
{
  g=function(y){ return( sqrt(2/pi)*exp(y-exp(2*y)/2)  )}
  f=function(y){ return( y*g(y)    )  }
  return( integrate(f,-Inf,Inf)$value )
}

# modified version of 'aggregatePrice' from highfrequency package
aggregatePrice = function (ts, FUN = "previoustick", on = "minutes", k = 1, marketopen = "09:30:00", marketclose = "16:00:00", tz = "GMT") 
{
  ts2 = aggregatets(ts, FUN = FUN, on, k)
  date = strsplit(as.character(index(ts)), " ")[[1]][1]
  
  #open
  a = as.POSIXct(paste(date, marketopen), tz = tz)
  b = as.xts(matrix(as.numeric(ts[1]),nrow=1), a)
  ts3 = c(b, ts2)
  
  #close
  aa = as.POSIXct(paste(date, marketclose), tz = tz)
  condition = index(ts3) < aa
  ts3 = ts3[condition]
  bb = as.xts(matrix(as.numeric(last(ts)),nrow=1), aa)
  ts3 = c(ts3, bb)
  
  return(ts3)
}


## Graphs and figures - FED   ----------
final_df_fed_events <- final_df_test %>%
  mutate(date_col = date(Datetime)) %>%
  group_by(date_col) %>%
  mutate(eventday = ifelse(Fed_speak_dummy ==1, "Event Day","Non-Event Day")) %>%
  mutate(timeofday = strftime(Datetime, format="%H:%M:%S")) %>%
  group_by(weekday, eventday, timeofday) %>%
  summarise(avg = mean(return5min))

# Figures
par(mfrow=c(1,1))
p = ggplot(data = final_df_all_new,aes(x = Datetime, y = return5min)) 
p= p+ geom_line()
p + facet_wrap(~EventDay)


p  = ggplot(data = Event_days_df,aes(x = Datetime, y = return5min))
p = p + geom_line()


p = p + facet_wrap(~weekday, ncol = 2, scales = "free_x")
p

# Graphs and figures - Trump
final_df_Trump_events <- final_df_test %>%
  mutate(date_col = date(Datetime)) %>%
  group_by(date_col) %>%
  mutate(eventday = ifelse(Trump_speak_dummy ==1, "Event Day","Non-Event Day")) %>%
  mutate(timeofday = strftime(Datetime, format="%H:%M:%S")) %>%
  group_by(weekday, eventday, timeofday) %>%
  summarise(avg = mean(return5min))

library(scales)
# Figures
q  = ggplot(data = final_df_Trump_events,
            aes(x = timeofday, y = avg, color = eventday))
q = q + geom_line()
q = q + scale_x_datetime(breaks = date_breaks("1 hour"), 
                         labels = date_format("%H"))
q

p = p + facet_wrap(~weekday, ncol = 2, scales = "free_x")
p

# Matched sample test
library("ggpubr")
ggboxplot(final_df_events, x = "eventday", y = "avg", 
          color = "eventday", palette = c("#00AFBB", "#E7B800"))
#order = c("Before", "After"),
#ylab = "DXY Index", xlab = "Groups")

# Subset weight data before treatment
before <- subset(final_df_events,  eventday == "Event Day", avg,
                 drop = TRUE)
# subset weight data after treatment
after <- subset(final_df_events,  eventday == "Non-Event Day", avg,
                drop = TRUE)
after <- after[1:993]

# Plot paired data
library(PairedData)
pd <- paired(before, after)
plot(pd, type= "profile") + theme_bw()

# Fed speakers
Fed_speaker_df <- final_df_test %>%
  spread(Speaker, Fed_speak_dummy)

Fedspeakers <- unique(na.omit(final_df_test$Speaker))
Fed_speaker_df <- dplyr::select(Fed_speaker_df, 
                                one_of("Datetime", "Close", "return5min", "Coding", "Fed_speak_dummy",
                                       Fedspeakers)) 


## Fed communication types -----
Fedcomtypes <- unique(na.omit(final_df_test$Type))
Fed_comtype_df <- final_df_test %>%
  spread(Type, Fed_speak_dummy) %>%
  dplyr::select(one_of("Datetime", "Close", "Coding", "Fed_speak_dummy", Fedcomtypes)) 

Fed_comtype_df_after <- Fed_comtype_df %>%
  arrange(Datetime) %>%
  mutate_at(.vars = Fedcomtypes, 
            .funs = list(~replace(Datetime, is.na(.), NA))) %>%
  fill_(Fedcomtypes, .direction = "down") %>%
  mutate_at(.vars = Fedcomtypes,
            .funs = list(~(Datetime - .)/60)) %>%
  rename_at(Fedcomtypes, ~paste0(.,"_post"))

Fed_comtype_df_before <- Fed_comtype_df %>%
  arrange(Datetime) %>%
  mutate_at(.vars = Fedcomtypes, 
            .funs = list(~replace(Datetime, is.na(.), NA))) %>%
  fill_(Fedcomtypes, .direction = "up") %>%
  mutate_at(.vars = Fedcomtypes,
            .funs = list(~(. - Datetime)/60)) %>%
  rename_at(Fedcomtypes, ~paste0(.,"_pre"))


# Matched sample
Fed_comtype_media <- Fed_comtype_df %>%
  dplyr::select(Datetime, Close, Media) %>%
  merge(Fed_comtype_df_before[ , c("Datetime", "Media_pre")], 
        by = "Datetime", all.x=FALSE) %>%
  merge(y = Fed_comtype_df_after[ , c("Datetime", "Media_post")], 
        by = "Datetime", all.x=FALSE) %>%
  filter((Media_pre == 30 & Media_post>=30) | (Media_post == 30 & Media_pre>=30) ) %>%
  mutate(Period = ifelse(is.na(Media_pre) != 1 & Media_pre == 30, "Before", 
                         ifelse(Media_post == 30, "After", NA))) %>%
  dplyr::select(Period, Close)

group_by(Fed_comtype_media, Period) %>%
  summarise(
    count = n(),
    mean = mean(Close, na.rm = TRUE),
    sd = sd(Close, na.rm = TRUE)
  )

library("ggpubr")
ggboxplot(Fed_comtype_media, x = "Period", y = "Close", 
          color = "Period", palette = c("#00AFBB", "#E7B800"),
          order = c("Before", "After"),
          ylab = "DXY Index", xlab = "Groups")

# Subset weight data before treatment
before <- subset(Fed_comtype_media,  Period == "Before", Close,
                 drop = TRUE)
before <- before[1:145]
# subset weight data after treatment
after <- subset(Fed_comtype_media,  Period == "After", Close,
                drop = TRUE)

# Plot paired data
library(PairedData)
pd <- paired(before, after)
plot(pd, type= "profile") + theme_bw()

## Fed sub-category ----- 
Fed_subcat_df <- final_df_test %>%
  spread(Subcategory, Fed_speak_dummy)
Fed_subcats <- unique(na.omit(final_df_test$Subcategory))
Fed_subcat_df <- dplyr::select(Fed_subcat_df, 
                               one_of("Datetime", "Close", "Coding", "Fed_speak_dummy",
                                      Fed_subcats)) 

# Fed category 
Fed_cat_df <- final_df_test %>%
  spread(Category, Fed_speak_dummy)
Fed_cats <- unique(na.omit(final_df_test$Category))
Fed_cat_df <- select(Fed_cat_df, 
                     one_of("Datetime", "Close", "Coding", "Fed_speak_dummy",
                            Fed_cats)) 

# Fed total
Fed_total_df <- Fed_cat_df
Fed_total_df <- left_join(Fed_total_df, Fed_subcat_df, by = c("Datetime", "Close", "Coding")) 
Fed_total_df <- left_join(Fed_total_df, Fed_speaker_df, by = c("Datetime", "Close", "Coding"))
Fed_total_df <- left_join(Fed_total_df, Fed_comtype_df, by = c("Datetime", "Close", "Coding"))
Fed_total_df$weekday = weekdays(Fed_total_df$Datetime)


## Package: eventstudies (w/ intraday data) -----------------

n = as.numeric(sum((Trump_df_window$New_Trump_dummy)==1))
event_list <- final_df_all_new %>%
  filter(Trump_df_window$New_Trump_dummy==1) %>%
  select(Datetime)
names(event_list) <- "when"
id <- paste("A"(rownames(event_list)))
event_list <- cbind(name=id, when=event_list)
View(event_list)

return_series <- final_df_all_new[-(1:5), , drop = FALSE] %>%
  dplyr::select(one_of("Datetime","return5min"))
return_series <- return_series[rep(names(return_series), c(1,n))]
names(return_series)[2:(n+1)] <- paste("A",as.character(seq(1:n)))
return_series <- as.xts(return_series, 
                        order.by=as.POSIXct(return_series$Datetime, tz = "Europe/Copenhagen"),
                        tz = "Europe/Copenhagen")

data(IndexReturns, package = "eventstudies")
intraday.es <- eventstudies::eventstudy(firm.returns = return_series,
                                        event.list = event_list,
                                        event.window = 60,
                                        type = "None",
                                        to.remap = TRUE,
                                        remap = "cumsum",
                                        inference = TRUE,
                                        inference.strategy = "bootstrap"
)
plot(intraday.es)



ts_Trump = as.zoo(Trump_df, order.by = as.Date(Trump_df$Datetime), format = "Y%-m%-d% hh:mm:ss") 

## Event study -----------------

startDate <- "2014-05-01"
endDate <- "2015-12-31"

# Firm Data
firmSymbols <- c("VOW.DE", "NSU.DE", "PAH3.DE", "BMW.DE", "DAI.DE")
firmNames <- c("VW preferred", "Audi", "Porsche Automobil Hld", "BMW", "Daimler")
firmSymbols %>% 
  tidyquant::tq_get(from = startDate, to = endDate) %>% 
  dplyr::mutate(date = format(date, "%d.%m.%Y")) -> firmData
knitr::kable(head(firmData), pad=0)

# Index Data
indexSymbol <- c("^GDAXI")
indexName <- c("DAX")
indexSymbol %>% 
  tidyquant::tq_get(from = startDate, to = endDate) %>% 
  dplyr::mutate(date = format(date, "%d.%m.%Y")) -> indexData
indexData$symbol <- "DAX"
knitr::kable(head(indexData), pad=0)

# Price files for firms and market
firmData %>% 
  dplyr::select(symbol, date, adjusted) %>% 
  readr::write_delim(path      = "02_firmDataPrice.csv", 
                     delim     = ";", 
                     col_names = F)

indexData %>% 
  dplyr::select(symbol, date, adjusted) %>% 
  readr::write_delim(path      = "03_marketDataPrice.csv", 
                     delim     = ";", 
                     col_names = F)

# Volume files for firms and market
firmData %>% 
  dplyr::select(symbol, date, volume) %>% 
  readr::write_delim(path      = "02_firmDataVolume.csv", 
                     delim     = ";", 
                     col_names = F)

indexData %>% 
  dplyr::select(symbol, date, volume) %>% 
  readr::write_delim(path      = "03_marketDataVolume.csv", 
                     delim     = ";", 
                     col_names = F)


group <- c(rep("VW Group", 3), rep("Other", 2))
request <- cbind(c(1:5), firmSymbols, rep(indexName, 5), rep("18.09.2015", 5), group, rep(-10, 5), rep(10, 5), rep(-11, 5), rep(250, 5))
request %>% 
  as.data.frame() %>% 
  readr::write_delim("01_requestFile.csv", delim = ";", col_names = F)

key <- "573e58c665fcc08cc6e5a660beaad0cb"
options(EventStudy.KEY = key)
estSetup <- EventStudyAPI$new()

library(EventStudy)
est <- EventStudyAPI$new()
est$authentication(apiKey = key)
# get S&P500 example data
getSP500ExampleFiles()
# set Event Study parameters
estType <- "arc"
dataFiles <- c("request_file" = "01_RequestFile.csv",
               "firm_data"    = "02_firmData.csv",
               "market_data"  = "03_MarketData.csv")
resultPath <- "results"
# Perform Event Study
arEventStudy <- estSetup$performDefaultEventStudy(estParams     = estType, 
                                                  dataFiles     = dataFiles, 
                                                  destDir       = resultPath)

# get & set parameters for abnormal return Event Study
# we use a garch model and csv as return
# Attention: fitting a GARCH(1, 1) model is compute intensive
esaParams <- EventStudy::ARCApplicationInput$new()
esaParams$setResultFileType("csv")
esaParams$setBenchmarkModel("garch")

dataFiles <- c("request_file" = "01_requestFile.csv",
               "firm_data"    = "02_firmData.csv",
               "market_data"  = "03_marketData.csv")

# check data files, you can do it also in our R6 class
EventStudy::checkFiles(dataFiles)

# now let us perform the Event Study
arEventStudy <- estSetup$performEventStudy(estParams     = esaParams, 
                                           dataFiles     = dataFiles, 
                                           downloadFiles = T)

knitr::kable(head(arEventStudy$arResults))
knitr::kable(head(arEventStudy$aarResults))

est <- EventStudyAPI$new()
est$authentication(apiKey = key)

# get & set parameters for abnormal return Event Study
esaParams <- EventStudy::AVyCApplicationInput$new()
esaParams$setResultFileType("csv")

avycEventStudy <- est$performEventStudy(estParams    = esaParams, 
                                        dataFiles     = dataFiles,
                                        downloadFiles = T)








