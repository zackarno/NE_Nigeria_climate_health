sm_files<-list.files(path = "environmental",pattern = "*.haldiapalong.csv", full.names = T)

library(ROpenWeatherMap)
library(owmr)
library(tidyverse)
library(lubridate)
library(dygraphs)
library(xts)


# make forecast prototype -------------------------------------------------
gfs<- read_csv("environmental/ee_precip_forecaste_test1.csv") %>% 
  with_tz("Asia/Dhaka") %>% 
  mutate(datetime=date) %>% select(-date)
gfs<-gfs %>% 
  mutate(mm=total_precipitation_surface-lag(total_precipitation_surface,default = 0)) %>% 
  select(-total_precipitation_surface) #%>% 
  group_by(datetime=as_date(datetime)) %>% 
  summarise(mm=sum(mm))

cfsr_gee<- read_csv("environmental/cfsr_20180101_20200819_GEE_median.csv")

chirps<- read_csv("environmental/chirps_20180101_20200819_GEE_median.csv")
chirps<- chirps %>% 
  mutate(datetime=mdy(`system:time_start`)) %>% select(-`system:time_start`) %>% 
  mutate(source="chirps") %>% 
  rename(mm="precipitation")

cfsr_gee<- cfsr_gee %>% 
  mutate(date= mdy(`system:time_start`),
         mm= Precipitation_rate_surface_6_Hour_Average*(1/0.997)* 60*60*6,
         tday=  rep_len(c("00:00:00","06:00:00","12:00:00","18:00:00"),length.out = nrow(.)),
         datetime_char= paste(date,tday),
         datetime= ymd_hms(datetime_char)
) %>% 
  with_tz("Asia/Dhaka")

cfsr_gee_simple<- cfsr_gee %>% select(datetime,mm) %>% mutate(source="cfsr")
gfs<- gfs %>% 
  mutate(source="gfs")# %>% 
  # rename(mm ="total_precipitation_surface")
historical_with_forecast_long<- bind_rows(cfsr_gee_simple,gfs,chirps)
historical_with_forecast_wide<- historical_with_forecast_long %>% pivot_wider(names_from = source,values_from = mm)

cfsr_gee_simple_daily<-cfsr_gee_simple %>% 
  group_by(datetime=as_date(datetime)) %>%
  summarise(mm=sum(mm)) %>%
  mutate(source="cfsr")
historical_with_forecast_long<- bind_rows(cfsr_gee_simple_daily,gfs,chirps)
historical_with_forecast_wide<- historical_with_forecast_long %>% pivot_wider(names_from = source,values_from = mm)


historical_with_forecast_xts<-xts(historical_with_forecast_wide,order.by =  historical_with_forecast_wide$datetime)
forecast_graph<-dygraph(historical_with_forecast_xts) %>% 
  dySeries("cfsr", label = "CFSR" ,axis=('y'),color="blue") %>% 
  dySeries("gfs", label = "GFS", axis=('y'),color="red") %>% 
  dySeries("chirps", label = "CHIRPS", axis=('y'),color="green") %>% 
  dyRangeSelector(dateWindow = c(min(historical_with_forecast_wide$datetime),
                                 max(historical_with_forecast_wide$datetime)))

htmlwidgets::saveWidget(forecast_graph, "precip_forecast_GFS_20200819.html")

data<- get_current_weather(api_key="cf52632da7d6942d976476254ec4e08a",city="mumbai")
ROpenWeatherMap::get_weather_forecast(api_key = "cf52632da7d6942d976476254ec4e08a")

devtools::install_github("mukul13/ROpenWeatherMap")

data=get_current_weather(api_key,city="Cox's Bazar")
data= get_weather_forecast(api_key,city="Cox's Bazar")


# forecast_data=get_weather_forecast(api_key,coordinates = c(51.51,-0.13))
# forecast_data= ROpenWeatherMap:::(api_key,city="Cox's Bazar")

library(lubridate)

sm5<-read_csv("environmental/soil_moisture_5cm_haldiapalong.csv" )
sm25<-read_csv("environmental/soil_moisture_150cm_haldiapalong.csv" )
cfsr<-read_csv("environmental/cfsr_2018_july25.csv")

cfsr_gee<- read_csv("environmental/cfsr_2018_july29_GEE_median.csv")
cfsr_mean<- read_csv("environmental/cfsr_2018_july29_GEE.csv")

cfsr_mean_gee<-cfsr_mean %>% 
  mutate(date= mdy(`system:time_start`),
         mm_6_hrs= Precipitation_rate_surface_6_Hour_Average*(1/0.997)* 60*60*6,
         tday=  rep_len(c("00:00:00","06:00:00","12:00:00","18:00:00"),length.out = nrow(.)),
         datetime_char= paste(date,tday),
         datetime= ymd_hms(datetime_char)
         
         ) #%>% 
  #with_tz(tz="Asia/Dhaka")

cfsr_gee_ts <- cfsr_gee %>% select(datetime, mm_6_hrs)
cfsr_gee_daily_ts<-cfsr_gee_ts %>%
  group_by(date=as.Date(datetime)) %>% 
  summarise(mm_day=sum(mm_6_hrs))
cfsr_mean_gee_ts <- cfsr_mean_gee %>% select(datetime, mm_6_hrs)

cfsr_gee_mean_daily_ts<-cfsr_mean_gee_ts %>%
  group_by(date=as.Date(datetime)) %>% 
  summarise(mm_day_mean=sum(mm_6_hrs))

cfsr_climate_engine<- cfsr %>% 
  mutate(datetime= ymd_h(DateTime),
         date=as.Date(datetime))

ce_gee_combined<-cfsr_climate_engine %>%
  select(date, mm_day_ce=`(mm) Precipitation (CFSR) at Collective site, 2018-01-01 to 2020-07-25`) %>% 
  left_join(cfsr_gee_daily_ts %>% 
              select(date, mm_day_gee=mm_day)) %>% 
  left_join(cfsr_gee_mean_daily_ts) %>% 
  mutate(datetime=ymd_hms(paste0(date,"00:00:00"))) %>% 
  select(-date)

ce_gee_combined_xts= xts::xts(ce_gee_combined,order.by = ce_gee_combined$datetime)

library(dygraphs)
dygraph(ce_gee_combined_xts) %>% 
  dySeries("mm_day_ce", label = "climateEngine" ,axis=('y'),color="red") %>% 
  dySeries("mm_day_gee", label = "mm_day_median", axis=('y'),color="green") %>%  
  dySeries("mm_day_mean", label = "mm_day_mean", axis=('y'),color="blue") 

cfsr_gee %>% tail()


  
ee_f_bgd =with_tz(ee_f, tz="Asia/Dhaka")
ymd_hms(ee_f$date, tz = "	UTC +06:00")
ee_f %>% group_by(day=as.Date(date)) %>% 
  summarise(daily_precip=sum(total_precipitation_surface))
data.frame(ee_f_bgd$date, ee_f$date)
ee_f_bgd %>% group_by(day=as.Date(date)) %>% 
  summarise(daily_precip=sum(total_precipitation_surface))
library(rvest)




colnames(cfsr)[2]<-"mm"
colnames(sm5)[2]<-"sm_5"
colnames(sm25)[2]<-"sm_25"
cfsr<- cfsr %>% 
  mutate(date_time=ymd_h(DateTime),
         year=str_sub(date_time,start=1,end=4),
         date_md=str_sub(date_time, start=6,end=10) %>%  as.Date(format="%m-%d"),
         rain_yn=ifelse(mm==0,0,1))

sm5<- sm5 %>% 
  mutate(date_time=ymd_h(DateTime),
         year=str_sub(date_time,start=1,end=4),
         date_md=str_sub(date_time, start=6,end=10) %>%  as.Date(format="%m-%d"))
sm25<- sm25 %>% 
  mutate(date_time=ymd_h(DateTime),
         year=str_sub(date_time,start=1,end=4),
         date_md=str_sub(date_time, start=6,end=10) %>%  as.Date(format="%m-%d"))

precip_sm<-cfsr %>% 
  left_join(sm5 %>% select(date_time,sm_5)) %>% left_join(sm25 %>% select(date_time,sm_25))
precip_sm_vals<-precip_sm %>% select(date_time, mm,sm_5,sm_25)
precip_sm_xts<-xts(precip_sm_vals, order.by = precip_sm_vals$date_time)

dygraph(precip_sm_xts) %>% 
  dyAxis("y2", independentTicks = TRUE, axisLineColor="red") %>%
  dySeries("mm", label = "precip accumulation" ,axis=('y'),color="blue") %>% 
  dySeries("sm_5", label = "sm 5", axis=('y2'),color="red")# %>%  
  # dySeries("sm_25", label = "sm 25", axis=('y2'),color="orange")   

cfsr<- cfsr %>% group_by(year) %>% 
  mutate(cumulative_mm=cumsum(mm)) %>% 
  ungroup()

cfsr %>% ggplot(aes(x=date_md,y=mm,color=year) )+geom_path()+
  scale_x_date(date_labels = "%b-%d",date_breaks = "2 week")+
  theme(
    axis.text.x = element_text(angle=45)
  )
cfsr %>% ggplot(aes(x=date_md,y=cumulative_mm,color=year) )+geom_path(size=2)+
  scale_x_date(date_labels = "%b-%d",date_breaks = "2 week")+
  theme(
    axis.text.x = element_text(angle=45)
  )
