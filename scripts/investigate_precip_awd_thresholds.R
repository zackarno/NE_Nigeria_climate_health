

chirps_gee_extracted <-  c(T,F)[1]

# admins ------------------------------------------------------------------


admins <- load_nga_admins()

ward <- admins$ward

# wards of interest
woi <- ward %>% 
  filter(ADM2_EN %in% c("Maiduguri","Jere"))

# muac --------------------------------------------------------------------

muac <- read_rds(file = here::here("data/muac_201701_202111.rds"))
muac %>% 
  filter(!is.na(new_otp_admissions)) %>% 
  group_by(LGA) %>% 
  count(sort=T) %>% 
  print(n=nrow(.))

muac_filt <- muac |> 
  filter(
    !is.na(new_otp_admissions)|(is.na(new_otp_admissions) & !is.na(muac_total_screened)),
    !is.na(State) 
  ) 
# there are some records with LGA missing, but ward present- should recode
ward_lga_lookup<- admins$ward$ADM2_EN |> 
  set_names(admins$ward$ADM3_EN)

muac_cleaned <- muac_filt|> 
  mutate(
    across(.cols= matches("^muac_|^new_"), ~replace_na(.x,0)),
    across(.cols = c("State","LGA","Ward"),~str_to_title(.x)),
    mo= str_to_title(mo),
    date= paste0(yr,"-",mo,"-01"),
    date_dt= lubridate::ymd(date),
    pct_muac_red= (muac_red/muac_total_screened)*100,
    LGA= str_replace_all(LGA, c("Askira-Uba"="Askira/Uba",
                                "Postiskum" ="Potiskum" ,
                                "Gaidam" ="Geidam",
                                "Tarmuwa" ="Tarmua" )),
    # fix these na LGAs that have wards
    LGA = if_else(is.na(LGA),recode(Ward, !!!ward_lga_lookup),LGA)
    
  )
otp_woi <- muac_cleaned %>% 
  filter(LGA %in% c("Maiduguri","Jere"))
otp_woi %>% 
  group_by(LGA,Ward) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  print(n=100)

# AWD ---------------------------------------------------------------------

awd <- readxl::read_excel(here::here("data/Borno_EWARS_awd_under5_2017_2021.xlsx")) |> 
  rename(ADM2_EN = `...1`)


ne_lgas<- admins$lga |> 
  st_drop_geometry() |> 
  filter(ADM1_EN %in% c("Yobe","Borno","Adamawa")) 

awd <- awd |> 
  mutate(
    ADM2_EN= str_replace_all(string = ADM2_EN,c("Askira Uba"="Askira/Uba","Kala Balge"="Kala/Balge" ))
  )
awd_long %>% 
  count(ADM2_EN) %>% 
  print(n=nrow(.))
awd_long <- awd |> 
  pivot_longer(-1,names_to = "date",values_to = "cases") |> 
  left_join(ne_lgas |> select(ADM1_EN,ADM2_EN)) |> 
  select(ADM1_EN,ADM2_EN,everything())

awd_long_mj <- awd_long %>% 
  filter(ADM2_EN %in% c("Maiduguri","Jere"))

# add year-wk - double checked there is only 1 week collected per year per location
awd_long_mj <- awd_long_mj %>%
  mutate(
    yr_wk=paste0(lubridate::year(date),"_",lubridate::week(date))
  )

awd_long_by_wk <- awd_long_mj %>% 
  mutate(
    date= lubridate::ymd(date),
    cases= as.numeric(cases)
  ) %>% 
  group_by(date) %>% 
  summarise(
    value=sum(cases,na.rm=T),.groups = "drop"
  ) %>% 
  mutate(
    yr_wk=paste0(lubridate::year(date),"_",lubridate::week(date)),
    parameter="awd_cases"
  )

# extract chirps ----------------------------------------------------------



if(chirps_gee_extracted){

chirps_src <- "UCSB-CHG/CHIRPS/DAILY"
chirps <- ee$ImageCollection(chirps_src)



woi_diss <-  woi %>% 
  summarise() %>% 
  st_simplify(dTolerance =  0.7)

object.size(woi_diss)


leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=woi_diss)
?ee_extract()

chirps_renamed <- chirps$ 
  # CHIRPS release is delayed by a month or 2 so you need to pre-filter
  # or face difficulty later
  filterDate("2016-01-01","2021-11-30") |> 
  ee$ImageCollection$map(
    function(x) {
      date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
      name <- ee$String$cat("pr_", date)
      x$select("precipitation")$rename(name)
    })


system.time(# 299 s
  chirps_daily_mj <- ee_extract(x = chirps_renamed,y=woi_diss,scale=5500,fun= ee$Reducer$mean(),sf=T)
)

# FOR NDVI it might make sense to mask out urban pixels... could potentially look at MODIS


chirps_daily_long <- chirps_daily_mj |> 
  st_drop_geometry() |> 
  pivot_longer(-1) |> 
  mutate(
    parameter="precip",
    date= str_remove(string = name, pattern = ".*pr_") |> 
      str_replace_all("_","-") |> lubridate::ymd()
    
  ) 
chirps_daily_long <- chirps_daily_long %>% 
  select(date,value,parameter)
# chirps_daily_long %>% 
#   write_rds(here::here("data/chirps_daily_maidu_jere_diss_gte2016.rds"))
}


# investiate time series --------------------------------------------------


chirps_daily_long <- read_rds(here::here("data/chirps_daily_maidu_jere_diss_gte2016.rds"))




otp_woi_monthly <- otp_woi %>% 
  group_by(date=date_dt) %>% 
  summarise(value=sum(new_otp_admissions)) %>% 
  mutate(parameter="new_otp")

chirps_otp <- bind_rows(chirps_daily_long,otp_woi_monthly)

chirps_daily_long %>% 
  ggplot(aes(x=date,y=value,color=parameter,group=parameter))+
  geom_line()+
  geom_line(data= otp_woi_monthly,aes(x=date,y=value/1000),lwd=2)+
  scale_y_continuous(
    sec.axis= sec_axis(trans = ~.x*1000,name = "cases")
  )

###############################################
# Daily chirps Vs weekly AWD #################
###############################################

chirps_daily_long %>% 
  ggplot(aes(x=date,y=value,color=parameter,group=parameter))+
  geom_line()+
  geom_line(data= awd_long_by_wk,aes(x=date,y=value/100),lwd=2)+
  scale_y_continuous(
    sec.axis= sec_axis(trans = ~.x*100,name = "cases")
  )



yrly_max_case_weeks <- awd_long_by_wk %>% 
  group_by(yr=lubridate::year(date)) %>%
  slice_max(order_by =value, n = 1 ) %>% 
  # filter(value==max(value)) %>% 
  mutate( 
    yrmo = paste0(yr,"_",lubridate::month(date))
  )


chirps_daily_long <- chirps_daily_long %>% 
  group_by(yr=lubridate::year(date)) %>% 
  mutate(
    cumulative_precip=cumsum(value)
  ) 
awd_long_by_wk %>% 
  filter(date>="2021-07-01")
chirps_daily_long %>% 
  filter(
    date %in% c(yrly_max_case_weeks$date[1:3],
                "2020-07-19",
                "2021-07-11")
  )
chirps_daily_long %>% 
  filter(date> "2017-03-01") %>% 
  print(n=nrow(.))

awd_first_peaks <- awd_long_by_wk %>% 
  filter(
    date %in%c(yrly_max_case_weeks$date[1:3],
               "2020-07-19",
               "2021-07-11")
  )
awd_long_by_wk %>% 
  filter(date>"2019-05-01",date<"2019-07-30")
chirps_daily_long %>% 
  filter(date>"2019-05-01",date<"2019-07-30") %>% 
  print(n=nrow(.))


chirps_daily_long%>% 
  ggplot(aes(x=date,cumulative_precip))+
  geom_line(color="#abd9e9",lwd=1)+

 
  labs(y="Yearly cummulative precipitation (mm)")+
  # drawing lines roughly where the rains start each year
  # geom_vline(xintercept=lubridate::ymd("2017-05-01"))+
  # geom_vline(xintercept=lubridate::ymd("2018-04-01"))+
  # geom_vline(xintercept=lubridate::ymd("2019-05-01"))+
  # geom_vline(xintercept=lubridate::ymd("2020-05-01"))+
  # geom_vline(xintercept=lubridate::ymd("2021-05-01"))+
  geom_vline(xintercept=lubridate::ymd("2017-07-09"),color="#f46d43",lwd=0.7,alpha= 0.5)+
  geom_vline(xintercept=lubridate::ymd("2018-07-15"),color="#f46d43",lwd=0.7,alpha= 0.5)+
  geom_vline(xintercept=lubridate::ymd("2019-06-30"),color="#f46d43",lwd=0.7,alpha= 0.5)+
  geom_vline(xintercept=lubridate::ymd("2020-07-19"),color="#f46d43",lwd=0.7,alpha= 0.5)+
  # geom_vline(xintercept=lubridate::ymd("2020-08-30"),)+
  geom_vline(xintercept=lubridate::ymd("2021-07-11"),color="#f46d43",lwd=0.7,alpha= 0.5)+
  geom_line(data= awd_long_by_wk,
            aes(x=date,y=value/2),
            lwd=1,color="#fdae61",alpha=0.75)+
  geom_hline(yintercept=127,color="#4575b4",lwd=1, linetype="dashed")+
  geom_hline(yintercept=184,color="#4575b4",lwd=1, linetype="dashed")+
  # geom_vline(xintercept=lubridate::ymd("2021-08-29"))+
  geom_point(data=awd_first_peaks, aes(x= date, y= value/2),fill="#fdae61", size= 2,pch=21)+
  
  # seems like cases are spiking between 70-300 mm of rain accumulation/
  # first half of the rainy season
  scale_y_continuous(
    sec.axis= sec_axis(trans = ~.x*2,name = "AWD Cases")
  )+
  scale_x_date(
    minor_breaks = "1 month",
    breaks= "1 month",
    # major="year",
    labels=scales::date_format("%b%y")
  )+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle=90,size = 6)
  )+
  annotate(
    "text", label = "127 mm",
    x = lubridate::ymd("2015-10-01"), y = 115, size = 3, colour = "#4575b4"
  )+
  annotate(
    "text", label = "186 mm",
    x = lubridate::ymd("2015-10-01"), y = 200, size = 3, colour = "#4575b4"
  )
  
ggsave(filename = "cumm_precip_vs_awd_cases.png",width = 8, height = 4, device = "png")

# 1.) plot soil moisture params
# 2.) plot NDVI params
# 3.) plot params all together


# linked map and graph of precip
# water sheds?
# JRC water change in mj

# smap ---------------------------------------------------------------------


library(xts)
library(tidyverse)
library(dygraphs)
smap <- read_rds(file = here::here("data/SMAP_soil_moisture_3day_maidu_jere.rds"))


smap %>% 
  ggplot(aes(x= date, y= value,color=parameter))+
  geom_line()+
  geom_line(data=chirps_daily_long)
  
  
smap_chirps_long <- bind_rows(chirps_daily_long %>% ungroup() %>% 
  select(-cumulative_precip,-yr),
  smap,awd_long_by_wk %>% 
    select(-yr_wk)
    )

smap_wide<- smap %>% pivot_wider(names_from = parameter,values_from = value)
smap_wide<- smap_chirps_long %>% pivot_wider(names_from = parameter,values_from = value)
# smap_wide %>% 
#   filter(across(everything(), ~ !is.na(.))) 

smap_wide_filt <- smap_wide %>% 
  select(date,precip,ssm,susm,awd_cases) %>% 
  mutate(awd_cases=awd_cases/100) %>% 
  filter(date>="2016-01-01")
smap_xts<-xts(smap_wide_filt,order.by =  smap_wide_filt$date)

smap_plot<-dygraph(smap_xts) %>% 
  dySeries("precip", label = "precip", axis=('y'),color="#74add1") %>%
  dySeries("ssm", label = "surface moisture" ,axis=('y'),color="#a6d96a") %>% 
  dySeries("susm", label = "sub-surface moisture", axis=('y'),color="#8c510a")  %>% 
  dySeries("awd_cases", label = "awd_cases", axis=('y'),color="red")  %>% 

  dyRangeSelector(dateWindow = c(min(smap_xts$date),
                                 max(smap_xts$date))) %>% 
  dyOptions(connectSeparatedPoints = TRUE)

smap_plot


# food prices -------------------------------------------------------------


food <- read_csv(here::here("data/WFP_2022Jan20_Nigeria_FoodPricesData.csv")) %>% 
  janitor::clean_names() 

# trying to see if i should eliminate retail or wholesale
food %>% 
  filter(year>=2017) %>% 
  group_by(commodity, price_type) %>% 
  summarise(total_records= n()) %>%
  print(n=nrow(.))

food_filt1 <- food %>% 
  filter(year>=2017) %>% 
  group_by(commodity, price_type) %>% 
  mutate(
    total_records= n(),
  ) %>% 
  ungroup() %>% 
  filter(total_records>=46) %>% 
  mutate(
    date= paste0(year,"-",month,"-01")
  )
  

food %>% 
  filter(price_type=="Retail")
  

smap %>% 
  ggplot(aes(x= date, y= value,color=parameter))+
  geom_line()+
  geom_line(data=chirps_daily_long)
  
  
smap_chirps_long <- bind_rows(chirps_daily_long %>% ungroup() %>% 
  select(-cumulative_precip,-yr),
  smap,awd_long_by_wk %>% 
    select(-yr_wk)
    )

smap_wide<- smap %>% pivot_wider(names_from = parameter,values_from = value)
smap_wide<- smap_chirps_long %>% pivot_wider(names_from = parameter,values_from = value)
# smap_wide %>% 
#   filter(across(everything(), ~ !is.na(.))) 

smap_wide_filt <- smap_wide %>% 
  select(date,precip,ssm,susm,awd_cases) %>% 
  mutate(awd_cases=awd_cases/100) %>% 
  filter(date>="2016-01-01")
smap_xts<-xts(smap_wide_filt,order.by =  smap_wide_filt$date)

smap_plot<-dygraph(smap_xts) %>% 
  dySeries("precip", label = "precip", axis=('y'),color="#74add1") %>%
  dySeries("ssm", label = "surface moisture" ,axis=('y'),color="#a6d96a") %>% 
  dySeries("susm", label = "sub-surface moisture", axis=('y'),color="#8c510a")  %>% 
  dySeries("awd_cases", label = "awd_cases", axis=('y'),color="red")  %>% 

  dyRangeSelector(dateWindow = c(min(smap_xts$date),
                                 max(smap_xts$date))) %>% 
  dyOptions(connectSeparatedPoints = TRUE)

smap_plot

library(tidyquant)
precip_3_day_max<- chirps_daily_long %>%
  # group_by(Device.name) %>%
  tq_mutate(
    # tq_mutate args
    select     = value,
    mutate_fun = rollapply,
    # rollapply args
    width      = 3,
    align      = "right",
    FUN        = sum,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "sum_3_day"
  )
precip_5_day_max<- chirps_daily_long %>%
  # group_by(Device.name) %>%
  tq_mutate(
    # tq_mutate args
    select     = value,
    mutate_fun = rollapply,
    # rollapply args
    width      = 5,
    align      = "right",
    FUN        = sum,
    # mean args
    na.rm      = TRUE,
    # tq_mutate args
    col_rename = "sum_5_day"
  )

precip_3_day_max %>% 
  ggplot(aes(x=date,y=sum_3_day,color=parameter,group=parameter))+
  # geom_line()+
  geom_point()
  geom_line(data= otp_woi_monthly,aes(x=date,y=value/1000),lwd=2)+
  scale_y_continuous(
    sec.axis= sec_axis(trans = ~.x*1000,name = "cases")
  )
precip_5_day_max %>% 
  ggplot(aes(x=date,y=sum_5_day,color=parameter,group=parameter))+
  geom_point()
  geom_line()+
  geom_line(data= otp_woi_monthly,aes(x=date,y=value/1000),lwd=2)+
  scale_y_continuous(
    sec.axis= sec_axis(trans = ~.x*1000,name = "cases")
  )
  precip_5_day_max %>% filter(date>"2016-05-01")
  precip_3_day_max %>% filter(date>"2016-05-01")
  
  precip_3_day_max %>% 
    mutate(
      yr= lubridate::year(date),
      wk=lubridate::week(date)
    ) %>% 
    group_by(yr, wk) %>% 
    slice_max(order_by = sum_3_day,n = 1,with_ties = F) %>% 
    ggplot(aes(x=date, y= sum_3_day))+
    geom_point()+
    geom_line()+
    geom_line(data= otp_woi_monthly,aes(x=date,y=value/500),lwd=2)+
    scale_y_continuous(
      sec.axis= sec_axis(trans = ~.x*500,name = "cases")
    )

# maybe easier to visualize as ecdf plot ----------------------------------
chirps_daily_long %>% 
    group_by(yr=lubridate::year(date)) %>% 
    mutate(
      cumulative_precip=cumsum(value)
    ) %>% 
    ggplot(aes(x=date,cumulative_precip))+
    geom_line()+
    geom_line(data= otp_woi_monthly,aes(x=date,y=value/100),lwd=2)+
    # drawing lines roughly where the rains start each year
    geom_vline(xintercept=lubridate::ymd("2017-05-01"))+
    geom_vline(xintercept=lubridate::ymd("2018-04-01"))+
    geom_vline(xintercept=lubridate::ymd("2019-05-01"))+
    geom_vline(xintercept=lubridate::ymd("2020-05-01"))+
    geom_vline(xintercept=lubridate::ymd("2021-05-01"))+
    # seems like cases are spiking between 70-300 mm of rain accumulation/
    # first half of the rainy season
    scale_y_continuous(
      sec.axis= sec_axis(trans = ~.x*100,name = "cases")
    )+
    scale_x_date(
      minor_breaks = "1 month",
      breaks= "1 month",
      # major="year",
      labels=scales::date_format("%b%y")
    )+
    theme_bw()+
    theme(
      axis.text.x = element_text(angle=90)
    )


  # I should look into:
  # animation for geographical hot spots - grid or raster
  # need to look at just AWD data as well
  
