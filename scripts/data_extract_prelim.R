library(tidyverse)
library(rgee)
library(sf)
invisible(purrr::map(list.files(here::here("R"),full.names = T),.f = source))

admins <- load_nga_admins()
ee_Initialize()
# ee_nc_rain <- ee_extract(x = terraclimate, y = nc["NAME"], sf = FALSE)
gee_dat_src <- "UCSB-CHG/CHIRPS/DAILY"
gee_dat <- ee$ImageCollection(gee_dat_src)
year_seq <- ee$List$sequence(1981, 2021)
month_seq <- ee$List$sequence(1,12)


# I know I want to use ee_extract to get most of the data. However, I remember it would time out and take 
# a while to get huge amounts of data. Therefore can I simplify the data into monthyl data prior to running?

# got this cool function fom dataninjas nice excuse to refresh on some python
# https://www.youtube.com/watch?v=-Gzn8-ED6vQ

yearly_monthly_sum <- function(year){
  monthly_sum <-  function(m){
    w <- gee_dat$filter(ee$Filter$calendarRange(year,year, 'year'))$
      filter(ee$Filter$calendarRange(m,m,'month'))$sum()
    
    return(w$set('year',year)$
             set('month',m)$
             set('system:time_start',ee$Date$fromYMD(year,m,1))
             )
  }
  return(month_seq$map(ee_utils_pyfunc(monthly_sum)))
}

# i think we now havae a monthly sum product
monthly_precip <- ee$ImageCollection$fromImages(
  year_seq$map(ee_utils_pyfunc(yearly_monthly_sum))$flatten()
)



# try a proof of concept just at state level.
ne_states <- admins$state |> # sweet new native pipe - about time
  filter(ADM1_EN %in% c("Borno","Adamawa","Yobe"))

ne_lgas <-  admins$lga |> 
  filter(ADM1_EN %in% c("Borno","Adamawa","Yobe"))

# remember to create ee features in memory they have to pretty small/simple
ne_states_simp <- st_simplify(ne_states,dTolerance = 0.7)
ne_lgas_simp <- st_simplify(ne_lgas,dTolerance = 0.7)



ee_print(monthly_precip)
# ee_get_date_ic(monthly_precip)
count <- monthly_precip$size()
cat("Count: ", count$getInfo())
(2021-1981)*12
41*12


monthly_precip_renamed <- monthly_precip$ 
  # CHIRPS release is delayed by a month or 2 so you need to pre-filter
  # or difficult issues will occur downstream
  
  filterDate("1981-01-01","2021-11-30") |> 
  ee$ImageCollection$map(
    function(x) {
      date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
      name <- ee$String$cat("pr_", date)
      x$select("precipitation")$rename(name)
      # return(x)
      # x$set("RGEE_NAME",name)
      # x$select("precipitation")$rename(name)
      # x$set("RGEE_NAME", name)
    })
# X123_precipitation
# precipitation
#Extract values - getInfo
system.time( # 29s on 8 gb machine
  ne_states_precip <- ee_extract(
    x = monthly_precip_renamed,
    y = ne_states_simp["ADM1_EN"],
    scale = 5500,
    fun = ee$Reducer$median(),
    sf = TRUE
  )
)
system.time( # 29s on 8 gb machine
  ne_lgas_precip <- ee_extract(
    x = monthly_precip_renamed,
    y = ne_lgas_simp["ADM2_EN"],
    scale = 5500,
    fun = ee$Reducer$median(),
    sf = TRUE
  )
)



ne_lgas_precip_long <- ne_lgas_precip |> 
  st_drop_geometry() |> 
  pivot_longer(-ADM2_EN) |> 
  mutate(
    date= str_remove(string = name, pattern = ".*pr_") |> 
      str_replace_all("_","-") |> lubridate::ymd()

  ) 


ne_states_precip_long <- ne_states_precip |> 
  st_drop_geometry() |> 
  pivot_longer(-ADM1_EN) |> 
  mutate(
    date= str_remove(string = name, pattern = ".*pr_") |> 
      str_replace_all("_","-") |> lubridate::ymd()

  ) 

# full record aggregated to monthly
windows();ne_states_precip_long |> 
  ggplot(aes(
    x=date, y= value, color=ADM1_EN
  ))+
  geom_line()

# full record aggregated to monthly
windows();ne_lgas_precip_long |> 
  ggplot(aes(
    x=date, y= value, color=ADM2_EN
  ))+
  geom_line()

ne_states_monthly_avg <- ne_states_precip_long |> 
  mutate(
    month_label= lubridate::month(date,label = T)
  ) |> 
  group_by(ADM1_EN, month_label) |> 
  summarise(
    mean_precip=mean(value),.groups = "drop"
    
  )
ne_lgas_monthly_avg <- ne_lgas_precip_long |> 
  mutate(
    month_label= lubridate::month(date,label = T)
  ) |> 
  group_by(ADM2_EN, month_label) |> 
  summarise(
    mean_precip=mean(value),.groups = "drop"
    
  )


ne_states_monthly_avg |> 
  ggplot(aes(x=month_label, y=mean_precip,color=ADM1_EN, group=ADM1_EN))+
  geom_line()+
  geom_point()

ne_lgas_monthly_avg |> 
  ggplot(aes(x=month_label, y=mean_precip,color=ADM2_EN, group=ADM2_EN))+
  geom_line()+
  geom_point()



# ee_print(monthly_precip_renamed)
# filtered <- monthly_precip_renamed$filterMetadata("ID", "equals", 491)
# 
# ee_print(filtered)
# monthly_precip_renamed |> ee_print()
# 
# monthly_precip |> ee_print()
# 
# 
# monthly_precip_renamed$
#   filterDate("2021-12-01","2021-12-31") |> 
#   ee_print()
















test |> 
  ee$ImageCollection$map(
    function(x) {
      x$select("precipitation")
    }
  )


ee_print(test)

monthly_precip_renamed <- monthly_precip |> 
  ee$ImageCollection$map(
    function(x) {
      date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
      name <- ee$String$cat("pr_", date)
      x$select("precipitation")$rename(name)
      # x$set("RGEE_NAME",name)
  # x$select("precipitation")$rename(name)
  # x$set("RGEE_NAME", name)
})



#Extract values - getInfo
system.time( # 29s on 8 gb machine
ne_states_precip <- ee_extract(
  x = monthly_precip_renamed,
  y = ne_states_simp["ADM1_EN"],
  scale = 5500,
  fun = ee$Reducer$median(),
  sf = TRUE,
)
)

# x`colnames(ne_states_precip)[1]

ne_states_precip |> head()
ne_states_precip |> 
  st_drop_geometry() |> 
  pivot_longer(-ADM1_EN)

# all games to put in correct dates.. but would be better if ee_extract did that for us?
all_years<-seq(1981,2021,1)
all_months <-  seq(1,12,1)

year_mo_combos<-expand_grid(all_years,all_months) |> 
  mutate(
    yr_mos=paste0(all_years,"-",all_months,"-01") |> lubridate::ymd()
  )

as.character(year_mo_combos$yr_mos) |> length()
test_df <- ne_states_precip
colnames(test_df)[2:(ncol(test_df)-1)] |> length()

ne_states_precip |> ncol()
colnames(ne_states_precip)
ne_states_precip |> tibble()
colnames(ne_states_precip)[ncol(ne_states_precip)-1]
wtf <- colnames(ne_states_precip)[2:(ncol(ne_states_precip)-1)] 


length(wtf)
colnames(ne_states_precip)[2:(ncol(ne_states_precip)-1)] <- as.character(year_mo_combos$yr_mos)

ne_states_precip |> tibble()
