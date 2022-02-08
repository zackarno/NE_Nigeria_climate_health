# ### Aggregations
# 
# **Spatial:**
#   
#   - State (all 3)
# - LGA (in all 3 States),
# - LGA (Maiduguri & Jere dissolved)
# - Wards (in Maiduuri & Jere LGAs only)
# 
# **Temporal:**
#   
#   - CHIRPS rainfall
# + daily precipitation - mean value per admin
# + monthly precipitation (summed at pixel level) - mean value per admin
# - MODIS TERRA (params: NDVI, Zscore NDVI, diff NDVI, VCI):
#   + 16 day composites - mean value per admin
# + Monthly composite (max NDVI pixel level) - mean value per admin
# - SMAP Soil Moisture (moisture params: ssm, susm, smp)
# + 3 day moisture parameters - mean value per admin
# + Monthly composite (mean or max val pixel level) - mean value per admin
# - Surface Temp
# 





modis_chirps_smap_awd_otp_mz_time_raw_df |> count(level)
mdf <- modis_chirps_smap_awd_otp_mz_time_raw_df


temporal_chunker <-  function(dat,time_step="month"){
  switch(
    time_step,
    "month"=lubridate::month(),
    "week"= lubridate::week(),
    NULL
  )
  
}


split(mdf, mdf$level)
# would make sense to give everything dates?
# otp case occur 2nd week of month?

mdf |> 
  filter(year(date)>=2017) |> 
  mutate(
    level2= if_else(level %in% c("ne_lgas_all","ne_lgas_borno"),"lga",level)
  ) |> 
  filter(level2=="lga") |> 
  # lets see how weekly looks - our only problem should be modis
  group_by(parameter,yr=year(date),wk=week(date),ADM_NAME) |>
  # filter(yr==2017, wk==1, parameter=="precipitation") |>
  arrange(ADM_NAME) |>
  # summarise(value=mean(value)) |> print(n=65)
  summarise(
    value_mean=mean(value,na.rm=T),
    value_sum= sum(value,na.rm=T),
    # value_sum = if_else(parameter=="precipitation", sum(value,na.rm=T),mean(value,na.rm=T)),
    .groups="drop"
  ) |> 
  mutate(value= if_else(parameter%in%c("precipitation","awd_cases","otp_cases"), value_sum,value_mean)) |> 
  select(-value_mean, - value_sum) |> 
  pivot_wider(id_cols = c("ADM_NAME",
                          # "level2",
                          "yr",
                          "wk"),names_from=parameter,
              values_from=value) |> 
  filter(!is.na(otp_cases)) |> 
  print(n=100)



ckfoo <- mdf |> 
  filter(year(date)>=2017) |> 
  mutate(
    level2= if_else(level %in% c("ne_lgas_all","ne_lgas_borno"),"lga",level)
  ) |> 
  filter(level2=="lga") |> 
  # lets see how weekly looks - our only problem should be modis
  group_by(parameter,yr=year(date),mo=month(date),ADM_NAME) |>
  summarise(
    value_mean=mean(value,na.rm=T),
    value_sum= sum(value,na.rm=T),
    # value_sum = if_else(parameter=="precipitation", sum(value,na.rm=T),mean(value,na.rm=T)),
    .groups="drop"
  ) |> 
  mutate(value= if_else(parameter%in%c("precipitation","awd_cases","otp_cases"), value_sum,value_mean)) |> 
  select(-value_mean, - value_sum) |> 
  pivot_wider(id_cols = c("ADM_NAME",
                          # "level2",
                          "yr",
                          "mo"),names_from=parameter,
              values_from=value) #|> 
  # filter(across(everything(), ~any(is.na(.x)))) |> 
  # filter(is.na(otp_cases)) |>
  # filter(across(everything(), ~ !is.na(.)))
  # filter(across(everything(), ~ is.na(.))) |>
  # filter(is.na(otp_cases)) |>
  print(n=100)
  # filter(!is.na(awd_cases))
  
  # group_split(level,.keep = T) |> 
  # pluck(4) |> 
  
ckfoo |> 
  group_by(ADM_NAME) |> 
  filter(!is.na(awd_cases)) |> 
  group_modify(~ broom::tidy(lm(awd_cases ~ precipitation, data = .x))) |> 
  filter(term=="precipitation") |> 
  arrange(p.value)

maidu_monthly <- ckfoo |> 
  filter(ADM_NAME=="Maiduguri") 
# https://uc-r.github.io/model_selection

library(leaps)
best_subset <- regsubsets(awd_cases ~precipitation+EVI+NDVI+NDVI_z_score , maidu_monthly, nvmax = 19)
results <- summary(best_subset)

tibble(predictors = 1:4,
       adj_R2 = results$adjr2,
       Cp = results$cp,
       BIC = results$bic
       # AIC = names(results)
       )

m1 <- lm(awd_cases~precipitation,maidu_monthly) 
m2 <- lm(awd_cases~precipitation+EVI,maidu_monthly) 
m3 <- lm(awd_cases~precipitation+EVI+NDVI,maidu_monthly) 
m4 <- lm(awd_cases~precipitation+EVI+NDVI+NDVI_z_score,maidu_monthly) 

# plot(m2)
# options(na.action = "na.fail") # Required for dredge to run
# library(MuMIn)
# options(na.action = "na.omit") # set back to default
# nada <- MuMIn::dredge(global.model = m4,beta = F, evaluate = T, rank = AICc)


library(glue)
ckfoo |> 
  filter(!is.na(awd_cases)) |> 
  mutate(
    date= ymd(glue("{yr}-{mo}-01"))
  ) |> 
  ggplot(aes(x= date, y= awd_prev_per_1000))+
  geom_line()+
  facet_wrap(~ADM_NAME)

ckfoo |> filter(ADM_NAME=="Shani") |> 
  print(n=61)

awd_ol <-  load_awd(lgas = NULL)
awd_ol |> 
  group_by(ADM2_EN,date= ymd(glue("{year(date)}-{month(date)}-01"))) |> 
  summarise(
    cases= sum(as.numeric(cases))
  ) |> 
  ggplot(aes(x=date, y= cases))+
  geom_line(color="blue")+
  facet_wrap(~ADM2_EN)


pop
ckfoo |> 
  group_by(ADM_NAME) |> 
  filter(!is.na(awd_cases)) |> 
  group_modify(~ broom::tidy(lm(awd_cases ~ precipitation, data = .x))) |> 
  filter(term=="precipitation") |>
  arrange(p.value) |> 
  print(n=64)
  
  
  iris %>%
  group_by(Sex) %>%
  group_modify(~ broom::tidy(lm(Diameter ~ Whole.weight, data = .x)))
ckfoo |> 
  # filter(across(everything(),~anyis.na(.)))
  filter(if_any(everything(),~is.na(.))) |> 
  print(n=500)
  filter(is.na(otp_cases))

mdf |>
  pivot_wider(id_cols = c(ADM_NAME,date),names_from=parameter, values_from=value) |> 
  filter(date>="2017-01-01") |> 
  # filter(!is.na(smp))
  filter(!is.na(awd_cases))
  print(n=100)
  arrange(date)

  