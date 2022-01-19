library(tidyverse)
library(rgee)
ee_Initialize()
chirps_src <- "UCSB-CHG/CHIRPS/DAILY"
chirps <- ee$ImageCollection(chirps_src)

# // Map filtering and reducing across year-month combinations and convert to ImageCollection
yr_mo_composite_stat_ic <- function(dat,month_range, year_range, stat="mean"){
  
  month_list <- ee$List$sequence(month_range[1], month_range[2])
  year_list <- ee$List$sequence(year_range[1],year_range[2])
  
  collapse_ic_fun<- switch(
    stat,
    "mean" = function(x)x$mean(),
    "max" = function(x)x$max(),
    "min" = function(x)x$min(),
    "median"= function(x)x$median(),
    "sum"= function(x)x$sum(),
    "sd" = x$reduce(ee$Reducer$stdDev()),
    NULL
  )
  
  composites <- ee$ImageCollection$fromImages(
    year_list$map(
      ee_utils_pyfunc(function (y) {
        month_list$map(
          ee_utils_pyfunc(function (m) {
            # dat_pre_filt <- 
            dat_filtered_yr_mo<- dat$
              filter(ee$Filter$calendarRange(y, y, 'year'))$
              filter(ee$Filter$calendarRange(m, m, 'month'))
            
            
            collapse_ic_fun(dat_filtered_yr_mo)$
              set('year',y)$
              set('month',m)$
              set('system:time_start',ee$Date$fromYMD(y,m,1))$
              set('system:time_end',ee$Date$fromYMD(y,m,28))

          })
        )
      }))$flatten())
  return(composites)
}


chirps_monthly_sum <- yr_mo_composite_stat_ic(dat = chirps,month_range = c(1,12),year_range = c(1981,2021),stat = "sum")
chirps_monthly_sum |> ee_print()
chirps_monthly_sum |> 
  ee_get_date_ic()

reprex::reprex()



# 
# # investigating how nested mapping works?
# seq_exa <- ee$List$sequence(1,5)
# seq_exb <- ee$List$sequence(6,11)
# seq_exb <- ee$List$sequence(1,12)
# # seq_exb <- ee$List(2,2,2,2)
# 
# 
# 
# howboutit <- seq_exa$map(
#     ee_utils_pyfunc(function(a){
#       seq_exb$map(
#       ee_utils_pyfunc(function(b){
#         ee$Number(a)$add(ee$Number(b))
#     }))
#       
#     }
#   )
#   )
# howboutit$getInfo()
#   
# howboutit$getInfo()
# seq_exa$get(0)$getInfo()
# ic_img <- ee$Image(ee$List(chirps_monthly_sum)$get(0))
# ee$List(chirps_monthly_sum)$size()$getInfo()
# ic_img$getInfo()
# chirps_monthly_sum$size()$getInfo()
