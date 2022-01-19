yr_mo_composite_stat_ic <- function(dat,month_range=c(1,12), year_range=c(2004,2005), stat="mean"){
  
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
  cat("caluclating monthly ",stat, " for years: ",year_range[1],"-", year_range[2])
  
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
              set('date',ee$Date$fromYMD(y,m,1))$
              # set('system:time_start',ee$Date$fromYMD(y,m,1))$
              set('system:time_start',ee$Date$millis(ee$Date$fromYMD(y,m,1)))
              
            
          })
        )
      }))$flatten())
  cat("returning and ImageCollection of ")
  return(composites)
}



