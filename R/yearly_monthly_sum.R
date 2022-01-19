
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

