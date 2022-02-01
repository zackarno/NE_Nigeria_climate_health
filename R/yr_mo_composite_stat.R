yr_mo_composite_stat_ic <- function(dat,
                                    month_range=c(1,12),
                                    year_range=c(2004,2005),
                                    stat="mean",
                                    monthly_stat_per = "year"
                                    ){
  
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
  
  
  if(monthly_stat_per=="year"){
  cat("calculating monthly ",stat, " for each month in each year from: ",year_range[1]," to ", year_range[2])
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
  }
  if(monthly_stat_per=="range"){
    composites <- 
      month_list$map(
      ee_utils_pyfunc(
        function(m) {
          ndvi_tfilt <- dat$filter(ee$Filter$calendarRange(year_range[1], year_range[2], 'year'))$
            filter(ee$Filter$calendarRange(m, m, 'month'))
          
          
          ndvi_mean <- ndvi_tfilt$
            mean()$
            rename("NDVI_mean")$
            set("system:time_start", ee$Date$millis(ee$Date$fromYMD(2000,m,1)))
        })
    )
          
    
    
    
        }
  
  cat("returning and ImageCollection of ")
  return(composites)
}


# take 2 ------------------------------------------------------------------
fun_sel <-  function(x){switch(x,
                               
                               "mean" = function(x)x$reduce(ee$Reducer$mean()), 
                               "max" = function(x)x$reduce(ee$Reducer$max()),
                               "min" = function(x)x$reduce(ee$Reducer$min()),
                               "median"= function(x)x$reduce(ee$Reducer$median()),
                               "sum"= function(x)x$reduce(ee$Reducer$stdDev()),
                               "sd" =  function(x)x$reduce(ee$Reducer$stdDev()),
                               NULL
                               
)
}


yr_mo_composite_stat_ic2 <- function(dat,
                                    month_range=c(1,12),
                                    year_range=c(2004,2005),
                                    stat=c("mean"),
                                    monthly_stat_per = "year"
){
  #v2
  
  month_list <- ee$List$sequence(month_range[1], month_range[2])
  year_list <- ee$List$sequence(year_range[1],year_range[2])
  num_years<- year_range[2]-year_range[1]
  
  
  
  collapse_ic_fun <- fun_sel(stat)

  
  
  if(monthly_stat_per=="year"){
    
    cat(crayon::green(glue::glue("calculating 1-year monthly {stat}s from {year_range[1]} to {year_range[2]}")),"\n")
    composites_list <-
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
        })
      )
  }
  if(monthly_stat_per=="range"){
    cat(crayon::green(glue::glue("calculating {num_years}-year monthly {stat}s from {year_range[1]} to {year_range[2]}")),"\n")
    composites_list <- 
      month_list$map(
        ee_utils_pyfunc(
          function(m) {
            dat_filtered_yr_mo <- dat$
              filter(ee$Filter$calendarRange(year_range[1], year_range[2], 'year'))$
              filter(ee$Filter$calendarRange(m, m, 'month'))
            
            bnames<- dat_filtered_yr_mo$first()$bandNames()
            bnames_new <- bnames$map(
              ee_utils_pyfunc(function(x){
                ee$String(x)$cat(ee$String("_"))$cat(ee$String(stat))
                
              })
            )$flatten()
            
            
            collapse_ic_fun(dat_filtered_yr_mo)$
            #   # select(bnames)$
            #   # rename(bnames_new)$
              set("system:time_start",
                  ee$Date$millis(ee$Date$fromYMD(year_range[1],m,1)))
            
     
          }
        )
      )
    }
        
      
  

  cat("returning  ImageCollection of x\n")
  return(ee$ImageCollection$fromImages(composites_list$flatten()))
}






