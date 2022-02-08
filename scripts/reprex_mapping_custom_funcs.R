
#################################################
# did get this one working - saved in R directory for use in scripts
#################################################


# requires the entire ic.... still cant figure out how to properly map pre-defined functions.
# need to figure out how the argument can be piped in

map_date_to_bandname_ic <- function(ic){
  ic |> 
    ee$ImageCollection$map(
      function(x){
        # can't use getInfo() in sever-side function
        bnames<- x$bandNames()
        date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
        
        # since bnames is technically a list rather than a simple string I need to map over it
        # this should make it flexible fore when there are more bans I want to rename anyways
        bnames_date <- bnames$map(
          ee_utils_pyfunc(function(x){
            ee$String(x)$cat(ee$String("_"))$cat(date)
            
          })
        )
        x$select(bnames)$rename(bnames_date)
      }
      
    )
  
}


# here is the issue
#######################################


add_date_to_name_band <- function(x){
  bnames<- x$first()$bandNames()$getInfo()
  date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
  name <- ee$String$cat(paste0(bnames,"_"), date)
  x$select(bnames)$rename(name) 
  
}
chirps$
  filterDate("2021-01-01","2021-01-10") |> 
  ee$ImageCollection$map(
    add_date_to_name_band()
  )


chirps$
  filterDate("2021-01-01","2021-01-10") |> 
  map_date_to_band_name_ic() |> 
  ee_print()

add_date_to_name_band<- function(x){
  bnames<- x$bandNames()
  date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
  
  bnames_date <- bnames$map(
    ee_utils_pyfunc(function(x){
      ee$String(x)$cat(ee$String("_"))$cat(date)
      
    })
  )
  
  x$select(bnames)$rename(bnames_date)
}



chirps$
  filterDate("2021-01-01","2021-01-10") |> 
  ee$ImageCollection$map(
    function(x){
      bnames<- x$bandNames()
      date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
      
      bnames_date <- bnames$map(
        ee_utils_pyfunc(function(x){
          ee$String(x)$cat(ee$String("_"))$cat(date)
          
        })
      )
      
      x$select(bnames)$rename(bnames_date)
    }
    
  )

ee$String(chirps$first()$bandNames()$flatten())

chirps$ 
  # CHIRPS release is delayed by a month or 2 so you need to pre-filter
  # or face difficulty later
  filterDate("1981-01-01","2021-11-30") |> 
  ee$ImageCollection$map(
    function(x) {
      date <- ee$Date(x$get("system:time_start"))$format('YYYY_MM_dd')
      # name <- ee$String$cat("pr_", date)name <- ee$String$cat("pr_", date)
      x$select("precipitation")$rename(name)
    })


bnames<- chirps$first()$bandNames()$getInfo()
date <- ee$Date(chirps$first()$get("system:time_start"))$format('YYYY_MM_dd')
name <- ee$String$cat(paste0(bnames,"_"), date)
chirps$first()$select(bnames)$rename(name) |> ee_print()