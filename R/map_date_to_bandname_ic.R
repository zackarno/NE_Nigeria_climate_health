#' map_date_to_bandname_ic
#' @description concatenate date to end of band name(s). Useful in conjunction with `ee_extract`
#' @param ic image collection




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