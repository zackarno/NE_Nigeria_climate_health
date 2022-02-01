ic_add_month_property <- function(ic,name="moy"){
  ic$map(
    function(img){
      moy = ee$Date(img$get('system:time_start'))$getRelative("month","year")
      img$set(name,moy)
    }
  )
  
}