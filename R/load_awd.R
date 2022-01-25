

load_awd <- function(path = "data/Borno_EWARS_awd_under5_2017_2021.xlsx",
                     lgas= c("Maiduguri","Jere")){
  awd <- readxl::read_excel(here::here(path)) |> 
    rename(ADM2_EN = `...1`)
  
  admins <- load_nga_admins()
  ward <- admins$ward
  # wards of interest
  woi <- ward %>% 
    filter(ADM2_EN %in% lgas)
  
  ne_lgas<- admins$lga |> 
    sf::st_drop_geometry() |> 
    filter(ADM1_EN %in% c("Yobe","Borno","Adamawa")) 
  
  awd <- awd |> 
    mutate(
      ADM2_EN= str_replace_all(string = ADM2_EN,c("Askira Uba"="Askira/Uba","Kala Balge"="Kala/Balge" ))
    )
  
  awd_long <- awd |> 
    pivot_longer(-1,names_to = "date",values_to = "cases") |> 
    left_join(ne_lgas |> select(ADM1_EN,ADM2_EN)) |> 
    select(ADM1_EN,ADM2_EN,everything())
  
  # add year-wk - double checked there is only 1 week collected per year per location
  awd_long <- awd_long %>%
    mutate(
      yr_wk=paste0(lubridate::year(date),"_",lubridate::week(date))
    )
  
  if(!is.null(lgas)){
    
    awd_long <- awd_long %>% 
      filter(ADM2_EN %in% lgas)
    

    
    

  }
  
  return(awd_long)
  
    
  
  
}



