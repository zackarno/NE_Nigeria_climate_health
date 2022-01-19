load_nga_admins <- function(path="data/nga_gis"){
  admin_file_names <- c("nga_admbnda_adm1_osgof_20190417",
                        "nga_admbnda_adm2_osgof_20190417",
                        "nga_admbnda_adm3_osgof_eha_20190417")
  admin_levels <- c("state","lga","ward")
  
  
  map(admin_file_names,
      ~sf::st_read(here::here(path),.x)) %>% 
    set_names(admin_levels)
  
  
  
}