library(rgee)
library(dplyr)
ee_Initialize()

chirps_src <- "UCSB-CHG/CHIRPS/DAILY"
chirps <- ee$ImageCollection(chirps_src)

chirps_list<- chirps$toList(chirps$size())


chirps_img_indx1 <- chirps_list$get(1)

# in order to access//print this object I need an extra step to with ee$Image
ee$Image(chirps_img_indx1) %>% ee_print()

# if I try to printing without the extra step I get an error.
chirps_img_indx1 %>%  ee_print()


reprex::reprex()
