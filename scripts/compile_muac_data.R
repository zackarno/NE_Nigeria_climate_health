library(tidyverse)
library(sf)
# compile nutrition sector data from jan 2017 to november 2019 and save to rds.
# no real cleaning done here
# could update once November/December 2021 data is available
# https://www.humanitarianresponse.info/en/operations/nigeria/nutrition/documents/document-type/3w-4w


write_output <- c(T,F)[2]

admins <-  load_nga_admins()
ward <- admins$ward

# wards of interest
woi <- ward %>% 
  filter(ADM2_EN %in% c("Maiduguri","Jere"))


muac_2017_2019 <- readxl::read_excel(here::here("data/NIS_Nutrition_2017_to_2019.xlsx"))

# no site level information
muac_2017_2019 <- muac_2017_2019 |> 
  select(
    State,
    LGA,
    Ward,
    muac_total_screened=MUAC_Screened_TOTAL_Children,
    muac_red = MUAC_Screened_YELLOW,
    new_otp_admissions= OTP_New_Admissions,
    yr= Activity_year,
    mo = `Activity_Month`
    
  )




muac_2021 <- readxl::read_excel(here::here("data/nutrition_sector_5w_jan_-_nov_2021_final.xlsx"),sheet="Jan_Nov 2021", skip=2)
# site level information available so tag it on in case it is useful for ward harmonization
muac_2021 <- muac_2021 |> 
  select(
    State,
    LGA,
    Ward,
    muac_total_screened=`Total Number of Children 6-59 months Screened`,
    muac_red= `RED MUAC Screened`,
    new_otp_admissions= `OTP New Admissions (Total)`,
    yr= `Activity Year`,
    mo = `Activity Month`,
    site_name= `Site Name`,
    site_type = `Site Type`,
    site_id = `Sites ID`,
    lon=`Longitude (E)`,
    lat=`Latitude (N)`
    
  )


# site level information available so tag it on in case it is useful for ward harmonization
muac_2020 <- readxl::read_excel(here::here("data/2020_nutrition_5w_january_-_december_final.xlsm"),
                                sheet = "5Ws", skip=2) |> 
  select(
    State,
    LGA,
    Ward,
    muac_total_screened=MUAC_Screened_TOTAL_Children,
    muac_red= `RED MUAC Screened`,
    new_otp_admissions= `OTP New Admissions`,
    yr= `Activity Year`,
    mo = `Activity Month`,
    site_name= `Site Name`,
    site_type = `Site Type`,
    site_id = `Sites ID`,
    lon=`Longitude (E)`,
    lat=`Latitude (N)`
    
  )


muac <- bind_rows(muac_2021,muac_2020,muac_2017_2019)

if(write_output){
  saveRDS(muac, here::here("data/muac_201701_202111.rds"))
  write_csv(muac, here::here("data/muac_201701_202111.csv"))
}



# to give to NGA office ---------------------------------------------------


muac$LGA %>% unique()
ward_lga_lookup<- admins$ward$ADM2_EN |> 
  set_names(admins$ward$ADM3_EN)

muac_cleaned <- muac %>% 
  mutate(
    # across(.cols= matches("^muac_|^new_"), ~replace_na(.x,0)),
    across(.cols = c("State","LGA","Ward","site_name"),~str_to_title(str_squish(.x))),
    mo= str_to_title(mo),
    date= paste0(yr,"-",mo,"-01"),
    date_dt= lubridate::ymd(date),
    LGA= str_replace_all(LGA, c("Askira-Uba"="Askira/Uba",
                                "Postiskum" ="Potiskum" ,
                                "Gaidam" ="Geidam",
                                "Tarmuwa" ="Tarmua" )),
    # fix these na LGAs that have wards
    LGA = if_else(is.na(LGA),recode(Ward, !!!ward_lga_lookup),LGA)
    
  )
muac_cleaned %>% 
  filter(!is.na(date_dt),!is.na(Ward)|!is.na(site_name)|!is.na(site_id)) %>% 
  filter(is.na(Ward))


woi <- woi %>% 
  unite(col = "admins_concat",c(ADM1_EN,ADM2_EN,ADM3_EN),sep = "_",remove = F)
woi %>% 
  st_drop_geometry() %>% 
  select(matches("ADM\\d_EN")) %>% 
  write_excel_clip()
muac %>% 
  filter(!is.na(site_name))

ward %>% 
  filter(str_detect(ADM3_EN,"musar"))


woi$ADM3_EN %>% sort() %>% write_excel_clip()
woi$admins_concat
wards_partial_clean <- muac_cleaned %>% 
  filter(!is.na(date_dt),!is.na(Ward)) %>% 
  # filter(LGA %in% c("Maiduguri","Jere") & State!="Borno")
  filter(State=="Borno",LGA %in% c("Maiduguri","Jere")) %>%
  unite(col = "admins_concat",State:Ward,sep = "_",remove = F) %>% 
  filter(!admins_concat %in% woi$admins_concat) %>% 
  group_by(State,LGA, Ward, site_name) %>% 
  summarise(
    n=n(),.groups = "drop"
  ) %>% 
  mutate(
    HQ_ward_guess = case_when(
      Ward %in% c("Bolori Ii",
                   "Bolori Ii",
                   "Bolori-Ii",
                   "Bolori 2",
                   "Bolori_ii",
                   "Bolori 2a&2c",
                   "Bolori 2b&D")~"Bolori II",
      
      # apparently there is teachers village in both bolori 1 & 2 - did both
      str_detect(Ward,"^Bolori.*|^Teacher.*")~"Bolori I",
      str_detect(Ward,"^Bulab.+in$|^Bulab.+")~"Bulabulin",
      str_detect(Ward,"^Dala.+")~"Dala",
      str_detect(Ward,"^Dus[um].+|^Muna_eth.*|Farm Centre.*")~"Dusuma",
      str_detect(Ward,"^G[ao]mboru.+|Intersos Clinic Gamboru")~"Gamboru",
      str_detect(Ward,"^Gomari.*|Gangamari|Ngomari")~"Gomari",
      Ward %in% c("Gwange","Gwange I","Gwange 1&2","Gwange-I","Gwnge 1","Gwnge 1")~"Gwange I",
      Ward %in% c("Gwange Ii")~"Gwange II",
      Ward %in% c("Gwange Iii","Gwange 3")~"Gwange III",
      Ward %in% c("Shehuri","Shehuri North","Shehuru North")~"Shehuri North",
      str_detect(Ward,"^Shuwari.*|Shok")~"Shehuri North",
      str_detect(Ward,"^Maisandari.*|Fatima Ali Sheriff")~ "Maisandari" ,
      str_detect(Ward,"^Lam[iu]sula*")~ "Lamisula" ,
      str_detect(Ward,"^Musari*|Maimusari.*")~ "Maimusari" ,
      str_detect(Ward,"^Mash.*mari*|505 Clinic/Hf|Mala Sheriff Camp")~ "Mashamari" ,
      str_detect(Ward,"^202.*|^Mair*")~ "Mairi" ,
      
      #,https://reliefweb.int/sites/reliefweb.int/files/resources/referral_pathway_mmc_jere_final_24.03.17.pdf
      str_detect(Ward,"^Gw[oa]zar*")~ "Dusuma" ,
      str_detect(Ward,"^Sabon.*|^Bale.*|Galtimari|^Gs.+imari$|^Jiddari.*")~ "Galtimari",
      str_detect(Ward,"^Dalori.*")~"WRONG LGA",
      str_detect(Ward,"^Fariah.*lock*|^Madinatu.*")~"Old Maiduguri",
      str_detect(Ward,"^K.+mari$")~"Khadammari",
      str_detect(Ward,"^Gajigana*")~"WRONG LGA",
      
      # Gajigana  - there is a ward called Gajigana in Magumeri... maybe it should b
      TRUE~ Ward
    )

  ) %>% 
  unite(col = "admins_concat_new",c(State,LGA,HQ_ward_guess),sep = "_",remove = F) %>% 
  mutate(
    HQ_ward_guess = case_when(
      admins_concat_new =="Borno_Jere_Gamboru"~"WRONG LGA",
      admins_concat_new =="Borno_Jere_Gwange I"~"WRONG LGA",
      admins_concat_new =="Borno_Jere_Maisandari"~"WRONG LGA",
      admins_concat_new =="Borno_Maiduguri_Gomari"~"WRONG LGA",
      admins_concat_new =="Borno_Maiduguri_Dusuma"~"WRONG LGA",
      TRUE~HQ_ward_guess
      ),
    HQ_LGA_guess = case_when(
      str_detect(Ward,"^Dalori.*")~"Konduga",
      admins_concat_new =="Borno_Jere_Gamboru"~"Maiduguri",
      admins_concat_new =="Borno_Jere_Gwange I"~"Maiduguri",
      admins_concat_new =="Borno_Jere_Maisandari"~"Maiduguri",
      admins_concat_new =="Borno_Maiduguri_Old Maiduguri"~"Jere",
      admins_concat_new= str_detect(Ward,"^Gajigana*")~"Magumeri",
      admins_concat_new =="Borno_Maiduguri_Gomari"~"Jere",
      admins_concat_new =="Borno_Maiduguri_Dusuma"~"Jere",
      TRUE ~ NA_character_
    )
    
  ) %>% 
  mutate(matching= admins_concat_new %in% woi$admins_concat) %>% 
  # count(HQ_ward_guess) %>% 
  print(
    n=306
  )


wards_partial_clean %>% 
  mutate(
    HQ_ward_guess2= case_when(
      matching|HQ_ward_guess=="WRONG LGA"|!is.na(HQ_LGA_guess)~HQ_ward_guess,
      TRUE~NA_character_
    )
  ) %>% 
  select(State,LGA,Ward,site_name,HQ_ward_guess=HQ_ward_guess2,HQ_LGA_guess) #%>% 
  filter(is.na(HQ_ward_guess)) %>% 
  count(Ward)
  
  
ward_harm <- readxl::read_excel(here::here("data/20220121_muac_maidu_jere_ward_harmonizer_from_nga_team.xlsx"),"name_harmonizer")

ward_dictionary <- ward_harm %>%
  filter(!is.na(ward_correction),(is.na(LGA_correction)|LGA_correction %in% c("Maiduguri","Jere"))) %>% 
  mutate(
    LGA_crct= if_else(is.na(LGA_correction),LGA,LGA_correction)
  ) %>% 
  unite(col ="admin_key", c(State,LGA,Ward),sep = "_",) %>% 
  select(admin_key,LGA_crct,ward_crct=ward_correction)

ward_key <- ward_dictionary$ward_crct %>% 
  set_names(ward_dictionary$admin_key)

lga_key <- ward_dictionary$LGA_crct %>% 
  set_names(ward_dictionary$admin_key)

muac_cleaned_wards_harmonized <- muac_cleaned %>% 
  unite(col ="admin_key", c(State,LGA,Ward),sep = "_",remove = F) %>% 
  mutate(
    ward_crct = if_else(admin_key %in% names(ward_key),recode(.x = `admin_key`,!!!ward_key),Ward),
    LGA_crct = if_else(admin_key %in% names(lga_key),recode(.x = `admin_key`,!!!lga_key),LGA)
  )

write_rds(muac_cleaned_wards_harmonized,here::here("data/20220121_MUAC_w_maidu_jere_wards_harmonized.rds"))
