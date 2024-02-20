zone_poly_setup <- function(input){
  require(sp)
  require(sf)
  require(tidyverse)
  
  load(file = "data/app_setup.rda")
  load(file = "data/Strikezone_Polys.rda")
  
  df <- input %>% 
    filter(!is.na(c(PlateLocHeight)))
  
  K_Zone_Points <- SpatialPoints(coords = cbind(df$PlateLocSide, df$PlateLocHeight))
  
  df <- df %>% 
    mutate(
      pitch_zone = case_when(
        over(K_Zone_Points, SZ_SpatialPoly_01, returnList = TRUE) == 1 ~ "KZONE_01",
        over(K_Zone_Points, SZ_SpatialPoly_02, returnList = TRUE) == 1 ~ "KZONE_02",
        over(K_Zone_Points, SZ_SpatialPoly_03, returnList = TRUE) == 1 ~ "KZONE_03",
        over(K_Zone_Points, SZ_SpatialPoly_04, returnList = TRUE) == 1 ~ "KZONE_04",
        over(K_Zone_Points, SZ_SpatialPoly_05, returnList = TRUE) == 1 ~ "KZONE_05",
        over(K_Zone_Points, SZ_SpatialPoly_06, returnList = TRUE) == 1 ~ "KZONE_06",
        over(K_Zone_Points, SZ_SpatialPoly_07, returnList = TRUE) == 1 ~ "KZONE_07",
        over(K_Zone_Points, SZ_SpatialPoly_08, returnList = TRUE) == 1 ~ "KZONE_08",
        over(K_Zone_Points, SZ_SpatialPoly_09, returnList = TRUE) == 1 ~ "KZONE_09",
        over(K_Zone_Points, SZ_SpatialPoly_11, returnList = TRUE) == 1 ~ "KZONE_11",
        over(K_Zone_Points, SZ_SpatialPoly_12, returnList = TRUE) == 1 ~ "KZONE_12",
        over(K_Zone_Points, SZ_SpatialPoly_13, returnList = TRUE) == 1 ~ "KZONE_13",
        over(K_Zone_Points, SZ_SpatialPoly_14, returnList = TRUE) == 1 ~ "KZONE_14",
        
        PlateLocSide < -10 & PlateLocHeight > 30 |
          PlateLocSide < 0 & PlateLocHeight > 42 ~ 'KZONE_11',
        
        PlateLocSide > 10 & PlateLocHeight > 30 |
          PlateLocSide > 0 & PlateLocHeight > 42 ~ 'KZONE_12',
        
        PlateLocSide < -10 & PlateLocHeight < 30 |
          PlateLocSide < 0 & PlateLocHeight < 18 ~ 'KZONE_13',
        
        PlateLocSide > 10 & PlateLocHeight < 30 |
          PlateLocSide > 0 & PlateLocHeight < 18 ~ 'KZONE_14'
      ),
      
      in_out_zone = case_when(
        PlateLocHeight >= 18 & PlateLocHeight <= 42 &
          abs(PlateLocSide) <= 10 ~ "In Zone",
        TRUE ~ "Out Zone"),
      
      tango_zone = case_when(
        PlateLocHeight >= 22 & PlateLocHeight <= 38 &
          abs(PlateLocSide) <= 20/3 ~ "Heart",
        
        abs(PlateLocSide) <= 40/3 & PlateLocHeight >= 14 & PlateLocHeight <= 22 ~ "Shadow",
        abs(PlateLocSide) <= 40/3 & PlateLocHeight >= 38 & PlateLocHeight <= 46 ~ "Shadow",
        abs(PlateLocSide) >= 20/3 & abs(PlateLocSide) <= 40/3 &
          PlateLocHeight >= 22 & PlateLocHeight <= 38 ~ "Shadow",
        
        abs(PlateLocSide) <= 20 & PlateLocHeight >= 6 & PlateLocHeight <= 14 ~ "Chase",
        abs(PlateLocSide) <= 20 & PlateLocHeight >= 46 & PlateLocHeight <= 54 ~ "Chase",
        abs(PlateLocSide) >= 40/3 & abs(PlateLocSide) <= 20 &
          PlateLocHeight >= 14 & PlateLocHeight <= 46 ~ "Chase",
        
        abs(PlateLocSide) >= 20 | PlateLocHeight <= 6 | PlateLocHeight >= 54 ~ "Waste")
    )
}

save(zone_poly_setup, file = "data/zone_poly_setup.rda")

