library(tidyverse)

data_setup <- function(input) {
  
  deg2rad <- function(deg) {(deg * pi) / (180)}
  
  tm_df <- input
  
  tm_df <- tm_df %>% 
    filter(
      !is.na(RelSpeed),
      !is.na(AutoPitchType),
      AutoPitchType != "Other") %>% 
    mutate(
      total_pitches = n(),
      PlateLocSide = 12*PlateLocSide,
      PlateLocHeight = 12*PlateLocHeight,
      hit_x = (sin(deg2rad(Bearing)) * Distance),
      hit_y = (cos(deg2rad(Bearing)) * Distance),
    )
  
  tm_df <- tm_df %>% 
    mutate(
      PlayResult = case_when(
        KorBB == "Strikeout" ~ "Strikeout",
        KorBB == "Walk" ~ "Walk", 
        PlayResult == "Undefined" ~ NA,
        TRUE ~ PlayResult
        ),
      PlayResult = factor(PlayResult, levels = c("Single", "Double", "Triple", "HomeRun",
                                               "Walk", "Strikeout", "Out", "Error", "Sacrifice",
                                               "FieldersChoice")),
      AutoHitType = factor(AutoHitType, levels = c("GroundBall", "LineDrive", "FlyBall", "Popup")),
      
      PitchCall = factor(PitchCall, levels = c("BallCalled", "BallinDirt", "BallIntentional", "InPlay",
                                             "StrikeCalled", "FoulBall", "StrikeSwinging", "HitByPitch")),
    
    contact_type = case_when(
      ExitSpeed * 1.5 - Angle >= 117 &
        ExitSpeed + Angle >= 124 &
        ExitSpeed >= 98 &
        Angle >= 4 & Angle <= 50 ~ "Barrel",
      
      ExitSpeed * 1.5 - Angle >= 111 &
        ExitSpeed + Angle >= 119 &
        ExitSpeed >= 95 &
        Angle >= 0 & Angle <= 52 ~ "Solid",
      
      ExitSpeed * 2 - Angle >= 87 &
        Angle <= 41 & 
        ExitSpeed * 2 + Angle <= 175 &
        ExitSpeed + Angle * 1.3 >= 89 &
        ExitSpeed >= 59 & Angle <= 72 ~ "Flare/Burner",
      
      ExitSpeed + Angle * 1.3 <= 112 &
        ExitSpeed + Angle * 1.55 >= 92 &
        ExitSpeed >= 72 & Angle <= 86 ~ "Flare/Burner",
      
      Angle <= 20 &
        ExitSpeed + Angle * 2.4 >= 98 &
        ExitSpeed >= 86 & ExitSpeed <= 95 ~ "Flare/Burner",
      
      ExitSpeed - Angle >= 76 &
        ExitSpeed + Angle * 2.4 >= 98 &
        ExitSpeed >= 95 &
        Angle <= 30 ~ "Flare/Burner",
      
      ExitSpeed <= 59 ~ "Weak",
      
      ExitSpeed + Angle * 2 >= 116 ~  "Under",
      
      ExitSpeed + Angle * 2 <= 116 ~  "Topped"
    ),
    
    contact_type = factor(contact_type, levels = c(
      "Barrel", "Solid", "Flare/Burner", "Under", "Topped", "Weak"
    ))
  )
  
  return(tm_df)
}

save(data_setup, file = "data/data_setup.rda")
