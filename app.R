
# Pitch Highlighter App ---------------------------------------------------

# load packages
library(tidyverse)
library(shiny)
library(shinythemes)
library(bslib)
library(ggforce)
library(htmltools)
library(sportyR)
library(gt)
library(gtExtras)

# Load setup files
load("data/data_setup.rda")
load("data/aesthetics.rda")
load("data/app_setup.rda")
load("data/Strikezone_Polys.rda")
load("data/zone_poly_setup.rda")

# ui
ui <- page_sidebar(
  title = "DS8 Analytics",
  sidebar = sidebar(open = "always",
    helpText("Upload one or multiple TrackMan .csv files and produce hitting visuals for various types of inputs"),

    fileInput("upload_1", buttonLabel = "Upload...",
              label = NULL, accept = c(".csv"), multiple = TRUE),
    
    textInput("player", label = "Enter Player Name", value = "", width = NULL,
              placeholder = "Last, First"),
    
    selectInput("pitcher_arm", label = NULL,
                choices = c("Pitcher Throws", "RHP", "LHP")),
    
    selectInput("batter_side", label = NULL,
                choices = c("Batter Stands", "RHB", "LHB")),
    
    selectInput("pitch_zone_choice", label = NULL,
                choices = c("Select Zone",
                            "Zone 1", "Zone 2", "Zone 3",
                            "Zone 4", "Zone 5", "Zone 6",
                            "Zone 7", "Zone 8", "Zone 9",
                            "Zone 11", "Zone 12", "Zone 13", "Zone 14",
                            "In Zone", "Out Zone",
                            "Heart", "Shadow", "Chase", "Waste")),
    
    selectInput("pitch_type_choice", label = NULL,
                choices = c(
                  "Pitch Type",
                  "Fastballs",
                  "Offspeed",
                  "Breaking",
                  "Changeup",
                  "Curveball",
                  "Cutter",
                  "Fastball",
                  "TwoSeamFastBall",
                  "Four-Seam Fastball",
                  "Sinker",
                  "Slider",
                  "Splitter")),
    
    selectInput("result_choice", label = NULL,
                choices = c(
                  "Play Result",
                  "Hits",
                  "Outs",
                  "BIP",
                  "Singles",
                  "Doubles",
                  "Triples",
                  "Home Runs")),
    
    selectInput("contact_choice", label = NULL,
                choices = c(
                  "Contact Type",
                  "Groundballs",
                  "Pop Ups",
                  "Line Drives",
                  "Fly Balls",
                  "Barrel",
                  "Solid",
                  "Flare/Burner",
                  "Under",
                  "Topped",
                  "Weak")),

    selectInput("pitch_tag", label = "Tag Type:",
                choices = c("TaggedPitchType", "AutoPitchType"),
                selected = "TaggedPitchType"),
    
    actionButton("goButton_1", "Update Page")),
  
  layout_columns(
    col_widths = c(4, 4, 4, 12),
    row_heights = c(2, 1),
    card(
      full_screen = TRUE,
      card_header("Pitch Locations"),
      plotOutput("pitch_loc")
    ),
    card(
      full_screen = TRUE,
      card_header("Spray Chart"),
      plotOutput("spray")
    ),
    card(
      full_screen = TRUE,
      card_header("LA vs EV"),
      plotOutput("radial")
    ),
    card(
      full_screen = TRUE,
      card_header("Overview"),
      gt_output("overview")
    )
  )
)

# server
server <- function(input, output) {
  
  options(shiny.maxRequestSize = 10 * 1024^2)
  
  df <- eventReactive(input$upload_1, {
    combined <- lapply(input$upload_1$datapath, read_csv)
    do.call(rbind, combined)
  })
  
  data_1 <- eventReactive(input$goButton_1, {
    
    load("data/data_setup.rda")
    load("data/aesthetics.rda")
    load("data/app_setup.rda")
    load("data/Strikezone_Polys.rda")
    load("data/zone_poly_setup.rda")
    
    df <- df() %>% 
      filter(
        Batter == input$player | Pitcher == input$player
      ) %>% 
      mutate(
        TaggedPitchType = case_when(
          TaggedPitchType == "FourSeamFastBall" ~ "Fastball",
          T ~ TaggedPitchType)
      )
    
    server_df <- data_setup(df)
    
    server_df <- zone_poly_setup(server_df)
    
    # Pitcher Handness ----
    if (input$pitcher_arm == "RHP") {
      server_df <- server_df %>% 
        filter(PitcherThrows == "Right")
    }
    else if (input$pitcher_arm == "LHP") {
      server_df <- server_df %>% 
        filter(PitcherThrows == "Left")
    }
    # Batter Side ----
    if (input$batter_side == "RHB") {
      server_df <- server_df %>% 
        filter(BatterSide == "Right")
    }
    else if (input$batter_side == "LHB") {
      server_df <- server_df %>% 
        filter(BatterSide == "Left")
    }
    
    # Pitch Type ----
    if (input$pitch_type_choice == "Fastballs") {
      server_df <- server_df %>% 
        filter(
          AutoPitchType %in% c("Cutter", "Sinker", "Fastball", "TwoSeamFastBall", "Four-Seam", "FourSeamFastBall") &
            TaggedPitchType %in% c("Cutter", "Sinker", "Fastball", "TwoSeamFastBall", "Four-Seam", "FourSeamFastBall"))
    }
    else if (input$pitch_type_choice == "Offspeed") {
      server_df <- server_df %>% 
        filter(
          AutoPitchType %in% c("ChangeUp", "Changeup", "Splitter") &
            TaggedPitchType %in% c("ChangeUp", "Changeup", "Splitter"))
    }
    else if (input$pitch_type_choice == "Breaking") {
      server_df <- server_df %>% 
        filter(
          AutoPitchType %in% c("Curveball", "Slider") &
            TaggedPitchType %in% c("Curveball", "Slider"))
    }
    else if (input$pitch_type_choice == "Changeup") {
      server_df <- server_df %>% 
        filter(
          AutoPitchType %in% c("Changeup", "ChangeUp") &
            TaggedPitchType %in% c("Changeup", "ChangeUp"))
    }
    else if (input$pitch_type_choice == "Curveball") {
      server_df <- server_df %>% 
        filter(AutoPitchType %in% c("Curveball") & TaggedPitchType %in% c("Curveball"))
    }
    else if (input$pitch_type_choice == "Cutter") {
      server_df <- server_df %>% 
        filter(AutoPitchType %in% c("Cutter") & TaggedPitchType %in% c("Cutter"))
    }
    else if (input$pitch_type_choice == "Fastball") {
      server_df <- server_df %>% 
        filter(AutoPitchType %in% c("Fastball") & TaggedPitchType %in% c("Fastball"))
    }
    else if (input$pitch_type_choice == "TwoSeamFastBall") {
      server_df <- server_df %>% 
        filter(AutoPitchType %in% c("TwoSeamFastBall") & TaggedPitchType %in% c("TwoSeamFastBall"))
    }
    else if (input$pitch_type_choice == "Four-Seam Fastball") {
      server_df <- server_df %>% 
        filter(AutoPitchType %in% c("Four-Seam", "FourSeamFastBall") &
                 TaggedPitchType %in% c("Four-Seam", "FourSeamFastBall"))
    }
    else if (input$pitch_type_choice == "Sinker") {
      server_df <- server_df %>% 
        filter(AutoPitchType %in% c("Sinker") & TaggedPitchType %in% c("Sinker"))
    }
    else if (input$pitch_type_choice == "Slider") {
      server_df <- server_df %>% 
        filter(AutoPitchType %in% c("Slider") & TaggedPitchType %in% c("Slider"))
    }
    else if (input$pitch_type_choice == "Splitter") {
      server_df <- server_df %>% 
        filter(AutoPitchType %in% c("Splitter") & TaggedPitchType %in% c("Splitter"))
    }
    
    
    # Result Type ----
    
    if (input$result_choice == "Hits") {
      server_df <- server_df %>% 
        filter(
          PlayResult %in% c("Single", "Double", "Triple", "HomeRun"))
    }
    else if (input$result_choice == "Outs") {
      server_df <- server_df %>% 
        filter(OutsOnPlay >= 1)
    }
    else if (input$result_choice == "BIP") {
      server_df <- server_df %>% 
        filter(ExitSpeed > 0)
    }
    else if (input$result_choice == "Singles") {
      server_df <- server_df %>% 
        filter(PlayResult == "Single")
    }
    else if (input$result_choice == "Doubles") {
      server_df <- server_df %>% 
        filter(PlayResult == "Double")
    }
    else if (input$result_choice == "Triples") {
      server_df <- server_df %>% 
        filter(PlayResult == "Triple")
    }
    else if (input$result_choice == "Home Runs") {
      server_df <- server_df %>% 
        filter(PlayResult == "HomeRun")
    }
    # Contact Type 
    if (input$contact_choice == "Groundballs") {
      server_df <- server_df %>% 
        filter(AutoHitType == "GroundBall" & !is.na(PlayResult))
    }
    else if (input$contact_choice == "Pop Ups") {
      server_df <- server_df %>% 
        filter(AutoHitType == "Popup" & !is.na(PlayResult))
    }
    else if (input$contact_choice == "Line Drives") {
      server_df <- server_df %>% 
        filter(AutoHitType == "LineDrive" & !is.na(PlayResult))
    }
    else if (input$contact_choice == "Fly Balls") {
      server_df <- server_df %>% 
        filter(AutoHitType == "FlyBall" & !is.na(PlayResult))
    }
    else if (input$contact_choice == "Barrel") {
      server_df <- server_df %>% 
        filter(contact_type == "Barrel" & !is.na(PlayResult))
    }
    else if (input$contact_choice == "Solid") {
      server_df <- server_df %>% 
        filter(contact_type == "Solid" & !is.na(PlayResult))
    }
    else if (input$contact_choice == "Flare/Burner") {
      server_df <- server_df %>% 
        filter(contact_type == "Flare/Burner" & !is.na(PlayResult))
    }
    else if (input$contact_choice == "Under") {
      server_df <- server_df %>% 
        filter(contact_type == "Under" & !is.na(PlayResult))
    }
    else if (input$contact_choice == "Topped") {
      server_df <- server_df %>% 
        filter(contact_type == "Topped" & !is.na(PlayResult))
    }
    else if (input$contact_choice == "Weak") {
      server_df <- server_df %>% 
        filter(contact_type == "Weak" & !is.na(PlayResult))
    }
    
    
    # Zone Selection ----
    if (input$pitch_zone_choice == "Zone 1") {
      server_df <- server_df %>% filter(pitch_zone == "KZONE_01")
    }
    else if (input$pitch_zone_choice == "Zone 2") {
      server_df <- server_df %>% filter(pitch_zone == "KZONE_02")
    }
    else if (input$pitch_zone_choice == "Zone 3") {
      server_df <- server_df %>% filter(pitch_zone == "KZONE_03")
    }
    else if (input$pitch_zone_choice == "Zone 4") {
      server_df <- server_df %>% filter(pitch_zone == "KZONE_04")
    }
    else if (input$pitch_zone_choice == "Zone 5") {
      server_df <- server_df %>% filter(pitch_zone == "KZONE_05")
    }
    else if (input$pitch_zone_choice == "Zone 6") {
      server_df <- server_df %>% filter(pitch_zone == "KZONE_06")
    }
    else if (input$pitch_zone_choice == "Zone 7") {
      server_df <- server_df %>% filter(pitch_zone == "KZONE_07")
    }
    else if (input$pitch_zone_choice == "Zone 8") {
      server_df <- server_df %>% filter(pitch_zone == "KZONE_08")
    }
    else if (input$pitch_zone_choice == "Zone 9") {
      server_df <- server_df %>% filter(pitch_zone == "KZONE_09")
    }
    else if (input$pitch_zone_choice == "Zone 11") {
      server_df <- server_df %>% filter(pitch_zone == "KZONE_11")
    }
    else if (input$pitch_zone_choice == "Zone 12") {
      server_df <- server_df %>% filter(pitch_zone == "KZONE_12")
    }
    else if (input$pitch_zone_choice == "Zone 13") {
      server_df <- server_df %>% filter(pitch_zone == "KZONE_13")
    }
    else if (input$pitch_zone_choice == "Zone 14") {
      server_df <- server_df %>% filter(pitch_zone == "KZONE_14")
    }
    else if (input$pitch_zone_choice == "In Zone") {
      server_df <- server_df %>% filter(in_out_zone == "In Zone")
    }
    else if (input$pitch_zone_choice == "Out Zone") {
      server_df <- server_df %>% filter(in_out_zone == "Out Zone")
    }
    else if (input$pitch_zone_choice == "Heart") {
      server_df <- server_df %>% filter(tango_zone == "Heart")
    }
    else if (input$pitch_zone_choice == "Shadow") {
      server_df <- server_df %>% filter(tango_zone == "Shadow")
    }
    else if (input$pitch_zone_choice == "Chase") {
      server_df <- server_df %>% filter(tango_zone == "Chase")
    }
    else if (input$pitch_zone_choice == "Waste") {
      server_df <- server_df %>% filter(tango_zone == "Waste")
    }
    
    # Pitch Tag ----
    if (input$pitch_tag == "TaggedPitchType") {
      server_df$pitch_tag <- server_df$TaggedPitchType
    }
    else if (input$pitch_tag == "AutoPitchType") {
      server_df$pitch_tag <- server_df$AutoPitchType 
    }
    
    server_df <- server_df %>% 
      mutate(
        pitch_tag = fct_infreq(pitch_tag)
      )
    
    return(server_df)
    
  })
  # Plots ----
  output$pitch_loc <- renderPlot({
    load(file = "data/app_setup.rda")
    load(file = "data/Strikezone_Polys.rda")
    load(file = "data/aesthetics.rda")
    
    ggplot() +
      geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
      geom_polygon(sz, mapping = aes(x, z), lty = 1, fill = "#ededed", color = "lightgrey") +
      geom_point(data_1() %>% filter(!is.na(AutoPitchType)), 
                 mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = pitch_tag),
                 size = 5, shape = 21) +
      scale_fill_manual(values = pitch_colors) +
      geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
      coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
      # coord_equal() +
      theme_minimal() +
      labs(fill = "") +
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom"
      )
    
    
  })
  
  output$spray <- renderPlot({
    load("data/data_setup.rda")
    load("data/aesthetics.rda")
    load("data/app_setup.rda")
    load("data/Strikezone_Polys.rda")
    load("data/zone_poly_setup.rda")
    
    # x <- c(0, 0)
    # z <- c(0, 400)
    # 
    # cf <- data_frame(x, z)
    # 
    geom_baseball("NCAA", xlims = c(-250,250), ylims = c(0,450), 
                  color_updates = list(plot_background = "#b2dfdb", infield_dirt = "#eeeeee",
                                       infield_grass = "#b2dfdb", pitchers_mound = "#eeeeee", 
                                       base = "#000000", pitchers_plate = "#000000",
                                       batters_box = "#000000", catchers_box = "#000000",
                                       foul_line = "#000000", running_lane = "#000000")) +
      geom_point(data_1() %>% filter(!is.na(PlayResult), !PlayResult %in% c("Walk", "Strikeout")),
                 mapping = aes(hit_x, hit_y, fill = PlayResult), shape = 21, size = 5) +
      # geom_path(cf, mapping = aes(x, z)) +
      scale_fill_manual(values = result_colors) +
      geom_arc(aes(x0 = 0, y0 = 118, r = 259, start = 5.15 - 2*pi, end = 1.15),
               color="#000000", linewidth =.125, alpha = .5) +
      labs(fill = "") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom"
      )
    
    
  })
  
  output$radial <- renderPlot({
    load("data/data_setup.rda")
    load("data/aesthetics.rda")
    load("data/app_setup.rda")
    load("data/Strikezone_Polys.rda")
    load("data/zone_poly_setup.rda")
    
    ggplot() +
      geom_point(data_1() %>% filter(!is.na(contact_type)),
                 mapping = aes(ExitSpeed, Angle, fill = contact_type), shape = 21, size = 5) +
      labs(fill = "",
           x = "MPH",
           y = "Angle") +
      theme_minimal() +
      scale_fill_manual(values = contact_colors) +
      coord_cartesian(xlim = c(0, 125), ylim = c(-90, 90)) +
      theme(
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.margin = margin(0, 1, 1, 0)
      )
    
    
  })
  
  output$overview <- render_gt({
    
    gt_df <- data_1() %>% 
      mutate(
        PA = case_when(
          !is.na(PlayResult) ~ 1
        ),
        AB = case_when(
          !PlayResult %in% c("Sacrifice", "Walk") & PA == 1 ~ 1
        ),
        H = case_when(
          PlayResult %in% c("Single", "Double", "Triple", "HomeRun") ~ 1
        ),
        x1B = case_when(
          PlayResult %in% c("Single") ~ 1
        ),
        x2B = case_when(
          PlayResult %in% c("Double") ~ 1
        ),
        x3B = case_when(
          PlayResult %in% c("Triple") ~ 1
        ),
        xHR = case_when(
          PlayResult %in% c("HomeRun") ~ 1
        ),
        SO = case_when(
          PlayResult == "Strikeout" ~ 1
        ),
        Swings = case_when(
          PitchCall %in% c("StrikeSwinging", "InPlay", "FoulBall") ~ 1
        ),
        Misses = case_when(
          PitchCall %in% c("StrikeSwinging") ~ 1
        )
      )
    
    gt_df %>% 
      summarise(
        total_pitches = mean(total_pitches),
        pitch_count = n(),
        PA = sum(PA, na.rm = T),
        AB = sum(AB, na.rm = T),
        H = sum(H, na.rm = T),
        x1B = sum(x1B, na.rm = T),
        x2B = sum(x2B, na.rm = T),
        x3B = sum(x3B, na.rm = T),
        xHR = sum(xHR, na.rm = T),
        SO = sum(SO, na.rm = T),
        Swings = sum(Swings, na.rm = T),
        Misses = sum(Misses, na.rm = T),
        EV = mean(ExitSpeed, na.rm = T),
        Angle = mean(Angle, na.rm = T)
      ) %>% 
      mutate(
        BA = H / AB,
        SLG = ((x1B + 2*x2B + 3*x3B + 4*xHR)/AB),
        prop = pitch_count / total_pitches,
        Swing_prop = Swings/pitch_count,
        Whiff_prop = Misses/Swings,
        K_prop = SO/AB
      ) %>% 
      ungroup() %>% 
      gt()  %>% 
      cols_label(
        total_pitches = md("Total Pitches"),
        pitch_count = md("Pitches"),
        prop = md("%"),
        x1B = md("1B"),
        x2B = md("2B"),
        x3B = md("3B"),
        xHR = md("HR"),
        Swing_prop = md("Sw%"),
        Whiff_prop = md("Whiff%"),
        K_prop = md("K%"),
        Angle = md("Angle")
      ) %>% 
      fmt_number(columns = c("EV"), decimals = 1) %>%  
      fmt_number(columns = c("Angle"), decimals = 1) %>% 
      fmt_percent(columns = c("prop", "Swing_prop", "Whiff_prop", "K_prop"), decimals = 1) %>% 
      cols_move(columns = c(prop), after = pitch_count) %>% 
      cols_move(columns = c(EV), after = K_prop) %>% 
      cols_move(columns = c(Angle), after = EV) %>% 
      cols_move(columns = c(BA), after = xHR) %>% 
      cols_move(columns = c(SLG), after = BA) %>% 
      fmt_number(columns = c(BA, SLG), decimals = 3) %>% 
      cols_align(align = c("center"),
                 columns = everything()
      ) %>% 
      gt_color_rows(EV, domain = c(0, 125), palette = c("#2166ac",'white', "#b2182b")) %>% 
      gt_color_rows(Angle, domain = c(-30, 75),
                    palette = c("#2166ac", "#2166ac", 'white','white', "#b2182b", "#b2182b", "#b2182b",
                                'white', 'white', "#2166ac", "#2166ac")) %>% 
      gt_theme_538() %>% 
      tab_options(table.font.size = "20px")
    
  })
  
}

shinyApp(ui, server)
