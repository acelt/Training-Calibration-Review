#### Script to produce figures and summary tables from AIM Core Methods training calibration ####

# Setup
library(tidyverse)
#install.packages("arcgis", repos = c("https://r-arcgis.r-universe.dev", "https://cloud.r-project.org"))
library(arcgis)
# this links to you Arcpro account (may require some set up)
my_token <- auth_binding()

# can also use this pop-up
#my_token <- auth_code()

# Input/Output path
output_path <- "C:\\Users\\alaurencetraynor\\Downloads"

# This should be the path to the calibration summary csv
# formatting should be identical to the template on sharepoint
# Keep as NA if using the plot service
input_path <- "\\\\blm.doi.net\\dfs\\nr\\users\\alaurencetraynor\\My Documents\\Analysis\\Tools\\Training Calibration Summary\\calibration_template_LCDO2025.csv"

# Needa way to select the appropriate crews, lets use state since this is in calibration form and should be unique per year (mostly)
state <- "UT"

# Do you want to use the plot calibration service or just enter the data by hand?
  use_plot_service <- TRUE

if(use_plot_service){
  plot_service_url <- "https://services1.arcgis.com/Hp6G80Pky0om7QvQ/arcgis/rest/services/BLM_Terrestrial_AIM_2024_Calibration_Plot_Service/FeatureServer/"
  
  # these are the detail tables (no geometry)
  lpi <- arc_read(url = paste0(plot_service_url,"8"), token = my_token) |>  
    filter(LPIAdminState == state)
  
  gap <- arc_read(url = paste0(plot_service_url,"13"), token = my_token) |> 
    filter(GapAdminState == state)
  
  hgt <- arc_read(url = paste0(plot_service_url,"9"), token = my_token) |> 
    filter(HgtAdminState == state)
  
  spp <- arc_read(url = paste0(plot_service_url,"11"),token = my_token) |>  
    filter(SpecAdminState == state)
  
  # Data Tidying
  # Attempt key is the concatenation of state, crew number and attempt so should be unique per calibration attempt
  
  # create empty lists for outputs
  lpi_plot <- list()
  gap_plot <- list()
  hgt_plot <- list()
  spp_plot <- list()
  
  # Let do LPI first
 
  for(i in unique(lpi$LPICrewNumber)){
    
    # split up by crew, then display progression of attempts
    lpi_plot[[i]] <- lpi[lpi$LPICrewNumber == i,] |> 
      
      # Make Indicator names pretty
      rename("Bare Ground" = "PctBG",
             "Basal Plant" = "PctBPC",
             "Foliar" = "PctFC",
             "Litter" = "PctLC",
             "Rock" = "PctRC",
             "Standing Dead" =  "PctSDC") |> 
      pivot_longer(cols = c(`Bare Ground`:`Standing Dead`), names_to = "Indicator", values_to = "Cover (%)") |> 
      group_by(LPIAttemptKey, Indicator) |> 
      mutate(Range = max(`Cover (%)`)-min(`Cover (%)`),
             Success = ifelse(Range>10, "Fail", "Success"),# calc calibration success per crew per attempt per indicator
             CalibrationKey = paste0(LPIAttemptKey,Indicator)) |> # this is a unique key for attempt + indicator
      ungroup() |> 
      ggplot(aes(x = Indicator, y = `Cover (%)`, group = CalibrationKey, fill = Success))+ # adding a group here for calibration attempt to show progress, could remove if needed
      geom_boxplot()+
      theme_bw(base_size = 18)+
      theme(axis.text.x = element_text(angle = 30, vjust = 0.7))+ 
      scale_color_brewer(type = "seq", palette = 1)+
      scale_fill_brewer(type = "qual", palette = 7, direction = -1)+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
    
    ggsave(filename = paste0(output_path, "/", i,"_lpi_difference_plot.jpg"),
           plot = lpi_plot[[i]],
           height = 8,
           width = 12,
           device = "jpg")
  }
  
  for(i in unique(gap$GapCrewNumber)){
    # Similar deal for gap
    gap_plot[[i]] <- gap[gap$GapCrewNumber == i,] |> 
      # Make Indicator names pretty
      rename("25-50cm" = "pctCanCat1",
             "51-100cm" = "pctCanCat2",
             "101-200cm" = "pctCanCat3",
             ">200cm" = "pctCanCat4") |> # we will want to order these so they show up in increasing order in the figure
      pivot_longer(cols = c(`25-50cm`:`>200cm`), names_to = "Indicator", values_to = "Canopy Gaps (%)") |> 
      mutate(Indicator = fct_relevel(Indicator, c("25-50cm","51-100cm", "101-200cm", ">200cm"))) |> 
      group_by(GapAttemptKey, Indicator) |> 
      mutate(Range = max(`Canopy Gaps (%)`)-min(`Canopy Gaps (%)`),
             Success = ifelse(Range>10, "Fail", "Success"),# calc calibration success per crew per attempt per indicator
             CalibrationKey = paste0(GapAttemptKey,Indicator)) |> # this is a unique key for attempt + indicator
      ungroup() |> 
      ggplot(aes(x = Indicator, y = `Canopy Gaps (%)`, group = CalibrationKey, fill = Success))+ # adding a group here for calibration attempt to show progress, could remove if needed
      geom_boxplot()+
      theme_bw(base_size = 18)+
      theme(axis.text.x = element_text(angle = 30, vjust = 0.7))+ 
      scale_color_brewer(type = "seq", palette = 1)+
      scale_fill_brewer(type = "qual", palette = 7, direction = -1)+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
    
    ggsave(filename = paste0(output_path, "/", i,"_gap_difference_plot.jpg"),
           plot = gap_plot[[i]],
           height = 8,
           width = 12,
           device = "jpg")

  }
  
} else{
  
  # just point to csv file
  data <- read.csv(input_path)
  
  # split out the methods based on column names to make for easy plotting
  lpi <- data[,c("crew","name","year","pct_foliar","pct_bare","pct_dead","pct_litter","pct_rock")]
  
  gap <- data[,c("crew","name","year","gaps_25_50_pct","gaps_51_100_pct","gaps_101_200_pct","gaps_201_pct")]
  
  hgt <- data[,c("crew","name","year","woody_1_50_ct","woody_51_100_ct","woody_101_200_ct","woody_201_ct","herb_1_10_ct","herb_11_30_ct","herb_31_50_ct","herb_50_ct")]
  
  spp <- data[,c("crew","name","year","spp_ct", "spp_time")]
  
  rm(data)
}

## Figures


## Tables
