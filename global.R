setwd("D:/PFWRA/R/practice_apps")


library(stringdist)
library(leaflet)
library(RColorBrewer)
library(sf)
library(dplyr)
library(lubridate)
library(stringr)
library(readxl)
library(plotly)
library(readr)
library(tidyr)
library(forcats)
library(htmlwidgets)
library(jsonlite)
library(htmltools)

source("Date_parser.R") 

#improve: 
#-write a parser for all data which ensures everything is as expected, like Date parser. 


#Load CIT data
CIT_trap <- read.csv("data/CIT.csv", stringsAsFactors = FALSE)

#Load lastYearCIT.csv which is updated daily
Year_CIT_trap <- read.csv("data/LastYearCIT.csv", stringsAsFactors = FALSE )

#Parse date col, setting uniform format
CIT_trap$Date <- parse_if_needed(CIT_trap$Date)
Year_CIT_trap$Date <- parse_if_needed(Year_CIT_trap$Date)

#Check for duplicates
duplicates <- CIT_trap[duplicated(CIT_trap), ]
nrow(duplicates)  # Total number of full-row duplicates


#check for duplicates in year data
duplicatesYear <- Year_CIT_trap[duplicated(Year_CIT_trap), ]
nrow(duplicatesYear)  # Total number of full-row duplicates

#need to load duplicates .csv after i first write it. 
saved_duplicates <- read.csv("data/duplicateRows.csv", stringsAsFactors = FALSE )
saved_duplicates$Date <- parse_if_needed(saved_duplicates$Date)

#merge duplicates
duplicateRows <- bind_rows(duplicatesYear, duplicates )

#check if duplicates already recorded in saved duplicates
new_rows <- anti_join(duplicateRows, saved_duplicates)

#if new duplicates to add then add them hear and write to file
if (nrow(new_rows) > 0) {
  duplicateRows <- bind_rows(new_rows, saved_duplicates)
  write.csv(data/duplicateRows, "duplicateRows.csv", row.names = FALSE)
  message(nrow(duplicateRows), " new duplicates saved.")
} else {
  message("No new duplicates to add.")
}

#remove duplicates from data
Year_CIT_trap <- Year_CIT_trap[!duplicated(Year_CIT_trap), ]
CIT_trap <- CIT_trap[!duplicated(CIT_trap), ]

#Check duplicates removed
duplicates <- CIT_trap[duplicated(CIT_trap), ]
nrow(duplicates)  # Total number of full-row duplicates
duplicatesYear <- Year_CIT_trap[duplicated(Year_CIT_trap), ]
nrow(duplicatesYear)  # Total number of full-row duplicates

#clean up environment
rm(duplicatesYear, new_rows, duplicateRows, saved_duplicates)

#check if new data is already in CIT trap database
new_rows <- anti_join(Year_CIT_trap, CIT_trap)

#if new rows to add, then add them and write to file
if (nrow(new_rows) > 0) {
  CIT_trap <- bind_rows(CIT_trap, new_rows)
  write.csv(data/CIT_trap, "CIT.csv", row.names = FALSE)
  message(nrow(new_rows), " new rows added.")
} else {
  message("No new rows to add.")
}

#clean environment
rm( new_rows)

#check you haven't made any duplicates
duplicates <- CIT_trap[duplicated(CIT_trap), ]
nrow(duplicates)  # Total number of full-row duplicates

#clean environment
rm(duplicates)

# Convert data table to an sf object using longitude and latitude columns as geometry.
CIT_trap <- st_as_sf(CIT_trap, coords = c("Longitude", "Latitude"))

#Set Coordinate reference System to WGS84 (EPSG:4326), as the coordinates are in degrees (longitude/latitude).
CIT_trap <- st_set_crs(CIT_trap, 4326)  # 

#then transform coordinates to NZGD2000 New Zealand Transverse Mercator (EPSG:2193) for analysis in meters
CIT_trap <- st_transform(CIT_trap, crs =2193 )


#--------------------------------------------------------------------------------
# #Load trapNZ data

# TNZ_trap <- st_read("data/my-projects-trap-records.shp")
# 
# #parse date col to get uniform date format
# TNZ_trap$record_dat <- parse_if_needed(TNZ_trap$record_dat)
# 
# #transform
# TNZ_trap <- st_transform(TNZ_trap, crs =2193 )
#------------------------------------------------------------------------------------
#Load area shape files. #change this to change the mapped area
areas<- st_read("data/areas.shp")

#ensure using the same crs
#st_crs(areas)
areas <- st_transform(areas, crs =2193 )

#---------------------------------------------------------------
#CIT transformations

#Get a col with just the year
CIT_trap<- mutate(CIT_trap, year = year(Date))

# Add a season column
CIT_trap$Season <- ifelse(
  format(CIT_trap$date, "%m") %in% c("03", "04", "05"), "Autumn",
  ifelse(format(CIT_trap$date, "%m") %in% c("06", "07", "08"), "Winter",
         ifelse(format(CIT_trap$date, "%m") %in% c("09", "10", "11"), "Spring",
                "Summer"
         )))

# Add a months column
CIT_trap$Month <- format(CIT_trap$date, "%B")

#Order months by the calendar
CIT_trap$Month <- factor(CIT_trap$Month, levels = month.name)

#Filter to catches by selecting all char values not c('NA', 'None', 'Unspecified', "")
CIT_trap %>%filter(!(Species %in% c('NA', 'None', 'Unspecified', ""))) -> CIT_catches24

#an additional filter, probably can remove
CIT_catches24 %>% 
  filter(!is.na(Species)) -> CIT_catches24

#reduce data for whole set
#reduce col data from catches subset
CIT_reduced_whole <- CIT_trap %>% 
  select(Area, Species, TrapType, TimeTaken, Bait, Line, Date, Month,TrapName, Season)

#reduce col data from catches subset
CIT_catches_reduced <- CIT_catches24 %>% 
  select(Area, Species, TrapType, TimeTaken, Bait,Line, Date,Month, TrapName, Season)

#rename data to merge sets for all data
CIT_reduced_whole <- CIT_reduced_whole %>%
  rename(Trap_type=TrapType, Time_taken= TimeTaken,trap_id=TrapName )

#rename data to merge sets for catch subset
CIT_catches_reduced <- CIT_catches_reduced %>%
  rename(Trap_type=TrapType, Time_taken= TimeTaken, trap_id=TrapName)

#--------------------------------------------------------------------
# #TNZ transformations
# 
# #add a column with just the year
# TNZ_trap<- mutate(TNZ_trap, year = year(record_dat))
# 
# # Add a season column
# TNZ_trap$Season <- ifelse(
#   format(TNZ_trap$record_dat, "%m") %in% c("03", "04", "05"), "Autumn",
#   ifelse(format(TNZ_trap$record_dat, "%m") %in% c("06", "07", "08"), "Winter",
#          ifelse(format(TNZ_trap$record_dat, "%m") %in% c("09", "10", "11"), "Spring",
#                 "Summer")
#   )
# )
# 
# 
# # Add a months column
# TNZ_trap$Month <- format(TNZ_trap$record_dat, "%B")
# 
# #Order months by the calendar
# TNZ_trap$Month <- factor(TNZ_trap$Month, levels = month.name)
# 
# #Filter to catches by selecting all char values not c('NA', 'None', 'Unspecified', "")
# TNZ_trap %>%
#   filter(!(species_ca %in% c('NA', 'None', 'Unspecified'))) -> TNZ_catches2020
# 
# #reduce TNZ_trap data to selected cols
# TNZ_reduced_whole <-TNZ_trap %>% 
#   select(project, species_ca,trap_type, bait_at_de, line, record_dat, Month,trap_id, Season)
# 
# #reduce TNZ_catches2020 data to selected cols
# TNZ_catches_reduced <-TNZ_catches2020 %>% 
#   select(project, species_ca,trap_type, bait_at_de, line, record_dat, Month,trap_id, Season)
# 
# # Rename data columns to merge with CIT_reduced_whole
# TNZ_reduced_whole <- TNZ_reduced_whole %>%
#   rename(Species = species_ca, Area= project,Trap_type = trap_type, Bait=bait_at_de, Line=line, Date=record_dat)
# 
# # Rename data columns to merge with CIT_catches_reduced
# TNZ_catches_reduced <- TNZ_catches_reduced %>%
#   rename(Species = species_ca, Area= project,Trap_type = trap_type, Bait=bait_at_de, Line=line, Date=record_dat)
# 
# 
# TNZ_reduced_whole$trap_id <- as.character(TNZ_reduced_whole$trap_id)
# 
# TNZ_catches_reduced$trap_id <- as.character(TNZ_catches_reduced$trap_id)
#--------------------------------------------------------------------------------
#Bind rows
#Change to commented out code if using trapNZ data

# #bind whole set
# All_traps <- bind_rows(TNZ_reduced_whole, CIT_reduced_whole)
All_traps<-CIT_reduced_whole

# 
# #bind catches subsets
# All_catches <- bind_rows(TNZ_catches_reduced, CIT_catches_reduced)
All_catches<-CIT_catches_reduced
#--------------------------------------------------------------------------------
#filter to date frames:
#since 2020, since 2021, since 2022, since 2023, since 2024,since begginning of 2025, last 90 days, last 30 days, since beggining of the month.

#Filter to data frames for both catches and all traps

#get todays date
#today <- Sys.Date()
#print(today)


# Define date filters (rolling and fixed starts)
date_filters <- list(
  since_2020     = as.Date("2020-01-01"),
  since_2021     = as.Date("2021-01-01"),
  since_2022     = as.Date("2022-01-01"),
  since_2023     = as.Date("2023-01-01"),
  since_2024     = as.Date("2024-01-01"),
  since_2025     = as.Date("2025-01-01"),
  last_90_days   = Sys.Date() - 90,
  last_30_days   = Sys.Date() - 30,
  this_month     = as.Date(format(Sys.Date(), "%Y-%m-01"))
)

# Initialize totals
catch_totals   <- list()
record_totals  <- list()
trap_totals    <- list()

All_traps_df <- All_traps %>% st_drop_geometry()
All_catches_df <- All_catches #%>% st_drop_geometry()


# Loop
for (name in names(date_filters)) {
  date_val <- date_filters[[name]]
  
  # Filter
  catches_df <- All_catches_df %>% filter(Date >= date_val)
  traps_df   <- All_traps_df   %>% filter(Date >= date_val)
  
  # Assign filtered catches if needed
  assign(paste0("catches_", name), catches_df)
  
  # Count totals
  catch_totals[[name]]  <- nrow(catches_df)
  record_totals[[name]] <- nrow(traps_df)
   trap_totals[[name]]   <- traps_df %>% distinct(trap_id) %>% nrow()
}

# Assemble summary
summary_df <- tibble(
  filter_name     = names(date_filters),
  total_catches   = unlist(catch_totals),
  total_records   = unlist(record_totals),
  total_traps     = unlist(trap_totals)
)

print(summary_df)

#------------------------------------------------------------------------------------------------
#in year calcs

# Define years you want to include
years <- 2020:2020 
#years <- 2020:2025

# Create a new list for in_year filters
in_year_filters <- list()

for (y in years) {
  in_year_filters[[paste0("in_", y)]] <- list(
    start = as.Date(paste0(y, "-01-01")),
    end = as.Date(paste0(y, "-12-31"))
  )
}

# Initialize output lists
in_year_catch_totals  <- list()
in_year_record_totals <- list()
in_year_trap_totals   <- list()

# Loop through in_year filters
for (name in names(in_year_filters)) {
  range <- in_year_filters[[name]]
  start_date <- range$start
  end_date <- range$end
  
  catches_df <- All_catches_df %>% filter(Date >= start_date, Date <= end_date)
  traps_df   <- All_traps_df   %>% filter(Date >= start_date, Date <= end_date)
  
  in_year_catch_totals[[name]]  <- nrow(catches_df)
  in_year_record_totals[[name]] <- nrow(traps_df)
  in_year_trap_totals[[name]]   <- traps_df %>% distinct(trap_id) %>% nrow()
}

# Assemble the summary table
in_year_summary_df <- tibble(
  filter_name   = names(in_year_filters),
  total_catches = unlist(in_year_catch_totals),
  total_records = unlist(in_year_record_totals),
  total_traps   = unlist(in_year_trap_totals)
)

print(in_year_summary_df)



#-----------------------------------------------------------------------------------------------------
# Years to process
#years <- 2020:2025

# Define rat alias grouping
rat_aliases <- c('Rat - Ship', 'Rat - Norway', 'Ship Rat', 'Norway Rat', 'Rat')
mustelids <- c('Stoat', 'Ferret', 'Weasel')

# Loop through each year
for (y in years) {
  
  df_name <- paste0("catches_since_", y)
  df <- get(df_name)
  
  # Clean species names
  df <- df %>%
    mutate(Species = case_when(
      Species %in% rat_aliases ~ 'Rat',
      TRUE ~ Species
    ))
  
  # Update the cleaned data frame in environment
  assign(df_name, df)
  
  # Filter and count species
  rats <- df %>% filter(Species == 'Rat')
  possums <- df %>% filter(Species == 'Possum')
  musts <- df %>% filter(Species %in% mustelids)
  
  # Save as summary variables
  assign(paste0("rats_", y, "_VAR"), nrow(rats))
  assign(paste0("possums_", y, "_VAR"), nrow(possums))
  assign(paste0("mustelids_", y, "_VAR"), nrow(musts))
}



#----------------------------------------------------------------------------------
#Prepare for mapping my filtering to target species

# #need to filter to these for Map
# reduced_catches_since_2020 <- catches_since_2020 %>%
#   filter(
#     (Species =='Rat' |
#        Species == 'Possum' |
#        Species == 'Stoat' |
#        Species == 'Ferret' |
#        Species == 'Weasel'|
#        Species == 'Rabbit'|
#        Species == 'Hedgehog'|
#        Species == 'Mouse'
#     )
#   )
# 
# 
# #order highest to lowest
# # Step 1: Compute species counts and sort
# species_counts <- reduced_catches_since_2020 %>%
#   count(Species) %>%  # Count occurrences of each species
#   arrange(desc(n))  # Sort by highest count
# 
# 
# # Step 2: Convert `Species` to a factor, ordered by count
# reduced_catches_since_2020 <- reduced_catches_since_2020 %>%
#   mutate(Species = factor(Species, levels = species_counts$Species))
# 
# catches_count <- reduced_catches_since_2020 %>%
#   group_by(geometry, Species) %>%  # Group by location & species
#   summarise(Count = n(), .groups = "drop")  # Count occurrences


# Define years and species to include on maps
#years <- 2020:2025 

map_species <- c('Rat', 'Possum', 'Stoat', 'Ferret', 'Weasel', 'Rabbit', 'Hedgehog', 'Mouse')

for (y in years) {
  
  # Step 1: Get the original sf object
  df_name <- paste0("catches_since_", y)
  df <- get(df_name)
  
  # Step 2: Filter to key species
  reduced_df <- df %>%
    filter(Species %in% map_species)
  
  # Step 3: Compute species counts and relevel Species as a factor
  species_counts <- reduced_df %>%
    count(Species) %>%
    arrange(desc(n))
  
  reduced_df <- reduced_df %>%
    mutate(Species = factor(Species, levels = species_counts$Species))
  
  # Step 4: Create grouped count by geometry
  grouped_counts <- reduced_df %>%
    group_by(geometry, Species) %>%
    summarise(Count = n(), .groups = "drop")
  
  # Save the outputs with appropriate names
  assign(paste0("reduced_catches_since_", y), reduced_df)
  assign(paste0("species_counts_", y), species_counts)
  assign(paste0("catches_count_", y), grouped_counts)
}


#-----------------------------------------------------------------
#Mapping 'reduced_catches_since_2020'

#for running all years:

# for (y in 2020:2025) {
#   message("Processing year: ", y)
#   
#   # Step 1: Get data
#   catches_count <- get(paste0("catches_count_", y))

  #removed this here as a work around to remove warnings re crs type for funtion 
# catches_count <- st_transform(catches_count, crs = 4326)

#for running one year
catches_count<-catches_count_2020

# Extract Longitude and Latitude
catches_count <- catches_count %>%
  mutate(Longitude = st_coordinates(.)[,1],  # Extract Longitude (X)
         Latitude = st_coordinates(.)[,2])   # Extract Latitude (Y)

# Create a color palette based on species
pal <- colorFactor(palette = "viridis", domain = catches_count$Species)


# High-contrast color palette
pal <- colorFactor(palette = "Dark2", domain = catches_count$Species)

# Example: Color based on a numeric or categorical variable (replace "YourAttribute" with an actual column)
pal_areas <- colorFactor(palette = "Set1", domain = areas$CustomerNa)

#change this to change the hex sizes
hex_size <- (st_bbox(areas)[4] - st_bbox(areas)[2]) / 40  # 100 hexagons fit in latitude range

sf_use_s2(FALSE)
areas <- st_make_valid(areas)

# Create a hexagonal grid over the study area
hex_grid <- st_make_grid(areas, cellsize = hex_size, what = "polygons", square = FALSE)

# Convert to sf object and give each hex an ID
hex_grid <- st_sf(hex_id = 1:length(hex_grid), geometry = hex_grid)

#  Clip hexagons to fit inside study area
hex_grid <- st_intersection(hex_grid, areas)

#  Ensure hex grid has same CRS as catch points
hex_grid <- st_transform(hex_grid, crs = st_crs(catches_count))

#  Assign each catch point to the nearest hexagon
nearest_hex <- st_nearest_feature(catches_count, hex_grid)
catches_count$hex_id <- hex_grid$hex_id[nearest_hex]

# Optional check: total count before aggregation
sum(catches_count$Count)

# Aggregate counts by hex + species
hex_counts <- catches_count %>%
  group_by(hex_id, Species) %>%
  summarise(Species_Count = sum(Count, na.rm = TRUE), .groups = "drop") %>%
  st_drop_geometry()

# Check: does the sum look right?
sum(hex_counts$Species_Count)

# Diagnostic: check for duplicate hex_id rows (likely caused by geometry intersections)
hex_grid %>%
  count(hex_id) %>%
  filter(n > 1) -> check

# 🛠️ Fix: Deduplicate hex_grid so that each hex_id is unique (important before joining)
hex_grid_union <- hex_grid %>%
  group_by(hex_id) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# Reproject to projected CRS for safe union
# hex_grid_union <- hex_grid %>%
#   st_transform(2193) %>%
#   group_by(hex_id) %>%
#   summarise(geometry = st_union(geometry), .groups = "drop") %>%
#   st_transform(4326)  # Return to WGS84 for leaflet

#  Pivot counts wider to make one row per hex
hex_counts_wide <- hex_counts %>%
  pivot_wider(names_from = Species, values_from = Species_Count, values_fill = 0)

#  Join counts back to hex grid (now with one row per hex_id)
hex_grid_final <- hex_grid_union %>%
  left_join(hex_counts_wide, by = "hex_id")


#  Calculate total species per hex (cleaner column selection)
species_cols <- intersect(
  c("Rat", "Mouse", "Possum", "Weasel", "Stoat", "Ferret", "Hedgehog", "Rabbit"),
  names(hex_grid_final)
)

hex_grid_final <- hex_grid_final %>%
  rowwise() %>%
  mutate(Total_Species = sum(c_across(all_of(species_cols)), na.rm = TRUE)) %>%
  ungroup()

# check species total hasn't changed
sum(hex_grid_final$Total_Species)  # This should now match `sum(hex_counts$Species_Count)`


# Ensure Total_Species is calculated
hex_grid_final <- hex_grid_final %>%
  mutate(Total_Species = ifelse(is.na(Total_Species), 0, Total_Species))

# Define your grading template
grade_template <- c(0, 1, 10, 30, 50)

# Compute per-year max
max_val <- max(hex_grid_final$Total_Species, na.rm = TRUE)
if (max_val == 0) max_val <- 1  # safeguard

# Extend the grading template to fit max, without collapsing bins
adjusted_max <- if (max_val <= max(grade_template)) max(grade_template) + 1 else max_val + 1

# Final bins, ensuring monotonicity
bins <- unique(c(grade_template[grade_template < adjusted_max], adjusted_max))

# Build palette
pal_hex <- colorBin(
  palette = c("#A9A9A9", "yellow", "orange", "red", "darkred")[1:(length(bins) - 1)],
  bins = bins,
  domain = hex_grid_final$Total_Species
)


hex_grid_final <- st_transform(hex_grid_final, 4326)


# Leaflet Map
hexmap<-leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  # Add hexagons
  addPolygons(data = hex_grid_final,
              fillColor = ~pal_hex(Total_Species),
              color = "black", weight = 0.5, opacity = 0.8,
              fillOpacity = ~ifelse(Total_Species == 0, 0.4, 0.7),
              popup = ~paste0(
                "<b>Species Counts:</b><br>",
                "Rat: ", Rat, "<br>",
                "Mouse: ", Mouse, "<br>",
                "Hedgehog: ", Hedgehog, "<br>",
                "Possum: ", Possum, "<br>",
                "Weasel: ", Weasel, "<br>",
                "Stoat: ", Stoat, "<br>",
                "Ferret: ", Ferret, "<br>",
                "Rabbit: ", Rabbit, "<br>",
                "<b>Total Selected Species:</b> ", Total_Species)) %>%

  # Add legend
  addLegend("bottomright", pal = pal_hex, values = hex_grid_final$Total_Species,
            title = "Catches per Hex", opacity = 1)

sum(hex_grid_final$Total_Species)

# #Save each map
#   assign(paste0("hexmap_", y), hexmap)
#   message("Map created: hexmap_", y)
# 
# }

 saveWidget(hexmap, "data/hexmap.html", selfcontained = TRUE)

# st_write(hex_grid_final, "data/hex_grid.shp", append = TRUE)

#hexmap

# hexmap_2020
# hexmap_2021
# hexmap_2022
# hexmap_2023
# hexmap_2024
# hexmap_2025

# All_trap_records<-length(All_traps[[1]]) #need to change this from All_traps
# All_catches<-length(catches_since_2020[[1]])

NumberTraps <- catches_since_2020 %>% 
  distinct(trap_id, .keep_all = TRUE)


summary_stats <- list(
  All_trap_records = length(All_traps[[1]]),
  All_catches = length(catches_since_2020[[1]]),
  trapLength <-length(NumberTraps[[1]])
)

All_trap_records = length(All_traps[[1]])
All_catches = length(catches_since_2020[[1]])
trapLength <-length(NumberTraps[[1]])

jsonlite::write_json(summary_stats, "data/summary_stats.json", auto_unbox = TRUE)


count_data <- reduced_catches_since_2020 %>%
  count(Species)

p <- ggplot(count_data, aes(x = Species, y = n, fill = Species)) +
  geom_col(color = "darkblue") +
  geom_text(aes(label = n), vjust = -0.2, fontface = "bold", size = 4) +
  scale_y_log10() +
  xlab("") + ylab("") +
  ggtitle("") +
  theme_grey() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 11),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

print(p)


# # Save the plot as a PNG first
# ggsave("data/bar_chart.png", p, width = 8, height = 5, dpi = 300, bg = "transparent")
# 
# # Create simple HTML
# htmltools::save_html(
#   tags$html(
#     tags$body(
#       tags$img(src = "bar_chart.png", style = "width:100%; max-width:800px;")
#     )
#   ),
#   file = "data/bar_chart.html"
# )

#saveWidget(p_plotly, "data/bar_chart_2024.html", selfcontained = TRUE)

#jsonlite::write_json(totals, "data/totals.json", auto_unbox = TRUE)
