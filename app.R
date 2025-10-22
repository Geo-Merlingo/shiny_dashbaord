setwd("D:/PFWRA/R/practice_apps")

source("global.R") 

library(plotly)
library(ggplot2)
library(bslib)
library(bsicons)
library(shiny)
library(leaflet)
library(rsconnect)

library(htmlwidgets)

#loop though plot below for years
#next add a slider for date since fields in global
#creating multiple maps and plots/ totals for different date extents. :)


ui <- page_fillable(
  layout_columns(
    col_widths = c(9, 3),
    
    navset_card_tab(
      height = 450,
      full_screen = TRUE,
      title = tags$h2("Catches Since 2020"),
      
      nav_panel(
        "Map",
        card_title("Map of Aggregated Catches"),
       hexmap
      ),
      
      nav_panel(
        "Bar Graph",
        card_title("Species caught"),
        plotOutput("bar_plot")  # a placeholder in the UI
      )
    ),
    
    layout_column_wrap(
      width = 1,
      gap = "1rem",
      value_box(
        title = "Total Catches",
        value = All_catches,
        showcase = bs_icon("graph-up"),
        theme = "purple"
      ),
      value_box(
        title = "Total Trap Services",
        value = All_trap_records,
        showcase = bs_icon("pie-chart"),
        theme = "teal"
      ),
      value_box(
        title = "Total Traps Deployed",
        value = trapLength,
        showcase = bs_icon("bar-chart"),
        theme = "pink"
      )
    )
  )
)


server <- function(input, output, session) {
  output$bar_plot <- renderPlot({
    p   # your ggplot object (e.g., created with ggplot(catches_data, aes(...)))
  })
}
print("Starting shiny app...")

shinyApp(ui, server)


