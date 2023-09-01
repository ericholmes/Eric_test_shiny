# load libraries ----------------------------------------------------------

library(shiny)
library(tidyverse)
library(palmerpenguins)
library()

# user interface ----------------------------------------------------------

ui <- fluidPage(
  #app title ----
  tags$h1("Shiny app"),
  #subtitle ----
  p(strong("Exploring antarctic penguin data")),
  
  sliderInput(inputId = "body_mass_input",
              label = "Select a range of body mass (g)",
              value = c(3000, 4000),
              min = 3000,
              max = 4000), #EO sliderInput
  
  selectInput(inputId = "island_input",
              label = "Choose and island",
              choices = c("Biscoe", "Dream", "Torgersen")),
  
  plotOutput(outputId = "bodyMass_scatterplot"),
  
  dataTableOutput(outputId = "penguin_data"),
  
) # End of fluidPage

# server instructions -----------------------------------------------------

server <- function(input, output, session) {
  
  body_mass_df <- reactive({
    penguins %>% filter(body_mass_g > input$body_mass_input[1] &
                          body_mass_g < input$body_mass_input[2])
  })
  
  # Render scatter plot ----
  output$bodyMass_scatterplot <- renderPlot({
    # create plot
    ggplot(na.omit(body_mass_df()), 
           aes(x = flipper_length_mm, y = bill_length_mm, 
               color = species, shape = species)) +
      geom_point() +
      scale_color_manual(values = c("Adelie" = "#FEA346", "Chinstrap" = "#B251F1", "Gentoo" = "#4BA4A4")) +
      scale_shape_manual(values = c("Adelie" = 19, "Chinstrap" = 17, "Gentoo" = 15)) +
      labs(x = "Flipper length (mm)", y = "Bill length (mm)", 
           color = "Penguin species", shape = "Penguin species") +
      theme_minimal() +
      theme(legend.position = c(0.85, 0.2),
            legend.background = element_rect(color = "white"))
  })
  
  
}

# combine ui and server  --------------------------------------------------

shinyApp(ui, server)
