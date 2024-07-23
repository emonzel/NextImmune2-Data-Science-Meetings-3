#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load necessary libraries
library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the data from the Excel file
counts <- read_excel("I:/DII ALL7/Elena/NI2_statistics_course/shiny_metaG.xlsx", sheet = "counts")
meta <- read_excel("I:/DII ALL7/Elena/NI2_statistics_course/shiny_metaG.xlsx", sheet = "meta")
mean_RA <- read_excel("I:/DII ALL7/Elena/NI2_statistics_course/shiny_metaG.xlsx", sheet = "mean_RA")

# Reshape the counts data for easier plotting
counts_long <- counts %>%
  pivot_longer(cols = starts_with("EM018"), names_to = "Sample", values_to = "Count") %>%
  inner_join(meta, by = c("Sample" = "Sample"))

# Inspect the data
head(counts_long)

# Inspect the mean_RA data
head(mean_RA)


# Define colors for each taxa
taxa_colors <- c(
  "Akkermansia_muciniphila" = "darkred",
  "Bacteroides_ovatus" = "yellow",
  "Bacteroides_uniformis" = "grey",
  "Bacteroides_thetaiotaomicron" = "darkgreen",
  "Bacteroides_caccae" = "purple",
  "Escherichia_coli" = "green",
  "Marvinbryantia_formatexigens" = "darkorange4",
  "Barnesiella_intestinihominis" = "black",
  "Clostridium_Qsymbiosum" = "red",
  "Desulfovibrio_piger" = "darkblue",
  "Collinsella_aerofaciens" = "orange",
  "Lactococcus_lactis" = "pink",
  "Lactococcus_cremoris" = "lightblue"
)


# UI
ui <- fluidPage(
  titlePanel("Microbial Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("data_type", "Select Data Type:", choices = c("Counts", "Mean Relative Abundance")),
      
      conditionalPanel(
        condition = "input.data_type == 'Counts'",
        selectInput("taxa", "Select Taxa:", choices = c("All", unique(counts_long$TAXA_ID))),
        selectInput("diet", "Select Diet:", choices = unique(counts_long$Diet)),
        sliderInput("day", "Select Day Range:", min = min(counts_long$Day), max = max(counts_long$Day),
                    value = c(min(counts_long$Day), max(counts_long$Day)), step = 1)
      ),
      
      conditionalPanel(
        condition = "input.data_type == 'Mean Relative Abundance'",
        selectInput("taxa_RA", "Select Taxa:", choices = c("All", unique(mean_RA$TAXA_ID))),
        selectInput("diet_RA", "Select Diet:", choices = unique(mean_RA$Diet)),
        sliderInput("day_RA", "Select Day Range:", min = min(mean_RA$Day), max = max(mean_RA$Day),
                    value = c(min(mean_RA$Day), max(mean_RA$Day)), step = 1)
      )
    ),
    
    mainPanel(
      plotOutput("dataPlot")
    )
  ),
  
  # Add custom CSS for improved styling
  tags$head(
    tags$style(HTML("
      body {background-color: #f0f0f0;}
      .title-panel {background-color: #4CAF50; color: white; padding: 10px;}
      .sidebar {background-color: #ffffff; padding: 15px; border-radius: 10px;}
      .main-panel {background-color: #ffffff; padding: 15px; border-radius: 10px;}
    "))
  )
)



#define server output

server <- function(input, output) {
  filtered_counts <- reactive({
    data <- counts_long %>%
      filter(Diet == input$diet,
             Day >= input$day[1],
             Day <= input$day[2])
    
    if (input$taxa != "All") {
      data <- data %>%
        filter(TAXA_ID == input$taxa)
    }
    
    data
  })
  
  filtered_RA <- reactive({
    data <- mean_RA %>%
      filter(Diet == input$diet_RA,
             Day >= input$day_RA[1],
             Day <= input$day_RA[2])
    
    if (input$taxa_RA != "All") {
      data <- data %>%
        filter(TAXA_ID == input$taxa_RA)
    }
    
    data
  })
  
  output$dataPlot <- renderPlot({
    if (input$data_type == "Counts") {
      ggplot(filtered_counts(), aes(x = Day, y = Count, color = TAXA_ID)) +
        geom_line() +
        geom_point(size = 3) +  # Increase the size of the points
        scale_color_manual(values = taxa_colors) +  # Apply the color scheme
        scale_x_continuous(breaks = c(0, 56, 70)) +  # Customize x-axis breaks
        labs(title = if (input$taxa == "All") {
          "Counts of All Taxa"
        } else {
          paste("Counts of", input$taxa, "under", input$diet, "Diet")
        },
        x = "Day", y = "Count") +
        theme_minimal()
    } else if (input$data_type == "Mean Relative Abundance") {
      ggplot(filtered_RA(), aes(x = Day, y = MeanRelativeAbundance, color = TAXA_ID)) +
        geom_line() +
        geom_point(size = 3) +  # Increase the size of the points
        scale_color_manual(values = taxa_colors) +  # Apply the color scheme
        scale_x_continuous(breaks = c(0, 56, 70)) +  # Customize x-axis breaks
        labs(title = if (input$taxa_RA == "All") {
          "Mean Relative Abundance of All Taxa"
        } else {
          paste("Mean Relative Abundance of", input$taxa_RA, "under", input$diet_RA, "Diet")
        },
        x = "Day", y = "Mean Relative Abundance") +
        theme_minimal()
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
