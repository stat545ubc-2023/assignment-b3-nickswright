library(shiny)
library(tidyverse)
library(palmerpenguins)
library(DT)
library(rsconnect)

#This shiny app allows users to examine the mass distribution of the Palmer penguins.
#For this app I selected these 3 features to improve the app's utility
  #1 An interactive table in which the user can select the number of entries, search for entries, and order the data
  #2 A button allowing users to download a .csv file of the table
  #3 A plot of penguin mass in which users can select the range of data displayed and the color fill

#User Interface
ui <- fluidPage(
  titlePanel("Palmer Penguins Mass"),
  sidebarLayout(
    sidebarPanel(
      h4("Use this app to explore the distribution of mass among penguins"),
      p("Data were collected and made available by Dr. Kristen Gorman and the Palmer Station, Antarctica LTER,
    a member of the Long Term Ecological Research Network."),
      sliderInput("mass_slider", "Select a mass range",
                  min = 2700, max = 6300, value = c(4000, 5000)),
      downloadButton("download_table", "Download Table")  # Adding download button to sidebar
    ),
    mainPanel(
      colourpicker::colourInput("bar_color", "Select Bar Color", value = "skyblue"),  # Allows user to select plot color, starting color is sky blue
      plotOutput("mass_histogram"), #Adds plot
      dataTableOutput("mass_table")  # Adds interactive table
    )
  )
)

#Server
server <- function(input, output) {
  observe(print(input$mass_slider))

  #Mass histogram table below
  output$mass_histogram <- renderPlot({
    penguins %>%
    filter(body_mass_g < input$mass_slider[2],
           body_mass_g > input$mass_slider[1])  %>%
      ggplot(aes(body_mass_g)) +
    geom_histogram(fill = input$bar_color) + #Makes the color fill selectable by user
      labs(title = "Distribution of Penguin Body Mass",
           x = "Body Mass (g)",
           y = "Frequency")
  })

  #Mass table code below
  output$mass_table <- renderDataTable({ #Interactive table code
    penguins %>%
      filter(body_mass_g < input$mass_slider[2], #Code for slider
             body_mass_g > input$mass_slider[1]) #Code for slider
  })

  #Download table as .csv code below
  output$download_table <- downloadHandler(
    filename = function() {
      paste("penguins_data_", Sys.Date(), ".csv", sep = "") #Adds file name
    },
    content = function(file) {
      write.csv(penguins, file)
    }
  )
}

shinyApp(ui=ui, server=server)

