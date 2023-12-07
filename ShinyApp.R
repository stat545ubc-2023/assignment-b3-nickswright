library(shiny)
library(tidyverse)
library(palmerpenguins)
library(DT)
library(rsconnect)
library(shinythemes)

#This shiny app allows users to examine the mass distribution of the Palmer penguins.
#For assignment B3 I selected these 3 features to improve the app's utility
  #1 An interactive table in which the user can select the number of entries, search for entries, and order the data
  #2 A button allowing users to download a .csv file of the table
  #3 A plot of penguin mass in which users can select the range of data displayed and the color fill

#For assignment B4 I added 3 new features
  #1 The shinytheme spacelab to give the app a cleaner background
  #2. An image of the three penguin species
  #3. Two separate tabs for the plot and table using tabsetPanel

#User Interface
ui <- fluidPage(
  theme = shinytheme("spacelab"), #Added spacelab color scheme
  titlePanel("Palmer Penguins Mass"),
  tabsetPanel( #Used to add two different tabs
    tabPanel("Plot", #Defines tab 1
  sidebarLayout(
    sidebarPanel(
      h4("Use this plot to explore the distribution of mass among penguins."),
    img(src = "lter_penguins.png", width = 250, height = 150), #Adds an image of the three penguin species and set size
    sliderInput("mass_slider", "Select a mass range",
                min = 2700, max = 6300, value = c(4000, 5000)) #Adds a slider that can be used to set the range of data displayed in plot
    ),
    mainPanel(
      colourpicker::colourInput("bar_color", "Select Bar Color", value = "dodgerblue3"),  # Allows user to select plot color, starting color is sky blue
      plotOutput("mass_histogram"), #Adds plot
    )
  )
),
tabPanel("Table", #Defines tab 2
         sidebarLayout(
           sidebarPanel(
             h4("Use this table to learn more about the penguins' other characteristics."),
             downloadButton("download_table", "Download Table"),  # Adding download button to sidebar
             p("Data were collected and made available by Dr. Kristen Gorman and the Palmer Station, Antarctica LTER,
    a member of the Long Term Ecological Research Network. Artwork by @allison_horst.")
           ),
    mainPanel(
      dataTableOutput("mass_table")  # Adds interactive table
    )
         )
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

