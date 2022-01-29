library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)

ui <- fluidPage(
    # App title
    titlePanel("City population in France"),
    # Sidebar layout
    sidebarLayout(
        # Sidebar panel
        sidebarPanel(
            textInput(inputId = "caption",
                      label = "Caption:",
                      value = "Visualization of City Population in France"),
            uiOutput("cities"),
            uiOutput("sliderYear"),
            verbatimTextOutput("summary"),
        ),
        # Main panel
        mainPanel(
            h3(textOutput("caption", container = span)),
            plotOutput("popPlot"),
            tableOutput("display"),
            h3("Average population during the select period"),
            tableOutput("average")
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    # Reactive File Reader function 
    # checks if the origin textfile changes every 2 seconds
    datasetInput <- reactiveFileReader(2000, 
                                       session, 
                                       'population.txt', 
                                       read.csv,
                                       header = TRUE, 
                                       sep=" ")
    #datasetInput <- reactive({
    #    df <- read.csv("population.txt", header = TRUE, sep=" ")
    #})
    
    # Filters the dataset for cities and year input by the user
    filteredDataset <- reactive ({
        df <- datasetInput()
        df <- df %>% 
            filter(city %in% input$cities, 
                   year %in% c(input$range[1]:input$range[2]))
    })
    
    # outputs a summary of the loaded dataset
    output$summary <- renderPrint ({
        df <- datasetInput() 
        summary(df)
    })
    
    # Renders the checkbox list of available cities
    output$cities <- renderUI ({
        df <- datasetInput()
        df <- df$city %>% unique()
        checkboxGroupInput("cities", "Cities available:", 
                                  choices = df,
                                  selected = "Paris",
                                  inline = TRUE)
    })
    
    # Renders the year range slider between the min and max available year
    output$sliderYear <- renderUI ({
        df <- datasetInput()
        sliderInput("range", 
                           "Year Range:", 
                           min = min(df$year), 
                           max = max(df$year),
                           value=c(min(df$year), max(df$year)),
                           round=TRUE,
                           step=1)
    })
    
    # Renders the caption text field
    output$caption <- renderText({
        input$caption
    })
    
    # Renders the lineplot of population vs. year
    output$popPlot <- renderPlot({
        ggplot(data=filteredDataset(), 
               aes(x=year, 
                   y=pop / 1000, 
                   color=city)) +
            geom_line(linetype = "dashed") +
            geom_point() +
            labs(x="Year", 
                 y="City population (in '000)")
    })
    
    # Renders the table displaying the data shown on the lineplot
    output$display <- renderTable ({
        df <- filteredDataset()
        if (nrow(df)==0) {
            print("no city selected")
        } else {
            spread(df, year, pop)
        }
    })
    
    # Renders the table displaying the average population of displayed cities
    # over the currently selected year range
    output$average <- renderTable ({
        df <- filteredDataset()
        if (nrow(df)==0) {
            print("no city selected")
        } else {
            df <- aggregate(df,
                            by=list(df$city),
                            FUN=mean)
            df <- df %>% 
                select(-c(year, 
                          city)) %>%
                rename(city=Group.1, 
                       population=pop)
            return(df)
        }
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)