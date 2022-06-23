library('shiny')
library('shinythemes')
library('tidycensus')
library('tidyverse')
library('tigris')
library('sf')
library('scales')

options(tigris_use_cache = TRUE)
countyDF <- get_decennial(geography = "county",
                          variables = "H1_001N", 
                          year = 2020, 
                          geometry = TRUE) %>%
  mutate(state = str_sub(NAME, str_locate(NAME, ',')[,1]+2, str_length(NAME)), 
         county = str_sub(NAME, 1, str_locate(NAME, ',')[,1]-1))

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("yeti"),

    # Application title
    titlePanel("Explore the US"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("instate", "State",
                      c("Utah", "California", "Nevada", "Arizona",
                        "Oregon", "Washington", "Idaho"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("statePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$statePlot <- renderPlot({
      # Filter by the state
      stateDF = countyDF %>% filter(state == input$instate)
      
      # Find breakpoint
      breakpoint = mean(stateDF$value)/max(stateDF$value)
      ggplot(data = stateDF, aes(fill = value)) +
        geom_sf() +
        scale_fill_gradientn(colors = c('#6883BA', '#F6E8EA', '#EF626C'), 
                             values = c(0, breakpoint, 1),
                             limits = c(0, max(stateDF$value)),
                             labels = label_comma(suffix = "K", 
                                                  bigmark = ",", 
                                                  scale = 1e-3)) +
        labs(title = paste(input$instate, 'Population by County'), 
             subtitle = 'Per US Decennial Census 2020', 
             fill = 'Population') +
        guides(fill = guide_colorbar(barheight = 15)) +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5, size = 18, face = 'bold'), 
              plot.subtitle = element_text(hjust = 0.5, size = 13, face = 'italic'), 
              legend.title = element_text(size = 10, face = "bold"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)