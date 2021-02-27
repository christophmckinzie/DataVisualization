library(shiny)
library(ggplot2)
library(stringr)

df <- read.csv("fludata.csv")

ui = fluidPage(
    # Application title
    titlePanel("New York Flu Cases, 2009-2016"),
    # Sidebar with a 3 inputs 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "season",
                        label = "Select Year:",
                        choices = unique(df$season)),
            radioButtons(inputId = "type",
                         label = "Influenza Type:",
                         choices = c(
                             "Both" = "INFLUENZA",
                             "Influenza_A" = "INFLUENZA_A",
                             "Influenza_B" = "INFLUENZA_B")),
            radioButtons(inputId = "region",
                         label = "Region:",
                         choices = c(
                             "All" = "NYC|CAPITAL DISTRICT|WESTERN|CENTRAL|METRO",
                             "New York City" = "NYC",
                             "Capital District" = "CAPITAL DISTRICT",
                             "Western" = "WESTERN",
                             "Central" = "CENTRAL",
                             "Metro" = "METRO"))
        ),
        # Show plot and table
        mainPanel(
            plotOutput("fluPlot"))
    )
)

server = function(input, output) {
    
selections = reactive({
    req(input$season)
    req(input$type)
    req(input$region)
    filter(df, season == input$season) %>%
        filter(str_detect(disease, input$type)) %>% 
        filter(grepl(input$region,region))
    
        
})

    output$fluPlot = renderPlot({
        ggplot(data = selections(), aes(x = factor(cdc_week), y = count)) +
            geom_bar(stat = 'identity', color = 'steelblue', fill = 'steelblue', fun = 'mean', width = 0.8) +
            labs(
                title = "Title",
                x = "CDC Week",
                y = "Count"
            ) +
            scale_y_continuous(limits = c(0, 5000)) +
            theme(axis.text.x = element_text(angle = 45, hjust=1))
    })
}

shinyApp(ui = ui, server = server)
