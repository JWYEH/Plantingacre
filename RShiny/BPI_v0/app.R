library(shiny)
library(shinythemes)
library(dplyr)
library(readr)

# Load data
trend_data <- read_csv("/Users/ekeze/Documents/GitHub/PlantingAcre/data/raw/state_df2.csv")


# Define UI for application that draws a histogram
ui <- navbarPage( 
    theme = shinytheme("lumen"),
    "Bayer Planting Intentions, BPI",
    tabPanel("State", 
        sidebarLayout(
            sidebarPanel(
                    
                    # Select type of trend to plot
                    selectInput(inputId = "state_name", label = strong("State:"),
                                choices = unique(trend_data$state_name),
                                selected = "DELAWARE"),
                    hr(),
                    helpText("Data from USDA."),
                    
                    # Select date range to be plotted
                    dateRangeInput("date", strong("Year range"), start = "2008-01-01", end = "2019-12-31", min = "2001-01-01", max = "2019-12-31"),
                    
                    
                    # Display only if the smoother is checked
                    conditionalPanel(condition = "input.smoother == true",
                                     sliderInput(inputId = "f", label = "Smoother span:",
                                                 min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                 animate = animationOptions(interval = 100))
                    )
            ),
        

            # Show a plot of the generated distribution
            mainPanel(
               plotOutput(outputId = "lineplot", height = "400px"),
               hr(),
               plotOutput(outputId = "lineplot2", height = "400px")
            )
        ),    
    ),
    tabPanel("National", "This panel is intentionally left blank")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Subset data
    selected_trends <- reactive({
        #req(input$date)
        #validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
        #validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
        trend_data %>%
            filter(
                state_name == input$state_name,
        #        date > as.POSIXct(input$date[1]) & date < as.POSIXct(input$date[2])
                )
    })
    

    output$lineplot2 <- renderPlot({
        color = "#434343"
        #par(mar = c(4, 4, 1, 1))
        plot(x = selected_trends()$year, y = selected_trends()$Value, type='l', main="Actual VS. Prediction",
             xlab = "Year", ylab = "Crop Acerage",col = "red", fg = color, col.lab = color, col.axis = color, lwd=2)
        par(new=TRUE)
        plot(x = selected_trends()$year, y = selected_trends()$.pred, axes = FALSE, ylab = "",  xlab = "",type='l', lty=2, col="blue", lwd=2)
        #par(new=TRUE)
        #plot(x = selected_trends()$year, y = selected_trends()$lm, axes = FALSE, ylab = "",  xlab = "",type='l', lty=2, col="green", lwd=2)
        legend("topleft",legend=c("Actual","XGB"),
               text.col=c("red","blue"),pch=c(16,15),col=c("red","blue"))        
        #legend("topleft",legend=c("Actual","XGB","lm"),
        #       text.col=c("red","blue","green"),pch=c(16,15),col=c("red","blue",'green'))

    })
    output$lineplot <- renderPlot({
        color = "#434343"
        #par(mar = c(4, 4, 1, 1))
        plot(x = selected_trends()$year, y = selected_trends()$Value, type='l', main="Corn VS. Soy",
             xlab = "Year", ylab = "Crop Acerage",col = "red", fg = color, col.lab = color, col.axis = color, lwd=2)
        par(new=TRUE)
        
        plot(x = selected_trends()$year, y = selected_trends()$AcresPlanted_Soy, axes = FALSE, ylab = "",  xlab = "",type='l', col="black", lwd=2)
        axis(side=4, at = pretty(range(selected_trends()$AcresPlanted_Soy)))
        
        legend("topleft",legend=c("Corn","Soy"),
               text.col=c("red","black"),pch=c(16,15),col=c("red","black"))
        # Display only if smoother is checked
        #if(input$smoother){
        #    smooth_curve <- lowess(x = as.numeric(selected_trends()$date), y = selected_trends()$close, f = input$f)
        #    lines(smooth_curve, col = "#E6553A", lwd = 3)
        #}
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
