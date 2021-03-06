#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(janitor)
library(lubridate)
library(viridis)
library(directlabels)
library(ggrepel)

# Define UI for application that draws a histogram

tbd <- tbc <- read_csv("data/cleaned-data-all-series.csv") %>% 
    mutate(label = if_else(Date == max(Date), as.character(`Province/State`), NA_character_)) %>% 
    filter(Date > (max(Date) - days(14)))
    
# tbd <- read_csv("data/us-deaths-cleaned.csv")
# tbr <- read_csv("data/us-recovered-cleaned.csv")

states <- tbd$`Province/State`[tbd$`Province/State` != "Total"] %>% 
    unique %>% 
    sort()

most_recent_day <- max(tbd$Date)

topdeathstates <- tbd %>% 
    filter(`Province/State` %in% states) %>% 
    filter(Date == most_recent_day) %>% 
    arrange(desc(Deaths)) %>% 
    slice(1:6) %>% 
    select(`Province/State`)

ui <- fluidPage(theme = shinytheme("superhero"),

    # Application title
    titlePanel("SARS-CoV-2 data in the US by State"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h2("Plot Inputs"),
            checkboxInput("logarithmicY", "Show y-axis on log scale", T),
            checkboxInput("ppnscale", "Scale y-axis per million residents", T),
            selectizeInput("states","Select states", choices = states, multiple = TRUE,
                               selected = "topdeathstates")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Confirmed Cases", plotOutput("confirmed", 
                                                               height = 500, 
                                                               width = 800)),
                        tabPanel("Deaths", plotOutput("deaths", 
                                                      height = 500, 
                                                      width = 800))
                        # tabPanel("Recoveries", plotOutput("recovered")
                        ),
        h2("About:"),
        p("This app shows simple visualizations of confirmed cases, deaths, and recovories from the SARS-CoV-2 virus in the United States over the past two weeks. You can select which states (including DC and US Territories) you want to show using the option list on the right. By default, the six states with the highest deaths in the most recent day in the set are shown."),
        p("Data come from the JHU github repo:  https://github.com/CSSEGISandData/COVID-19"),
        p("The by-state breakdowns in this series only go back to 9 March, so that's the date where the plots start, as well."),
        p("Contact: 	casualinferenceblog@gmail.com"),
        p("Currently, the Recoveries tab has been removed because it looks like either my data source isn't tracking it or there simply aren't any."),
        p("Note: On 23 March, I had to revise the data source, since JHU's feed ceased updating US states for their time series data. I now generate the time series data above from their daily updates.")
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observe({
        
        updateSelectizeInput(session, inputId = "states",
                                 label = "Select states:",
                                 choices = states, selected = topdeathstates$`Province/State`)
        
        
    })

    output$confirmed <- renderPlot({
        
        
        if(!input$ppnscale){
            outc <- tbc %>% 
                filter(`Province/State` %in% c(input$states)) %>% 
                ggplot(aes(x = Date, y = Confirmed, color = `Province/State`)) + 
                geom_line(size = 1) + 
                geom_line(data = filter(tbc, `Province/State` == "Total"), color = "black", size = 2) +
                theme_bw() +
                xlim(c(min(tbc$Date), date(max(tbc$Date) + 2))) +
                scale_color_viridis_d(guide = "none") +
                geom_label_repel(aes(label = label),
                                 nudge_x = 1,
                                 na.rm = TRUE)

        } else{
            outc <- tbc %>% 
                filter(`Province/State` %in% c(input$states)) %>% 
                ggplot(aes(x = Date, y = `Confirmed per million residents`, color = `Province/State`)) + 
                geom_line(size = 1) + 
                geom_line(data = filter(tbc, `Province/State` == "Total"), color = "black", size = 2) +
                theme_bw() +
                xlim(c(min(tbc$Date), date(max(tbc$Date) + 2))) +
                scale_color_viridis_d(guide = "none") +
                geom_label_repel(aes(label = label),
                                 nudge_x = 1,
                                 na.rm = TRUE)
        }
        
        
        if(input$logarithmicY)
            outc <- outc + scale_y_log10()
        
        return(outc)
        
    })
    
    output$deaths <- renderPlot({
        
        if(!input$ppnscale){
            outd <- tbd %>% 
                filter(`Province/State` %in% c(input$states)) %>% 
                ggplot(aes(x = Date, y = Deaths, color = `Province/State`)) + 
                geom_line(size = 1) + 
                geom_line(data = filter(tbd, `Province/State` == "Total"), color = "black", size = 2) +
                theme_bw() +
                xlim(c(min(tbc$Date), date(max(tbc$Date) + 2))) +
                scale_color_viridis_d(guide = "none") +
                geom_label_repel(aes(label = label),
                                 nudge_x = 1,
                                 na.rm = TRUE)
            
        } else{
            outd <- tbd %>% 
                filter(`Province/State` %in% c(input$states)) %>% 
                ggplot(aes(x = Date, y = `Deaths per million residents`, color = `Province/State`)) + 
                geom_line(size = 1) + 
                geom_line(data = filter(tbd, `Province/State` == "Total"), color = "black", size = 2) +
                theme_bw() +
                xlim(c(min(tbc$Date), date(max(tbc$Date) + 2))) +
                scale_color_viridis_d(guide = "none") +
                geom_label_repel(aes(label = label),
                                 nudge_x = 1,
                                 na.rm = TRUE)
        }
        
        
        if(input$logarithmicY)
            outd <- outd + scale_y_log10()
        
        return(outd)
    })
    
    # output$recovered <- renderPlot({
    #     
    #     
    #     if(!input$ppnscale){
    #         outr <- tbd %>% 
    #             filter(`Province/State` %in% c(input$states)) %>% 
    #             ggplot(aes(x = Date, y = Confirmed, color = `Province/State`)) + 
    #             geom_line(size = 1) + 
    #             geom_line(data = filter(tbd, `Province/State` == "Total"), color = "black", size = 2) +
    #             theme_bw() +
    #             scale_color_viridis_d()
    #         
    #     } else{
    #         outr <- tbd %>% 
    #             filter(`Province/State` %in% c(input$states)) %>% 
    #             ggplot(aes(x = Date, y = `Confirmed per million residents`, color = `Province/State`)) + 
    #             geom_line(size = 1) + 
    #             geom_line(data = filter(tbd, `Province/State` == "Total"), color = "black", size = 2) +
    #             theme_bw() +
    #             scale_color_viridis_d()
    #     }
    #     
    #     if(input$logarithmicY)
    #         outr <- outr + scale_y_log10()
    #     
    #     return(outr)
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)