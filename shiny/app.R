#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# library(googlesheets4)
library(readxl)
library(tidyverse)
library(ggrepel)
library(viridis)
library(directlabels)
library(stringr)

pad_time <- function(tb){
  
  pads <- tb %>% 
    group_by(`Player Name`) %>% 
    slice(1) %>% 
    mutate(`Game Time` = max(tb$`Game Time`)) 
  
  tb %>% 
    bind_rows(pads)
}

rbl <- NULL

ui <- fluidPage(
  
  titlePanel("Half Space Player Ratings Visualizations"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("datasheet",
                "Upload Game Log Sheet (.csv)",
                accept = ".csv"),
      checkboxGroupInput("players","Select players for cumulative score plot:",
                         choices = NULL),
      selectInput("eventplayer", "Select player for event plot:",
                  choices = NULL),
      radioButtons("labs", "Label Major Events for Cumulative Score Plot?",
                   c("Yes", "No"),
                   selected = "Yes")
      
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Timeline Plot", plotOutput("timelinePlot")),
                  tabPanel("Event Plot", plotOutput("eventPlot")),
                  tabPanel("Cumulative Team Score", plotOutput("cumulativePlot"))
                  
      )
      
      
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    
    if (is.null(input$datasheet))
      return(NULL)
    # pnames <- read_xlsx(input$datasheet$datapath, sheet = "Game Log")$`Player Name` %>% unique
    pnames <- read_csv(input$datasheet$datapath)$`Player Name` %>% unique
    
    updateCheckboxGroupInput(session, inputId = "players",
                             label = "Select players for cumulative score plot:",
                             choices = pnames)
    
    updateSelectInput(session, inputId = "eventplayer",
                      label = "Select player for event plot:",
                      choices = pnames)
    
  })
  
  
  output$timelinePlot <- renderPlot({
    
    if (is.null(input$datasheet))
      return(NULL)
    
    playersofinterest <- input$players
    
    rbl <- read_csv(input$datasheet$datapath) %>%
      pad_time() %>%
      filter(`Player Name` %in% playersofinterest) %>%
      arrange(`Player Name`, `Game Time`) %>%
      group_by(`Player Name`) %>%
      mutate(`Net Score` = cumsum(`Event Score`)) %>%
      ungroup()
    
    rblstep <- bind_rows(old = rbl,
                         new = rbl %>% group_by(`Player Name`) %>%  mutate(`Net Score` = lag(`Net Score`)),
                         .id = "source") %>%
      filter(!is.na(`Net Score`)) %>%
      arrange(`Game Time`, source)
    
    lastpoints <- rbl %>%
      group_by(`Player Name`) %>%
      arrange(desc(`Game Time`)) %>%
      slice(1)
    
    rbl %>%
      ggplot(aes(x = `Game Time`, y = `Net Score`, color = `Player Name`)) +
      geom_step() +
      geom_ribbon(aes(x = `Game Time`, ymin = 0, ymax = `Net Score`, fill = `Player Name`),
                  data = rblstep, alpha = .1) +
      geom_label_repel(aes(label = `Player Name`),
                       nudge_x = 1, na.rm = T, data = lastpoints) +
      theme_bw() +
      scale_x_continuous(breaks = seq(0, 90, by = 15), labels = seq(0, 90, by = 15)) +
      scale_y_continuous(breaks = seq(-10, 212, by = 2)) +
      # coord_cartesian(xlim = c(0, 100), ylim = c(-1, 10)) +
      theme(axis.line.y=element_blank(),
            axis.line.x =element_blank(),
            legend.position = "top",
            panel.grid.minor = element_blank()) +
      # coord_cartesian(xlim = c(0, 110)) +
      scale_color_brewer(palette = "Set1", guide = "none") +
      scale_fill_brewer(palette = "Set1", guide = "none")
    
    
  })
  
  
  output$eventPlot <- renderPlot({
    
    if (is.null(input$datasheet))
      return(NULL)
    
    playersofinterest <- input$eventplayer
    
    rbl <- read_csv(input$datasheet$datapath) %>%
      pad_time() %>%
      filter(`Player Name` %in% playersofinterest) %>%
      arrange(`Player Name`, `Game Time`) %>%
      group_by(`Player Name`) %>%
      mutate(`Net Score` = cumsum(`Event Score`)) %>%
      ungroup()
    
    
    
    rbl %>% 
      mutate(`Net Score` = cumsum(`Event Score`)) %>% 
      ggplot(aes(x = `Game Time`, y = `Net Score`)) + 
      geom_point() + 
      geom_step() +
      geom_label_repel(aes(label = `Event Type`, fill = `Game Situtaiton`),
                       point.padding = .5, box.padding = .65, label.padding = .1) +
      theme_bw() +
      scale_x_continuous(breaks = seq(0, 90, by = 15), labels = seq(0, 90, by = 15)) +
      scale_y_continuous(breaks = seq(-100, 100, by = 2)) +
      theme(axis.line.y=element_blank(),
            axis.line.x =element_blank(),
            legend.position = "top",
            panel.grid.minor = element_blank()) +
      facet_grid(`Player Name` ~ .) +
      scale_fill_viridis(discrete = T, begin = .35)
    
    
  })
  
  output$cumulativePlot <- renderPlot({
    
    if (is.null(input$datasheet))
      return(NULL)
    
    rbl <- read_csv(input$datasheet$datapath) %>%
      pad_time() %>%
      arrange(`Game Time`) %>%
      mutate(`Net Score` = cumsum(`Event Score`))
    
    
    rblstepc <- bind_rows(old = rbl, 
                          new = rbl %>%  mutate(`Net Score` = lag(`Net Score`)),
                          .id = "source") %>%
      filter(!is.na(`Net Score`)) %>% 
      arrange(`Game Time`, source)
    
    
    labtab <- rbl %>% 
      filter(abs(`Event Score`) > 1) %>% 
      mutate(newlab = map2_chr(`Player Name`, `Event Type`, stringr::str_c, sep = " - "))
    
    if(input$labs == "No") labtab <- slice(labtab, 0)
    
    rbl %>% 
      ggplot(aes(x = `Game Time`, y = `Net Score`)) + 
      geom_step() +
      geom_ribbon(aes(x = `Game Time`, ymin = 0, ymax = `Net Score`), 
                  data = rblstepc, alpha = .1) +
      theme_bw() +
      geom_label_repel(aes(label = newlab, fill = `Game Situtaiton`),
                       point.padding = .15, data = labtab, nudge_y = 12.5) +
      scale_x_continuous(breaks = seq(0, 90, by = 15), labels = seq(0, 90, by = 15)) +
      scale_y_continuous(breaks = seq(-10, 212, by = 5)) +
      theme(axis.line.y=element_blank(),
            axis.line.x =element_blank(),
            legend.position = "top",
            panel.grid.minor = element_blank()) +
      scale_color_brewer(palette = "Set1", guide = "none") +
      scale_fill_brewer(palette = "Set1", guide = "none")
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
