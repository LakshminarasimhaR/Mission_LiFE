# Load necessary libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(readr)

# Load data from an online source
data <- read_csv("1952023.csv")

# Ensure that dates are in the correct format
data$Date <- as.Date(data$Date, format = "%d-%m-%Y")

# Create the sidebar
sidebar <- dashboardSidebar(
  tags$style(".skin-blue .main-sidebar {background-color: #F5F5F5;}"),
  tags$style(".control-label {color: #000000;}"),  # Add this line
  sidebarMenu(
    pickerInput(
      inputId = "state",
      label = "Select a State:",
      choices = unique(data$State),
      options = list(`actions-box` = TRUE),
      multiple = TRUE
    )
  )
)

# Create the dashboard body
body <- dashboardBody(
  tags$div(
    tags$h1("Mission LiFE", style = "font-size: 30px; text-align: center; color: blue;"),
    tags$h2("Summary of outreach activities by zoos", style = "font-size: 20px; text-align: center; color: blue;")
  ),
  fluidRow(
    box(
      textOutput("totalParticipants"),
      width = 4,
      solidHeader = TRUE,
      status = "primary"
    ),
    box(
      textOutput("totalEvents"),
      width = 4,
      solidHeader = TRUE,
      status = "primary"
    ),
    box(
      textOutput("totalZoos"),
      width = 4,
      solidHeader = TRUE,
      status = "primary"
    ),
    box(
      textOutput("totalStates"),
      width = 4,
      solidHeader = TRUE,
      status = "primary"
    )
  ),
  tabsetPanel(
    tabPanel("Participants", plotOutput("participantsPlot")),
    tabPanel("State-wise Events", plotOutput("eventsPlot")),
    tabPanel("Event Type", plotOutput("eventTypePlot")),
    tabPanel("Zoo-wise Events", plotOutput("zooEventsPlot")),
    tabPanel("Daily Events", plotOutput("dailyEventsPlot"))
  )
)

# Create the UI for the application
ui <- dashboardPage(
  dashboardHeader(title = "Mission LiFE"),
  sidebar,
  body
)

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    data %>%
      filter(State %in% input$state) %>%
      group_by(State) %>%
      summarise(Total_Participants = sum(`Number of Participants`, na.rm = TRUE),
                Total_Events = n())
  })
  
  filtered_event_data <- reactive({
    data %>%
      filter(State %in% input$state) %>%
      group_by(`Type of Event`) %>%
      summarise(Total_Events_Type = n())
  })
  
  filtered_zoo_data <- reactive({
    data %>%
      filter(State %in% input$state) %>%
      group_by(Zoo_Name) %>%
      summarise(Total_Zoo_Events = n())
  })
  
  filtered_daily_data <- reactive({
    data %>%
      filter(State %in% input$state) %>%
      group_by(Date) %>%
      summarise(Total_Events = n())
  })
  
  output$participantsPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = State, y = Total_Participants)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = Total_Participants), vjust = -0.3, angle = 270, size = 3) +
      theme_minimal() +
      theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
      coord_flip() +
      labs(title = "Total Number of Participants per State/UT",
           x = "State",
           y = "Number of Participants")
  })
  
  output$eventsPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = State, y = Total_Events)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = Total_Events), hjust = -0.3, size = 3) +
      theme_minimal() +
      theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
      coord_flip() +
      labs(title = "Total Number of Events per State/UT",
           x = "State",
           y = "Number of Events")
  })
  
  output$eventTypePlot <- renderPlot({
    ggplot(filtered_event_data(), aes(x = `Type of Event`, y = Total_Events_Type)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = Total_Events_Type), hjust = -0.3, size = 3) +
      theme_minimal() +
      theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
      coord_flip() +
      labs(title = "Total Number of Each Type of Event",
           x = "Type of Event",
           y = "Number of Events")
  })
  
  output$zooEventsPlot <- renderPlot({
    ggplot(filtered_zoo_data(), aes(x = Zoo_Name, y = Total_Zoo_Events)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = Total_Zoo_Events), hjust = -0.3, size = 3) +
      theme_minimal() +
      theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
      coord_flip() +
      labs(title = "Total Number of Events per Zoo",
           x = "Zoo Name",
           y = "Number of Events")
  })
  
  output$dailyEventsPlot <- renderPlot({
    ggplot(filtered_daily_data(), aes(x = Date, y = Total_Events)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = Total_Events), vjust = -0.3, check_overlap = TRUE, size = 3) +
      theme_minimal() +
      theme(plot.margin = margin(1, 1, 1, 1, "cm")) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      scale_x_date(date_breaks = "1 day", date_labels = "%Y-%m-%d") +
      labs(title = "Total Number of Events per Day",
           x = "Date",
           y = "Number of Events")
  })
  
  output$totalParticipants <- renderText({
    paste0("Total number of participants: ", sum(filtered_data()$Total_Participants))
  })
  
  output$totalEvents <- renderText({
    paste0("Total number of events: ", sum(filtered_data()$Total_Events))
  })
  
  output$totalZoos <- renderText({
    paste("Total number of Zoos:", n_distinct(filtered_zoo_data()$Zoo_Name))
  })
  
  output$totalStates <- renderText({
    paste("Total number of States/UT's:", n_distinct(filtered_data()$State))
  })
  
  output$report <- downloadHandler(
    filename = "AwarenessEventReport.pdf",
    content = function(file) {
      rmarkdown::render("report.Rmd", output_file = file,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
