library(shiny)
library(tidyr)
library(dplyr)
library(rlang)
library(readxl)
library(DT) # data table
library(readr) # read in csv data
library(stringi) # remove accent symbols on letters
library(scales) # percentile calculations
library(plotly) # radar chart
library(shinythemes)  # Themes

# Define UI for the application
ui <- fluidPage(
  theme = shinythemes::shinytheme("journal"), 
  titlePanel("NBA Lineups Analysis"),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Home",
               h2("Welcome to the NBA Lineup Analysis App"),
               p("This app allows you to analyze NBA lineups and player stats. You can filter and sort lineups by team and other metrics."),
               p("Select a team and sorting option in the 'Lineup Analysis' tab to get started."),
               br(),
               p("To use the app, follow these steps:"),
               tags$ul(
                 tags$li("Select a team or choose 'All Teams' to view all teams."),
                 tags$li("Choose a sorting criteria based on numeric values."),
                 tags$li("Click on a lineup in the table to view detailed player stats.")
               )
      ),
      tabPanel("Lineup Analysis",
               selectInput("team", "Select Team:", choices = NULL),
               selectInput("sort_by", "Sort By:", choices = NULL),
               h3("Lineup Data"),
               DTOutput("lineupTable"),  # Interactive table with selectable rows
               h3("Player Stats for Selected Lineup"),
               tableOutput("playerTable"),  # Table for displaying player stats
               tags$p(tags$small("Stat Definitions"))
      ),
      tabPanel("Lineup Builder",
               h3("Build Your Custom NBA Lineup"),
               p("Select 5 total players across positions."),
               
               # Layout for positioning the selectors and total metric
               fluidRow(
                 column(6,
                        selectInput("Guard", "Select Guard (PG/SG):", choices = NULL, multiple = TRUE),
                        selectInput("Forward", "Select Forward (SF/PF):", choices = NULL, multiple = TRUE),
                        selectInput("Center", "Select Center (C):", choices = NULL, multiple = TRUE),
                        actionButton("build_lineup", "Build Lineup"),
                        DTOutput("customLineup")
                 ),
                 column(6,
                        selectInput("metric", "Select Metric:", choices = c("PER", "OWS", "DWS", "WS", "OBPM", "DBPM", "BPM", "VORP")),
                        tableOutput("metricTotal")
                 )
               ),
               tags$p(tags$small("Stat Definitions"))
      ),
      tabPanel("Lineup Composition",
               h3("Radar Chart of Player Stats Percentiles"),
               uiOutput("radarChart"),
               tags$p(tags$small("Stat Definitions"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Define a list of abbreviations and their descriptions
  stat_descriptions <- list(
    PER = "Player Efficiency Rating",
    OWS = "Offensive Win Shares",
    DWS = "Defensive Win Shares",
    WS = "Win Shares",
    OBPM = "Offensive Box Plus-Minus",
    DBPM = "Defensive Box Plus-Minus",
    BPM = "Box Plus-Minus",
    VORP = "Value Over Replacement Player",
    Player = "The name of the player",
    Team = "The team the player is associated with",
    Age = "The player's age",
    GP = "Games Played - Number of games the player has participated in",
    W = "Wins - The number of games won by the player's team",
    L = "Losses - The number of games lost by the player's team",
    Min = "Minutes - The total minutes the player has played",
    PTS = "Points - The total points scored by the player",
    FGM = "Field Goals Made - The number of field goals made by the player",
    FGA = "Field Goals Attempted - The number of field goals attempted by the player",
    `FG%` = "Field Goal Percentage - The percentage of successful field goals made (FGM / FGA)",
    `3PM` = "Three-Point Field Goals Made - The number of three-point shots made",
    `3PA` = "Three-Point Field Goals Attempted - The number of three-point shots attempted",
    `3P%` = "Three-Point Percentage - The percentage of successful three-point shots made (3PM / 3PA)",
    FTM = "Free Throws Made - The number of free throws made by the player",
    FTA = "Free Throws Attempted - The number of free throws attempted by the player",
    `FT%` = "Free Throw Percentage - The percentage of successful free throws made (FTM / FTA)",
    OREB = "Offensive Rebounds - The number of rebounds grabbed on the offensive end",
    DREB = "Defensive Rebounds - The number of rebounds grabbed on the defensive end",
    REB = "Rebounds - The total number of rebounds grabbed by the player (OREB + DREB)",
    AST = "Assists - The total number of assists by the player",
    TOV = "Turnovers - The total number of times the player lost possession of the ball",
    STL = "Steals - The total number of steals by the player",
    BLK = "Blocks - The total number of shots blocked by the player",
    PF = "Personal Fouls - The total number of personal fouls committed by the player",
    FP = "Fantasy Points - A fantasy basketball scoring system based on stats",
    DD2 = "Double-Double (Points and Rebounds or Assists) - The number of times the player has achieved a double-double",
    TD3 = "Triple-Double (Points, Rebounds, Assists) - The number of times the player has achieved a triple-double",
    `+/-` = "Plus/Minus - The point differential when the player is on the court"
  )
  
  
  # Read and clean data
  Lineups <- read_excel("Lineups.xlsx")
  Players <- read_excel("Players.xlsx") %>% 
    mutate(Player = stri_trans_general(Player, "Latin-ASCII"))
  Advanced <- read_excel("Advanced.xlsx") %>% 
    select(Player, Team, Pos, PER, OWS, DWS, WS, OBPM, DBPM, BPM, VORP, Awards) %>% 
    mutate(Player = stri_trans_general(Player, "Latin-ASCII"))
  Players.custom <- left_join(Players, Advanced, by = c("Player", "Team"))
  
  Players <- Players %>%
    mutate(Player = gsub("(^[A-Za-z'\\-])[A-Za-z'\\-]*\\s([A-Za-z]+)", "\\1. \\2", Player))
  
  # Reactive Values
  selected_player_stats <- reactiveVal(NULL)
  lineup_source <- reactiveVal(NULL)  # Track source of lineup selection
  
  # Update dropdown choices
  observe({
    updateSelectInput(session, "team", choices = c("All Teams", unique(Lineups$Team)))
    updateSelectInput(session, "sort_by", choices = names(Lineups)[sapply(Lineups, is.numeric)])
    updateSelectInput(session, "Guard", choices = Players.custom$Player[Players.custom$Pos %in% c("PG", "SG")])
    updateSelectInput(session, "Forward", choices = Players.custom$Player[Players.custom$Pos %in% c("SF", "PF")])
    updateSelectInput(session, "Center", choices = Players.custom$Player[Players.custom$Pos == "C"])
  })
  
  # Lineup Analysis
  output$lineupTable <- renderDT({
    req(input$team, input$sort_by)
    filtered_data <- if (input$team == "All Teams") Lineups else Lineups %>% filter(Team == input$team)
    datatable(filtered_data %>% arrange(desc(!!sym(input$sort_by))) %>% select(Lineups, Team, !!sym(input$sort_by)), selection = "single", options = list(pageLength = 5))
  })
  
  # Render the player stats for the selected lineup
  output$playerTable <- renderTable({
    req(input$lineupTable_rows_selected)
    selected <- input$lineupTable_rows_selected
    
    # Extract the selected lineup
    selected_lineup <- Lineups %>% 
      arrange(desc(!!sym(input$sort_by))) %>% 
      slice(selected) %>% 
      pull(Lineups)
    
    # Extract the team of the selected lineup
    selected_team <- Lineups %>%
      arrange(desc(!!sym(input$sort_by))) %>%
      slice(selected) %>%
      pull(Team)
    
    # Extract player names from the selected lineup
    players <- unlist(strsplit(selected_lineup, " - "))
    
    # Identify duplicate players
    duplicate_players <- Players %>%
      filter(Player %in% players) %>%
      group_by(Player) %>%
      filter(n() > 1) %>%
      pull(Player) %>%
      unique()
    
    # Filter and display player stats
    player_stats <- Players %>%
      filter(Player %in% players) %>%
      filter(
        (Player %in% duplicate_players & Team == selected_team) |
          !(Player %in% duplicate_players)
      )
    
    # Store the player stats for the selected lineup
    selected_player_stats(player_stats)
    lineup_source("analysis")  # Set source to lineup analysis
    player_stats
  })
  
  observeEvent(input$build_lineup, {
    req(input$Guard, input$Forward, input$Center)
    selected_players <- c(input$Guard, input$Forward, input$Center)
    custom_lineup <- Players.custom %>% filter(Player %in% selected_players)
    
    selected_player_stats(custom_lineup)
    lineup_source("builder")  # Set source to lineup builder
    
    # Ensure JSON is properly formatted
    stat_descriptions_json <- jsonlite::toJSON(stat_descriptions, auto_unbox = TRUE)
    
    output$customLineup <- renderDT({
      custom_lineup <- selected_player_stats()
      
      # Create the datatable with custom tooltips for column headers
      datatable(
        custom_lineup,
        options = list(
          pageLength = 5,         # Display 5 rows per page
          autoWidth = TRUE,       # Automatically adjust column widths
          dom = 'tip',            # Show table and pagination controls
          initComplete = JS(
            # Adding tooltips to the column headers directly
            paste0(
              "function(settings, json) {",
              "var descriptions = ", stat_descriptions_json, ";",
              "this.api().columns().header().each(function (col, i) {",
              "var colName = $(col).text();",
              "if (descriptions[colName]) {",
              "$(col).attr('title', descriptions[colName]);",  # Add description to the column header
              "}",
              "});",
              "}"
            )
          )
        ),
        selection = "none",       # No row selection
        rownames = FALSE          # Hide row names
      )
    })
  })
  
  
  # Show Total Metric for the Selected Metric
  output$metricTotal <- renderTable({
    req(selected_player_stats(), input$metric)
    custom_lineup <- selected_player_stats()
    
    # Calculate the sum of the selected metric
    total_metric <- sum(custom_lineup[[input$metric]], na.rm = TRUE)
    
    # Create a data frame to display the sum
    data.frame(Metric = input$metric, Total = total_metric)
  })
  
  # Render radar charts for each player in the selected lineup
  output$radarChart <- renderUI({
    req(selected_player_stats())  # Ensure data is available
    
    # Get the selected player stats
    player_stats <- selected_player_stats()
    
    # Get the list of players selected in the lineup table
    selected_players <- player_stats$Player
    selected_teams <- player_stats$Team
    
    # Get the source of the lineup
    source <- lineup_source()
    
    # Select the appropriate dataset based on the source
    dataset <- if (source == "builder") Players.custom else Players
    
    # Grab percentile of players stats
    player_stats_percentiles <- dataset %>%
      mutate(
        PTS = percent_rank(PTS) * 100,
        AST = percent_rank(AST) * 100,
        REB = percent_rank(REB) * 100,
        `FG%` = percent_rank(`FG%`) * 100,
        `3P%` = percent_rank(`3P%`) * 100,
        STL = percent_rank(STL) * 100,
        BLK = percent_rank(BLK) * 100
      )
    
    # Filter the pre-calculated percentiles for selected players, considering teams
    player_percentiles_selected <- player_stats_percentiles %>%
      filter(Player %in% selected_players & Team %in% selected_teams)
    
    # Create a list to hold the radar charts for each player
    radar_plots <- lapply(1:nrow(player_percentiles_selected), function(i) {
      # Extract stats for the player
      player_data <- player_percentiles_selected[i, c("PTS", "AST", "REB", "FG%", "3P%", "STL", "BLK")]
      
      # Prepare the data for the radar chart
      radar_data <- data.frame(
        stats = c("PTS", "AST", "REB", "FG%", "3P%", "STL", "BLK"),
        value = as.numeric(player_data),
        max = 100, min = 0
      )
      
      # Create a radar chart using Plotly
      radar_plot <- plot_ly(
        type = 'scatterpolar',
        r = radar_data$value,
        theta = radar_data$stats,
        fill = 'toself',
        name = player_percentiles_selected$Player[i],
        mode = 'lines+markers',
        hoverinfo = 'text',
        hovertext = paste(radar_data$stats, ": ", selected_player_stats()[i, radar_data$stats], " (Percentile: ",
                          round(player_data, 1), "%)", sep = "")
      ) %>%
        layout(
          polar = list(
            radialaxis = list(
              visible = TRUE,
              range = c(0, 100)
            )
          ),
          title = player_percentiles_selected$Player[i],
          showlegend = FALSE,
          margin = list(t = 35, r = 40, b = 40, l = 40)
        )
      
      return(radar_plot)
    })
    
    # Group radar charts into rows of 3 charts each
    num_rows <- ceiling(length(radar_plots) / 3)
    
    # Create the UI layout with 3 radar charts per row
    chart_ui <- lapply(1:num_rows, function(i) {
      row_start <- (i - 1) * 3 + 1
      row_end <- min(i * 3, length(radar_plots))
      div(class = "row", do.call(tagList, radar_plots[row_start:row_end]))
    })
    
    do.call(tagList, chart_ui)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

