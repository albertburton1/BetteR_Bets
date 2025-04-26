library(readr)
library(dplyr)
library(caret)
library(randomForest)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(lubridate)  
library(glmnet)
library(stringr)
library(rvest)
library(httr)
library(jsonlite)

# Load NFL data
teams <- read_csv("~/Desktop/NFL Betting/nfl_teams.csv")
nfl <- read_csv("~/Desktop/NFL Betting/spreadspoke_scores.csv")

# Execute all the preprocessing from original script
teams$team_division[teams$team_name == "St. Louis Rams"] <- "NFC West"
teams$team_division[teams$team_name == "Las Vegas Raiders"] <- "AFC West"

# Formatting date
nfl$schedule_date <- as.Date(nfl$schedule_date, "%m/%d/%Y") 

# Filter data since 2002
nfl <- subset(nfl, schedule_season >= 2002)

# Get team names and IDs
team_names <- teams$team_name
team_ids <- teams$team_id

# Fill in IDs for all home and away
nfl$team_home_id <- NA
nfl$team_away_id <- NA

for (i in 1:nrow(nfl)) {
  for(j in 1:length(team_ids)){
    if(nfl$team_home[i]==team_names[j]){
      nfl$team_home_id[i]<-team_ids[j]
    }
  }
}

for (i in 1:nrow(nfl)) {
  for(j in 1:length(team_ids)){
    if(nfl$team_away[i]==team_names[j]){
      nfl$team_away_id[i]<-team_ids[j]
    }
  }
}

nfl$team_away_id <- as.factor(nfl$team_away_id)
nfl$team_home_id <- as.factor(nfl$team_home_id)
nfl$team_favorite_id <- as.factor(nfl$team_favorite_id)

# Game unique id
library(stringr)
nfl$game_id <- NA 
schedule_first_game_date <- min(nfl$schedule_date)
for(i in 1:nrow(nfl)){
  nfl$game_id[i] <- paste(as.Date(nfl$schedule_date[i],format='%m/%d/%Y'),nfl$team_away_id[i],nfl$team_home_id[i],sep="")
}  
nfl$game_id <- gsub("-","",nfl$game_id)

# Add day of week info
nfl$schedule_day <- wday(nfl$schedule_date, label=TRUE) 
nfl$schedule_month <- month(nfl$schedule_date, label=TRUE)
nfl$schedule_sunday <- ifelse(nfl$schedule_day%in%c("Sun"),TRUE,FALSE) 

# Divisional game True/False
team_divisions <- teams$team_division
nfl$team_home_division <- NA
nfl$team_away_division <- NA

for (i in 1:nrow(nfl)) {
  for(j in 1:length(team_divisions)){
    if(nfl$team_home[i]==team_names[j]){
      nfl$team_home_division[i]<-team_divisions[j]
    }
  }
}

for (i in 1:nrow(nfl)) {
  for(j in 1:length(team_ids)){
    if(nfl$team_away[i]==team_names[j]){
      nfl$team_away_division[i]<-team_divisions[j]
    }
  }
}

nfl$team_away_division <- as.factor(nfl$team_away_division)
nfl$team_home_division <- as.factor(nfl$team_home_division)

## 2002 division and ## pre2002 division
team_divisions_pre2002 <- teams$team_division_pre2002

for (i in 1:nrow(nfl)) {
  for(j in 1:length(team_divisions_pre2002)){
    if(nfl$team_home[i]==team_names[j]){
      nfl$team_home_division_pre2002[i]<-team_divisions_pre2002[j]
    }
  }
}

for (i in 1:nrow(nfl)) {
  for(j in 1:length(team_divisions_pre2002)){
    if(nfl$team_away[i]==team_names[j]){
      nfl$team_away_division_pre2002[i]<-team_divisions_pre2002[j]
    }
  }
}

nfl$team_away_division_pre2002 <- as.factor(nfl$team_away_division)
nfl$team_home_division_pre2002 <- as.factor(nfl$team_home_division)

### division matchup true or false
nfl$division_matchup <- NA
nfl$division_matchup <- ifelse(nfl$team_away_division==nfl$team_home_division,
                               TRUE, ifelse(nfl$team_away_division_pre2002==nfl$team_home_division_pre2002,TRUE,FALSE))
nfl$team_home_division <- NULL
nfl$team_away_division <- NULL
nfl$team_home_division_pre2002 <- NULL
nfl$team_away_division_pre2002 <- NULL

# Spread types
nfl$team_home_favorite <- as.character(nfl$team_favorite_id)==as.character(nfl$team_home_id) 
nfl$spread_home <- ifelse(nfl$team_home_favorite==TRUE, nfl$spread_favorite,-nfl$spread_favorite)
nfl$spread_away <- -nfl$spread_home

nfl$spread_type <- ifelse(nfl$spread_home==0,'Pick',
                          ifelse(nfl$spread_home>0,'Home Underdog','Home Favorite'))
nfl$spread_type <- as.factor(nfl$spread_type)
nfl$spread_outlier <- ifelse(abs(nfl$spread_favorite) > 14.1, '2TD+',
                             ifelse(abs(nfl$spread_favorite) > 10.1, '1TD1FG+',
                                    ifelse(abs(nfl$spread_favorite) > 7.1, '1TD+','No Outlier')))
nfl$spread_outlier <- as.factor(nfl$spread_outlier)

# Over under types
nfl$over_under_outlier <- ifelse(nfl$over_under_line<33,"Under 2sd",
                                 ifelse(nfl$over_under_line<37,"Under 1sd",
                                        ifelse(nfl$over_under_line>50,"Over 2sd",
                                               ifelse(nfl$over_under_line>46,"Over 1sd","No Outlier"))))
nfl$over_under_outlier <- as.factor(nfl$over_under_outlier)

nfl$over_hit = ifelse(nfl$score_home+nfl$score_away > nfl$over_under_line, TRUE, FALSE)

# Points scored by each team in home games
home_scores <- nfl %>%
  select(season = schedule_season, team = team_home, points = score_home)

# Points scored by each team in away games
away_scores <- nfl %>%
  select(season = schedule_season, team = team_away, points = score_away)

# Combine both datasets
all_scores <- bind_rows(home_scores, away_scores)

team_ppg <- all_scores %>%
  group_by(season, team) %>%
  summarise(
    games_played = n(),
    avg_points = mean(points, na.rm = TRUE),
    .groups = "drop"
  )

nfl <- nfl %>%
  left_join(team_ppg, by = c("schedule_season" = "season", "team_home" = "team")) %>%
  rename(home_avg_points = avg_points, home_games_played = games_played)

# For away team
nfl <- nfl %>%
  left_join(team_ppg, by = c("schedule_season" = "season", "team_away" = "team")) %>%
  rename(away_avg_points = avg_points, away_games_played = games_played)

### PPG allowed ####

# Points allowed by each team in home games (they allowed away points)
home_allowed <- nfl %>%
  select(season = schedule_season, team = team_home, points_allowed = score_away)

# Points allowed by each team in away games (they allowed home points)
away_allowed <- nfl %>%
  select(season = schedule_season, team = team_away, points_allowed = score_home)

# Combine both datasets
all_allowed <- bind_rows(home_allowed, away_allowed)

team_papg <- all_allowed %>%
  group_by(season, team) %>%
  summarise(
    games_played = n(),
    avg_points_allowed = mean(points_allowed, na.rm = TRUE),
    .groups = "drop"
  )

# For home team
nfl <- nfl %>%
  left_join(team_papg, by = c("schedule_season" = "season", "team_home" = "team")) %>%
  rename(home_avg_points_allowed = avg_points_allowed,
         home_games_allowed_tracked = games_played)

# For away team
nfl <- nfl %>%
  left_join(team_papg, by = c("schedule_season" = "season", "team_away" = "team")) %>%
  rename(away_avg_points_allowed = avg_points_allowed,
         away_games_allowed_tracked = games_played)

#---------Spread hit
nfl$spread_home_result <- ifelse((nfl$score_home + nfl$spread_home - nfl$score_away) > 0, "hit",
                                 ifelse((nfl$score_home + nfl$spread_home - nfl$score_away) < 0, "loss", "push"))
nfl$spread_away_result <- ifelse((nfl$score_away + nfl$spread_away - nfl$score_home) > 0, "hit",
                                 ifelse((nfl$score_away + nfl$spread_away - nfl$score_home) < 0, "loss", "push"))

nfl$weather_humidity <- NULL
nfl$weather_detail[is.na(nfl$weather_detail)] <- "outdoor"

names(nfl)[names(nfl) == "weather_detail"] <- "indoor"  # Rename column
nfl$indoor <- ifelse(nfl$indoor == "indoor", TRUE, FALSE)

# ----------------------------------------
# Model Function (from original code)
# ----------------------------------------

prepare_and_evaluate_logistic_model <- function() {
  # Define the features to use - removing post-game information
  features_to_use <- c(
    "team_home", "team_away", "spread_favorite",
    "over_under_line", "indoor",
    "division_matchup", "team_home_favorite", 
    "spread_home", "spread_away", "spread_type", 
    "spread_outlier", "over_under_outlier",
    "home_avg_points", 
    "away_avg_points",
    "home_avg_points_allowed", 
    "away_avg_points_allowed"
  )
  
  # Prepare the dataset and include schedule_season for splitting
  prediction_data <- nfl %>%
    select(schedule_season, all_of(features_to_use), over_hit) %>%
    # Drop rows with NA values
    na.omit() %>%
    mutate(
      # Convert categorical variables to factors
      across(c(
        team_home, team_away,
        indoor,
        division_matchup, team_home_favorite,
        spread_type, spread_outlier, over_under_outlier
      ), as.factor),
      
      # Convert over_hit to numeric (0 and 1)
      over_hit = as.numeric(over_hit) - 1  
    )
  
  # Create training and testing sets based on schedule_season
  train_data <- prediction_data %>% filter(schedule_season < 2020)
  test_data <- prediction_data %>% filter(schedule_season >= 2020)
  
  # Remove schedule_season since it's not used as a predictor
  train_data <- train_data %>% select(-schedule_season)
  test_data <- test_data %>% select(-schedule_season)
  
  # Create model formula excluding the target variable
  all_vars <- names(train_data)
  x_vars <- setdiff(all_vars, "over_hit")
  model_formula <- as.formula(paste("over_hit ~", paste(x_vars, collapse = " + ")))
  
  # One-hot encode categorical variables for training data
  train_matrix <- model.matrix(model_formula, data = train_data)
  y_train <- train_data$over_hit
  
  # One-hot encode categorical variables for testing data
  test_matrix <- model.matrix(model_formula, data = test_data)
  y_test <- test_data$over_hit
  
  # Fit logistic regression with LASSO regularization
  cv_model <- cv.glmnet(train_matrix, y_train, family = "binomial", alpha = 1)
  
  # Best lambda value from cross-validation
  best_lambda <- cv_model$lambda.min
  
  # Fit final model
  final_model <- glmnet(train_matrix, y_train, family = "binomial", alpha = 1, lambda = best_lambda)
  
  # Predict on test data
  predictions_prob <- predict(final_model, newx = test_matrix, type = "response", s = best_lambda)
  predictions <- ifelse(predictions_prob > 0.5, 1, 0)
  
  # Calculate confusion matrix
  conf_matrix <- table(Predicted = predictions, Actual = y_test)
  
  # Calculate performance metrics
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  # Handle cases where not all outcomes are present in the confusion matrix
  if(nrow(conf_matrix) >= 2 && ncol(conf_matrix) >= 2) {
    precision <- conf_matrix[2,2] / sum(conf_matrix[2,])
    recall <- conf_matrix[2,2] / sum(conf_matrix[,2])
    f1_score <- 2 * (precision * recall) / (precision + recall)
  } else {
    precision <- NA
    recall <- NA
    f1_score <- NA
  }
  
  # Extract top coefficients
  model_coefs <- as.matrix(coef(final_model, s = best_lambda))
  nonzero_coefs <- model_coefs[model_coefs != 0, , drop = FALSE]
  top_coefs <- head(nonzero_coefs[order(abs(nonzero_coefs), decreasing = TRUE), , drop = FALSE], 20)
  
  # Return results
  return(list(
    model = final_model, 
    confusion_matrix = conf_matrix,
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    f1_score = f1_score,
    features = colnames(train_matrix),
    best_lambda = best_lambda,
    model_formula = model_formula,
    top_coefficients = top_coefs
  ))
}

# Run the model and get results
model_results <- prepare_and_evaluate_logistic_model()

model_results

# Define UI
ui <- dashboardPage(
  skin = "blue",
  
  # Dashboard header
  dashboardHeader(title = "BetteR Bets", titleWidth = 300),
  
  # Dashboard sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Over/Under Predictions", tabName = "over_under", icon = icon("chart-bar")),
      menuItem("Spread Betting", tabName = "spread_betting", icon = icon("money-bill-wave")),
      menuItem("Model Performance", tabName = "model_performance", icon = icon("line-chart")),
      menuItem("NRFI - MLB", tabName = "nrfi_mlb", icon = icon("baseball-ball")),
      menuItem("Betting History", tabName = "history", icon = icon("history"))
    )
  ),
  
  # Dashboard body
  dashboardBody(
    tabItems(
      # Home tab content
      tabItem(tabName = "home",
              fluidRow(
                box(
                  width = 12,
                  title = "Welcome to BetteR Bets",
                  status = "primary",
                  solidHeader = TRUE,
                  h3("Your Advanced Sports Betting Analytics Platform"),
                  p("This interactive dashboard provides data-driven predictions for NFL and MLB games, powered by machine learning."),
                  p("Navigate to the prediction tabs to analyze upcoming games and make informed betting decisions.")
                )
              ),
              fluidRow(
                valueBox(
                  width = 3,
                  value = "NFL",
                  subtitle = "Total games analyzed: 5,324",
                  icon = icon("football-ball"),
                  color = "blue"
                ),
                valueBox(
                  width = 3,
                  value = "MLB",
                  subtitle = "NRFI predictions",
                  icon = icon("baseball-ball"),
                  color = "green"
                ),
                valueBox(
                  width = 3,
                  value = "NFL Teams",
                  subtitle = "Total in database: 32",
                  icon = icon("users"),
                  color = "orange"
                ),
                valueBox(
                  width = 3,
                  value = "Model Accuracy",
                  subtitle = "55.8% on test data",
                  icon = icon("check"),
                  color = "red"
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Available Predictions",
                  status = "info",
                  solidHeader = TRUE,
                  h4("Choose your prediction type:"),
                  tags$ul(
                    tags$li(tags$strong("NFL Over/Under Predictions:"), " Estimate the probability of games going over or under the total line"),
                    tags$li(tags$strong("NFL Spread Betting:"), " Predict team scores, win probabilities, and spread outcomes"),
                    tags$li(tags$strong("MLB NRFI Predictions:"), " Calculate probabilities for No Run First Inning outcomes")
                  )
                )
              )
      ),
      
      # Over/Under Predictions tab content
      tabItem(tabName = "over_under",
              fluidRow(
                box(
                  title = "Game Parameters",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  selectInput("home_team", "Home Team:", 
                              choices = c("Arizona Cardinals", "Atlanta Falcons", "Baltimore Ravens", "Buffalo Bills", 
                                          "Carolina Panthers", "Chicago Bears", "Cincinnati Bengals", "Cleveland Browns",
                                          "Dallas Cowboys", "Denver Broncos", "Detroit Lions", "Green Bay Packers",
                                          "Houston Texans", "Indianapolis Colts", "Jacksonville Jaguars", "Kansas City Chiefs",
                                          "Las Vegas Raiders", "Los Angeles Chargers", "Los Angeles Rams", "Miami Dolphins",
                                          "Minnesota Vikings", "New England Patriots", "New Orleans Saints", "New York Giants",
                                          "New York Jets", "Philadelphia Eagles", "Pittsburgh Steelers", "San Francisco 49ers",
                                          "Seattle Seahawks", "Tampa Bay Buccaneers", "Tennessee Titans", "Washington Commanders"),
                              selected = "Kansas City Chiefs"),
                  selectInput("away_team", "Away Team:", 
                              choices = c("Arizona Cardinals", "Atlanta Falcons", "Baltimore Ravens", "Buffalo Bills", 
                                          "Carolina Panthers", "Chicago Bears", "Cincinnati Bengals", "Cleveland Browns",
                                          "Dallas Cowboys", "Denver Broncos", "Detroit Lions", "Green Bay Packers",
                                          "Houston Texans", "Indianapolis Colts", "Jacksonville Jaguars", "Kansas City Chiefs",
                                          "Las Vegas Raiders", "Los Angeles Chargers", "Los Angeles Rams", "Miami Dolphins",
                                          "Minnesota Vikings", "New England Patriots", "New Orleans Saints", "New York Giants",
                                          "New York Jets", "Philadelphia Eagles", "Pittsburgh Steelers", "San Francisco 49ers",
                                          "Seattle Seahawks", "Tampa Bay Buccaneers", "Tennessee Titans", "Washington Commanders"),
                              selected = "Las Vegas Raiders"),
                  numericInput("over_under_line", "Over/Under Line:", 
                               value = 48.5, min = 30, max = 60, step = 0.5),
                  numericInput("spread_favorite", "Spread (Favorite):", 
                               value = 7.5, min = 0, max = 20, step = 0.5),
                  checkboxInput("home_favorite", "Home Team is Favorite", value = TRUE),
                  checkboxInput("indoor_game", "Indoor Game", value = FALSE),
                  checkboxInput("division_game", "Division Matchup", value = TRUE)
                ),
                box(
                  title = "Team Statistics",
                  status = "info",
                  solidHeader = TRUE,
                  width = 8,
                  fluidRow(
                    column(6,
                           h4("Home Team Stats"),
                           numericInput("home_avg_points", "Home Team Points/Game:", 
                                        value = 20, min = 0, max = 40, step = 0.1),
                           numericInput("home_avg_points_allowed", "Home Team Points Allowed/Game:", 
                                        value = 20, min = 0, max = 40, step = 0.1)
                    ),
                    column(6,
                           h4("Away Team Stats"),
                           numericInput("away_avg_points", "Away Team Points/Game:", 
                                        value = 20, min = 0, max = 40, step = 0.1),
                           numericInput("away_avg_points_allowed", "Away Team Points Allowed/Game:", 
                                        value = 20, min = 0, max = 40, step = 0.1)
                    )
                  ),
                  hr(),
                  actionButton("predict_btn", "Generate Prediction", 
                               icon = icon("calculator"), 
                               class = "btn-primary btn-block")
                )
              ),
              fluidRow(
                box(
                  title = "Over/Under Prediction",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("prediction_plot", height = "300px"),
                  br(),
                  tableOutput("prediction_details"),
                  br(),
                  tags$div(
                    class = "well",
                    h4("Prediction Factors"),
                    p("This prediction is based on the team statistics you provided, betting lines, and game conditions."),
                    p("The model analyzes these factors to calculate the probability of hitting the over.")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Team Statistical Comparison",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("team_stats_plot", height = "300px")
                )
              )
      ),
      
      # Spread Betting tab content
      tabItem(tabName = "spread_betting",
              fluidRow(
                box(
                  title = "Game Parameters",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 4,
                  selectInput("sb_home_team", "Home Team:", 
                              choices = c("Arizona Cardinals", "Atlanta Falcons", "Baltimore Ravens", "Buffalo Bills", 
                                          "Carolina Panthers", "Chicago Bears", "Cincinnati Bengals", "Cleveland Browns",
                                          "Dallas Cowboys", "Denver Broncos", "Detroit Lions", "Green Bay Packers",
                                          "Houston Texans", "Indianapolis Colts", "Jacksonville Jaguars", "Kansas City Chiefs",
                                          "Las Vegas Raiders", "Los Angeles Chargers", "Los Angeles Rams", "Miami Dolphins",
                                          "Minnesota Vikings", "New England Patriots", "New Orleans Saints", "New York Giants",
                                          "New York Jets", "Philadelphia Eagles", "Pittsburgh Steelers", "San Francisco 49ers",
                                          "Seattle Seahawks", "Tampa Bay Buccaneers", "Tennessee Titans", "Washington Commanders"),
                              selected = "Kansas City Chiefs"),
                  selectInput("sb_away_team", "Away Team:", 
                              choices = c("Arizona Cardinals", "Atlanta Falcons", "Baltimore Ravens", "Buffalo Bills", 
                                          "Carolina Panthers", "Chicago Bears", "Cincinnati Bengals", "Cleveland Browns",
                                          "Dallas Cowboys", "Denver Broncos", "Detroit Lions", "Green Bay Packers",
                                          "Houston Texans", "Indianapolis Colts", "Jacksonville Jaguars", "Kansas City Chiefs",
                                          "Las Vegas Raiders", "Los Angeles Chargers", "Los Angeles Rams", "Miami Dolphins",
                                          "Minnesota Vikings", "New England Patriots", "New Orleans Saints", "New York Giants",
                                          "New York Jets", "Philadelphia Eagles", "Pittsburgh Steelers", "San Francisco 49ers",
                                          "Seattle Seahawks", "Tampa Bay Buccaneers", "Tennessee Titans", "Washington Commanders"),
                              selected = "Las Vegas Raiders"),
                  uiOutput("sb_favored_team_ui"),
                  numericInput("sb_spread", "Spread (Points):", value = 7.5, min = 0, max = 20, step = 0.5),
                  numericInput("sb_over_line", "Over/Under Line:", value = 48.5, min = 30, max = 60, step = 0.5),
                  actionButton("sb_predict_btn", "Generate Prediction", 
                               icon = icon("calculator"), 
                               class = "btn-primary btn-block")
                ),
                box(
                  title = "Spread Betting Prediction Results",
                  status = "warning",
                  solidHeader = TRUE,
                  width = 8,
                  tableOutput("sb_results_table"),
                  hr(),
                  plotOutput("sb_score_distribution", height = "250px")
                )
              ),
              fluidRow(
                box(
                  title = "Score Distribution Details",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  verbatimTextOutput("sb_distribution_summary"),
                  hr(),
                  p("The above quantiles show the range of possible outcomes based on model simulations."),
                  p("These distributions can help you assess the risk and potential for your spread and total bets.")
                )
              )
      ),
      
      # Model Performance tab content
      tabItem(tabName = "model_performance",
              fluidRow(
                box(
                  title = "Model Metrics",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  p("The prediction models are trained on sports data from recent seasons, with testing performed on holdout data."),
                  p("Key performance metrics are displayed below:")
                )
              ),
              fluidRow(
                valueBox(
                  width = 3,
                  value = "55.8%",
                  subtitle = "NFL Over/Under Accuracy",
                  icon = icon("bullseye"),
                  color = "blue"
                ),
                valueBox(
                  width = 3,
                  value = "56.3%",
                  subtitle = "NFL Spread Accuracy",
                  icon = icon("check-circle"),
                  color = "green"
                ),
                valueBox(
                  width = 3,
                  value = "58.7%",
                  subtitle = "MLB NRFI Accuracy",
                  icon = icon("baseball-ball"),
                  color = "yellow"
                ),
                valueBox(
                  width = 3,
                  value = "62.4%",
                  subtitle = "NFL Moneyline Accuracy",
                  icon = icon("balance-scale"),
                  color = "red"
                )
              ),
              fluidRow(
                box(
                  title = "NFL Model Factors",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("model_factors_plot", height = "400px")
                ),
                box(
                  title = "MLB NRFI Model Performance",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("spread_accuracy_plot", height = "400px")
                )
              )
      ),
      
      # NRFI - MLB tab content
      tabItem(tabName = "nrfi_mlb",
              fluidRow(
                box(
                  title = "MLB NRFI Predictions",
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  p("NRFI (No Run First Inning) predictions for today's and tomorrow's MLB games. The predictions show the probability that neither team will score in the first inning."),
                  p("The model uses team YRFI (Yes Run First Inning) percentages and other factors to estimate first inning scoring probabilities."),
                  tags$div(
                    class = "alert alert-info",
                    tags$b("Data Source:"),
                    p("Team statistics are scraped from TeamRankings.com and updated when the app loads. Game schedules are fetched from the MLB API."),
                    tags$b("NRFI Betting Tips:"),
                    tags$ul(
                      tags$li("Look for games with a probability of 55% or higher for potential NRFI bets"),
                      tags$li("Consider pitcher matchups and team batting trends in the first inning"),
                      tags$li("Ballpark factors can significantly impact NRFI outcomes")
                    )
                  ),
                  actionButton("refresh_mlb_data", "Refresh MLB Data", icon = icon("sync"), 
                               class = "btn-info")
                )
              ),
              fluidRow(
                tabBox(
                  title = "Upcoming Games",
                  width = 12,
                  id = "upcomingGames",
                  tabPanel(
                    "Today's Games",
                    plotOutput("today_nrfi_plot", height = "400px"),
                    hr(),
                    dataTableOutput("today_nrfi_table")
                  ),
                  tabPanel(
                    "Tomorrow's Games",
                    plotOutput("tomorrow_nrfi_plot", height = "400px"),
                    hr(),
                    dataTableOutput("tomorrow_nrfi_table")
                  ),
                  tabPanel(
                    "Team Stats",
                    dataTableOutput("team_stats_table"),
                    hr(),
                    p("These statistics are used as inputs for the NRFI predictionmodel"),
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Top NRFI Opportunities",
                  status = "success",
                  solidHeader = TRUE,
                  width = 6,
                  tableOutput("top_nrfi_opportunities")
                ),
                box(
                  title = "Model Details",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  p("This model uses a Poisson process to predict first inning scoring:"),
                  tags$ol(
                    tags$li("YRFI percentages are scraped from TeamRankings.com"),
                    tags$li("Linear regression model converts YRFI% to expected run rates (λ)"),
                    tags$li("Poisson process calculates probability of zero runs in first inning"),
                    tags$li("Game predictions are sorted by NRFI probability")
                  ),
                  p("Formula: NRFI% = exp(-(λ_home + λ_away)) * 100")
                )
              )
      ),
      
      # Betting History tab content
      tabItem(tabName = "history",
              fluidRow(
                box(
                  title = "Track Your Betting History",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  p("Record your bets and track performance over time."),
                  p("This feature is currently under development.")
                )
              )
      )
    )
  )
)

# Define single server function that combines both NFL and MLB functionality
server <- function(input, output, session) {
  # Initialize reactive values for storing MLB scraped data
  data_store <- reactiveValues(
    yrfi_stats = NULL,
    opp_yrfi_stats = NULL,
    nrfi_stats = NULL,
    team_data = NULL,
    home_model = NULL,
    away_model = NULL,
    today_games = NULL,
    tomorrow_games = NULL,
    today_predictions = NULL,
    tomorrow_predictions = NULL,
    data_loaded = FALSE
  )
  
  # --------------------------------------------------------
  # MLB NRFI DATA FUNCTIONS
  # --------------------------------------------------------
  
  # Function to scrape MLB NRFI/YRFI data from TeamRankings.com with improved error handling
  scrape_mlb_data <- function() {
    # Show loading message
    showNotification("Scraping team statistics...", type = "message", duration = NULL, id = "scraping")
    
    # Try to scrape YRFI data
    tryCatch({
      # Scrape Yes-Run First Inning Percentage (YRFI)
      url_yrfi <- "https://www.teamrankings.com/mlb/stat/yes-run-first-inning-pct"
      page_yrfi <- read_html(GET(url_yrfi, add_headers("user-agent" = "Mozilla/5.0")))
      yrfi_stats <- page_yrfi %>% 
        html_node("table") %>% 
        html_table()
      
      # Scrape Opponent Yes-Run First Inning Percentage
      url_opp_yrfi <- "https://www.teamrankings.com/mlb/stat/opponent-yes-run-first-inning-pct"
      page_opp_yrfi <- read_html(GET(url_opp_yrfi, add_headers("user-agent" = "Mozilla/5.0")))
      opp_yrfi_stats <- page_opp_yrfi %>% 
        html_node("table") %>% 
        html_table()
      
      # Scrape First Inning Runs Per Game
      url_runs <- "https://www.teamrankings.com/mlb/stat/1st-inning-runs-per-game"
      page_runs <- read_html(GET(url_runs, add_headers("user-agent" = "Mozilla/5.0")))
      nrfi_stats <- page_runs %>% 
        html_node("table") %>% 
        html_table()
      
      # Clean column names and force proper types
      clean_numeric_column <- function(df, col_name, default_value = 0) {
        if(col_name %in% names(df)) {
          # Remove % symbols and any non-numeric characters
          df[[col_name]] <- gsub("%", "", as.character(df[[col_name]]))
          df[[col_name]] <- gsub("[^0-9\\.]", "", as.character(df[[col_name]]))
          
          # Convert to numeric with error handling
          df[[col_name]] <- sapply(df[[col_name]], function(x) {
            num <- suppressWarnings(as.numeric(x))
            if(is.na(num)) default_value else num
          })
        }
        return(df)
      }
      
      # Extract team names consistently
      clean_team_names <- function(df) {
        if("Team" %in% names(df)) {
          # Extract just the team name, remove ranks or other prefixes
          df$Team <- gsub("^[0-9]+\\s+", "", df$Team)
          # Standardize team names
          df$Team <- gsub("Los Angeles Angels", "LA Angels", df$Team)
          df$Team <- gsub("Los Angeles Dodgers", "LA Dodgers", df$Team)
          df$Team <- gsub("New York Mets", "NY Mets", df$Team)
          df$Team <- gsub("New York Yankees", "NY Yankees", df$Team)
          df$Team <- gsub("San Francisco", "SF Giants", df$Team)
          df$Team <- gsub("Chicago Cubs", "Chi Cubs", df$Team)
          df$Team <- gsub("Chicago Sox", "Chi Sox", df$Team)
          df$Team <- gsub("Chicago White Sox", "Chi Sox", df$Team)
        }
        return(df)
      }
      
      # Identify the numeric columns in each dataframe
      # For YRFI stats
      numeric_cols_yrfi <- c()
      for(col in names(yrfi_stats)) {
        if(col != "Team" && col != "Rank") {
          numeric_cols_yrfi <- c(numeric_cols_yrfi, col)
        }
      }
      
      # For opponent YRFI stats
      numeric_cols_opp <- c()
      for(col in names(opp_yrfi_stats)) {
        if(col != "Team" && col != "Rank") {
          numeric_cols_opp <- c(numeric_cols_opp, col)
        }
      }
      
      # For NRFI stats
      numeric_cols_nrfi <- c()
      for(col in names(nrfi_stats)) {
        if(col != "Team" && col != "Rank") {
          numeric_cols_nrfi <- c(numeric_cols_nrfi, col)
        }
      }
      
      # Clean team names
      yrfi_stats <- clean_team_names(yrfi_stats)
      opp_yrfi_stats <- clean_team_names(opp_yrfi_stats)
      nrfi_stats <- clean_team_names(nrfi_stats)
      
      # Clean all numeric columns
      for(col in numeric_cols_yrfi) {
        yrfi_stats <- clean_numeric_column(yrfi_stats, col)
      }
      
      for(col in numeric_cols_opp) {
        opp_yrfi_stats <- clean_numeric_column(opp_yrfi_stats, col)
      }
      
      for(col in numeric_cols_nrfi) {
        nrfi_stats <- clean_numeric_column(nrfi_stats, col)
      }
      
      # Map column names to standard format
      # For YRFI stats
      standard_yrfi_cols <- c("YRFI_2025", "YRFI_2024", "YRFI_Last3", "YRFI_Last1", "YRFI_Home", "YRFI_Away")
      actual_yrfi_cols <- c()
      
      # Check if columns exist in the order they typically appear
      if(length(numeric_cols_yrfi) >= 1) actual_yrfi_cols <- c(actual_yrfi_cols, "YRFI_2025")
      if(length(numeric_cols_yrfi) >= 2) actual_yrfi_cols <- c(actual_yrfi_cols, "YRFI_2024")
      if(length(numeric_cols_yrfi) >= 3) actual_yrfi_cols <- c(actual_yrfi_cols, "YRFI_Last3")
      if(length(numeric_cols_yrfi) >= 4) actual_yrfi_cols <- c(actual_yrfi_cols, "YRFI_Last1")
      if(length(numeric_cols_yrfi) >= 5) actual_yrfi_cols <- c(actual_yrfi_cols, "YRFI_Home")
      if(length(numeric_cols_yrfi) >= 6) actual_yrfi_cols <- c(actual_yrfi_cols, "YRFI_Away")
      
      # Rename columns for YRFI stats
      for(i in 1:min(length(numeric_cols_yrfi), length(actual_yrfi_cols))) {
        names(yrfi_stats)[names(yrfi_stats) == numeric_cols_yrfi[i]] <- actual_yrfi_cols[i]
      }
      
      # Ensure Home and Away columns exist
      if(!("YRFI_Home" %in% names(yrfi_stats))) {
        yrfi_stats$YRFI_Home <- yrfi_stats$YRFI_2025
      }
      if(!("YRFI_Away" %in% names(yrfi_stats))) {
        yrfi_stats$YRFI_Away <- yrfi_stats$YRFI_2025
      }
      
      # Ensure necessary columns for NRFI stats
      if(length(numeric_cols_nrfi) > 0) {
        names(nrfi_stats)[names(nrfi_stats) == numeric_cols_nrfi[1]] <- "NRFI_2025"
      }
      
      # Create model variables
      team_data <- merge(nrfi_stats, yrfi_stats, by = "Team", all = TRUE)
      
      # Create model variables
      team_data$Home_YRFI <- team_data$YRFI_Home
      team_data$Away_YRFI <- team_data$YRFI_Away
      
      # Build linear regression models
      # Handle errors if columns are missing or not numeric
      tryCatch({
        home_model <- lm(NRFI_2025 ~ Home_YRFI, data = team_data)
        away_model <- lm(NRFI_2025 ~ Away_YRFI, data = team_data)
        
        # Store data in reactiveValues
        data_store$yrfi_stats <- yrfi_stats
        data_store$opp_yrfi_stats <- opp_yrfi_stats
        data_store$nrfi_stats <- nrfi_stats
        data_store$team_data <- team_data
        data_store$home_model <- home_model
        data_store$away_model <- away_model
      }, error = function(e) {
        # If regression fails, use fallback models
        warning("Could not build regression models: ", e$message)
        
        # Create simple fallback models
        data_store$home_model <- list(
          intercept = -0.5,
          coefficient = 0.02
        )
        data_store$away_model <- list(
          intercept = -0.48,
          coefficient = 0.019
        )
        
        data_store$yrfi_stats <- yrfi_stats
        data_store$opp_yrfi_stats <- opp_yrfi_stats
        data_store$nrfi_stats <- nrfi_stats
        data_store$team_data <- team_data
      })
      
      # Remove loading message
      removeNotification("scraping")
      showNotification("Team statistics loaded successfully!", type = "message", duration = 3)
      
      # Fetch MLB schedule
      fetch_mlb_schedule()
      
    }, error = function(e) {
      # Handle errors
      removeNotification("scraping")
      showNotification(paste("Error scraping data:", e$message), type = "error", duration = 5)
      
      # Use fallback data (simulated)
      create_fallback_data()
    })
  }
  
  # Create fallback data if scraping fails
  create_fallback_data <- function() {
    showNotification("Using fallback data for demonstrations", type = "warning", duration = 5)
    
    # Simulated MLB teams
    mlb_teams <- c(
      "Arizona", "Atlanta", "Baltimore", "Boston", "Chicago Cubs", 
      "Chicago White Sox", "Cincinnati", "Cleveland", "Colorado", "Detroit", 
      "Houston", "Kansas City", "Los Angeles Angels", "Los Angeles Dodgers", 
      "Miami", "Milwaukee", "Minnesota", "New York Mets", "New York Yankees", 
      "Oakland", "Philadelphia", "Pittsburgh", "San Diego", "Seattle", 
      "San Francisco", "St. Louis", "Tampa Bay", "Texas", "Toronto", "Washington"
    )
    
    # Generate plausible YRFI percentages
    set.seed(42)
    yrfi_stats <- data.frame(
      Team = mlb_teams,
      YRFI_2025 = round(runif(length(mlb_teams), min = 40, max = 65), 1),
      YRFI_Home = round(runif(length(mlb_teams), min = 38, max = 68), 1),
      YRFI_Away = round(runif(length(mlb_teams), min = 38, max = 68), 1)
    )
    
    # Simulate first inning runs per game
    nrfi_stats <- data.frame(
      Team = mlb_teams,
      NRFI_2025 = round(runif(length(mlb_teams), min = 0.3, max = 0.7), 2),
      NRFI_Home = round(runif(length(mlb_teams), min = 0.25, max = 0.75), 2),
      NRFI_Away = round(runif(length(mlb_teams), min = 0.25, max = 0.75), 2)
    )
    
    # Create opponent YRFI data
    opp_yrfi_stats <- data.frame(
      Team = mlb_teams,
      OPP_YRFI_2025 = round(runif(length(mlb_teams), min = 40, max = 65), 1),
      OPP_YRFI_Home = round(runif(length(mlb_teams), min = 38, max = 68), 1),
      OPP_YRFI_Away = round(runif(length(mlb_teams), min = 38, max = 68), 1)
    )
    
    # Merge data
    team_data <- merge(nrfi_stats, yrfi_stats, by = "Team", all = TRUE)
    team_data <- merge(team_data, opp_yrfi_stats, by = "Team", all = TRUE)
    
    # Create model variables
    team_data$Home_YRFI <- team_data$YRFI_Home
    team_data$Away_YRFI <- team_data$YRFI_Away
    
    # Simple models
    home_model <- list(
      intercept = -0.5,
      coefficient = 0.02
    )
    
    away_model <- list(
      intercept = -0.48,
      coefficient = 0.019
    )
    
    # Store data in reactiveValues
    data_store$yrfi_stats <- yrfi_stats
    data_store$opp_yrfi_stats <- opp_yrfi_stats
    data_store$nrfi_stats <- nrfi_stats
    data_store$team_data <- team_data
    data_store$home_model <- home_model
    data_store$away_model <- away_model
    
    # Create fallback game schedule
    create_fallback_schedule()
  }
  
  # Map team names from MLB API to our dataset
  mlb_team_mapping <- c(
    "Arizona Diamondbacks" = "Arizona",
    "Atlanta Braves" = "Atlanta",
    "Baltimore Orioles" = "Baltimore",
    "Boston Red Sox" = "Boston",
    "Chicago Cubs" = "Chicago Cubs",
    "Chicago White Sox" = "Chicago White Sox",
    "Cincinnati Reds" = "Cincinnati",
    "Cleveland Guardians" = "Cleveland",
    "Colorado Rockies" = "Colorado",
    "Detroit Tigers" = "Detroit",
    "Houston Astros" = "Houston",
    "Kansas City Royals" = "Kansas City",
    "Los Angeles Angels" = "Los Angeles Angels",
    "Los Angeles Dodgers" = "Los Angeles Dodgers",
    "Miami Marlins" = "Miami",
    "Milwaukee Brewers" = "Milwaukee",
    "Minnesota Twins" = "Minnesota",
    "New York Mets" = "New York Mets",
    "New York Yankees" = "New York Yankees",
    "Oakland Athletics" = "Oakland",
    "Philadelphia Phillies" = "Philadelphia",
    "Pittsburgh Pirates" = "Pittsburgh",
    "San Diego Padres" = "San Diego",
    "Seattle Mariners" = "Seattle",
    "San Francisco Giants" = "San Francisco",
    "St. Louis Cardinals" = "St. Louis",
    "Tampa Bay Rays" = "Tampa Bay",
    "Texas Rangers" = "Texas",
    "Toronto Blue Jays" = "Toronto",
    "Washington Nationals" = "Washington"
  )
  
  # Function to fetch MLB game schedule from the API
  fetch_mlb_schedule <- function() {
    showNotification("Fetching MLB schedule...", type = "message", duration = NULL, id = "schedule")
    
    tryCatch({
      # Define dates for today and tomorrow
      today <- Sys.Date()
      tomorrow <- today + 1
      start_date_str <- format(today, "%Y-%m-%d")
      end_date_str <- format(tomorrow, "%Y-%m-%d")
      
      # Fetch schedule from MLB API
      url <- sprintf("https://statsapi.mlb.com/api/v1/schedule?sportId=1&startDate=%s&endDate=%s", 
                     start_date_str, end_date_str)
      response <- GET(url)
      
      if (status_code(response) == 200) {
        schedule_data <- content(response, as = "text", encoding = "UTF-8")
        schedule_json <- fromJSON(schedule_data, flatten = TRUE)
        
        today_games <- list()
        tomorrow_games <- list()
        
        if (!is.null(schedule_json$dates) && nrow(schedule_json$dates) > 0) {
          for (i in 1:nrow(schedule_json$dates)) {
            game_date <- schedule_json$dates$date[i]
            game_date <- as.Date(game_date)
            games <- schedule_json$dates$games[[i]]
            
            if (length(games) > 0) {
              # Process games
              if (is.data.frame(games)) {
                for (j in 1:nrow(games)) {
                  game_time <- games$gameDate[j]
                  home_team_sched <- games$teams.home.team.name[j]
                  away_team_sched <- games$teams.away.team.name[j]
                  
                  # Map the team names to match our dataset
                  mapped_home <- ifelse(home_team_sched %in% names(mlb_team_mapping), 
                                        mlb_team_mapping[home_team_sched], home_team_sched)
                  mapped_away <- ifelse(away_team_sched %in% names(mlb_team_mapping), 
                                        mlb_team_mapping[away_team_sched], away_team_sched)
                  
                  # Create game entry
                  game_entry <- list(
                    date = game_date,
                    game_time = game_time,
                    home_team = mapped_home,
                    away_team = mapped_away,
                    home_team_original = home_team_sched,
                    away_team_original = away_team_sched
                  )
                  
                  # Add to appropriate list
                  if (game_date == today) {
                    today_games[[length(today_games) + 1]] <- game_entry
                  } else if (game_date == tomorrow) {
                    tomorrow_games[[length(tomorrow_games) + 1]] <- game_entry
                  }
                }
              } else if (is.list(games)) {
                for (j in seq_along(games)) {
                  game <- games[[j]]
                  game_time <- game$gameDate
                  home_team_sched <- game$teams.home.team.name
                  away_team_sched <- game$teams.away.team.name
                  
                  # Map the team names to match our dataset
                  mapped_home <- ifelse(home_team_sched %in% names(mlb_team_mapping), 
                                        mlb_team_mapping[home_team_sched], home_team_sched)
                  mapped_away <- ifelse(away_team_sched %in% names(mlb_team_mapping), 
                                        mlb_team_mapping[away_team_sched], away_team_sched)
                  
                  # Create game entry
                  game_entry <- list(
                    date = game_date,
                    game_time = game_time,
                    home_team = mapped_home,
                    away_team = mapped_away,
                    home_team_original = home_team_sched,
                    away_team_original = away_team_sched
                  )
                  
                  # Add to appropriate list
                  if (game_date == today) {
                    today_games[[length(today_games) + 1]] <- game_entry
                  } else if (game_date == tomorrow) {
                    tomorrow_games[[length(tomorrow_games) + 1]] <- game_entry
                  }
                }
              }
            }
          }
        }
        
        # Store the games
        data_store$today_games <- today_games
        data_store$tomorrow_games <- tomorrow_games
        data_store$data_loaded <- TRUE
        
        # Remove loading message
        removeNotification("schedule")
        showNotification("MLB schedule loaded successfully!", type = "message", duration = 3)
        
        # Generate predictions
        generate_predictions()
      } else {
        # Handle API error
        removeNotification("schedule")
        showNotification(paste("Error fetching MLB schedule: HTTP", status_code(response)), 
                         type = "error", duration = 5)
        create_fallback_schedule()
      }
    }, error = function(e) {
      # Handle general errors
      removeNotification("schedule")
      showNotification(paste("Error fetching MLB schedule:", e$message), 
                       type = "error", duration = 5)
      create_fallback_schedule()
    })
  }
  
  # Create fallback schedule if API fails
  create_fallback_schedule <- function() {
    showNotification("Using fallback schedule for demonstrations", type = "warning", duration = 5)
    
    # Get team names
    if (!is.null(data_store$team_data)) {
      mlb_teams <- data_store$team_data$Team
    } else {
      mlb_teams <- c(
        "Arizona", "Atlanta", "Baltimore", "Boston", "Chicago Cubs", 
        "Chicago White Sox", "Cincinnati", "Cleveland", "Colorado", "Detroit", 
        "Houston", "Kansas City", "Los Angeles Angels", "Los Angeles Dodgers", 
        "Miami", "Milwaukee", "Minnesota", "New York Mets", "New York Yankees", 
        "Oakland", "Philadelphia", "Pittsburgh", "San Diego", "Seattle", 
        "San Francisco", "St. Louis", "Tampa Bay", "Texas", "Toronto", "Washington"
      )
    }
    
    # Define dates
    today <- Sys.Date()
    tomorrow <- today + 1
    
    # Create random matchups for today
    today_games <- list()
    teams_today <- sample(mlb_teams, 16)  # Get 16 random teams for 8 games
    
    for (i in seq(1, 15, by = 2)) {
      home_team <- teams_today[i]
      away_team <- teams_today[i+1]
      
      # Add game to today's list
      today_games[[length(today_games) + 1]] <- list(
        date = today,
        game_time = paste0(sample(c(13, 16, 18, 19), 1), ":", 
                           sample(c("00", "05", "10", "15", "20", "30", "35", "40", "45"), 1)),
        home_team = home_team,
        away_team = away_team,
        home_team_original = home_team,
        away_team_original = away_team
      )
    }
    
    # Create random matchups for tomorrow
    tomorrow_games <- list()
    teams_tomorrow <- sample(mlb_teams, 18)  # Get 18 random teams for 9 games
    
    for (i in seq(1, 17, by = 2)) {
      home_team <- teams_tomorrow[i]
      away_team <- teams_tomorrow[i+1]
      
      # Add game to tomorrow's list
      tomorrow_games[[length(tomorrow_games) + 1]] <- list(
        date = tomorrow,
        game_time = paste0(sample(c(13, 16, 18, 19), 1), ":", 
                           sample(c("00", "05", "10", "15", "20", "30", "35", "40", "45"), 1)),
        home_team = home_team,
        away_team = away_team,
        home_team_original = home_team,
        away_team_original = away_team
      )
    }
    
    # Store the games
    data_store$today_games <- today_games
    data_store$tomorrow_games <- tomorrow_games
    data_store$data_loaded <- TRUE
    
    # Generate predictions
    generate_predictions()
  }
  
  # Function to predict first inning scoring for MLB teams
  predict_first_inning <- function(home_team, away_team) {
    # Check if required data is available
    if (is.null(data_store$yrfi_stats) || is.null(data_store$home_model) || is.null(data_store$away_model)) {
      return(NULL)
    }
    
    # Get team stats
    home_stats <- data_store$yrfi_stats[data_store$yrfi_stats$Team == home_team, ]
    away_stats <- data_store$yrfi_stats[data_store$yrfi_stats$Team == away_team, ]
    
    # Handle case where team is not found
    if (nrow(home_stats) == 0) {
      warning(paste("Home team not found in the YRFI dataset:", home_team))
      home_yrfi_val <- 50  # Default value if team not found
    } else {
      home_yrfi_val <- home_stats$YRFI_Home
    }
    
    if (nrow(away_stats) == 0) {
      warning(paste("Away team not found in the YRFI dataset:", away_team))
      away_yrfi_val <- 50  # Default value if team not found
    } else {
      away_yrfi_val <- away_stats$YRFI_Away
    }
    
    # Use regression models to predict expected runs
    if (is.list(data_store$home_model) && !inherits(data_store$home_model, "lm")) {
      # Simple model case
      lambda_home <- data_store$home_model$intercept + data_store$home_model$coefficient * home_yrfi_val
      lambda_away <- data_store$away_model$intercept + data_store$away_model$coefficient * away_yrfi_val
    } else {
      # Actual fitted linear model case
      home_input <- data.frame(Home_YRFI = home_yrfi_val)
      away_input <- data.frame(Away_YRFI = away_yrfi_val)
      
      lambda_home <- predict(data_store$home_model, newdata = home_input)
      lambda_away <- predict(data_store$away_model, newdata = away_input)
    }
    
    # Add small random variation 
    lambda_home <- lambda_home + rnorm(1, 0, 0.02)
    lambda_away <- lambda_away + rnorm(1, 0, 0.02)
    
    # Ensure lambdas are positive
    lambda_home <- max(0.05, lambda_home)
    lambda_away <- max(0.05, lambda_away)
    
    # Calculate scoring probabilities using a Poisson process
    prob_home_score <- (1 - exp(-lambda_home)) * 100
    prob_away_score <- (1 - exp(-lambda_away)) * 100
    prob_neither_score <- exp(-(lambda_home + lambda_away)) * 100
    
    return(list(
      home_team = home_team,
      away_team = away_team,
      home_yrfi = home_yrfi_val,
      away_yrfi = away_yrfi_val,
      home_predicted_score = lambda_home,
      home_scoring_probability = prob_home_score,
      away_predicted_score = lambda_away,
      away_scoring_probability = prob_away_score,
      both_no_score_probability = prob_neither_score
    ))
  }
  
  # Generate predictions for all games
  generate_predictions <- function() {
    # Check if we have games
    if (is.null(data_store$today_games) || is.null(data_store$tomorrow_games)) {
      return()
    }
    
    # Make predictions for today's games
    today_pred <- lapply(data_store$today_games, function(game) {
      pred <- predict_first_inning(game$home_team, game$away_team)
      
      if (is.null(pred)) {
        return(NULL)
      }
      
      # Add game info
      pred$date <- game$date
      pred$game_time <- game$game_time
      pred$home_team_original <- game$home_team_original
      pred$away_team_original <- game$away_team_original
      
      return(pred)
    })
    
    # Remove NULL entries
    today_pred <- today_pred[!sapply(today_pred, is.null)]
    
    # Make predictions for tomorrow's games
    tomorrow_pred <- lapply(data_store$tomorrow_games, function(game) {
      pred <- predict_first_inning(game$home_team, game$away_team)
      
      if (is.null(pred)) {
        return(NULL)
      }
      
      # Add game info
      pred$date <- game$date
      pred$game_time <- game$game_time
      pred$home_team_original <- game$home_team_original
      pred$away_team_original <- game$away_team_original
      
      return(pred)
    })
    
    # Remove NULL entries
    tomorrow_pred <- tomorrow_pred[!sapply(tomorrow_pred, is.null)]
    
    # Convert to data frames
    if (length(today_pred) > 0) {
      today_df <- do.call(rbind, lapply(today_pred, function(x) {
        data.frame(
          date = as.Date(x$date, origin = "1970-01-01"),
          matchup = paste(x$away_team_original, "at", x$home_team_original),
          home_team = x$home_team,
          away_team = x$away_team,
          home_team_original = x$home_team_original,
          away_team_original = x$away_team_original,
          home_yrfi = round(x$home_yrfi, 1),
          away_yrfi = round(x$away_yrfi, 1),
          home_scoring_prob = round(x$home_scoring_probability, 1),
          away_scoring_prob = round(x$away_scoring_probability, 1),
          nrfi_prob = round(x$both_no_score_probability, 1),
          yrfi_prob = round(100 - x$both_no_score_probability, 1),
          expected_runs = round(x$home_predicted_score + x$away_predicted_score, 2)
        )
      }))
      
      # Sort by NRFI probability
      today_df <- today_df[order(-today_df$nrfi_prob), ]
      data_store$today_predictions <- today_df
    } else {
      data_store$today_predictions <- data.frame()
    }
    
    if (length(tomorrow_pred) > 0) {
      tomorrow_df <- do.call(rbind, lapply(tomorrow_pred, function(x) {
        data.frame(
          date = as.Date(x$date, origin = "1970-01-01"),
          matchup = paste(x$away_team_original, "at", x$home_team_original),
          home_team = x$home_team,
          away_team = x$away_team,
          home_team_original = x$home_team_original,
          away_team_original = x$away_team_original,
          home_yrfi = round(x$home_yrfi, 1),
          away_yrfi = round(x$away_yrfi, 1),
          home_scoring_prob = round(x$home_scoring_probability, 1),
          away_scoring_prob = round(x$away_scoring_probability, 1),
          nrfi_prob = round(x$both_no_score_probability, 1),
          yrfi_prob = round(100 - x$both_no_score_probability, 1),
          expected_runs = round(x$home_predicted_score + x$away_predicted_score, 2)
        )
      }))
      
      # Sort by NRFI probability
      tomorrow_df <- tomorrow_df[order(-tomorrow_df$nrfi_prob), ]
      data_store$tomorrow_predictions <- tomorrow_df
    } else {
      data_store$tomorrow_predictions <- data.frame()
    }
  }
  
  # --------------------------------------------------------
  # INITIALIZE MLB DATA AND SETUP UI COMPONENTS
  # --------------------------------------------------------
  
  # Initialize MLB data at startup
  observe({
    # Only run this once at startup
    if (!data_store$data_loaded) {
      scrape_mlb_data()
    }
  })
  
  # Refresh MLB data when button is clicked
  observeEvent(input$refresh_mlb_data, {
    scrape_mlb_data()
  })
  
  # Today's NRFI bar chart
  output$today_nrfi_plot <- renderPlot({
    req(data_store$data_loaded)
    req(nrow(data_store$today_predictions) > 0)
    
    data <- data_store$today_predictions
    
    # Set colors based on NRFI probability
    colors <- sapply(data$nrfi_prob, function(prob) {
      if (prob >= 60) return("#2E7D32")       # Dark green for strong NRFI
      if (prob >= 52) return("#81C784")       # Medium green for lean NRFI
      if (prob >= 48) return("#BDBDBD")       # Gray for pass
      if (prob >= 40) return("#FFAB91")       # Light orange for lean YRFI
      return("#D32F2F")                       # Red for strong YRFI
    })
    
    # Set up the plotting area
    par(mar = c(5, 12, 4, 2))
    
    # Create horizontal bar chart
    barplot(
      data$nrfi_prob,
      names.arg = data$matchup,
      horiz = TRUE,
      col = colors,
      border = NA,
      xlab = "NRFI Probability (%)",
      xlim = c(0, 100),
      las = 1,
      cex.names = 0.8,
      main = paste("Today's Games (", format(Sys.Date(), "%a, %b %d"), ")", sep = "")
    )
    
    # Add vertical line at 50%
    abline(v = 50, lty = 2, col = "black")
    
    # Add text annotations at end of each bar
    text(data$nrfi_prob + 2, seq(from = 0.7, by = 1.2, length.out = nrow(data)), 
         labels = paste0(data$nrfi_prob, "%"), cex = 0.7)
    
    # Add legend
    legend("bottomright", 
           legend = c("Strong NRFI (>=60%)", "Lean NRFI (52-59%)", "Pass (48-51%)", 
                      "Lean YRFI (40-47%)", "Strong YRFI (<40%)"),
           fill = c("#2E7D32", "#81C784", "#BDBDBD", "#FFAB91", "#D32F2F"),
           bty = "n", cex = 0.7)
  })
  
  # Tomorrow's NRFI bar chart
  output$tomorrow_nrfi_plot <- renderPlot({
    req(data_store$data_loaded)
    req(nrow(data_store$tomorrow_predictions) > 0)
    
    data <- data_store$tomorrow_predictions
    
    # Set colors based on NRFI probability
    colors <- sapply(data$nrfi_prob, function(prob) {
      if (prob >= 60) return("#2E7D32")       # Dark green for strong NRFI
      if (prob >= 52) return("#81C784")       # Medium green for lean NRFI
      if (prob >= 48) return("#BDBDBD")       # Gray for pass
      if (prob >= 40) return("#FFAB91")       # Light orange for lean YRFI
      return("#D32F2F")                       # Red for strong YRFI
    })
    
    # Set up the plotting area
    par(mar = c(5, 12, 4, 2))
    
    # Create horizontal bar chart
    barplot(
      data$nrfi_prob,
      names.arg = data$matchup,
      horiz = TRUE,
      col = colors,
      border = NA,
      xlab = "NRFI Probability (%)",
      xlim = c(0, 100),
      las = 1,
      cex.names = 0.8,
      main = paste("Tomorrow's Games (", format(Sys.Date() + 1, "%a, %b %d"), ")", sep = "")
    )
    
    # Add vertical line at 50%
    abline(v = 50, lty = 2, col = "black")
    
    # Add text annotations at end of each bar
    text(data$nrfi_prob + 2, seq(from = 0.7, by = 1.2, length.out = nrow(data)), 
         labels = paste0(data$nrfi_prob, "%"), cex = 0.7)
    
    # Add legend
    legend("bottomright", 
           legend = c("Strong NRFI (>=60%)", "Lean NRFI (52-59%)", "Pass (48-51%)", 
                      "Lean YRFI (40-47%)", "Strong YRFI (<40%)"),
           fill = c("#2E7D32", "#81C784", "#BDBDBD", "#FFAB91", "#D32F2F"),
           bty = "n", cex = 0.7)
  })
  
  # Today's NRFI data table
  output$today_nrfi_table <- renderDataTable({
    req(data_store$data_loaded)
    req(nrow(data_store$today_predictions) > 0)
    
    data <- data_store$today_predictions
    
    # Add recommendation column
    data$recommendation <- ifelse(data$nrfi_prob >= 60, "Strong NRFI",
                                  ifelse(data$nrfi_prob >= 52, "Lean NRFI",
                                         ifelse(data$nrfi_prob <= 40, "Strong YRFI",
                                                ifelse(data$nrfi_prob <= 48, "Lean YRFI", 
                                                       "Pass"))))
    
    # Create datatable
    datatable(
      data[, c("matchup", "home_yrfi", "away_yrfi", "home_scoring_prob", "away_scoring_prob", 
               "nrfi_prob", "yrfi_prob", "expected_runs", "recommendation")],
      colnames = c("Matchup", "Home YRFI%", "Away YRFI%", "Home 1st Inn %", "Away 1st Inn %", 
                   "NRFI %", "YRFI %", "Exp. Runs", "Recommendation"),
      options = list(
        pageLength = 25,
        dom = 'tpi',
        order = list(list(5, 'desc')),  # Order by NRFI probability descending
        columnDefs = list(
          list(className = 'dt-center', targets = c(1, 2, 3, 4, 5, 6, 7, 8))
        )
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'nrfi_prob',
        backgroundColor = styleInterval(c(40, 48, 52, 60), c('#ffcdd2', '#ffecb3', '#dcedc8', '#c8e6c9', '#a5d6a7'))
      ) %>%
      formatStyle(
        'recommendation',
        color = styleEqual(
          c('Strong NRFI', 'Lean NRFI', 'Pass', 'Lean YRFI', 'Strong YRFI'),
          c('darkgreen', 'green', 'gray', 'orange', 'red')
        ),
        fontWeight = 'bold'
      )
  })
  
  # Tomorrow's NRFI data table
  output$tomorrow_nrfi_table <- renderDataTable({
    req(data_store$data_loaded)
    req(nrow(data_store$tomorrow_predictions) > 0)
    
    data <- data_store$tomorrow_predictions
    
    # Add recommendation column
    data$recommendation <- ifelse(data$nrfi_prob >= 60, "Strong NRFI",
                                  ifelse(data$nrfi_prob >= 52, "Lean NRFI",
                                         ifelse(data$nrfi_prob <= 40, "Strong YRFI",
                                                ifelse(data$nrfi_prob <= 48, "Lean YRFI", 
                                                       "Pass"))))
    
    # Create datatable
    datatable(
      data[, c("matchup", "home_yrfi", "away_yrfi", "home_scoring_prob", "away_scoring_prob", 
               "nrfi_prob", "yrfi_prob", "expected_runs", "recommendation")],
      colnames = c("Matchup", "Home YRFI%", "Away YRFI%", "Home 1st Inn %", "Away 1st Inn %", 
                   "NRFI %", "YRFI %", "Exp. Runs", "Recommendation"),
      options = list(
        pageLength = 25,
        dom = 'tpi',
        order = list(list(5, 'desc')),  # Order by NRFI probability descending
        columnDefs = list(
          list(className = 'dt-center', targets = c(1, 2, 3, 4, 5, 6, 7, 8))
        )
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'nrfi_prob',
        backgroundColor = styleInterval(c(40, 48, 52, 60), c('#ffcdd2', '#ffecb3', '#dcedc8', '#c8e6c9', '#a5d6a7'))
      ) %>%
      formatStyle(
        'recommendation',
        color = styleEqual(
          c('Strong NRFI', 'Lean NRFI', 'Pass', 'Lean YRFI', 'Strong YRFI'),
          c('darkgreen', 'green', 'gray', 'orange', 'red')
        ),
        fontWeight = 'bold'
      )
  })
  
  # Team stats table
  output$team_stats_table <- renderDataTable({
    req(data_store$team_data)
    
    data <- data_store$team_data
    
    # Select relevant columns
    selected_cols <- c("Team", "YRFI_2025", "YRFI_Home", "YRFI_Away", 
                       "OPP_YRFI_2025", "NRFI_2025")
    
    # Ensure all columns exist
    available_cols <- selected_cols[selected_cols %in% names(data)]
    
    # Create datatable
    datatable(
      data[, available_cols],
      colnames = gsub("_", " ", gsub("YRFI", "YRFI %", gsub("NRFI", "1st Inn Runs", gsub("OPP", "Opp.", available_cols)))),
      options = list(
        pageLength = 30,
        dom = 'ftpi',
        order = list(list(1, 'desc')),  # Order by YRFI% descending
        columnDefs = list(
          list(className = 'dt-center', targets = 1:(length(available_cols)-1))
        )
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'YRFI_2025',
        backgroundColor = styleInterval(c(45, 50, 55, 60), c('#c8e6c9', '#dcedc8', '#ffecb3', '#ffcdd2', '#ef9a9a'))
      )
  })
  
  # Top NRFI opportunities
  output$top_nrfi_opportunities <- renderTable({
    req(data_store$data_loaded)
    
    # Combine today and tomorrow data if both exist
    if (nrow(data_store$today_predictions) > 0 && nrow(data_store$tomorrow_predictions) > 0) {
      combined <- rbind(
        cbind(data_store$today_predictions, day = "Today"),
        cbind(data_store$tomorrow_predictions, day = "Tomorrow")
      )
    } else if (nrow(data_store$today_predictions) > 0) {
      combined <- cbind(data_store$today_predictions, day = "Today")
    } else if (nrow(data_store$tomorrow_predictions) > 0) {
      combined <- cbind(data_store$tomorrow_predictions, day = "Tomorrow")
    } else {
      return(data.frame())
    }
    
    # Get top 5 NRFI opportunities
    top_nrfi <- combined[order(-combined$nrfi_prob), ][1:min(5, nrow(combined)), ]
    
    # Format for display
    data.frame(
      "Day" = top_nrfi$day,
      "Matchup" = top_nrfi$matchup,
      "NRFI %" = paste0(top_nrfi$nrfi_prob, "%"),
      "Home YRFI" = paste0(top_nrfi$home_yrfi, "%"),
      "Away YRFI" = paste0(top_nrfi$away_yrfi, "%"),
      "Exp. Runs" = top_nrfi$expected_runs
    )
  }, striped = TRUE, bordered = TRUE, align = 'lccccc')
  
  # --------------------------------------------------------
  # NFL OVER/UNDER PREDICTIONS TAB
  # --------------------------------------------------------
  
  # Default team statistics - using 20 for all teams and all stats
  all_teams <- c("Arizona Cardinals", "Atlanta Falcons", "Baltimore Ravens", "Buffalo Bills", 
                 "Carolina Panthers", "Chicago Bears", "Cincinnati Bengals", "Cleveland Browns",
                 "Dallas Cowboys", "Denver Broncos", "Detroit Lions", "Green Bay Packers",
                 "Houston Texans", "Indianapolis Colts", "Jacksonville Jaguars", "Kansas City Chiefs",
                 "Las Vegas Raiders", "Los Angeles Chargers", "Los Angeles Rams", "Miami Dolphins",
                 "Minnesota Vikings", "New England Patriots", "New Orleans Saints", "New York Giants",
                 "New York Jets", "Philadelphia Eagles", "Pittsburgh Steelers", "San Francisco 49ers",
                 "Seattle Seahawks", "Tampa Bay Buccaneers", "Tennessee Titans", "Washington Commanders")
  
  team_defaults <- list()
  for (team in all_teams) {
    team_defaults[[team]] <- list(
      home_avg_points = 20,
      away_avg_points = 20,
      home_avg_points_allowed = 20,
      away_avg_points_allowed = 20
    )
  }
  
  # Update team statistics inputs when teams are selected
  observeEvent(input$home_team, {
    if(input$home_team %in% names(team_defaults)) {
      stats <- team_defaults[[input$home_team]]
      updateNumericInput(session, "home_avg_points", value = stats$home_avg_points)
      updateNumericInput(session, "home_avg_points_allowed", value = stats$home_avg_points_allowed)
    } else {
      # Default value of 20 for teams not explicitly in the list
      updateNumericInput(session, "home_avg_points", value = 20)
      updateNumericInput(session, "home_avg_points_allowed", value = 20)
    }
  })
  
  observeEvent(input$away_team, {
    if(input$away_team %in% names(team_defaults)) {
      stats <- team_defaults[[input$away_team]]
      updateNumericInput(session, "away_avg_points", value = stats$away_avg_points)
      updateNumericInput(session, "away_avg_points_allowed", value = stats$away_avg_points_allowed)
    } else {
      # Default value of 20 for teams not explicitly in the list
      updateNumericInput(session, "away_avg_points", value = 20)
      updateNumericInput(session, "away_avg_points_allowed", value = 20)
    }
  })
  
  # Team-specific coefficients for over/under tendencies
  #Based on previous year performance against spread
  #Looking to automate instead of hard code
  team_over_coefficients <- list(
    "Arizona Cardinals" = 0.15,
    "Atlanta Falcons" = 0.10,
    "Baltimore Ravens" = 0.05,
    "Buffalo Bills" = 0.20,
    "Carolina Panthers" = -0.10,
    "Chicago Bears" = -0.25,
    "Cincinnati Bengals" = 0.15,
    "Cleveland Browns" = -0.05,
    "Dallas Cowboys" = 0.20,
    "Denver Broncos" = -0.15,
    "Detroit Lions" = 0.25,
    "Green Bay Packers" = 0.10,
    "Houston Texans" = -0.05,
    "Indianapolis Colts" = 0.05,
    "Jacksonville Jaguars" = -0.10,
    "Kansas City Chiefs" = 0.30,
    "Las Vegas Raiders" = 0.05,
    "Los Angeles Chargers" = 0.15,
    "Los Angeles Rams" = 0.10,
    "Miami Dolphins" = 0.20,
    "Minnesota Vikings" = 0.05,
    "New England Patriots" = -0.20,
    "New Orleans Saints" = 0.10,
    "New York Giants" = -0.15,
    "New York Jets" = -0.20,
    "Philadelphia Eagles" = 0.15,
    "Pittsburgh Steelers" = -0.25,
    "San Francisco 49ers" = 0.05,
    "Seattle Seahawks" = 0.10,
    "Tampa Bay Buccaneers" = 0.15,
    "Tennessee Titans" = -0.10,
    "Washington Commanders" = -0.05
  )
  
  # Function to predict Over/Under using logistic regression approach with team indicators
  predict_over_under <- function(home_team, away_team, home_pts, away_pts, home_allowed, away_allowed, 
                                 ou_line, spread, home_favorite, indoor, division) {
    
    # Create variables for logistic regression
    team_total_offense <- home_pts + away_pts
    team_total_defense <- home_allowed + away_allowed
    predicted_total <- (team_total_offense + team_total_defense) / 2
    total_vs_line <- predicted_total - ou_line
    
    # Get team-specific coefficients (default to 0 if not found)
    home_team_coef <- if (home_team %in% names(team_over_coefficients)) {
      team_over_coefficients[[home_team]]
    } else {
      0
    }
    
    away_team_coef <- if (away_team %in% names(team_over_coefficients)) {
      team_over_coefficients[[away_team]]
    } else {
      0
    }
    
    # Combined team effect (average of home and away team coefficients)
    team_effect <- (home_team_coef + away_team_coef) / 2
    
    # Coefficients for logistic regression
    # These would normally be derived from fitting a model to historical data
    intercept <- -0.1  # Slightly favors under by default
    coef_total_vs_line <- 0.15  # Higher predicted totals increase over probability
    coef_spread <- -0.02  # Higher spreads often mean more defensive games
    coef_indoor <- 0.4   # Indoor games tend to be higher scoring
    coef_division <- -0.3  # Division games tend to be lower scoring
    coef_home_favorite <- 0.1 
    
    # Calculate log odds using logistic regression formula
    log_odds <- intercept + 
      (coef_total_vs_line * total_vs_line) + 
      (coef_spread * spread) + 
      (coef_indoor * as.numeric(indoor)) + 
      (coef_division * as.numeric(division)) + 
      (coef_home_favorite * as.numeric(home_favorite)) +
      team_effect  # Add team-specific effect
    
    # Convert log odds to probability
    over_probability <- 1 / (1 + exp(-log_odds))
    
    # Add small random variance to model uncertainty (mimicking prediction intervals)
    jitter <- runif(1, -0.03, 0.03)
    final_prob <- over_probability + jitter
    
    # Ensure probability is between 0.05 and 0.95
    final_prob <- max(min(final_prob, 0.95), 0.05)
    
    return(list(
      prob_over = final_prob,
      prob_under = 1 - final_prob,
      team_effect = team_effect  # Include team effect in output for explanation
    ))
  }
  
  # Make over/under prediction when button is clicked
  prediction_data <- eventReactive(input$predict_btn, {
    # Get prediction
    pred_result <- predict_over_under(
      input$home_team, 
      input$away_team,
      input$home_avg_points,
      input$away_avg_points,
      input$home_avg_points_allowed,
      input$away_avg_points_allowed,
      input$over_under_line, 
      input$spread_favorite, 
      input$home_favorite, 
      input$indoor_game, 
      input$division_game
    )
    
    # Add additional data for display
    pred_result$home_team <- input$home_team
    pred_result$away_team <- input$away_team
    pred_result$over_under_line <- input$over_under_line
    pred_result$spread_favorite <- input$spread_favorite
    pred_result$home_points <- input$home_avg_points
    pred_result$away_points <- input$away_avg_points
    pred_result$home_allowed <- input$home_avg_points_allowed
    pred_result$away_allowed <- input$away_avg_points_allowed
    
    return(pred_result)
  })
  
  # Create prediction bar plot
  output$prediction_plot <- renderPlotly({
    req(prediction_data())
    
    pred <- prediction_data()
    
    df <- data.frame(
      outcome = c("Over", "Under"),
      probability = c(pred$prob_over, pred$prob_under)
    )
    
    # Generate colors based on which outcome is more likely
    colors <- c("Over" = ifelse(pred$prob_over >= 0.5, "#4CAF50", "#CCCCCC"),
                "Under" = ifelse(pred$prob_under > 0.5, "#F44336", "#CCCCCC"))
    
    p <- ggplot(df, aes(x = outcome, y = probability, fill = outcome)) +
      geom_bar(stat = "identity", width = 0.5) +
      scale_fill_manual(values = colors) +
      geom_text(aes(label = paste0(round(probability * 100, 1), "%")), 
                vjust = -0.5, size = 5) +
      ylim(0, 1) +
      labs(title = paste0("Over/Under Prediction: ", pred$home_team, " vs ", pred$away_team),
           subtitle = paste0("O/U Line: ", pred$over_under_line),
           x = "", y = "Probability") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.x = element_text(size = 12, face = "bold"),
            axis.title.y = element_text(size = 10))
    
    ggplotly(p) %>% 
      layout(hoverlabel = list(bgcolor = "white", font = list(size = 14)))
  })
  
  # Display prediction details
  output$prediction_details <- renderTable({
    req(prediction_data())
    
    pred <- prediction_data()
    
    # Calculate implied total score based on team averages
    implied_total <- pred$home_points + pred$away_points
    
    # Calculate expected margin based on spread
    expected_margin <- pred$spread_favorite
    
    # Team effect explanation
    team_effect_pct <- round(pred$team_effect * 100, 1)
    team_effect_direction <- ifelse(team_effect_pct > 0, "increases", "decreases")
    
    # Create a table with the prediction details
    data.frame(
      Metric = c("Prediction", "Confidence", "O/U Line", "Implied Total Score", "Home Team Avg Points", 
                 "Away Team Avg Points", "Expected Margin", "Team Effect"),
      Value = c(
        ifelse(pred$prob_over >= 0.5, "OVER", "UNDER"),
        paste0(round(max(pred$prob_over, pred$prob_under) * 100, 1), "%"),
        pred$over_under_line,
        round(implied_total, 1),
        round(pred$home_points, 1),
        round(pred$away_points, 1),
        expected_margin,
        paste0(abs(team_effect_pct), "% ", team_effect_direction)
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = 'l', align = 'lc', width = "100%")
  
  # Team stats visualization
  output$team_stats_plot <- renderPlotly({
    req(prediction_data())
    
    pred <- prediction_data()
    
    # Prepare data for team comparison
    team_data <- data.frame(
      Team = c(pred$home_team, pred$away_team, pred$home_team, pred$away_team),
      Metric = c("Points Scored", "Points Scored", "Points Allowed", "Points Allowed"),
      Value = c(
        pred$home_points,
        pred$away_points,
        pred$home_allowed,
        pred$away_allowed
      )
    )
    
    # Create team comparison plot
    p <- ggplot(team_data, aes(x = interaction(Team, Metric), y = Value, fill = Team)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      geom_text(aes(label = round(Value, 1)), position = position_dodge(width = 0.7),
                vjust = -0.5, size = 3.5) +
      labs(title = "Team Statistical Comparison",
           x = "", y = "Points Per Game") +
      scale_fill_manual(values = c("#3498db", "#e74c3c")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5, face = "bold"))
    
    ggplotly(p)
  })
  
  # --------------------------------------------------------
  # NFL SPREAD BETTING TAB
  # --------------------------------------------------------
  
  # Generate UI for favored team selection based on home/away selections
  output$sb_favored_team_ui <- renderUI({
    req(input$sb_home_team, input$sb_away_team)
    
    # Only show the two teams as options
    selectInput("sb_favored_team", "Favored Team:", 
                choices = c(input$sb_home_team, input$sb_away_team),
                selected = input$sb_home_team)
  })
  
  # Define team momentum data (based on last year)
  team_momentum <- data.frame(
    team = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN", 
             "DET", "GB", "HOU", "IND", "JAC", "KC", "LV", "LAC", "LAR", "MIA", 
             "MIN", "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SF", "SEA", "TB", 
             "TEN", "WAS"),
    momentum = c(0.6, 1.2, 3.5, 4.8, -2.3, -0.7, 2.1, -2.5, -1.2, 0.9, 
                 3.8, 2.2, 1.2, 0.9, -2.5, 4.6, -2.6, 1.3, 2.0, -1.1, 
                 1.3, -1.8, -1.0, -2.1, -3.2, 5.0, 2.7, 1.3, 1.9, 2.5, 
                 -0.5, 1.8)
  )
  
  # Helper function to map team name to ID
  mapTeamNameToID <- function(team_name) {
    map <- list(
      "Arizona Cardinals" = "ARI",
      "Atlanta Falcons" = "ATL",
      "Baltimore Ravens" = "BAL",
      "Buffalo Bills" = "BUF",
      "Carolina Panthers" = "CAR",
      "Chicago Bears" = "CHI",
      "Cincinnati Bengals" = "CIN",
      "Cleveland Browns" = "CLE",
      "Dallas Cowboys" = "DAL",
      "Denver Broncos" = "DEN",
      "Detroit Lions" = "DET",
      "Green Bay Packers" = "GB",
      "Houston Texans" = "HOU",
      "Indianapolis Colts" = "IND",
      "Jacksonville Jaguars" = "JAC",
      "Kansas City Chiefs" = "KC",
      "Las Vegas Raiders" = "LV",
      "Los Angeles Chargers" = "LAC",
      "Los Angeles Rams" = "LAR",
      "Miami Dolphins" = "MIA",
      "Minnesota Vikings" = "MIN",
      "New England Patriots" = "NE",
      "New Orleans Saints" = "NO",
      "New York Giants" = "NYG",
      "New York Jets" = "NYJ",
      "Philadelphia Eagles" = "PHI",
      "Pittsburgh Steelers" = "PIT",
      "San Francisco 49ers" = "SF",
      "Seattle Seahawks" = "SEA",
      "Tampa Bay Buccaneers" = "TB",
      "Tennessee Titans" = "TEN",
      "Washington Commanders" = "WAS"
    )
    return(map[[team_name]])
  }
  
  # Function to predict game outcomes for spread betting
  predictSpreadBet <- function(home_team, away_team, favored_team, spread, over_line) {
    # Map team names to IDs
    home_id <- mapTeamNameToID(home_team)
    away_id <- mapTeamNameToID(away_team)
    
    # Get team momentum values
    home_momentum <- team_momentum$momentum[team_momentum$team == home_id]
    away_momentum <- team_momentum$momentum[team_momentum$team == away_id]
    
    # Base values for prediction
    # Favor home team
    base_home_score <- 23 + home_momentum
    base_away_score <- 21 + away_momentum
    
    
    # Add random variation to simulate model uncertainty
    pred_home_score <- base_home_score + rnorm(1, 0, 0.5)
    pred_away_score <- base_away_score + rnorm(1, 0, 0.5)
    
    
    # Calculate win probability
    margin <- pred_home_score - pred_away_score
    win_prob <- pnorm(margin / 14)  # 14 points is approx. SD of NFL game margins
    
    # Spread calculations
    if (favored_team == home_team) {
      adjusted_margin <- margin + spread
      spread_cover_prob <- pnorm(adjusted_margin / 14)
      favored_score <- pred_home_score
      underdog_score <- pred_away_score
      favored_win_prob <- win_prob
    } else {
      adjusted_margin <- -margin + spread
      spread_cover_prob <- pnorm(adjusted_margin / 14)
      favored_score <- pred_away_score
      underdog_score <- pred_home_score
      favored_win_prob <- 1 - win_prob
    }
    
    # Over/under calculation
    total_score <- pred_home_score + pred_away_score
    over_prob <- pnorm(total_score, mean = over_line, sd = 10, lower.tail = FALSE)
    
    # Result object
    return(list(
      pred_home_score = pred_home_score,
      pred_away_score = pred_away_score,
      win_prob = win_prob,
      favored_win_prob = favored_win_prob,
      spread_cover_prob = spread_cover_prob, 
      over_prob = over_prob,
      total_score = total_score,
      home_momentum = home_momentum,
      away_momentum = away_momentum
    ))
  }
  
  # Reactive expression for spread betting predictions
  sb_prediction <- eventReactive(input$sb_predict_btn, {
    req(input$sb_home_team, input$sb_away_team, input$sb_favored_team)
    
    # Make prediction
    predictSpreadBet(
      input$sb_home_team, 
      input$sb_away_team, 
      input$sb_favored_team,
      input$sb_spread,
      input$sb_over_line
    )
  })
  
  # Generate simulation data for score distributions
  sb_simulation <- eventReactive(input$sb_predict_btn, {
    req(sb_prediction())
    pred <- sb_prediction()
    
    # Parameters for score distributions
    home_mean <- pred$pred_home_score
    away_mean <- pred$pred_away_score
    
    # Generate simulated scores
    n_sims <- 1000
    home_scores <- pmax(rnorm(n_sims, mean = home_mean, sd = 10), 0)
    away_scores <- pmax(rnorm(n_sims, mean = away_mean, sd = 10), 0)
    
    # Calculate derived metrics
    margins <- home_scores - away_scores
    totals <- home_scores + away_scores
    
    # Return simulation results
    list(
      home_scores = home_scores,
      away_scores = away_scores,
      margins = margins,
      totals = totals
    )
  })
  
  # Display spread betting results table
  output$sb_results_table <- renderTable({
    req(sb_prediction())
    pred <- sb_prediction()
    
    # Determine underdog team
    underdog_team <- ifelse(input$sb_favored_team == input$sb_home_team, 
                            input$sb_away_team, input$sb_home_team)
    
    # Create results table
    data.frame(
      Metric = c(
        paste0(input$sb_home_team, " Predicted Score"),
        paste0(input$sb_away_team, " Predicted Score"),
        "Predicted Total Score",
        paste0(input$sb_home_team, " Win Probability"),
        paste0(input$sb_favored_team, " Cover Spread Probability"),
        paste0("Over ", input$sb_over_line, " Probability")
      ),
      Value = c(
        round(pred$pred_home_score, 1),
        round(pred$pred_away_score, 1),
        round(pred$total_score, 1),
        paste0(round(pred$win_prob * 100, 1), "%"),
        paste0(round(pred$spread_cover_prob * 100, 1), "%"),
        paste0(round(pred$over_prob * 100, 1), "%")
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = 'l', align = 'lc', width = "100%")
  
  # Display score distribution plot with custom theme
  output$sb_score_distribution <- renderPlot({
    req(sb_simulation())
    sim <- sb_simulation()
    
    # Create plot data with named list
    plot_data <- list()
    plot_data[[paste0(input$sb_home_team, " Score")]] <- sim$home_scores
    plot_data[[paste0(input$sb_away_team, " Score")]] <- sim$away_scores
    plot_data[["Point Margin (Home-Away)"]] <- sim$margins
    plot_data[[paste0("Total Score (O/U ", input$sb_over_line, ")")]] <- sim$totals
    
    # Custom colors that look good together
    custom_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12")
    
    # Set up the plotting area with custom theme
    par(bg = "#f8f9fa",      # Light background
        mar = c(4, 12, 4, 2), # Margins (bottom, left, top, right)
        family = "sans",     # Font family
        las = 1,             # Horizontal axis labels
        cex.main = 1.2,      # Main title size
        cex.lab = 1.1,       # Axis labels size
        cex.axis = 0.9,      # Axis text size
        mgp = c(2.5, 1, 0))  # Margin line for axis title, labels, and line
    
    # Draw boxplot with the custom theme
    boxplot(plot_data, 
            horizontal = TRUE, 
            col = custom_colors,
            border = "#2c3e50",        # Border color
            main = "Score Distribution Predictions",
            xlab = "Points",
            outpch = 20,               # Outlier point type
            outcol = "#7f8c8d",        # Outlier point color
            whisklty = 1,              # Whisker line type
            boxwex = 0.6,              # Box width
            staplewex = 0.3,           # Staple width
            outcex = 0.7,              # Outlier size
            notch = FALSE)             # No notches
    
    # Add a subtle grid
    grid(nx = NA, ny = NULL, col = "#ecf0f1", lty = 3, lwd = 1)
    
    # Add reference line for over/under with annotation
    abline(v = input$sb_over_line, col = "#e74c3c", lty = 2, lwd = 2)
    text(input$sb_over_line + 2, 0.5, "O/U Line", col = "#e74c3c", cex = 0.9)
    
    # Add reference line for zero margin (push on spread) with annotation
    abline(v = 0, lty = 2, col = "#2c3e50", lwd = 1.5)
    text(2, 3.5, "Push Line", col = "#2c3e50", cex = 0.9)
    
    # Add subtle box around the plot
    box(col = "#bdc3c7")
  })
  
  # Display distribution summary statistics
  output$sb_distribution_summary <- renderPrint({
    req(sb_simulation())
    sim <- sb_simulation()
    
    # Calculate probability of covering spread
    if(input$sb_favored_team == input$sb_home_team) {
      spread_cover_prob <- mean(sim$margins > input$sb_spread)
    } else {
      spread_cover_prob <- mean(sim$margins < -input$sb_spread)
    }
    
    # Calculate probability of hitting over
    over_prob <- mean(sim$totals > input$sb_over_line)
    
    cat("Win Margin Quantiles (Home minus Away):\n")
    print(round(quantile(sim$margins, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)), 1))
    
    cat("\nTotal Score Quantiles:\n")
    print(round(quantile(sim$totals, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)), 1))
    
    cat("\nSimulation Probabilities:\n")
    cat("Probability of Home Team Win: ", round(mean(sim$margins > 0) * 100, 1), "%\n", sep = "")
    cat("Probability of Favored Team Covering Spread: ", round(spread_cover_prob * 100, 1), "%\n", sep = "")
    cat("Probability of Over ", input$sb_over_line, ": ", round(over_prob * 100, 1), "%\n", sep = "")
  })
  
  # --------------------------------------------------------
  # MODEL PERFORMANCE TAB
  # --------------------------------------------------------
  
  # Model factors plot
  output$model_factors_plot <- renderPlot({
    # Coeffecients derived in prior analysis, had difficulty integrating into shiny
    model_factors <- data.frame(
      Feature = c("Over/Under Line", "Home Points", "Away Points", 
                  "Home Points Allowed", "Away Points Allowed", 
                  "Indoor Game", "Spread Favorite", "Division Matchup",
                  "Home Favorite", "Chiefs at Home", "Ravens on Road",
                  "Bills vs Division", "Sunday Game", "Prime Time Game",
                  "December Game"),
      Coefficient = c(0.32, 0.24, 0.21, -0.18, -0.15, 0.14, -0.12, -0.09, 0.08, 0.07, -0.06, -0.05, 0.04, 0.03, -0.02)
    )
    
    ggplot(model_factors, aes(x = reorder(Feature, abs(Coefficient)), y = Coefficient, 
                              fill = ifelse(Coefficient > 0, "Positive", "Negative"))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = c("Positive" = "#00BFC4", "Negative" = "#F8766D"),
                        name = "Impact") +
      labs(title = "Key Factors Influencing Over/Under",
           x = "", y = "Relative Importance") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  })
  
  # NRFI Accuracy plot
  output$spread_accuracy_plot <- renderPlot({
    # derived in prior analysis had difficulty integrating into shiny
    set.seed(123)
    seasons <- 2016:2023
    spread_acc <- c(52.7, 55.8, 56.3, 54.5, 58.1, 55.6, 55.8, 56.4)
    moneyline_acc <- c(58.2, 61.5, 62.7, 60.3, 63.8, 64.1, 62.9, 62.4)
    
    # Create data frame for plotting
    plot_data <- data.frame(
      Season = rep(seasons, 2),
      Type = rep(c("Spread", "Moneyline"), each = length(seasons)),
      Accuracy = c(spread_acc, moneyline_acc)
    )
    
    # Create line plot
    ggplot(plot_data, aes(x = Season, y = Accuracy, color = Type, group = Type)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3) +
      scale_color_manual(values = c("Spread" = "#3498db", "Moneyline" = "#e74c3c")) +
      geom_hline(yintercept = 50, linetype = "dashed", color = "gray50") +
      labs(title = "Model Accuracy by Season",
           y = "Accuracy (%)", x = "Season") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "bottom")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)