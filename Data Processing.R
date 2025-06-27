

# 1. Fetch Data -----------------------------------------------------------


# Load game results
raw_results <- load_match_results("ENG","M", 2010:2024, "1st")

# Edit team names to match Understat format
raw_results <- raw_results %>%
  mutate(across(c(Home, Away), ~ str_replace(.x, 'Utd', 'United') %>%
                  str_replace('Cardiff City', 'Cardiff') %>%
                  str_replace("Nott'ham Forest", 'Nottingham Forest')),
         Venue = str_replace(Venue, "St James' Park", "St. James's Park"))


# Each row represents a single game
df_input <- raw_results %>%
  mutate(
    # Add a unique identifier for each match
    MatchID = row_number(),
    
    # Determine the result of each fixture from the home team’s perspective
    FixtureResult = case_when(
      HomeGoals > AwayGoals ~ "HomeWin",
      HomeGoals < AwayGoals ~ "AwayWin",
      TRUE ~ "Draw" 
    ),
    DrawResult = case_when(
      HomeGoals == AwayGoals ~ 'Draw',
      TRUE ~ 'NoDraw'
    ))
df_input <- df_input %>%
  dplyr::select(
    MatchID,
    Season_End_Year,
    Wk,
    Date,
    Time,
    Home,
    Away,
    Home_xG,
    Away_xG,
    HomeGoals,
    AwayGoals,
    Venue,
    FixtureResult,
    DrawResult
  )

club_names <- unique(c(df_input$Home, df_input$Away))


df_input <- calculate_moving_average_xG(df_input)

df_input <- calculate_goal_difference(df_input)


df_input_with_elo <- fetch_and_merge_elo(club_names, df_input)


# Add a new column for absolute Elo difference
df_input_with_elo <- df_input_with_elo %>%
  mutate(
    Elo_Difference = (abs(Home_Elo - Away_Elo))  # Calculate the absolute difference
  )




# Add a new column for absolute xG MA difference
df_input_with_elo <- df_input_with_elo %>%
  mutate(
    xG_Difference = (abs(Home_xG_MA - Away_xG_MA))  # Calculate the absolute difference
  )

# Path to the uploaded file
folder_path <- "C:\\Users\\jaeyt\\Documents\\UCL\\Year 3\\Project\\Betting Odds"

# Import the odds data
odds_data <- import_odds_data_by_season(c(seq(20,24)), folder_path)
# View the cleaned data
print(head(odds_data))

odds_data <- standardize_team_names(odds_data)
# Apply the function to calculate implied probabilities
odds_with_probabilities <- calculate_implied_probabilities(odds_data)

