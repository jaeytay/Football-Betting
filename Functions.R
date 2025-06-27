#functions
# 1. Load packages --------------------------------------------------------

# https://www.football-data.co.uk/notes.txt (odds)
setwd("~/UCL/Year 3/Project/Script")
library(worldfootballR)
library(BradleyTerry2)
library(dplyr)
library(stringr)
library(tidyr)
library(glmnet)
library(caret)
library(MLmetrics)
library(zoo)
library(elo)
library(httr)
library(readr)
library(purrr)
library(tibble)


# Function to calculate 5-week moving average for Home_xG and Away_xG
calculate_moving_average_xG <- function(df_input) {

  # Calculate the 5-week moving average for Home_xG and Away_xG
  df_input <- df_input %>%
    group_by(Home) %>%
    mutate(Home_xG_MA = rollapply(lag(Home_xG), width = 3, FUN = mean, align = "right", fill = 1)) %>%
    ungroup() %>%
    group_by(Away) %>%
    mutate(Away_xG_MA = rollapply(lag(Away_xG), width = 3, FUN = mean, align = "right", fill = 1)) %>%
    ungroup()
  
  df_input$Home_xG_MA[is.na(df_input$Home_xG_MA)] <- 1
  df_input$Away_xG_MA[is.na(df_input$Away_xG_MA)] <- 1

  return(df_input)
}




# Function to calculate cumulative goal difference for each team before each fixture
calculate_goal_difference <- function(df_input) {
  
  # Initialize columns for Goal Difference (GD) before each fixture
  df_input <- df_input %>%
    arrange(Season_End_Year, Date) %>%  # Ensure data is sorted by season and date
    group_by(Season_End_Year, Home) %>%
    mutate(
      Cumulative_Home_GD = lag(cumsum(HomeGoals - AwayGoals), default = 0)
    ) %>%
    ungroup() %>%
    group_by(Season_End_Year, Away) %>%
    mutate(
      Cumulative_Away_GD = lag(cumsum(AwayGoals - HomeGoals), default = 0)
    ) %>%
    ungroup()
  
  # Replace NA values with 0 (happens if it's the first game of the season)
  df_input$Cumulative_Home_GD[is.na(df_input$Cumulative_Home_GD)] <- 0
  df_input$Cumulative_Away_GD[is.na(df_input$Cumulative_Away_GD)] <- 0
  
  # Now, create a Goal Difference column for each fixture (before the match)
  df_input <- df_input %>%
    mutate(
      Home_Goal_Difference_Before = Cumulative_Home_GD,
      Away_Goal_Difference_Before = Cumulative_Away_GD
    ) %>%
    dplyr::select(-Cumulative_Home_GD, -Cumulative_Away_GD)
  
  return(df_input)
}






fetch_and_merge_elo <- function(club_names, df_input) {
  
  # Initialize a list to store each club's Elo data
  elo_data_list <- list()
  empty_club_list <- c()  # List to store clubs that return empty data
  
  # Function to standardize club names for the API
  standardize_club_name <- function(club) {
    club <- gsub(" ", "", club)  # Remove all spaces
    club <- gsub("ManchesterUnited", "ManUnited", club)  # Shorten Manchester United
    club <- gsub("ManchesterCity", "ManCity", club)      # Shorten Manchester City
    club <- gsub("NewcastleUnited", "Newcastle", club)   # Shorten Newcastle United
    club <- gsub("LeicesterCity", "Leicester", club)     # Shorten Leicester City
    club <- gsub("NorwichCity", "Norwich", club)         # Shorten Norwich City
    club <- gsub("LeedsUnited", "Leeds", club)           # Shorten Leeds United
    club <- gsub("NottinghamForest", "Forest", club)     # Shorten Nottingham Forest
    club <- gsub("SwanseaCity", "Swansea", club)         # Shorten Swansea City
    club <- gsub("StokeCity", "Stoke", club)             # Shorten Stoke City
    club <- gsub("HullCity", "Hull", club)               # Shorten Hull City
    club <- gsub("WiganAthletic", "Wigan", club)         # Shorten Wigan Athletic
    club <- gsub("BirminghamCity", "Birmingham", club)   # Shorten Birmingham City
    club <- gsub("LutonTown", "Luton", club)             # Shorten Luton Town
    return(club)
  }
  
  # Loop over each club to fetch data
  for (club in club_names) {
    
    # Standardize the club name for the API
    club_name_api <- standardize_club_name(club)
    
    # Construct the URL
    url <- paste0("http://api.clubelo.com/", club_name_api)
    
    # Fetch the data from the API
    response <- GET(url)
    
    # Check if the response is valid
    if (status_code(response) == 200) {
      # Read the content of the response as CSV
      content <- content(response, as = "text")
      elo_df <- read_csv(content, show_col_types = FALSE)
      
      # Check if the returned data is empty (CSV only has headers)
      if (nrow(elo_df) == 0) {
        warning(paste("No data returned for", club))
        empty_club_list <- c(empty_club_list, club)  # Add to empty club list
      } else {
        # Use the 'From' date for merging with match data and retain only necessary columns
        elo_df <- elo_df %>%
          mutate(Date = as.Date(From)) %>%  # Convert 'From' to date format
          dplyr::select(Date, Club, Elo)  # Retain only relevant columns
        
        # Store the data with an additional column for the club name
        elo_df <- elo_df %>% mutate(Club = club)
        elo_data_list[[club]] <- elo_df
      }
      
    } else {
      warning(paste("Failed to fetch data for", club))
      empty_club_list <- c(empty_club_list, club)  # Add to empty club list
    }
  }
  
  # Combine all Elo data into one data frame
  elo_data_combined <- bind_rows(elo_data_list)
  
  # Join Elo data with df_input
  df_input <- df_input %>%
    mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
  
  # Function to get the last known Elo rating before a given date
  get_last_known_elo <- function(df, date, team) {
    last_elo <- df %>%
      filter(Club == team & Date < date) %>%
      arrange(desc(Date)) %>%
      slice_head(n = 1) %>%
      pull(Elo)
    
    # If no rows are found, return NA or default Elo
    if (length(last_elo) == 0) {
      return(NA)  # You can replace NA with 1500 if preferred
    }
    
    return(last_elo)
  }
  
  # Add Elo ratings to the input data for Home and Away teams
  df_input_with_elo <- df_input %>%
    rowwise() %>%
    mutate(
      Home_Elo = get_last_known_elo(elo_data_combined, Date, Home),
      Away_Elo = get_last_known_elo(elo_data_combined, Date, Away)
    ) %>%
    ungroup()
  
  # Handle cases where Elo data is missing (default to 1500 if NA)
  df_input_with_elo <- df_input_with_elo %>%
    mutate(Home_Elo = ifelse(is.na(Home_Elo), 1500, Home_Elo),
           Away_Elo = ifelse(is.na(Away_Elo), 1500, Away_Elo))
  
  # Print clubs that returned empty data
  if (length(empty_club_list) > 0) {
    message("The following clubs returned empty data (only column headers, no rows):")
    print(empty_club_list)
  }
  
  return(df_input_with_elo)
}





import_odds_data_by_season <- function(season_end_years, folder_path) {
  # Ensure the input is numeric
  season_end_years <- as.numeric(season_end_years)
  
  # Initialize an empty list to store data
  odds_data_list <- list()
  
  # Columns related to betting houses (adjust if needed)
  betting_house_columns <- c(
    "B365H", "B365D", "B365A",  # Bet365
    "BWH", "BWD", "BWA",        # Betway
    "IWH", "IWD", "IWA",        # Interwetten
    "PSH", "PSD", "PSA",        # Pinnacle
    "WHH", "WHD", "WHA",        # William Hill
    "MaxH" , "MaxD", "MaxA"     # Max
  )
  
  # Loop through the specified years and read the corresponding files
  for (year in season_end_years) {
    # Construct the file name
    start_year <- sprintf("%02d", year - 1)
    end_year <- sprintf("%02d", year)
    file_name <- paste0(folder_path, "/", start_year, end_year, ".csv")  # Adjust file name format if needed
    
    if (file.exists(file_name)) {
      # Read the file
      odds_data <- read_csv(file_name, show_col_types = FALSE)  # Silences diagnostic output
      
      # Select relevant columns (adjust based on actual column names)
      odds_data <- odds_data %>%
        select(HomeTeam, AwayTeam, Date, FTR, all_of(betting_house_columns))  # Removed MatchID
      
      # Rename columns
      odds_data <- odds_data %>%
        rename(Home = HomeTeam, Away = AwayTeam)
      
      # Add season information to the data
      odds_data <- odds_data %>% mutate(Season_End_Year = paste0("20", end_year))
      
      # Convert FTR to FixtureResult
      odds_data <- odds_data %>%
        mutate(
          FixtureResult = case_when(
            FTR == "H" ~ "HomeWin",
            FTR == "D" ~ "Draw",
            FTR == "A" ~ "AwayWin",
            TRUE ~ NA_character_
          )
        )
      
      # Append to the list
      odds_data_list[[year]] <- odds_data
    } else {
      warning(paste("File not found:", file_name))
    }
  }
  
  # Combine all the data into a single data frame
  combined_odds_data <- bind_rows(odds_data_list, .id = "Season")
  
  return(combined_odds_data)
}





# Function to standardize team names
standardize_team_names <- function(odds_data) {
  # Create a mapping of team names (adjust as necessary)
  team_name_mapping <- c(
    "Man United" = "Manchester United",
    "Newcastle" = "Newcastle United",
    "Man City" = "Manchester City",
    "Nott'm Forest" = "Nottingham Forest",
    "Leeds" = "Leeds United",
    "Leicester" = "Leicester City",
    "Norwich" = "Norwich City"
    # Add more mappings as needed...
  )
  
  # Replace the team names in the odds data
  odds_data <- odds_data %>%
    mutate(
      Home = recode(Home, !!!team_name_mapping),
      Away = recode(Away, !!!team_name_mapping)
    )
  
  return(odds_data)
}



calculate_scores <- function(predictions_df) {
  # Calculate Log Score and Brier Score for each row
  scored_df <- predictions_df %>%
    mutate(
      # Log score: Negative log likelihood of the predicted probabilities
      LogScore = case_when(
        FixtureResult == "HomeWin" ~ -log(pmax(AdjustedProb_HomeWin, 1e-15)),  # Avoid -Inf for log(0)
        FixtureResult == "AwayWin" ~ -log(pmax(AdjustedProb_AwayWin, 1e-15)),
        FixtureResult == "Draw" ~ -log(pmax(AdjustedProb_Draw, 1e-15)),
        TRUE ~ NA_real_
      ),
      # Brier score: Mean squared error of the predicted probabilities
      BrierScore = case_when(
        FixtureResult == "HomeWin" ~ (1 - AdjustedProb_HomeWin)^2 + AdjustedProb_AwayWin^2 + AdjustedProb_Draw^2,
        FixtureResult == "AwayWin" ~ AdjustedProb_HomeWin^2 + (1 - AdjustedProb_AwayWin)^2 + AdjustedProb_Draw^2,
        FixtureResult == "Draw" ~ AdjustedProb_HomeWin^2 + AdjustedProb_AwayWin^2 + (1 - AdjustedProb_Draw)^2,
        TRUE ~ NA_real_
      )
    )
  
  # Summarize total scores
  total_scores <- scored_df %>%
    summarise(
      TotalLogScore = sum(LogScore, na.rm = TRUE),
      TotalBrierScore = sum(BrierScore, na.rm = TRUE)
    )
  
  # Return both the detailed dataframe and the total scores
  list(ScoredData = scored_df, TotalScores = total_scores)
}




calculate_scores_bookmaker <- function(predictions_df) {
  # Calculate Log Score and Brier Score for each row
  scored_df <- predictions_df %>%
    mutate(
      # Log score: Negative log likelihood of the predicted probabilities
      LogScore = case_when(
        FixtureResult == "HomeWin" ~ -log(pmax(ImpliedProb_MaxH, 1e-15)),  # Avoid -Inf for log(0)
        FixtureResult == "AwayWin" ~ -log(pmax(ImpliedProb_MaxA, 1e-15)),
        FixtureResult == "Draw" ~ -log(pmax(ImpliedProb_MaxD, 1e-15)),
        TRUE ~ NA_real_
      ),
      # Brier score: Mean squared error of the predicted probabilities
      BrierScore = case_when(
        FixtureResult == "HomeWin" ~ (1 - ImpliedProb_MaxH)^2 + ImpliedProb_MaxA^2 + ImpliedProb_MaxD^2,
        FixtureResult == "AwayWin" ~ ImpliedProb_MaxH^2 + (1 - ImpliedProb_MaxA)^2 + ImpliedProb_MaxD^2,
        FixtureResult == "Draw" ~ ImpliedProb_MaxH^2 + ImpliedProb_MaxA^2 + (1 - ImpliedProb_MaxD)^2,
        TRUE ~ NA_real_
      )
    )
  
  # Summarize total scores
  total_scores <- scored_df %>%
    summarise(
      TotalLogScore = sum(LogScore, na.rm = TRUE),
      TotalBrierScore = sum(BrierScore, na.rm = TRUE)
    )
  
  # Return both the detailed dataframe and the total scores
  list(ScoredData = scored_df, TotalScores = total_scores)
}





calculate_implied_probabilities <- function(odds_data) {
  # List of betting houses and corresponding odds columns
  betting_houses <- c("B365", "BW", "IW", "PS", "WH" , "Max")
  
  # Loop through each betting house to calculate implied probabilities
  for (house in betting_houses) {
    odds_data <- odds_data %>%
      mutate(
        !!paste0("ImpliedProb_", house, "H") := 1 / .data[[paste0(house, "H")]],
        !!paste0("ImpliedProb_", house, "D") := 1 / .data[[paste0(house, "D")]],
        !!paste0("ImpliedProb_", house, "A") := 1 / .data[[paste0(house, "A")]]
      )
  }
  
  # Normalize probabilities for each betting house
  odds_data <- odds_data %>%
    rowwise() %>%  # Apply row-wise calculations
    mutate(
      Total_B365 = sum(ImpliedProb_B365H, ImpliedProb_B365D, ImpliedProb_B365A, na.rm = TRUE),
      Total_BW = sum(ImpliedProb_BWH, ImpliedProb_BWD, ImpliedProb_BWA, na.rm = TRUE),
      Total_IW = sum(ImpliedProb_IWH, ImpliedProb_IWD, ImpliedProb_IWA, na.rm = TRUE),
      Total_PS = sum(ImpliedProb_PSH, ImpliedProb_PSD, ImpliedProb_PSA, na.rm = TRUE),
      Total_WH = sum(ImpliedProb_WHH, ImpliedProb_WHD, ImpliedProb_WHA, na.rm = TRUE),
      Total_Max = sum(ImpliedProb_MaxH, ImpliedProb_MaxD, ImpliedProb_MaxA, na.rm = TRUE)
    ) %>%
    mutate(
      ImpliedProb_B365H = ImpliedProb_B365H / Total_B365,
      ImpliedProb_B365D = ImpliedProb_B365D / Total_B365,
      ImpliedProb_B365A = ImpliedProb_B365A / Total_B365,
      ImpliedProb_BWH = ImpliedProb_BWH / Total_BW,
      ImpliedProb_BWD = ImpliedProb_BWD / Total_BW,
      ImpliedProb_BWA = ImpliedProb_BWA / Total_BW,
      ImpliedProb_IWH = ImpliedProb_IWH / Total_IW,
      ImpliedProb_IWD = ImpliedProb_IWD / Total_IW,
      ImpliedProb_IWA = ImpliedProb_IWA / Total_IW,
      ImpliedProb_PSH = ImpliedProb_PSH / Total_PS,
      ImpliedProb_PSD = ImpliedProb_PSD / Total_PS,
      ImpliedProb_PSA = ImpliedProb_PSA / Total_PS,
      ImpliedProb_WHH = ImpliedProb_WHH / Total_WH,
      ImpliedProb_WHD = ImpliedProb_WHD / Total_WH,
      ImpliedProb_WHA = ImpliedProb_WHA / Total_WH,
      ImpliedProb_MaxH = ImpliedProb_MaxH / Total_Max,
      ImpliedProb_MaxD = ImpliedProb_MaxD / Total_Max,
      ImpliedProb_MaxA = ImpliedProb_MaxA / Total_Max
    ) %>%
    ungroup()  # Ensure that all columns are retained by ungrouping
  
  return(odds_data)
}



year_on_year_comparison <- function(year1_predictions, year2_predictions) {
  # Calculate team-level accuracy for Year 1
  team_stats_year1 <- year1_predictions %>%
    mutate(correct = as.numeric(PredictedOutcome == FixtureResult)) %>%
    group_by(Home) %>%
    summarize(
      accuracy_year1 = mean(correct) * 100,  # Overall accuracy
      accuracy_home_win_year1 = mean(correct[FixtureResult == "HomeWin"], na.rm = TRUE) * 100,
      accuracy_draw_year1 = mean(correct[FixtureResult == "Draw"], na.rm = TRUE) * 100,
      accuracy_away_win_year1 = mean(correct[FixtureResult == "AwayWin"], na.rm = TRUE) * 100
    ) %>%
    rename(Team = Home)
  
  # Calculate team-level accuracy for Year 2
  team_stats_year2 <- year2_predictions %>%
    mutate(correct = as.numeric(PredictedOutcome == FixtureResult)) %>%
    group_by(Home) %>%
    summarize(
      accuracy_year2 = mean(correct) * 100,  # Overall accuracy
      accuracy_home_win_year2 = mean(correct[FixtureResult == "HomeWin"], na.rm = TRUE) * 100,
      accuracy_draw_year2 = mean(correct[FixtureResult == "Draw"], na.rm = TRUE) * 100,
      accuracy_away_win_year2 = mean(correct[FixtureResult == "AwayWin"], na.rm = TRUE) * 100
    ) %>%
    rename(Team = Home)
  
  # Merge the two dataframes for comparison
  comparison_df <- team_stats_year1 %>%
    inner_join(team_stats_year2, by = "Team") %>%
    mutate(
      accuracy_difference = accuracy_year2 - accuracy_year1,
      accuracy_home_win_difference = accuracy_home_win_year2 - accuracy_home_win_year1,
      accuracy_draw_difference = accuracy_draw_year2 - accuracy_draw_year1,
      accuracy_away_win_difference = accuracy_away_win_year2 - accuracy_away_win_year1
    ) %>%
    select(
      Team, accuracy_year1, accuracy_year2, accuracy_difference,
      accuracy_home_win_year1, accuracy_home_win_year2, accuracy_home_win_difference,
      accuracy_draw_year1, accuracy_draw_year2, accuracy_draw_difference,
      accuracy_away_win_year1, accuracy_away_win_year2, accuracy_away_win_difference
    )
  
  return(comparison_df)
}

blend_odds_with_results <- function(odds_data, season_end_year, actual_results) {
  # Ensure Season_End_Year is an integer in both datasets
  odds_data <- odds_data %>%
    mutate(Season_End_Year = as.integer(Season_End_Year))
  
  actual_results <- actual_results %>%
    mutate(Season_End_Year = as.integer(Season_End_Year))
  
  # Filter odds data for the specified season end year
  odds_filtered <- odds_data %>%
    filter(Season_End_Year == season_end_year)
  
  # Merge the odds data with actual results based on Home, Away, and Season_End_Year
  blended_data <- odds_filtered %>%
    left_join(actual_results, by = c("Home", "Away", "Season_End_Year", "FixtureResult"))
  
  # Return the blended data
  return(blended_data)
}


# Function to evaluate implied probabilities
evaluate_implied_probabilities <- function(df, odds_col_prefix = "Max") {
  df %>%
    rowwise() %>%
    mutate(
      # Extract implied probabilities
      ImpliedProb_H = !!sym(paste0("ImpliedProb_", odds_col_prefix, "H")),
      ImpliedProb_A = !!sym(paste0("ImpliedProb_", odds_col_prefix, "A")),
      ImpliedProb_D = !!sym(paste0("ImpliedProb_", odds_col_prefix, "D")),
      
      # Select the outcome with the highest implied probability
      PredictedOutcome = case_when(
        ImpliedProb_H == max(ImpliedProb_H, ImpliedProb_A, ImpliedProb_D) ~ "HomeWin",
        ImpliedProb_A == max(ImpliedProb_H, ImpliedProb_A, ImpliedProb_D) ~ "AwayWin",
        ImpliedProb_D == max(ImpliedProb_H, ImpliedProb_A, ImpliedProb_D) ~ "Draw",
        TRUE ~ NA_character_
      ),
      
      # Log Score components for each outcome
      LogScore = case_when(
        FixtureResult == "HomeWin" ~ -log(ImpliedProb_H),
        FixtureResult == "AwayWin" ~ -log(ImpliedProb_A),
        FixtureResult == "Draw" ~ -log(ImpliedProb_D),
        TRUE ~ NA_real_
      ),
      
      # Brier Score components for each outcome
      BrierScore = (ifelse(FixtureResult == "HomeWin", 1, 0) - ImpliedProb_H)^2 +
        (ifelse(FixtureResult == "AwayWin", 1, 0) - ImpliedProb_A)^2 +
        (ifelse(FixtureResult == "Draw", 1, 0) - ImpliedProb_D)^2
    ) %>%
    ungroup()
}

# Function to evaluate bookmaker performance
evaluate_bookmaker_performance <- function(df, bookmaker_prefixes) {
  results <- lapply(bookmaker_prefixes, function(prefix) {
    evaluated <- evaluate_implied_probabilities(df, odds_col_prefix = prefix)
    
    accuracy <- mean(evaluated$PredictedOutcome == evaluated$FixtureResult, na.rm = TRUE)
    
    scores <- evaluated %>%
      summarize(
        Total_LogScore = sum(LogScore, na.rm = TRUE),
        Total_BrierScore = sum(BrierScore, na.rm = TRUE)
      )
    
    data.frame(
      Bookmaker = prefix,
      Accuracy = accuracy,
      Total_LogScore = scores$Total_LogScore,
      Total_BrierScore = scores$Total_BrierScore
    )
  })
  
  do.call(rbind, results)
}



# Function to summarize betting results
summarize_betting_results <- function(bets) {
  bets %>%
    filter(!is.na(Bet)) %>%
    summarize(
      Total_Profit = sum(Profit, na.rm = TRUE),
      Total_Stakes = sum(Stake, na.rm = TRUE),
      ROI = (Total_Profit / Total_Stakes) * 100,
      Total_Bets = n()
    )
}


# Function to calculate team-level scores
calculate_team_scores <- function(predictions_df) {
  predictions_df %>%
    group_by(Home) %>%
    summarise(
      TotalLogScore = sum(LogScore, na.rm = TRUE),
      TotalBrierScore = sum(BrierScore, na.rm = TRUE)
    ) %>%
    rename(Team = Home)
}
