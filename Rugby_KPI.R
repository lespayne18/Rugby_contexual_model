library(bayestestR)   
library(bayesplot)    
library(brms)         
library(dplyr)
library(fitdistrplus)
library(ggplot2)
library(janitor)
library(mgcv)         
library(patchwork)    
library(performance)
library(readxl)  
library(splines)    
library(stringr)
library(tibble)
library(writexl)

################ Start #####################
# View, name or remove data if needed ###
data <- read_excel("~/LP RU KPI analysis/Datasets/SRKPI_Onlykeyvariables.xlsx")

################# /  Cleaning / ##################

# Ensure the ID column is numeric
data <- data %>%
  mutate(ID = as.numeric(ID))

# Create a unique GameID and relocate it next to the ID column
data <- data %>%
  mutate(
    GameID = as.numeric(factor(paste(homeTeamName, awayTeamName, datePlayed, sep = "_")))
  ) %>%
  relocate(GameID, .after = ID)  # Move the GameID column next to the ID column

# Rename all columns in the data to remove spaces and standardize with dots
colnames(data) <- make.names(colnames(data), unique = TRUE)

# Create a unique GamepossID and relocate it next to GameID
data <- data %>%
  mutate(
    GamepossID = paste(GameID, Possession.number, Phase.number, sep = "_")  # Combine GameID, Possession.number, and Phase.number
  ) %>%
  relocate(GamepossID, .after = GameID)  # Move GamepossID next to GameID

# Relocate 'Phase.number' next to 'teamName'
data <- data %>%
  relocate(Phase.number, .after = teamName)

# Relocate 'Possession.number' next to 'teamName'
data <- data %>%
  relocate(Possession.number, .after = teamName)

# Create a new column 'playing_group' based on 'playerpositionID'
data <- data %>%
  mutate(
    playing_pos = case_when(
      playerpositionID >= 1 & playerpositionID <= 8 ~ "Forward",  # Numbers 1-8 are Forwards
      playerpositionID >= 9 & playerpositionID <= 15 ~ "Back",    # Numbers 9-15 are Backs
      TRUE ~ NA_character_  # Handle cases where playerpositionID doesn't fall in these ranges
    )
  )

# Create a new column 'distinct_playing_pos' based on 'playerpositionID'
data <- data %>%
  mutate(
    distinct_playing_pos = case_when(
      playerpositionID >= 1 & playerpositionID <= 3 ~ "Front_Row",       # Numbers 1-3: Front Row
      playerpositionID >= 4 & playerpositionID <= 5 ~ "Second_Row",     # Numbers 4-5: Second Row
      playerpositionID >= 6 & playerpositionID <= 8 ~ "Back_Row",       # Numbers 6-8: Back Row
      playerpositionID >= 9 & playerpositionID <= 10 ~ "Halves",        # Numbers 9-10: Halves
      playerpositionID %in% c(11, 14, 15) ~ "Outside_Backs",            # Numbers 11, 14, 15: Outside Backs
      playerpositionID >= 12 & playerpositionID <= 13 ~ "Inside_Centres",# Numbers 12-13: Inside Centres
      TRUE ~ NA_character_  # Handle cases where playerpositionID doesn't fall in these ranges
    )
  )

# Create a dataframe for the 2023 Super Rugby Pacific Standings
super_rugby_standings <- data.frame(
  Team = c(
    "Chiefs", "Crusaders", "Blues", "ACT Brumbies", "Hurricanes", "NSW Waratahs", 
    "Fijian Drua", "Queensland Reds", "Highlanders", "Western Force", "Melbourne Rebels", "Moana Pasifika"
  ),
  P = c(14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14),  # Played
  W = c(13, 10, 10, 10, 9, 6, 6, 5, 5, 5, 4, 1),          # Wins
  W_Percent = c(92.86, 71.43, 71.43, 71.43, 64.29, 42.86, 42.86, 35.71, 35.71, 35.71, 28.57, 7.14), # Win %
  L = c(1, 4, 4, 4, 5, 8, 8, 9, 9, 9, 10, 13),            # Losses
  D = rep("-", 12),                                       # Draws
  B = rep(1, 12),                                         # Bye
  P_Plus = c(487, 450, 446, 474, 480, 387, 370, 391, 362, 346, 406, 354), # Points For
  P_Minus = c(261, 278, 292, 393, 338, 408, 492, 451, 459, 494, 477, 610), # Points Against
  Plus_Minus = c(226, 172, 154, 81, 142, -21, -122, -60, -97, -148, -71, -256), # Point Difference
  T_Plus = c(65, 65, 60, 66, 70, 54, 54, 54, 48, 46, 57, 50),                # Tries For
  T_Minus = c(38, 36, 39, 52, 47, 53, 73, 61, 60, 70, 68, 92),               # Tries Against
  BP = c(7, 8, 6, 6, 5, 7, 2, 4, 4, 2, 5, 4),                                # Bonus Points
  Pts = c(59, 48, 46, 46, 41, 31, 26, 24, 24, 22, 21, 8)                     # Points
)

# Rename the column 'team' to 'teamName'
super_rugby_standings <- super_rugby_standings %>%
  rename(teamName = Team)

filtered_data <- data %>%
  dplyr::select(-action, -`ActionType...Qualified.ID`, -period,
                -player_advantage, -sequence_id, -hometeamHTscore, 
                -kickofftime, -awayteamHTscore, -playerShirtNumber, 
                -MatchTime, -season, -roundNumber, -`Phase.example`, 
                -Actionresult, -qualifier3, -qualifier4, -qualifier5)

# Calculate the x_coord displacement
filtered_data <- filtered_data %>%
  mutate(x_displacement = x_coord_end - x_coord) %>%  # Calculate x_displacement
  relocate(x_displacement, .after = Metres.made)  # Move it next to 'Metres.made'

##################  Filtering ##################

# Create only tackle and carry dataframe 
filtered_data_carry <- filtered_data %>%
  filter(actionName %in% c("Carry","Tackle"))  # Keep only 'Carry' and 'Tackle' actions

#  Filter out rows where ActionResultName contains "Missed"
filtered_data_carry <- filtered_data_carry %>%
  filter(!grepl("Missed", ActionResultName, ignore.case = TRUE))

################# Create othercarry dataframe ##################

# Create a filtered dataset called Other_carry - all Carry’s that do not have a qualifier associated with a tackle
Other_carry <- filtered_data_carry %>%
  filter(
    ActionResultName %in% c("Tackled", "Pass CarryEnd", "Kick CarryEnd", "Try Scored CarryEnd", "Penalty Won CarryEnd", 
                            "Other CarryEnd", "Penalty Conceded CarryEnd", "Penalty Won", "Error CarryEnd", "Other") &  # Match specified ActionResultName values
      is.na(qualifier4Name)  # Only include rows where qualifier4Name is NA
  )

# Remove carries in Other_carry from filtered_data_carry using GamepossID -  Filtered_data_carry is now pure carrys ######
filtered_data_carry <- filtered_data_carry %>%
  filter(!(GamepossID %in% Other_carry$GamepossID & actionName == "Carry"))

######################### Tackle X SD and Y SD for each Possession-Phase #####################
# Calculate Tackle X SD and Y SD for each Possession-Phase
# Identify Possession-Phases where x_coord_SD and y_coord_SD = 0
phases_no_sd <- filtered_data_carry %>%
  filter(actionName == "Tackle") %>%  # Focus only on tackles
  group_by(GamepossID) %>%
  summarize(
    x_coord_SD = sd(x_coord, na.rm = TRUE),  # Calculate SD for x_coord
    y_coord_SD = sd(y_coord, na.rm = TRUE),  # Calculate SD for y_coord
    .groups = "drop"  # Ensure output is ungrouped
  )

################ Calculate multi tackle dataframe #########
# Filter and extract single_carry_single_tackle based on conditions
single_carry_single_tackle <- filtered_data_carry %>%
  filter(GamepossID %in% (phases_no_sd %>%
                            filter(is.na(x_coord_SD) | is.na(y_coord_SD)) %>%
                            pull(GamepossID)))  # Include all rows for identified GamepossIDs with NA

# Filter and extract single_carry_multiple_tackles based on conditions
single_carry_multiple_tackles <- filtered_data_carry %>%
  filter(GamepossID %in% (phases_no_sd %>%
                            filter(x_coord_SD == 0 & y_coord_SD == 0) %>%
                            pull(GamepossID)))  # Include all rows for identified GamepossIDs with 0.00

# Filter and extract Multi_carry_multiple_tackles based on conditions
multi_carry_multi_tackle <- filtered_data_carry %>%
  filter(GamepossID %in% (phases_no_sd %>%
                            filter(x_coord_SD > 0 | y_coord_SD > 0) %>%
                            pull(GamepossID)))  # Include all rows for identified GamepossIDs with >0

# Remove values or df that now not needed
(remove(phases_no_sd))

############## Filter checks ################
# Identify rows with multiple carries in a single phase and store them in a new dataframe
outliers_singlecarry <- single_carry_single_tackle %>%
  filter(actionName == "Carry") %>%  # Focus only on carry rows
  group_by(GamepossID, Phase.number) %>%
  mutate(
    num_carries = n()  # Count the number of carries in each phase
  ) %>%
  ungroup() %>%
  filter(num_carries > 1)  # Keep only rows from phases with multiple carries

# Step 1: Append the outliers from `outliers_singlecarry` to `multi_carry_multi_tackle`
multi_carry_multi_tackle <- bind_rows(multi_carry_multi_tackle, outliers_singlecarry)

# Step 2: Remove the outliers from `single_carry_single_tackle`
single_carry_single_tackle <- single_carry_single_tackle %>%
  filter(!GamepossID %in% outliers_singlecarry$GamepossID)

remove(outliers_singlecarry)

############ DATA PREPARATION ########
# Function to add and relocate new columns for a dataframe and reverse tackle coordinates
add_columns <- function(df) {
  df %>%
    mutate(
      x_tackle_moved = NA,  # Add the new column
      post.contact.metres = NA  # Add the column for 'post.contact.metres'
    ) %>%
    relocate(x_tackle_moved, .after = y_coord_end) %>%  # Move 'x_tackle_moved' next to 'y_coord_end'
    relocate(post.contact.metres, .after = Metres.made) %>%  # Move 'post.contact.metres' next to 'Metres.made'
    mutate(
      tackle_x_coord_reversed = ifelse(
        actionName == "Tackle",  # Only reverse for tackles
        case_when(
          x_coord >= 0 & x_coord <= 100 ~ 100 - x_coord,  # Standard reversal for 0-100
          x_coord > 100 ~ -(x_coord - 100),               # Reverse >100 to negatives
          x_coord < 0 ~ 100 - x_coord                     # Reverse negatives to >100
        ),
        NA  # Assign NA for non-tackles
      )
    ) %>%
    relocate(tackle_x_coord_reversed, .after = y_coord_end)  # Move 'tackle_x_coord_reversed' next to 'y_coord_end'
}

# Apply the function to all dataframes
filtered_data_carry <- add_columns(filtered_data_carry)
single_carry_single_tackle <- add_columns(single_carry_single_tackle)
single_carry_multiple_tackles <- add_columns(single_carry_multiple_tackles)
multi_carry_multi_tackle <- add_columns(multi_carry_multi_tackle)

##### Start linking carries and tackles in single carry/tackle dataframe  #######
# Step 1: Ensure 'concatenated_actions' column exists
if (!"concatenated_actions" %in% colnames(single_carry_single_tackle)) {
  single_carry_single_tackle <- single_carry_single_tackle %>%
    mutate(concatenated_actions = NA_character_)  # Initialize the column if it doesn't exist
}

# Step 2: Add a new column to concatenate only 'Carry -> Tackle'
# Conditions: must be same GamepossID, same phase, same possession number, must not already be linked,
# teams must be different
single_carry_single_tackle <- single_carry_single_tackle %>%
  mutate(
    concatenated_actions = case_when(
      # Check for a tackle AFTER the carry
      actionName == "Carry" & 
        lead(actionName) == "Tackle" & 
        GamepossID == lead(GamepossID) &  # Ensure the same GamepossID
        Phase.number == lead(Phase.number) &  # Ensure the same Phase.Number
        Possession.number == lead(Possession.number) &  # Ensure the same Possession.Number
        teamName != lead(teamName) ~ paste(actionName, "->", lead(actionName)),  # Concatenate "Carry -> Tackle"
      
      # Check for a tackle BEFORE the carry
      actionName == "Carry" & 
        lag(actionName) == "Tackle" & 
        GamepossID == lag(GamepossID) &  # Ensure the same GamepossID
        Phase.number == lag(Phase.number) &  # Ensure the same Phase.Number
        Possession.number == lag(Possession.number) &  # Ensure the same Possession.Number
        teamName != lag(teamName) ~ paste(actionName, "->", lag(actionName)),  # Concatenate "Carry -> Tackle",
      
      TRUE ~ NA_character_  # Default case
    )
  ) %>%
  relocate(concatenated_actions, .after = actionName)  # Relocate concatenated_actions column after actionName

# Step 3: Fill 'x_tackle_moved' for rows where concatenated_actions is "Carry -> Tackle"
single_carry_single_tackle <- single_carry_single_tackle %>%
  mutate(
    x_tackle_moved = case_when(
      concatenated_actions == "Carry -> Tackle" & lead(actionName) == "Tackle" & GamepossID == lead(GamepossID) ~ lead(tackle_x_coord_reversed, default = NA),  # Fetch from next row only if GamepossID matches
      concatenated_actions == "Carry -> Tackle" & lag(actionName) == "Tackle" & GamepossID == lag(GamepossID) ~ lag(tackle_x_coord_reversed, default = NA),  # Fetch from previous row only if GamepossID matches
      TRUE ~ NA
    )
  ) %>%
  relocate(x_tackle_moved, .after = y_coord_end)  # Relocate x_tackle_moved next to y_coord_end

# Step 4: Add a column to indicate whether the linked tackle occurs before or after the carry
single_carry_single_tackle <- single_carry_single_tackle %>%
  mutate(
    link.direction = case_when(
      concatenated_actions == "Carry -> Tackle" & lead(actionName, default = NA) == "Tackle" & GamepossID == lead(GamepossID) ~ "after",  # Linked to a tackle after the carry
      concatenated_actions == "Carry -> Tackle" & lag(actionName, default = NA) == "Tackle" & GamepossID == lag(GamepossID) ~ "before",  # Linked to a tackle before the carry
      TRUE ~ NA_character_  # No link
    )
  ) %>%
  relocate(link.direction, .after = concatenated_actions)  # Relocate link.direction next to concatenated_actions

# Step 5: Calculate 'post.contact.metres' as the difference between 'x_coord_end' and 'x_tackle_moved'
single_carry_single_tackle <- single_carry_single_tackle %>%
  mutate(
    post.contact.metres = ifelse(
      !is.na(x_coord_end) & !is.na(x_tackle_moved), 
      x_coord_end - x_tackle_moved, 
      NA_real_
    )
  ) %>%
  relocate(post.contact.metres, .after = Metres.made)  # Relocate 'post.contact.metres' next to 'Metres.made'

# Step 6: Create 'tackle_name' and 'tackle_team' columns based on linked tackles
single_carry_single_tackle <- single_carry_single_tackle %>%
  mutate(
    tackle_name = case_when(
      link.direction == "after" ~ lead(playerName, default = NA),  # Tackler is in the next row
      link.direction == "before" ~ lag(playerName, default = NA),  # Tackler is in the previous row
      TRUE ~ NA_character_
    ),
    tackle_team = case_when(
      link.direction == "after" ~ lead(teamName, default = NA),  # Tackler's team (next row)
      link.direction == "before" ~ lag(teamName, default = NA),  # Tackler's team (previous row)
      TRUE ~ NA_character_
    )
  ) %>%
  relocate(tackle_name, tackle_team, .after = link.direction)  # Move next to link.direction

# Step 7: Calculate 'tackle_meters' as the difference between 'x_tackle_moved' and 'x_coord_end'
single_carry_single_tackle <- single_carry_single_tackle %>%
  mutate(
    tackle_meters = ifelse(
      !is.na(x_tackle_moved) & !is.na(x_coord_end), 
      x_tackle_moved - x_coord_end,  # Difference between tackle point and where the carry ends
      NA_real_
    )
  ) %>%
  relocate(tackle_meters, .after = post.contact.metres)  # Move next to post.contact.metres

# Step 8: Rename 'qualifier5Name' to 'Tacklers_committed' and convert to numeric
single_carry_single_tackle <- single_carry_single_tackle %>%
  rename(Tacklers_committed = qualifier5Name) %>%  # Rename column
  mutate(
    Tacklers_committed = case_when(
      str_detect(Tacklers_committed, "0") ~ 1,  
      str_detect(Tacklers_committed, "1") ~ 1,
      str_detect(Tacklers_committed, "2") ~ 2,
      str_detect(Tacklers_committed, "3") ~ 3,
      str_detect(Tacklers_committed, "4") ~ 4,  
      TRUE ~ NA_real_  # Set NA for any unexpected values
    )
  )

####### Start linking carries and tackles in single_carry_multiple_tackles dataframe #######
# Step 1: Ensure 'concatenated_actions' column exists
if (!"concatenated_actions" %in% colnames(single_carry_multiple_tackles)) {
  single_carry_multiple_tackles <- single_carry_multiple_tackles %>%
    mutate(concatenated_actions = NA_character_)  # Initialize the column if it doesn't exist
}

# Step 2: Add a new column to concatenate only 'Carry -> Tackle'
# Conditions: must be same GamepossID, same phase, same possession number, must not already be linked,
# teams must be different
single_carry_multiple_tackles <- single_carry_multiple_tackles %>%
  mutate(
    concatenated_actions = case_when(
      # Check for a tackle AFTER the carry
      actionName == "Carry" & 
        lead(actionName) == "Tackle" & 
        GamepossID == lead(GamepossID) &  # Ensure the same GamepossID
        Phase.number == lead(Phase.number) &  # Ensure the same Phase.Number
        Possession.number == lead(Possession.number) &  # Ensure the same Possession.Number
        teamName != lead(teamName) ~ paste(actionName, "->", lead(actionName)),  # Concatenate "Carry -> Tackle"
      
      # Check for a tackle BEFORE the carry
      actionName == "Carry" & 
        lag(actionName) == "Tackle" & 
        GamepossID == lag(GamepossID) &  # Ensure the same GamepossID
        Phase.number == lag(Phase.number) &  # Ensure the same Phase.Number
        Possession.number == lag(Possession.number) &  # Ensure the same Possession.Number
        teamName != lag(teamName) ~ paste(actionName, "->", lag(actionName)),  # Concatenate "Carry -> Tackle",
      
      TRUE ~ NA_character_  # Default case
    )
  ) %>%
  relocate(concatenated_actions, .after = actionName)  # Relocate concatenated_actions column after actionName

# Step 3: Fill 'x_tackle_moved' for rows where concatenated_actions is "Carry -> Tackle"
single_carry_multiple_tackles <- single_carry_multiple_tackles %>%
  mutate(
    x_tackle_moved = case_when(
      concatenated_actions == "Carry -> Tackle" & lead(actionName) == "Tackle" & GamepossID == lead(GamepossID) ~ lead(tackle_x_coord_reversed, default = NA),  # Fetch from next row only if GamepossID matches
      concatenated_actions == "Carry -> Tackle" & lag(actionName) == "Tackle" & GamepossID == lag(GamepossID) ~ lag(tackle_x_coord_reversed, default = NA),  # Fetch from previous row only if GamepossID matches
      TRUE ~ NA
    )
  ) %>%
  relocate(x_tackle_moved, .after = y_coord_end)  # Relocate x_tackle_moved next to y_coord_end

# Step 4: Add a column to indicate whether the linked tackle occurs before or after the carry
single_carry_multiple_tackles <- single_carry_multiple_tackles %>%
  mutate(
    link.direction = case_when(
      concatenated_actions == "Carry -> Tackle" & lead(actionName, default = NA) == "Tackle" & GamepossID == lead(GamepossID) ~ "after",  # Linked to a tackle after the carry
      concatenated_actions == "Carry -> Tackle" & lag(actionName, default = NA) == "Tackle" & GamepossID == lag(GamepossID) ~ "before",  # Linked to a tackle before the carry
      TRUE ~ NA_character_  # No link
    )
  ) %>%
  relocate(link.direction, .after = concatenated_actions)  # Relocate link.direction next to concatenated_actions

# Step 5: Calculate 'post.contact.metres' as the difference between 'x_coord_end' and 'x_tackle_moved'
single_carry_multiple_tackles <- single_carry_multiple_tackles %>%
  mutate(
    post.contact.metres = ifelse(
      !is.na(x_coord_end) & !is.na(x_tackle_moved), 
      x_coord_end - x_tackle_moved, 
      NA_real_
    )
  ) %>%
  relocate(post.contact.metres, .after = Metres.made)  # Relocate 'post.contact.metres' next to 'Metres.made'

# Step 6: Create 'tackle_name' and 'tackle_team' columns based on linked tackles
single_carry_multiple_tackles <- single_carry_multiple_tackles %>%
  mutate(
    tackle_name = case_when(
      link.direction == "after" ~ lead(playerName, default = NA),  # Tackler is in the next row
      link.direction == "before" ~ lag(playerName, default = NA),  # Tackler is in the previous row
      TRUE ~ NA_character_
    ),
    tackle_team = case_when(
      link.direction == "after" ~ lead(teamName, default = NA),  # Tackler's team (next row)
      link.direction == "before" ~ lag(teamName, default = NA),  # Tackler's team (previous row)
      TRUE ~ NA_character_
    )
  ) %>%
  relocate(tackle_name, tackle_team, .after = link.direction)  # Move next to link.direction

# Step 7: Calculate 'tackle_meters' as the difference between 'x_tackle_moved' and 'x_coord_end'
single_carry_multiple_tackles <- single_carry_multiple_tackles %>%
  mutate(
    tackle_meters = ifelse(
      !is.na(x_tackle_moved) & !is.na(x_coord_end), 
      x_tackle_moved - x_coord_end,  # Difference between tackle point and where the carry ends
      NA_real_
    )
  ) %>%
  relocate(tackle_meters, .after = post.contact.metres)  # Move next to post.contact.metres

# Step 8: Rename 'qualifier5Name' to 'Tacklers_committed' and convert to numeric
single_carry_multiple_tackles <- single_carry_multiple_tackles %>%
  rename(Tacklers_committed = qualifier5Name) %>%  # Rename column
  mutate(
    Tacklers_committed = case_when(
      str_detect(Tacklers_committed, "0") ~ 1,   
      str_detect(Tacklers_committed, "1") ~ 1,
      str_detect(Tacklers_committed, "2") ~ 2,
      str_detect(Tacklers_committed, "3") ~ 3,
      str_detect(Tacklers_committed, "4") ~ 4,  
      TRUE ~ NA_real_  # Set NA for any unexpected values
    )
  )

############## Start linking carries and tackles in multi_carry_multi_tackle #############
# Step 1: Ensure 'concatenated_actions' column exists
if (!"concatenated_actions" %in% colnames(multi_carry_multi_tackle)) {
  multi_carry_multi_tackle <- multi_carry_multi_tackle %>%
    mutate(concatenated_actions = NA_character_)  # Initialize the column if it doesn't exist
}

# Step 2: Add a new column to concatenate only 'Carry -> Tackle'
# Additional conditions: same GamepossID, within 3 IDs
multi_carry_multi_tackle <- multi_carry_multi_tackle %>%
  mutate(
    concatenated_actions = case_when(
      # Check for a tackle AFTER the carry
      actionName == "Carry" & 
        lead(actionName) == "Tackle" & 
        GamepossID == lead(GamepossID) &  # Ensure the same GamepossID
        abs(ID - lead(ID)) <= 3 &  # Ensure actions are within 3 IDs
        teamName != lead(teamName) ~ paste(actionName, "->", lead(actionName)),  # Concatenate "Carry -> Tackle"
      
      # Check for a tackle BEFORE the carry
      actionName == "Carry" & 
        lag(actionName) == "Tackle" & 
        GamepossID == lag(GamepossID) &  # Ensure the same GamepossID
        abs(ID - lag(ID)) <= 3 &  # Ensure actions are within 3 IDs
        teamName != lag(teamName) ~ paste(actionName, "->", lag(actionName)),  # Concatenate "Carry -> Tackle"
      
      TRUE ~ NA_character_  # Default case
    )
  ) %>%
  relocate(concatenated_actions, .after = actionName)  # Relocate concatenated_actions column after actionName

# Step 3: Fill 'x_tackle_moved' for rows where concatenated_actions is "Carry -> Tackle"
multi_carry_multi_tackle <- multi_carry_multi_tackle %>%
  mutate(
    x_tackle_moved = case_when(
      concatenated_actions == "Carry -> Tackle" & lead(actionName) == "Tackle" & 
        GamepossID == lead(GamepossID) & abs(ID - lead(ID)) <= 3 ~ lead(tackle_x_coord_reversed, default = NA),  # Fetch from next row
      concatenated_actions == "Carry -> Tackle" & lag(actionName) == "Tackle" & 
        GamepossID == lag(GamepossID) & abs(ID - lag(ID)) <= 3 ~ lag(tackle_x_coord_reversed, default = NA),  # Fetch from previous row
      TRUE ~ NA
    )
  ) %>%
  relocate(x_tackle_moved, .after = y_coord_end)  # Relocate x_tackle_moved next to y_coord_end

# Step 4: Add a column to indicate whether the linked tackle occurs before or after the carry
multi_carry_multi_tackle <- multi_carry_multi_tackle %>%
  mutate(
    link.direction = case_when(
      concatenated_actions == "Carry -> Tackle" & 
        lead(actionName, default = NA) == "Tackle" & 
        GamepossID == lead(GamepossID) & abs(ID - lead(ID)) <= 3 ~ "after",  # Linked to a tackle after the carry
      concatenated_actions == "Carry -> Tackle" & 
        lag(actionName, default = NA) == "Tackle" & 
        GamepossID == lag(GamepossID) & abs(ID - lag(ID)) <= 3 ~ "before",  # Linked to a tackle before the carry
      TRUE ~ NA_character_  # No link
    )
  ) %>%
  relocate(link.direction, .after = concatenated_actions)  # Relocate link.direction next to concatenated_actions

# Step 5: Calculate 'post.contact.metres' as the difference between 'x_coord_end' and 'x_tackle_moved'
multi_carry_multi_tackle <- multi_carry_multi_tackle %>%
  mutate(
    post.contact.metres = ifelse(
      !is.na(x_coord_end) & !is.na(x_tackle_moved), 
      x_coord_end - x_tackle_moved, 
      NA_real_
    )
  ) %>%
  relocate(post.contact.metres, .after = Metres.made)  # Relocate 'post.contact.metres' next to 'Metres.made'

# Step 6: Create 'tackle_name' and 'tackle_team' columns based on linked tackles
multi_carry_multi_tackle <- multi_carry_multi_tackle %>%
  mutate(
    tackle_name = case_when(
      link.direction == "after" ~ lead(playerName, default = NA),  # Tackler is in the next row
      link.direction == "before" ~ lag(playerName, default = NA),  # Tackler is in the previous row
      TRUE ~ NA_character_
    ),
    tackle_team = case_when(
      link.direction == "after" ~ lead(teamName, default = NA),  # Tackler's team (next row)
      link.direction == "before" ~ lag(teamName, default = NA),  # Tackler's team (previous row)
      TRUE ~ NA_character_
    )
  ) %>%
  relocate(tackle_name, tackle_team, .after = link.direction)  # Move next to link.direction

# Step 7: Calculate 'tackle_meters' as the difference between 'x_tackle_moved' and 'x_coord_end'
multi_carry_multi_tackle <- multi_carry_multi_tackle %>%
  mutate(
    tackle_meters = ifelse(
      !is.na(x_tackle_moved) & !is.na(x_coord_end), 
      x_tackle_moved - x_coord_end,  # Difference between tackle point and where the carry ends
      NA_real_
    )
  ) %>%
  relocate(tackle_meters, .after = post.contact.metres)  # Move next to post.contact.metres

# Step 8: Rename 'qualifier5Name' to 'Tacklers_committed' and convert to numeric
multi_carry_multi_tackle <- multi_carry_multi_tackle %>%
  rename(Tacklers_committed = qualifier5Name) %>%  # Rename column
  mutate(
    Tacklers_committed = case_when(
      str_detect(Tacklers_committed, "0") ~ 1,   
      str_detect(Tacklers_committed, "1") ~ 1,
      str_detect(Tacklers_committed, "2") ~ 2,
      str_detect(Tacklers_committed, "3") ~ 3,
      str_detect(Tacklers_committed, "4") ~ 4,  
      TRUE ~ NA_real_  # Set NA for any unexpected values
    )
  )

############################# / Remove results that are clear mistakes / ###################
# Filter out rows with specific GamepossID values
single_carry_single_tackle <- single_carry_single_tackle %>%
  filter(!GamepossID %in% c("33_7_1", "36_35_1", "44_2_5"))

single_carry_multiple_tackles <- single_carry_multiple_tackles %>%
  filter(!GamepossID %in% c("59_76_1", "36_54_2"))

multi_carry_multi_tackle <- multi_carry_multi_tackle %>%
  filter(!GamepossID %in% c("80_4_1", "25_66_3"))

############## FILTER OUT NA values ##########################
# Filter rows where post.contact.metres is not NA for each dataset
single_carry_single_tackle <- single_carry_single_tackle %>%
  filter(!is.na(post.contact.metres))

single_carry_multiple_tackles <- single_carry_multiple_tackles %>%
  filter(!is.na(post.contact.metres))

multi_carry_multi_tackle <- multi_carry_multi_tackle %>%
  filter(!is.na(post.contact.metres))

####### Further Refining - Take out tackles that occur before the carry starts or ends ########

# Create a new dataframe with the filtered condition:
# new dataframe filtering rows where x_tackle_moved is the same as, 1 value less than or greater than x_coord:
single_carry_single_tackle_analysis <- single_carry_single_tackle %>%
  filter(!is.na(x_tackle_moved) & x_tackle_moved >= (x_coord - 1))

single_carry_single_tackle <- single_carry_single_tackle %>%
  mutate(Tacklers_committed = as.numeric(Tacklers_committed))

# Filter for single_carry_multiple_tackles
single_carry_multiple_tackles_analysis <- single_carry_multiple_tackles %>%
  filter(!is.na(x_tackle_moved) & x_tackle_moved >= (x_coord - 1))

single_carry_multiple_tackles <- single_carry_multiple_tackles %>%
  mutate(Tacklers_committed = as.numeric(Tacklers_committed))

# Filter for multi_carry_multi_tackle
multi_carry_multi_tackle_analysis <- multi_carry_multi_tackle %>%
  filter(!is.na(x_tackle_moved) & x_tackle_moved >= (x_coord - 1))

multi_carry_multi_tackle <- multi_carry_multi_tackle %>%
  mutate(Tacklers_committed = as.numeric(Tacklers_committed))

# Now try binding the datasets again
Combined_dataset <- bind_rows(
  single_carry_single_tackle,
  single_carry_multiple_tackles,
  multi_carry_multi_tackle
)

# Remove the 'x_tackle_reversed' column from the Combined_dataset
Combined_dataset <- Combined_dataset %>% 
  dplyr::select(-tackle_x_coord_reversed)

# Remove the 'num-carries'column from the dataset 
Combined_dataset <- Combined_dataset %>% 
  dplyr::select(-`num_carries`)

# Add a new column `pre.contact.metres` and move it next to `post.contact.metres`
Combined_dataset <- Combined_dataset %>%
  mutate(
    pre.contact.metres = case_when(
      x_tackle_moved > x_coord  ~ x_tackle_moved - x_coord,  # Positive difference
      x_tackle_moved == x_coord ~ 0,                         # No difference
      x_tackle_moved < x_coord  ~ NA_real_                   # Set to NA when x_tackle_moved is less than x_coord
    )
  ) %>%
  relocate(pre.contact.metres, .after = post.contact.metres)  # Move next to `post.contact.metres`

# Extract GamepossID where post.contact.metres is 2 or more units greater than Metres.made
# Ensure columns are numeric
Combined_dataset <- Combined_dataset %>%
  mutate(
    post.contact.metres = as.numeric(post.contact.metres),
    Metres.made = as.numeric(Metres.made)
  )

# Extract GamepossID where post.contact.metres is 2 or more units greater than Metres.made
extracted_gameposs_ids <- Combined_dataset %>%
  dplyr::filter(post.contact.metres - Metres.made >= 2) %>%
  dplyr::select(GamepossID)

# Remove rows with extracted GamepossID values from Combined_dataset
Combined_dataset <- Combined_dataset %>%
  filter(!(GamepossID %in% extracted_gameposs_ids$GamepossID))

# Filter to remove rows where post.contact.metres is between -6 and -20
Combined_dataset <- Combined_dataset %>%
  filter(!(post.contact.metres >= -20 & post.contact.metres <= -6))

# Adjust x_tackle_moved values based on conditions
Combined_dataset <- Combined_dataset %>%
  # Remove rows where x_tackle_moved is 3 or more units below x_coord
  filter(!(x_tackle_moved < x_coord - 2)) %>%
  # Round up x_tackle_moved values only if they are 1-2 units below x_coord
  mutate(x_tackle_moved = ifelse(x_tackle_moved >= x_coord - 2 & x_tackle_moved < x_coord, 
                                 x_coord, x_tackle_moved))

# Create the new column 'carryend_location' based on x_coord_end
Combined_dataset <- Combined_dataset %>%
  mutate(
    carryend_location = case_when(
      x_coord_end >= -10 & x_coord_end <= 22 ~ "defensive_22",
      x_coord_end >= 23 & x_coord_end <= 77 ~ "center_field",
      x_coord_end >= 78 & x_coord_end <= 110 ~ "offensive_22",
      TRUE ~ NA_character_  # Assign NA if values do not match any category
    )
  ) %>%
  relocate(carryend_location, .after = Metres.From.tryline)  # Move next to Metres.From.tryline

# Create the new column 'tackle_location' based on x_coord_end
Combined_dataset <- Combined_dataset %>%
  mutate(
    tackle_location = case_when(
      x_coord_end >= -10 & x_coord_end <= 22 ~ "offensive_22",
      x_coord_end >= 23 & x_coord_end <= 77 ~ "center_field",
      x_coord_end >= 78 & x_coord_end <= 110 ~ "defensive_22",
      TRUE ~ NA_character_  # Assign NA if values do not match any category
    )
  ) %>%
  relocate(tackle_location, .after = carryend_location)  # Move next to Metres.From.tryline

# Create the new column 'carrystart_location' based on x_coord_end
Combined_dataset <- Combined_dataset %>%
  mutate(
    carrystart_location = case_when(
      x_coord >= -10 & x_coord <= 22 ~ "defensive_22",
      x_coord >= 23 & x_coord <= 77 ~ "center_field",
      x_coord >= 78 & x_coord <= 110 ~ "offensive_22",
      TRUE ~ NA_character_  # Assign NA if values do not match any category
    )
  ) %>%
  relocate(carrystart_location, .after = Metres.From.tryline)  # Move next to Metres.From.tryline

# Compute metres.from.tryline based on x_coord and relocate it next to x_displacement
Combined_dataset <- Combined_dataset %>%
  mutate(metres.from.tryline = case_when(
    x_coord <= 100 ~ 100 - x_coord_end,  # Normal case (x_coord ≤ 100)
    x_coord > 100 ~ 0                    # If x_coord is greater than 100, set to 0
  )) %>%
  relocate(metres.from.tryline, .after = x_displacement)  # Move metres.from.tryline next to x_displacement

# Remove metres.from.tryline from Combined_dataset
Combined_dataset <- Combined_dataset %>% 
  dplyr::select(-Metres.From.tryline)

# Compute the winning margin
Combined_dataset <- Combined_dataset %>%
  mutate(winning_margin = abs(hometeamFTscore - awayteamFTscore))

# Assign team status as Home or Away
Combined_dataset <- Combined_dataset %>%
  mutate(team_status = case_when(
    teamName == homeTeamName ~ "Home",
    teamName == awayTeamName ~ "Away",
    TRUE ~ NA_character_
  ))

# Fix x_coord
Combined_dataset <- Combined_dataset %>%
  mutate(
    metres.from.tryline = pmax(0, 100 - x_coord)
  ) %>%
  relocate(metres.from.tryline, .after = x_displacement)

# check x_coord direction
ggplot(Combined_dataset,
       aes(x = metres.from.tryline, y = Metres.made)) +
  geom_point(size = 1, alpha = 0.8) +
  geom_abline(intercept = 0, slope = -1, linetype = 2) +
  scale_x_reverse(limits = c(100, 0)) +
  labs(
    x = "Metres from attacking try line",
    y = "Metres made"
  ) +
  theme_minimal()

ggplot(Combined_dataset,
       aes(x = metres.from.tryline, y = post.contact.metres)) +
  geom_point(size = 1, alpha = 0.8) +
  geom_abline(intercept = 0, slope = -1, linetype = 2) +
  scale_x_reverse(limits = c(100, 0)) +
  labs(
    x = "Metres from attacking try line",
    y = "Post contact metres"
  ) +
  theme_minimal()

# Initial eligible carries
initial_carries <- filtered_data_carry %>%
  filter(actionName == "Carry") %>%
  nrow()

# Final analytical sample
final_carries <- nrow(Combined_dataset)

# Number excluded
excluded_carries <- initial_carries - final_carries

initial_carries
final_carries
excluded_carries

# Export Combined_dataset to an Excel file
write_xlsx(Combined_dataset, "Combined_Dataset.xlsx")

############## Defenders Beaten ###########################

# ------------------------- DATA PRE-PROCESSING ------------------------- #
filtered_defenders <- read_excel("~/LP RU KPI analysis/Datasets/raw_offload_defenders.xlsx") %>%
  rename(metres_from_tryline = Metres.From.tryline) %>%
  
  # Categorize playing positions and calculate metres from the try line
  mutate(
    distinct_playing_pos = case_when(
      playerpositionID %in% 1:3 ~ "Front_Row",
      playerpositionID %in% 4:5 ~ "Second_Row",
      playerpositionID %in% 6:8 ~ "Back_Row",
      playerpositionID %in% 9:10 ~ "Halves",
      playerpositionID %in% c(11, 14, 15) ~ "Outside_Backs",
      playerpositionID %in% 12:13 ~ "Centres",
      TRUE ~ NA_character_
    ),
    metres_from_tryline = pmax(0, 100 - x_coord)
  ) %>%
  
  # Remove passes
  filter(actionName != "Pass")

# Step 1: Count successful defenders beaten
defender_counts <- filtered_defenders %>%
  filter(ActionTypeName == "Defender Beaten") %>%
  group_by(GamepossID) %>%
  summarise(Defenders_Beaten = n(), .groups = "drop")

# Step 2: Extract failed defenders beaten events
failed_defenders <- filtered_defenders %>%
  filter(ActionTypeName == "Failed Defender Beaten") %>%
  mutate(Defenders_Beaten = 0)
summary(failed_defenders)

# Step 3: Remove any existing Defenders_Beaten column if present
if("Defenders_Beaten" %in% colnames(filtered_defenders)) {
  filtered_defenders <- filtered_defenders %>% select(-Defenders_Beaten)
}

# Step 4: Add successful beat counts into main data
filtered_defenders <- filtered_defenders %>%
  left_join(defender_counts, by = "GamepossID")

# Step 5a: Extract failed defenders beaten rows and assign 0
failed_defenders <- filtered_defenders %>%
  filter(str_to_lower(str_trim(ActionTypeName)) == "failed defender beaten") %>%
  mutate(Defenders_Beaten = 0)

# Step 5b: Remove potential duplicates (i.e., same GamepossID from successful events)
filtered_defenders <- filtered_defenders %>%
  filter(!(GamepossID %in% failed_defenders$GamepossID))

# Step 5c: Append failed attempts into main data
filtered_defenders <- bind_rows(
  filtered_defenders,
  failed_defenders
)

# Step 6: One row per GamepossID
filtered_defenders <- filtered_defenders %>% distinct(GamepossID, .keep_all = TRUE)

# Step 7: Remove unnecessary columns
columns_to_remove <- c(
  "ID", "teamName", "offload_zone", "result", "actionName", "ActionResultName", 
  "qualifier3Name", "qualifier4Name", "qualifier5Name", "x_coord", "y_coord", 
  "x_coord_end", "y_coord_end", "Metres.made", "x_displacement", "ps_endstamp", 
  "datePlayed", "hometeamFTscore", "awayteamFTscore", 
  "hometeamCurrentScore", "awayteamCurrentScore", "playerpositionID", "playerpositionName"
)
filtered_defenders <- filtered_defenders[, !(colnames(filtered_defenders) %in% columns_to_remove)]

# Step 8: Final cleaning and formatting
filtered_defenders <- filtered_defenders %>%
  mutate(
    playerName = as.factor(playerName),
    distinct_playing_pos = as.factor(distinct_playing_pos),
    defender_beaten_zone = as.factor(defender_beaten_zone),
    score_advantage = abs(score_advantage)  # Convert to absolute value
  ) %>%
  filter(
    !is.na(metres_from_tryline) &
      !is.na(defender_beaten_zone) &
      !is.na(score_advantage) &
      !is.na(distinct_playing_pos)
  )

# Step 9a: Numeric-capped version for modelling (capped at 4)
filtered_defenders <- filtered_defenders %>%
  mutate(
    Defenders_Beaten = as.numeric(Defenders_Beaten),
    Defenders_Beaten = ifelse(Defenders_Beaten >= 4, 4, Defenders_Beaten)
  )

# Step 9b: Factor version for visualization
filtered_defenders <- filtered_defenders %>%
  mutate(
    Defenders_Beaten_cat = case_when(
      Defenders_Beaten == 4 ~ "4+",
      TRUE ~ as.character(Defenders_Beaten)
    ),
    Defenders_Beaten_cat = factor(Defenders_Beaten_cat, levels = c("0", "1", "2", "3", "4+"))
  )

# Step 10: Plot using Defenders_Beaten_cat
ggplot(filtered_defenders, aes(x = Defenders_Beaten_cat)) +
  geom_bar(fill = "grey70", colour = "black") +
  labs(
    title = "Distribution of Defenders Beaten",
    x = "Defenders Beaten",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.line.y = element_line(colour = "black"))
packageVersion("brms")

#────────────────────────────────────────────────────────────────────────────
# Fit Bayesian Model with spline
#────────────────────────────────────────────────────────────────────────────
priors_nb <- c(
  prior(normal(0, 0.5), class = "b"),        # fixed effects
  prior(exponential(2), class = "sd"),       # random intercept (player)
  prior(gamma(2, 0.2), class = "shape")      # negative binomial dispersion
)

brm_defenders <- brm(
  Defenders_Beaten ~
    distinct_playing_pos +
    score_advantage +
    splines::ns(metres_from_tryline, df = 4) +
    (1 | playerName),
  data    = filtered_defenders,
  family  = negbinomial(),
  prior   = priors_nb,
  iter    = 4000,
  warmup  = 1000,
  chains  = 4,
  cores   = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 15),
  seed    = 123
)

saveRDS(brm_defenders_nb, "brm_defenders_nb.rds")

# Load the saved model from file
brm_defenders_nb <- readRDS("brm_defenders_nb.rds")

# 1. Print full summary (fixed + random)
print(summary(brm_defenders_nb), digits = 3)
performance(brm_defenders_nb)
pp_check(brm_defenders_nb)
VarCorr(brm_defenders_nb)

########################### OFFLOADS ###################################
# Start / import # 
filtered_offload <- read_excel("~/LP RU KPI analysis/Datasets/raw_offload_defenders.xlsx") %>%
  rename(metres_from_tryline = Metres.From.tryline)

# Keep only Offload rows
filtered_offload <- filtered_offload %>%
  dplyr::filter(ActionTypeName == "Offload")

# Filter offoads and give succesful Y or N
filtered_offload <- filtered_offload %>%
  mutate(
    successful = case_when(
      ActionResultName == "Own Player" ~ "YES",
      ActionResultName == "To Ground"  ~ "NO",
      TRUE                              ~ NA_character_
    )
  ) %>%
  relocate(successful, .after = ActionTypeName)

# Make offload zone
filtered_offload <- filtered_offload %>%
  mutate(
    offload_zone = case_when(
      x_coord >= 0 & x_coord < 22 ~ "Defensive_22",
      x_coord >= 22 & x_coord <= 78 ~ "Midfield",
      x_coord > 78 & x_coord <= 100 ~ "Offensive_22",
      TRUE ~ NA_character_
    )
  )

# Categorize playing positions and calculate metres from the try line
filtered_offload <- filtered_offload %>%
  mutate(
    distinct_playing_pos = case_when(
      playerpositionID %in% 1:3 ~ "Front_Row",
      playerpositionID %in% 4:5 ~ "Second_Row",
      playerpositionID %in% 6:8 ~ "Back_Row",
      playerpositionID %in% 9:10 ~ "Halves",
      playerpositionID %in% c(11, 14, 15) ~ "Outside_Backs",
      playerpositionID %in% 12:13 ~ "Centres",
      TRUE ~ NA_character_
    ),
  )

# Remove unnecessary columns, including defender_beaten_zone
columns_to_remove <- c(
  "ID","GamepossID","Possession.number","Phase.number",
  "action","ActionType...Qualified.ID","Actionresult",
  "qualifier3","qualifier4","qualifier5",
  "period","ps_timestamp","ps_endstamp","MatchTime",
  "sequence_id","player_advantage","datePlayed","kickofftime",
  "hometeamFTscore","awayteamFTscore","roundNumber","season",
  "playerShirtNumber","Phase.example",
  "defender_beaten_zone","result","actionName",
  "qualifier3Name","qualifier4Name","qualifier5Name","y_coord","y_coord_end",
  "Metres.made","x_displacement",
  "hometeamCurrentScore","awayteamCurrentScore",
  "playerpositionID","playerpositionName"
)

filtered_offload <- filtered_offload[, !(names(filtered_offload) %in% columns_to_remove)]

# Explicitly remove rows with NA in ActionResultName
filtered_offload <- filtered_offload %>%
  filter(!is.na(ActionResultName))

# Convert categorical variables to factors
filtered_offload <- filtered_offload %>%
  mutate(
    playerName = as.factor(playerName),
    distinct_playing_pos = as.factor(distinct_playing_pos),
    offload_zone = as.factor(offload_zone)
  ) %>%
  filter(!is.na(metres_from_tryline) & !is.na(offload_zone) & 
           !is.na(score_advantage) & !is.na(distinct_playing_pos))

filtered_offload <- filtered_offload %>%
  # ensure score advantage is absolute 
  mutate(score_advantage = abs(score_advantage)) 

# Give numerical values
filtered_offload <- filtered_offload %>%
  mutate(
    successful_bin = ifelse(successful == "YES", 1, 0)
  )

filtered_offload <- filtered_offload %>%
  mutate(
    x_coord = as.numeric(x_coord),
    metres_from_tryline = pmax(0, 100 - x_coord)
  )

# -------- Bayesian Mixed-Effects Logistic Regression - Offload ------- 

brm_offload_full <- brm(
  formula = successful_bin ~ distinct_playing_pos + 
    ns(metres_from_tryline, df = 4) + 
    score_advantage + 
    (1 | playerName),
  data = filtered_offload, 
  family = bernoulli(),
  chains = 4, iter = 4000, warmup = 1000, cores = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 15)
)

# Save the fitted model object
saveRDS(brm_offload_full, file = "brm_offload_full.rds")

# Load the saved model from file
brm_offload_full <- readRDS("brm_offload_full.rds")

# Inspect results
summary(brm_offload_full)
performance(brm_offload_full)
pp_check(brm_offload_full)
VarCorr(brm_offload_full)

############################### POST CONTACT METRES #############################
# ------------------------- DATA PRE-PROCESSING ------------------------- #
regression_data <- read_excel("~/Combined_Dataset.xlsx")%>%
  clean_names() %>%
  rename(datePlayed = date_played) %>%
  mutate(datePlayed = as.Date(datePlayed))  # Convert date to Date format

# Prepare regression dataset with selected variables 
regression_data <- regression_data %>% 
  dplyr::select(
    player_name, team_name, tackle_team, tackle_name, possession_number, action_type_name,
    carryend_location, tacklers_committed, metres_made, post_contact_metres, 
    metres_from_tryline, distinct_playing_pos, score_advantage
  )

# Ensure the specified columns are numeric
regression_data <- regression_data %>%
  mutate(across(c(possession_number, tacklers_committed, metres_made, 
                  post_contact_metres, metres_from_tryline, score_advantage), 
                ~ as.numeric(.)))

# Convert categorical variables to factors
regression_data <- regression_data %>%
  mutate(
    player_name = as.factor(player_name),
    tackle_team = as.factor(tackle_team),
    tackle_name = as.factor(tackle_name),
    team_name = as.factor(team_name),
    carryend_location = as.factor(carryend_location),
    distinct_playing_pos = as.factor(distinct_playing_pos),
    action_type_name = as.factor(action_type_name)
  )

# Make score advantage absolute
regression_data <- regression_data %>%
  mutate(score_advantage = abs(score_advantage))

# Adjust data from feedback 
regression_data <- regression_data %>%
  mutate(
    # Tacklers as factor
    tacklers_committed_cat  = factor(tacklers_committed),
    # ensure position is factor
    distinct_playing_pos    = factor(distinct_playing_pos),
    carryend_location     = factor(carryend_location),
    player_name             = factor(player_name)
  )

##### -- Skew T Regression Analysis Models ---- 30-45 min on LaTrobe laptop for both models #######
## Metres Made 

brm_mm_ctx <- brm(
  metres_made ~
    ns(metres_from_tryline, df = 4) +
    distinct_playing_pos +
    score_advantage +
    action_type_name +
    (1 | player_name),
  data    = regression_data,
  family  = negbinomial(),
  iter    = 4000, chains = 4, cores = 4,
  control = list(adapt_delta = 0.95),
  seed    = 123
)

## Post-Contact Metres
brm_pcm_ctx <- brm(
  post_contact_metres ~
    ns(metres_from_tryline, df = 4) +
    distinct_playing_pos +
    score_advantage +
    action_type_name +
    (1 | player_name),
  data    = regression_data,
  family  = skew_normal(),
  iter    = 4000, chains = 4, cores = 4,
  control = list(adapt_delta = 0.95),
  seed    = 123
)

# ──────────────────────────────────────────────────────────────────
# Summaries + Random‐Intercept Variance & ICC
# Print summaries - performance will start sampling again - 20min

summary(brm_mm_ctx)
summary(brm_pcm_ctx)
pp_check(brm_mm_ctx)
pp_check(brm_pcm_ctx)
levels(regression_data$action_type_name)
nobs(brm_mm_ctx)
VarCorr(brm_pcm_ctx)
VarCorr(brm_mm_ctx)
performance(brm_mm_ctx)
performance(brm_pcm_ctx)