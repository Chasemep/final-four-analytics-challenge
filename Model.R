################################################################################
# IMPORTS AND INSTALLS
################################################################################
library(tools)
################################################################################

################################################################################
# Read Data
################################################################################
# File paths
test_path = "data/NCAA_Seed_Test_Set2.0.csv"
train_path = "data/NCAA_Seed_Training_Set2.0.csv"
submission_template = "data/submission_template2.0.csv"

# Read data to be cleaned
test_input = read.csv(test_path, na.strings = "", stringsAsFactors = FALSE)
train_input = read.csv(train_path, na.strings = "", stringsAsFactors = FALSE)
################################################################################

################################################################################
# SPLIT RECORDS FUNCTION
################################################################################
# Function to clean and split record strings
clean_and_split <- function(vec) {
  # Month mapping
  month_map <- c(
    Jan = 1, Feb = 2, Mar = 3, Apr = 4, May = 5, Jun = 6,
    Jul = 7, Aug = 8, Sep = 9, Oct = 10, Nov = 11, Dec = 12
  )
  
  # Translate 00 to 0
  vec <- gsub("-00$", "-0", vec)
  
  # Split by dash
  parts <- strsplit(as.character(vec), "-")
  
  # Helper to convert part (month or number) to integer string
  convert_part <- function(p) {
    if (is.na(p) || p == "" || p == "NA") return("0")
    
    p_clean <- tolower(trimws(p))
    
    # Month mapping
    month_map <- c(
      jan = 1, feb = 2, mar = 3, apr = 4, may = 5, jun = 6,
      jul = 7, aug = 8, sep = 9, oct = 10, nov = 11, dec = 12
    )
    
    if (p_clean %in% names(month_map)) {
      return(as.character(month_map[p_clean]))
    }
    
    # If not a month, return original (as.integer will handle it)
    return(p)
  }
  
  first <- sapply(parts, function(p) if(length(p) >= 1) convert_part(p[1]) else "0")
  second <- sapply(parts, function(p) if(length(p) >= 2) convert_part(p[2]) else "0")
  
  return(data.frame(Win = as.integer(first), Loss = as.integer(second)))
}
################################################################################

################################################################################
# SPLIT FUNCTION CALLS
################################################################################
# Train
train_records_wl <- clean_and_split(train_input$WL)
train_records_conf <- clean_and_split(train_input$Conf.Record)
train_records_nonconf <- clean_and_split(train_input$Non.ConferenceRecord)
train_records_road <- clean_and_split(train_input$RoadWL)
train_records_q1 <- clean_and_split(train_input$Quadrant1)
train_records_q2 <- clean_and_split(train_input$Quadrant2)
train_records_q3 <- clean_and_split(train_input$Quadrant3)
train_records_q4 <- clean_and_split(train_input$Quadrant4)

# Test
test_records_wl <- clean_and_split(test_input$WL)
test_records_conf <- clean_and_split(test_input$Conf.Record)
test_records_nonconf <- clean_and_split(test_input$Non.ConferenceRecord)
test_records_road <- clean_and_split(test_input$RoadWL)
test_records_q1 <- clean_and_split(test_input$Quadrant1)
test_records_q2 <- clean_and_split(test_input$Quadrant2)
test_records_q3 <- clean_and_split(test_input$Quadrant3)
test_records_q4 <- clean_and_split(test_input$Quadrant4)
################################################################################

################################################################################
# DATA FRAME
################################################################################
# Train
train <- data.frame(
  RecordID = train_input$RecordID,
  Season = sapply(strsplit(train_input$Season, "-"), `[`, 1),
  Team = train_input$Team,
  Conference = train_input$Conference,
  Overall.Seed = train_input$Overall.Seed,
  Bid.Type = train_input$Bid.Type,
  Net.Rank = train_input$NET.Rank,
  prevNET = train_input$PrevNET,
  AvgOppNETRank = train_input$AvgOppNETRank,
  AvgOppNET = train_input$AvgOppNET,
  NETSOS = train_input$NETSOS,
  NETNonConfSOS = train_input$NETNonConfSOS,
  Win = train_records_wl$Win,
  Loss = train_records_wl$Loss,
  Conf.Win = train_records_conf$Win,
  Conf.Loss = train_records_conf$Loss,
  Non.Conference.Win = train_records_nonconf$Win,
  Non.Conference.Loss = train_records_nonconf$Loss,
  RoadWin = train_records_road$Win,
  RoadLoss = train_records_road$Loss,
  Quadrant1.Win = train_records_q1$Win,
  Quadrant1.Loss = train_records_q1$Loss,
  Quadrant2.Win = train_records_q2$Win,
  Quadrant2.Loss = train_records_q2$Loss,
  Quadrant3.Win = train_records_q3$Win,
  Quadrant3.Loss = train_records_q3$Loss,
  Quadrant4.Win = train_records_q4$Win,
  Quadrant4.Loss = train_records_q4$Loss
)

# Test
test <- data.frame(
  RecordID = test_input$RecordID,
  Season = sapply(strsplit(test_input$Season, "-"), `[`, 1),
  Team = test_input$Team,
  Conference = test_input$Conference,
  Bid.Type = test_input$Bid.Type,
  Net.Rank = test_input$NET.Rank,
  prevNET = test_input$PrevNET,
  AvgOppNETRank = test_input$AvgOppNETRank,
  AvgOppNET = test_input$AvgOppNET,
  NETSOS = test_input$NETSOS,
  NETNonConfSOS = test_input$NETNonConfSOS,
  Win = test_records_wl$Win,
  Loss = test_records_wl$Loss,
  Conf.Win = test_records_conf$Win,
  Conf.Loss = test_records_conf$Loss,
  Non.Conference.Win = test_records_nonconf$Win,
  Non.Conference.Loss = test_records_nonconf$Loss,
  RoadWin = test_records_road$Win,
  RoadLoss = test_records_road$Loss,
  Quadrant1.Win = test_records_q1$Win,
  Quadrant1.Loss = test_records_q1$Loss,
  Quadrant2.Win = test_records_q2$Win,
  Quadrant2.Loss = test_records_q2$Loss,
  Quadrant3.Win = test_records_q3$Win,
  Quadrant3.Loss = test_records_q3$Loss,
  Quadrant4.Win = test_records_q4$Win,
  Quadrant4.Loss = test_records_q4$Loss
)
################################################################################

################################################################################
# RATIO CALCULATION
################################################################################
# Train
train$WL.Ratio = train_records_wl$Win/(train_records_wl$Loss+1)
train$Conf.Ratio = train_records_conf$Win/(train_records_conf$Loss+1)
train$Non.Conference.Ratio = train_records_nonconf$Win/(train_records_nonconf$Loss+1)
train$RoadRatio = train_records_road$Win/(train_records_road$Loss+1)
train$Quadrant1.Ratio = train_records_q1$Win/(train_records_q1$Loss+1)
train$Quadrant2.Ratio = train_records_q2$Win/(train_records_q2$Loss+1)
train$Quadrant3.Ratio = train_records_q3$Win/(train_records_q3$Loss+1)
train$Quadrant4.Ratio = train_records_q4$Win/(train_records_q4$Loss+1)

# Test
test$WL.Ratio = test_records_wl$Win/(test_records_wl$Loss+1)
test$Conf.Ratio = test_records_conf$Win/(test_records_conf$Loss+1)
test$Non.Conference.Ratio = test_records_nonconf$Win/(test_records_nonconf$Loss+1)
test$RoadRatio = test_records_road$Win/(test_records_road$Loss+1)
test$Quadrant1.Ratio = test_records_q1$Win/(test_records_q1$Loss+1)
test$Quadrant2.Ratio = test_records_q2$Win/(test_records_q2$Loss+1)
test$Quadrant3.Ratio = test_records_q3$Win/(test_records_q3$Loss+1)
test$Quadrant4.Ratio = test_records_q4$Win/(test_records_q4$Loss+1)
################################################################################

################################################################################
# WAB
################################################################################
# Import match data collected from https://collegebasketballdata.com
match_data = read.csv("C:\\Users\\Chase\\Desktop\\NCAAStuff\\CleanData\\college_basketball_games_2020_2025.csv")

# Combined training and test data to lower calculations
all_teams_data = rbind(train[, c("Team", "Season", "Net.Rank")], 
                       test[, c("Team", "Season", "Net.Rank")])

# Merge Net Rank for home teams into match data
match_data = merge(match_data, all_teams_data, 
                    by.x = c("homeTeam", "season"), 
                    by.y = c("Team", "Season"), 
                    all.x = TRUE)
names(match_data)[names(match_data) == "Net.Rank"] <- "homeTeamRating"

# Merge Net Rank for away teams into match data
match_data = merge(match_data, all_teams_data, 
                    by.x = c("awayTeam", "season"), 
                    by.y = c("Team", "Season"), 
                    all.x = TRUE)
names(match_data)[names(match_data) == "Net.Rank"] <- "awayTeamRating"

# Fill missing ratings with a default (e.g., 350 for non-DI or weak teams)
match_data$homeTeamRating[is.na(match_data$homeTeamRating)] <- 350
match_data$awayTeamRating[is.na(match_data$awayTeamRating)] <- 350

# Predict the probability of the home team winning using a logistic model
model = glm(homeWinner ~ I(awayTeamRating - homeTeamRating), 
            data = match_data, family = binomial)

# 1. Calculate Probability of home team win for the actual team
match_data$homeTeamWinProb = predict(model, match_data, type = "response")

# Calculate Probability for a "Bubble Team" (e.g. Rank 85)
# Probability a Bubble Team wins at THIS Home Venue
R_bubble = 85
bubble_home_df = match_data
bubble_home_df$homeTeamRating = R_bubble
match_data$bubbleWinProb_Home = predict(model, bubble_home_df, type = "response")

# Probability a Bubble Team wins at THIS Away Venue
bubble_away_df = match_data
bubble_away_df$awayTeamRating = R_bubble
match_data$bubbleWinProb_Away = predict(model, bubble_away_df, type = "response")

# WAB = Actual Result (1 or 0) - Expected Result for a Bubble Team
match_data$WAB_home_game = as.numeric(match_data$homeWinner) - match_data$bubbleWinProb_Home
match_data$WAB_away_game = as.numeric(match_data$awayWinner) - match_data$bubbleWinProb_Away

# WAB from Home Games
home_wab = aggregate(WAB_home_game ~ homeTeam + season, data = match_data, sum)
names(home_wab) = c("Team", "Season", "WAB_Home_Total")

# WAB from Away Games
away_wab = aggregate(WAB_away_game ~ awayTeam + season, data = match_data, sum)
names(away_wab) = c("Team", "Season", "WAB_Away_Total")

# Merge them together
final_wab_df = merge(home_wab, away_wab, by = c("Team", "Season"), all = TRUE)
final_wab_df[is.na(final_wab_df)] <- 0
final_wab_df$WAB = final_wab_df$WAB_Home_Total + final_wab_df$WAB_Away_Total

traindf_final <- merge(train, final_wab_df[, c("Team", "Season", "WAB")], 
                       by = c("Team", "Season"), all.x = TRUE)

testdf_final <- merge(test, final_wab_df[, c("Team", "Season", "WAB")], 
                      by = c("Team", "Season"), all.x = TRUE)

train$WAB = traindf_final$WAB
test$WAB = testdf_final$WAB
train$WAB[is.na(train$WAB)] = 0
test$WAB[is.na(test$WAB)] = 0

colSums(is.na(train))
train = subset(train, !is.na(NETNonConfSOS)&!is.na(Net.Rank))
colSums(is.na(train))

colSums(is.na(test))
################################################################################

################################################################################
# Linear Model
################################################################################
names(train)
head(train)
model = lm(Overall.Seed~Net.Rank+prevNET+AvgOppNETRank+AvgOppNET+NETSOS+NETNonConfSOS+Win+Loss+Conf.Win+Conf.Loss+Non.Conference.Win+RoadWin+RoadLoss+Quadrant1.Win+Quadrant1.Loss+Quadrant2.Win+Quadrant2.Loss+Quadrant3.Win+Quadrant3.Loss+Quadrant4.Win+WL.Ratio+Conf.Ratio+Non.Conference.Ratio+RoadRatio+Quadrant1.Ratio+Quadrant2.Ratio+Quadrant3.Ratio+Quadrant4.Ratio, data = train)
# Removed +Quadrant4.Loss+Non.Conference.Loss

summary(model)

predicted_seeds = round(predict(model, test))
length(predicted_seeds)
################################################################################

################################################################################
# XGBoost
################################################################################
library(xgboost)
trainXG = subset(train, !is.na(train$Overall.Seed))
names(train)
XTrain <- as.matrix(trainXG[, c("WAB", "Net.Rank", "Win", "Loss","Conf.Win","Conf.Loss","Non.Conference.Win","Non.Conference.Loss","RoadWin","RoadLoss","Quadrant1.Win","Quadrant1.Loss","Quadrant2.Win","Quadrant2.Loss","Quadrant3.Win","Quadrant3.Loss","Quadrant4.Win","Quadrant4.Loss","WL.Ratio","Conf.Ratio","Non.Conference.Ratio","RoadRatio","Quadrant1.Ratio","Quadrant2.Ratio","Quadrant3.Ratio","Quadrant4.Ratio")])
XTest <- as.matrix(test[, c("WAB", "Net.Rank", "Win", "Loss","Conf.Win","Conf.Loss","Non.Conference.Win","Non.Conference.Loss","RoadWin","RoadLoss","Quadrant1.Win","Quadrant1.Loss","Quadrant2.Win","Quadrant2.Loss","Quadrant3.Win","Quadrant3.Loss","Quadrant4.Win","Quadrant4.Loss","WL.Ratio","Conf.Ratio","Non.Conference.Ratio","RoadRatio","Quadrant1.Ratio","Quadrant2.Ratio","Quadrant3.Ratio","Quadrant4.Ratio")])
y <- trainXG$Overall.Seed

good <- complete.cases(XTrain, y)
XTrain <- XTrain[good, ]
y <- y[good]

dtrain <- xgb.DMatrix(data = XTrain, label = y)

param_list = list(
  objective = "reg:squarederror",
  eta = 0.05,
  gamma = 1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.5
)

set.seed(112) # Setting seed
xgbcv = xgb.cv(params = param_list,
               data = dtrain,
               nrounds = 1000,
               nfold = 5,
               print_every_n = 10,
               early_stopping_rounds = 30,
               maximize = F)

best_nrounds <- xgbcv$niter

model_xgb <- xgb.train(
  data = dtrain,
  params = param_list,
  nrounds = best_nrounds
)

predicted_seeds <- round(predict(model_xgb, XTest))
################################################################################

################################################################################
# SUBMISSION
################################################################################
# Load submission template
submission = read.csv(submission_template)
submission$Overall.Seed = predicted_seeds
submission$Overall.Seed[is.na(test$Bid.Type)] = 0
submission$Overall.Seed[submission$Overall.Seed>68] = 68
submission$Overall.Seed[submission$Overall.Seed<0] = 1
names(submission)[2] = "Overall Seed"

# Write final submission file
write.csv(submission, "submissions/submission.csv", row.names = FALSE)
################################################################################