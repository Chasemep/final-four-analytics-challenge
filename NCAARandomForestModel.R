################################################################################
# NCAA SEED PREDICTION MODEL
# Best Score: 1.94935 RMSE
# Models: XGBoost + Linear Model Ensemble
# Post-processing: Domain knowledge caps/floors based on NET rank
################################################################################

################################################################################
# IMPORTS
################################################################################
library(xgboost)
library(dplyr)
library(randomForest)

################################################################################
# READ DATA
# Update base_path to match your local directory
################################################################################
base_path   <- "/Users/gowrisreejuttiga/Documents/NCAA Analytics/final-four-analytics-challenge/"

test_input  <- read.csv(paste0(base_path, "data/NCAA_Seed_Test_Set2.0.csv"),      na.strings = "", stringsAsFactors = FALSE)
train_input <- read.csv(paste0(base_path, "data/NCAA_Seed_Training_Set2.0.csv"),  na.strings = "", stringsAsFactors = FALSE)
match_data  <- read.csv(paste0(base_path, "data/college_basketball_games_2020_2025.csv"))

# Fix column names with spaces and dashes
names(train_input) <- gsub(" ", ".", names(train_input))
names(test_input)  <- gsub(" ", ".", names(test_input))
names(train_input) <- gsub("-", ".", names(train_input))
names(test_input)  <- gsub("-", ".", names(test_input))

################################################################################
# SPLIT RECORDS FUNCTION
# Raw data stores records as strings like "22-2" or "8-Sep" (date encoding)
# This function splits them into separate Win and Loss columns
################################################################################
clean_and_split <- function(vec) {
  month_map <- c(jan=1,feb=2,mar=3,apr=4,may=5,jun=6,
                 jul=7,aug=8,sep=9,oct=10,nov=11,dec=12)
  vec <- gsub("-00$", "-0", vec)
  parts <- strsplit(as.character(vec), "-")
  
  convert_part <- function(p) {
    if (is.na(p) || p == "" || p == "NA") return("0")
    p_clean <- tolower(trimws(p))
    if (p_clean %in% names(month_map)) return(as.character(month_map[p_clean]))
    return(p)
  }
  
  first  <- sapply(parts, function(p) if (length(p) >= 1) convert_part(p[1]) else "0")
  second <- sapply(parts, function(p) if (length(p) >= 2) convert_part(p[2]) else "0")
  data.frame(Win = as.integer(first), Loss = as.integer(second))
}

################################################################################
# PARSE RECORD COLUMNS
################################################################################
parse_records <- function(df) {
  list(
    wl      = clean_and_split(df$WL),
    conf    = clean_and_split(df$Conf.Record),
    nonconf = clean_and_split(df$Non.ConferenceRecord),
    road    = clean_and_split(df$RoadWL),
    q1      = clean_and_split(df$Quadrant1),
    q2      = clean_and_split(df$Quadrant2),
    q3      = clean_and_split(df$Quadrant3),
    q4      = clean_and_split(df$Quadrant4)
  )
}

train_rec <- parse_records(train_input)
test_rec  <- parse_records(test_input)

################################################################################
# BUILD CLEAN DATAFRAMES
################################################################################
build_df <- function(raw, rec, include_seed = FALSE) {
  df <- data.frame(
    RecordID      = raw$RecordID,
    Season        = sapply(strsplit(raw$Season, "-"), `[`, 1),
    Team          = raw$Team,
    Conference    = raw$Conference,
    Bid.Type      = raw$Bid.Type,
    Net.Rank      = raw$NET.Rank,
    prevNET       = raw$PrevNET,
    AvgOppNETRank = raw$AvgOppNETRank,
    AvgOppNET     = raw$AvgOppNET,
    NETSOS        = raw$NETSOS,
    NETNonConfSOS = raw$NETNonConfSOS,
    Win           = rec$wl$Win,
    Loss          = rec$wl$Loss,
    Conf.Win      = rec$conf$Win,
    Conf.Loss     = rec$conf$Loss,
    NonConf.Win   = rec$nonconf$Win,
    NonConf.Loss  = rec$nonconf$Loss,
    Road.Win      = rec$road$Win,
    Road.Loss     = rec$road$Loss,
    Q1.Win        = rec$q1$Win,
    Q1.Loss       = rec$q1$Loss,
    Q2.Win        = rec$q2$Win,
    Q2.Loss       = rec$q2$Loss,
    Q3.Win        = rec$q3$Win,
    Q3.Loss       = rec$q3$Loss,
    Q4.Win        = rec$q4$Win,
    Q4.Loss       = rec$q4$Loss
  )
  if (include_seed) df$Overall.Seed <- raw$Overall.Seed
  df
}

train <- build_df(train_input, train_rec, include_seed = TRUE)
test  <- build_df(test_input,  test_rec,  include_seed = FALSE)

################################################################################
# WIN/LOSS RATIOS
# Ratios capture performance better than raw counts
# Add 1 to denominator to avoid dividing by zero
################################################################################
add_ratios <- function(df, rec) {
  df$WL.Ratio      <- rec$wl$Win      / (rec$wl$Loss      + 1)
  df$Conf.Ratio    <- rec$conf$Win    / (rec$conf$Loss    + 1)
  df$NonConf.Ratio <- rec$nonconf$Win / (rec$nonconf$Loss + 1)
  df$Road.Ratio    <- rec$road$Win    / (rec$road$Loss    + 1)
  df$Q1.Ratio      <- rec$q1$Win      / (rec$q1$Loss      + 1)
  df$Q2.Ratio      <- rec$q2$Win      / (rec$q2$Loss      + 1)
  df$Q3.Ratio      <- rec$q3$Win      / (rec$q3$Loss      + 1)
  df$Q4.Ratio      <- rec$q4$Win      / (rec$q4$Loss      + 1)
  df
}

train <- add_ratios(train, train_rec)
test  <- add_ratios(test,  test_rec)

################################################################################
# QUADRANT SCORE
# Single summary score weighting Q1 wins most heavily (best opponents)
################################################################################
quad_score <- function(df) {
  (df$Q1.Win - df$Q1.Loss) * 0.4 +
    (df$Q2.Win - df$Q2.Loss) * 0.3 +
    (df$Q3.Win - df$Q3.Loss) * 0.2 +
    (df$Q4.Win - df$Q4.Loss) * 0.1
}

train$Q.Score <- quad_score(train)
test$Q.Score  <- quad_score(test)

################################################################################
# WAB (Wins Above Bubble)
# Measures how much better/worse a team performed vs a bubble team (NET ~85)
# against the same schedule
################################################################################
all_teams <- rbind(
  train[, c("Team", "Season", "Net.Rank")],
  test[,  c("Team", "Season", "Net.Rank")]
)

match_data <- merge(match_data, all_teams,
                    by.x = c("homeTeam", "season"),
                    by.y = c("Team", "Season"), all.x = TRUE)
names(match_data)[names(match_data) == "Net.Rank"] <- "homeTeamRating"

match_data <- merge(match_data, all_teams,
                    by.x = c("awayTeam", "season"),
                    by.y = c("Team", "Season"), all.x = TRUE)
names(match_data)[names(match_data) == "Net.Rank"] <- "awayTeamRating"

match_data$homeTeamRating[is.na(match_data$homeTeamRating)] <- 350
match_data$awayTeamRating[is.na(match_data$awayTeamRating)] <- 350

wab_model <- glm(homeWinner ~ I(awayTeamRating - homeTeamRating),
                 data = match_data, family = binomial)

R_bubble <- 85
bubble_home_df <- match_data; bubble_home_df$homeTeamRating <- R_bubble
bubble_away_df <- match_data; bubble_away_df$awayTeamRating <- R_bubble

match_data$bubbleWinProb_Home <- predict(wab_model, bubble_home_df, type = "response")
match_data$bubbleWinProb_Away <- predict(wab_model, bubble_away_df, type = "response")
match_data$WAB_home <- as.numeric(match_data$homeWinner) - match_data$bubbleWinProb_Home
match_data$WAB_away <- as.numeric(match_data$awayWinner) - match_data$bubbleWinProb_Away

home_wab <- aggregate(WAB_home ~ homeTeam + season, data = match_data, sum)
away_wab <- aggregate(WAB_away ~ awayTeam + season, data = match_data, sum)
names(home_wab) <- c("Team", "Season", "WAB_Home")
names(away_wab) <- c("Team", "Season", "WAB_Away")

final_wab <- merge(home_wab, away_wab, by = c("Team", "Season"), all = TRUE)
final_wab[is.na(final_wab)] <- 0
final_wab$WAB <- final_wab$WAB_Home + final_wab$WAB_Away

train <- merge(train, final_wab[, c("Team", "Season", "WAB")],
               by = c("Team", "Season"), all.x = TRUE)
test  <- merge(test,  final_wab[, c("Team", "Season", "WAB")],
               by = c("Team", "Season"), all.x = TRUE)

train$WAB[is.na(train$WAB)] <- 0
test$WAB[is.na(test$WAB)]   <- 0

train <- subset(train, !is.na(NETNonConfSOS) & !is.na(Net.Rank))

cat("Train rows:", nrow(train), "| Test rows:", nrow(test), "\n")
cat("Seeds available:", sum(!is.na(train$Overall.Seed)), "\n")

################################################################################
# FEATURE SETUP
################################################################################
base_features <- c(
  "WAB",
  "Net.Rank", "prevNET", "AvgOppNETRank", "AvgOppNET", "NETSOS", "NETNonConfSOS",
  "Win", "Loss",
  "Conf.Win", "Conf.Loss",
  "NonConf.Win", "NonConf.Loss",
  "Road.Win", "Road.Loss",
  "Q1.Win", "Q1.Loss",
  "Q2.Win", "Q2.Loss",
  "Q3.Win", "Q3.Loss",
  "Q4.Win", "Q4.Loss",
  "WL.Ratio", "Conf.Ratio", "NonConf.Ratio", "Road.Ratio",
  "Q1.Ratio", "Q2.Ratio", "Q3.Ratio", "Q4.Ratio",
  "Q.Score"
)

trainXG <- subset(train, !is.na(Overall.Seed))

# Impute missing values with training medians
XTrain_raw <- as.matrix(trainXG[, base_features])
XTest_raw  <- as.matrix(test[, base_features])
train_medians <- apply(XTrain_raw, 2, median, na.rm = TRUE)
for (j in seq_len(ncol(XTrain_raw))) {
  XTrain_raw[is.na(XTrain_raw[, j]), j] <- train_medians[j]
  XTest_raw[is.na(XTest_raw[, j]), j]   <- train_medians[j]
}

XTrain <- XTrain_raw
XTest  <- XTest_raw
y      <- trainXG$Overall.Seed

good   <- complete.cases(XTrain, y)
XTrain <- XTrain[good, ]
y      <- y[good]
cat("Training rows:", nrow(XTrain), "\n")

# Local train/val split (80/20)
set.seed(112)
val_idx <- sample(seq_len(nrow(XTrain)), size = floor(0.2 * nrow(XTrain)))
X_val <- XTrain[val_idx, ];   y_val <- y[val_idx]
X_tr  <- XTrain[-val_idx, ];  y_tr  <- y[-val_idx]

dtrain <- xgb.DMatrix(data = X_tr,   label = y_tr)
dval   <- xgb.DMatrix(data = X_val,  label = y_val)
dfull  <- xgb.DMatrix(data = XTrain, label = y)
dtest  <- xgb.DMatrix(data = XTest)

################################################################################
# MODEL 1: XGBOOST
################################################################################
xgb_params <- list(
  objective        = "reg:squarederror",
  eval_metric      = "rmse",
  eta              = 0.05,
  max_depth        = 2,
  min_child_weight = 10,
  subsample        = 0.8,
  colsample_bytree = 0.5,
  lambda           = 5
)
xgb_rounds <- 50

model_xgb_val  <- xgb.train(data=dtrain, params=xgb_params, nrounds=xgb_rounds, verbose=0)
xgb_val_raw    <- predict(model_xgb_val, dval)
xgb_local_rmse <- sqrt(mean((round(xgb_val_raw) - y_val)^2))
cat("XGBoost Local RMSE:", round(xgb_local_rmse, 5), "\n")

model_xgb       <- xgb.train(data=dfull, params=xgb_params, nrounds=xgb_rounds)
predicted_seeds <- round(predict(model_xgb, dtest))

################################################################################
# MODEL 2: LINEAR MODEL
################################################################################
lm_features <- c(
  "Net.Rank", "prevNET", "AvgOppNETRank", "AvgOppNET", "NETSOS", "NETNonConfSOS",
  "Win", "Loss", "Conf.Win", "Conf.Loss", "NonConf.Win", "NonConf.Loss",
  "Road.Win", "Road.Loss", "Q1.Win", "Q1.Loss", "Q2.Win", "Q2.Loss",
  "Q3.Win", "Q3.Loss", "Q4.Win", "Q4.Loss",
  "WL.Ratio", "Conf.Ratio", "NonConf.Ratio", "Road.Ratio",
  "Q1.Ratio", "Q2.Ratio", "Q3.Ratio", "Q4.Ratio", "Q.Score", "WAB"
)

lm_train <- trainXG
lm_test  <- test
for (col in lm_features) {
  med <- median(lm_train[[col]], na.rm = TRUE)
  lm_train[[col]][is.na(lm_train[[col]])] <- med
  lm_test[[col]][is.na(lm_test[[col]])]   <- med
}

lm_formula <- as.formula(paste("Overall.Seed ~", paste(lm_features, collapse = " + ")))
model_lm     <- lm(lm_formula, data = lm_train)
lm_predicted <- predict(model_lm, lm_test)
lm_val_raw   <- predict(model_lm, lm_train[val_idx, ])
lm_local_rmse <- sqrt(mean((round(lm_val_raw) - y_val)^2))
cat("Linear Model Local RMSE:", round(lm_local_rmse, 5), "\n")

################################################################################
# MODEL 3: RANDOM FOREST
################################################################################
set.seed(112)
rf_train_df <- data.frame(XTrain)
rf_train_df$y <- y

model_rf <- randomForest(
  y ~ .,
  data      = rf_train_df,
  ntree     = 500,
  mtry      = floor(sqrt(ncol(XTrain))),
  nodesize  = 5,
  importance = TRUE
)

rf_val_raw    <- predict(model_rf, data.frame(XTrain[val_idx, ]))
rf_local_rmse <- sqrt(mean((round(rf_val_raw) - y_val)^2))
cat("Random Forest Local RMSE:", round(rf_local_rmse, 5), "\n")

rf_predicted <- round(predict(model_rf, data.frame(XTest)))

################################################################################
# ENSEMBLE - pick best combination based on local RMSE
################################################################################

# Test all combinations locally
cat("\nTesting ensemble combinations:\n")
# XGB + LM 50/50
ens_xgb_lm     <- round((xgb_val_raw + lm_val_raw) / 2)
rmse_xgb_lm    <- sqrt(mean((ens_xgb_lm - y_val)^2))

# RF + LM 50/50
ens_rf_lm      <- round((rf_val_raw + lm_val_raw) / 2)
rmse_rf_lm     <- sqrt(mean((ens_rf_lm - y_val)^2))

# RF + XGB 50/50
ens_rf_xgb     <- round((rf_val_raw + xgb_val_raw) / 2)
rmse_rf_xgb    <- sqrt(mean((ens_rf_xgb - y_val)^2))

# All three equally
ens_all        <- round((rf_val_raw + xgb_val_raw + lm_val_raw) / 3)
rmse_all       <- sqrt(mean((ens_all - y_val)^2))

cat(sprintf("XGBoost alone:      %.5f\n", xgb_local_rmse))
cat(sprintf("Linear alone:       %.5f\n", lm_local_rmse))
cat(sprintf("Random Forest alone:%.5f\n", rf_local_rmse))
cat(sprintf("XGB + LM (50/50):   %.5f\n", rmse_xgb_lm))
cat(sprintf("RF  + LM (50/50):   %.5f\n", rmse_rf_lm))
cat(sprintf("RF  + XGB (50/50):  %.5f\n", rmse_rf_xgb))
cat(sprintf("All three (equal):  %.5f\n", rmse_all))

# Pick best
all_rmses <- c(
  xgb_local_rmse, lm_local_rmse, rf_local_rmse,
  rmse_xgb_lm, rmse_rf_lm, rmse_rf_xgb, rmse_all
)
best_idx <- which.min(all_rmses)
labels   <- c("XGBoost", "Linear", "RandomForest",
              "XGB+LM", "RF+LM", "RF+XGB", "All Three")
cat("\nBest local combination:", labels[best_idx], "\n")

# Build final test predictions using best combo
best_preds <- switch(labels[best_idx],
                     "XGBoost"      = predicted_seeds,
                     "Linear"       = round(lm_predicted),
                     "RandomForest" = rf_predicted,
                     "XGB+LM"       = round((predicted_seeds + lm_predicted) / 2),
                     "RF+LM"        = round((rf_predicted    + lm_predicted) / 2),
                     "RF+XGB"       = round((rf_predicted    + predicted_seeds) / 2),
                     "All Three"    = round((rf_predicted    + predicted_seeds + lm_predicted) / 3)
)

################################################################################
# POST-PROCESSING: Domain knowledge fixes based on NET rank
# Based on training data averages:
#   NET 1-5   avg seed: 5.5
#   NET 6-10  avg seed: 10.4
#   NET 11-15 avg seed: 13.8
#   NET 16-20 avg seed: 20.5
#   NET 21-30 avg seed: 25.5
#   NET 31-50 avg seed: 35.9
#   NET 51-100 avg seed: 48.3
################################################################################
pred_df_final <- data.frame(
  RecordID     = test$RecordID,
  NetRank      = test$Net.Rank,
  Bid.Type     = test$Bid.Type,
  EnsembleSeed = best_preds
)

pred_df_final$FinalSeed <- pred_df_final$EnsembleSeed

# Hard-code top 5 NET rank teams to realistic seeds
pred_df_final$FinalSeed <- ifelse(pred_df_final$NetRank == 1, 2,
                                  ifelse(pred_df_final$NetRank == 2, 4,
                                         ifelse(pred_df_final$NetRank == 3, 6,
                                                ifelse(pred_df_final$NetRank == 4, 8,
                                                       ifelse(pred_df_final$NetRank == 5, 10,
                                                              pred_df_final$FinalSeed)))))

# Cap: NET rank <= 15 should not have seed > 16
pred_df_final$FinalSeed <- ifelse(
  pred_df_final$NetRank <= 15 &
    pred_df_final$FinalSeed > 16 &
    pred_df_final$FinalSeed != 0,
  16, pred_df_final$FinalSeed)

# Cap: NET rank 16-20 should not have seed > 25
pred_df_final$FinalSeed <- ifelse(
  pred_df_final$NetRank >= 16 & pred_df_final$NetRank <= 20 &
    pred_df_final$FinalSeed > 25 & pred_df_final$FinalSeed != 0,
  25, pred_df_final$FinalSeed)

# Floor: NET rank 51+ should not get seed below 35
pred_df_final$FinalSeed <- ifelse(
  pred_df_final$NetRank >= 51 &
    pred_df_final$FinalSeed < 35 &
    pred_df_final$FinalSeed != 0,
  35, pred_df_final$FinalSeed)

# Zero out non-tournament teams
pred_df_final$FinalSeed[is.na(pred_df_final$Bid.Type)] <- 0
pred_df_final$FinalSeed[pred_df_final$FinalSeed > 68]  <- 68
pred_df_final$FinalSeed[pred_df_final$FinalSeed < 1 & pred_df_final$FinalSeed != 0] <- 1

cat("\nFinal seed distribution:\n")
print(table(pred_df_final$FinalSeed[pred_df_final$FinalSeed > 0]))
cat("Non-tournament (0):", sum(pred_df_final$FinalSeed == 0), "\n")
cat("Tournament teams:", sum(pred_df_final$FinalSeed > 0), "\n")

################################################################################
# SUBMISSION
# Uses match() to preserve template row order (critical!)
################################################################################
sub_template <- read.csv(paste0(base_path, "data/submission_template2.0.csv"))

idx <- match(sub_template$RecordID, pred_df_final$RecordID)
sub_template$`Overall Seed` <- pred_df_final$FinalSeed[idx]
sub_template$`Overall Seed`[is.na(sub_template$`Overall Seed`)] <- 0
sub_template <- sub_template[, c("RecordID", "Overall Seed")]

out_path <- paste0(base_path, "submissions/submission_fixed.csv")
write.csv(sub_template, out_path, row.names = FALSE)
cat("Submission saved to:", out_path, "\n")
