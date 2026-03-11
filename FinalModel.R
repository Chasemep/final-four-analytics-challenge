################################################################################
# IMPORTS
################################################################################
library(xgboost)
library(dplyr)

################################################################################
# READ DATA
################################################################################
test_input       <- read.csv("data/NCAA_Seed_Test_Set2.0.csv",      na.strings = "", stringsAsFactors = FALSE)
train_input      <- read.csv("data/NCAA_Seed_Training_Set2.0.csv",  na.strings = "", stringsAsFactors = FALSE)
match_data       <- read.csv("data/college_basketball_games_2020_2025.csv")

# Fix column names with spaces and dashes
names(train_input) <- gsub(" ", ".", names(train_input))
names(test_input)  <- gsub(" ", ".", names(test_input))
names(train_input) <- gsub("-", ".", names(train_input))
names(test_input)  <- gsub("-", ".", names(test_input))

################################################################################
# SPLIT RECORDS FUNCTION
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
# XGBOOST
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
cat("XGBoost training rows:", nrow(XTrain), "\n")

# Local train/val split
set.seed(112)
val_idx <- sample(seq_len(nrow(XTrain)), size = floor(0.2 * nrow(XTrain)))
X_val <- XTrain[val_idx, ];   y_val <- y[val_idx]
X_tr  <- XTrain[-val_idx, ];  y_tr  <- y[-val_idx]

dtrain <- xgb.DMatrix(data = X_tr,   label = y_tr)
dval   <- xgb.DMatrix(data = X_val,  label = y_val)
dfull  <- xgb.DMatrix(data = XTrain, label = y)
dtest  <- xgb.DMatrix(data = XTest)

# Fixed params (grid search unreliable with only 248 rows)
best_params <- list(
  objective        = "reg:squarederror",
  eval_metric      = "rmse",
  eta              = 0.05,
  max_depth        = 2,
  min_child_weight = 10,
  subsample        = 0.8,
  colsample_bytree = 0.5,
  lambda           = 5
)
best_rounds <- 50

model_val  <- xgb.train(data=dtrain, params=best_params, nrounds=best_rounds, verbose=0)
val_preds  <- round(predict(model_val, dval))
local_rmse <- sqrt(mean((val_preds - y_val)^2))
cat("XGBoost Local Validation RMSE:", round(local_rmse, 5), "\n")

model_xgb     <- xgb.train(data=dfull, params=best_params, nrounds=best_rounds)
predicted_seeds <- round(predict(model_xgb, dtest))
predicted_seeds

################################################################################
# LINEAR MODEL
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
model_lm   <- lm(lm_formula, data = lm_train)
lm_predicted <- predict(model_lm, lm_test)

lm_val_preds  <- round(predict(model_lm, lm_train[val_idx, ]))
lm_local_rmse <- sqrt(mean((lm_val_preds - y_val)^2))
cat("Linear Model Local RMSE:", round(lm_local_rmse, 5), "\n")

################################################################################
# ENSEMBLE
################################################################################
ensemble_seeds <- round((predicted_seeds + lm_predicted) / 2)

xgb_val_raw   <- predict(model_val, dval)
lm_val_raw    <- predict(model_lm, lm_train[val_idx, ])
ens_val_preds <- (xgb_val_raw + lm_val_raw) / 2
ens_rmse      <- sqrt(mean((ens_val_preds - y_val)^2))
cat("Ensemble Local RMSE:", round(ens_rmse, 5), "\n")

# Use best performing model locally
best_preds <- if (ens_rmse <= min(local_rmse, lm_local_rmse)) {
  cat("Using: Ensemble\n"); ensemble_seeds
} else if (lm_local_rmse <= local_rmse) {
  cat("Using: Linear Model\n"); round(lm_predicted)
} else {
  cat("Using: XGBoost\n"); predicted_seeds
}

################################################################################
# RANKING LOGIC
################################################################################
train_copy      <- trainXG 
train_preds_xgb <- predict(model_xgb, xgb.DMatrix(XTrain))
train_preds_lm  <- predict(model_lm, lm_train)
train_copy$EnsembleSeed <- (train_preds_xgb + train_preds_lm) / 2

test_copy <- test
test_copy$EnsembleSeed <- (predicted_seeds + lm_predicted) / 2

pred_df_train <- data.frame(
  RecordID     = train_copy$RecordID,
  Season       = train_copy$Season,
  NetRank      = train_copy$Net.Rank,
  Bid.Type     = train_copy$Bid.Type,
  EnsembleSeed = train_copy$EnsembleSeed,
  Source       = "Train"
)

pred_df_test <- data.frame(
  RecordID     = test_copy$RecordID,
  Season       = test_copy$Season,
  NetRank      = test_copy$Net.Rank,
  Bid.Type     = test_copy$Bid.Type,
  EnsembleSeed = test_copy$EnsembleSeed,
  Source       = "Test"
)

pred_df_combined <- rbind(pred_df_train, pred_df_test)
pred_df_combined$FinalRank <- 0

for (s in unique(pred_df_combined$Season)) {
  to_be_ranked_mask <- pred_df_combined$Season == s & !is.na(pred_df_combined$Bid.Type)
  season_indices <- which(to_be_ranked_mask)
  
  if (length(season_indices) > 0) {
    sorted_indices <- season_indices[order(pred_df_combined$EnsembleSeed[season_indices], 
                                           pred_df_combined$NetRank[season_indices])]
    
    pred_df_combined$FinalRank[sorted_indices] <- 1:length(sorted_indices)
  }
}

cat("\nGlobal Rank distribution (Tournament Teams Only):\n")
print(table(pred_df_combined$FinalRank[pred_df_combined$FinalRank > 0]))

################################################################################
# SUBMISSION
################################################################################
sub_template <- read.csv("data/submission_template2.0.csv")

# Extract only Test set ranks and match to template RecordID to preserve order
test_ranks <- subset(pred_df_combined, Source == "Test")
idx <- match(sub_template$RecordID, test_ranks$RecordID)
sub_template$`Overall Seed` <- test_ranks$FinalRank[idx]
sub_template$`Overall Seed`[is.na(sub_template$`Overall Seed`)] <- 0
sub_template <- sub_template[, c("RecordID", "Overall Seed")]

out_path <- "submissions/submission.csv"
write.csv(sub_template, out_path, row.names = FALSE)
cat("Submission saved to:", out_path, "\n")