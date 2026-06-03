################################################################################
# IMPORTS
################################################################################
library(xgboost)
library(dplyr)
library(openxlsx)
library(rvest)

################################################################################
# READ DATA
################################################################################
test_input       <- read.csv("data/NCAA_Seed_Test_Set_2026.csv",      na.strings = "", stringsAsFactors = FALSE)
train_input      <- read.csv("data/NCAA_Seed_Training_Set2.0.csv",  na.strings = "", stringsAsFactors = FALSE)
match_data       <- read.csv("data/college_basketball_games_2020_2025.csv")

# Fix column names with spaces and dashes
names(train_input) <- gsub(" ", ".", names(train_input))
names(test_input)  <- gsub(" ", ".", names(test_input))
names(train_input) <- gsub("-", ".", names(train_input))
names(test_input)  <- gsub("-", ".", names(test_input))

################################################################################
# FETCH 2025-26 ACTUAL SEEDS FROM WIKIPEDIA
################################################################################
get_actual_seeds_2026 <- function() {
  url        <- "https://en.wikipedia.org/wiki/2026_NCAA_Division_I_men%27s_basketball_tournament"
  local_path <- "C:/Users/Chase/.gemini/antigravity-ide/brain/aca4c5f8-41cf-4f0d-8cb7-645cee9f1ecc/.system_generated/steps/103/content.md"

  webpage <- NULL
  tryCatch({ webpage <- read_html(url) },
           error = function(e) {
             if (file.exists(local_path)) webpage <<- read_html(local_path)
           })

  if (is.null(webpage)) {
    warning("Could not load 2026 actual seeds from Wikipedia; actual seeds will be NA for 2025-26.")
    return(data.frame(School = character(), Actual_Tournament_Seed = integer(),
                      Actual_Overall_Seed = integer(), Actual_Region = character(),
                      stringsAsFactors = FALSE))
  }

  tables         <- html_nodes(webpage, "table")
  extracted_list <- list()
  regions        <- c("East", "West", "South", "Midwest")
  region_count   <- 1

  for (i in seq_along(tables)) {
    headers <- trimws(html_text(html_nodes(tables[i], "th")))
    if (any(grepl("Overall seed", headers, ignore.case = TRUE)) &&
        any(grepl("School",       headers, ignore.case = TRUE))) {
      df <- html_table(tables[i], fill = TRUE)[[1]]
      # Only the four regional tables (16-18 rows, 7 cols)
      if (nrow(df) >= 15 && nrow(df) <= 20 && ncol(df) >= 5 && ncol(df) <= 10) {
        df[] <- lapply(df, as.character)
        df$Region <- if (region_count <= 4) regions[region_count] else "Unknown"
        region_count <- region_count + 1
        extracted_list[[length(extracted_list) + 1]] <- df
      }
    }
  }

  if (length(extracted_list) == 0) {
    warning("No seed tables found in Wikipedia page.")
    return(data.frame(School = character(), Actual_Tournament_Seed = integer(),
                      Actual_Overall_Seed = integer(), Actual_Region = character(),
                      stringsAsFactors = FALSE))
  }

  combined       <- bind_rows(extracted_list)
  names(combined) <- make.names(names(combined))
  combined$Seed        <- as.integer(gsub("[*†]", "", combined$Seed))
  combined$Overall.seed <- as.integer(combined$Overall.seed)

  map_wiki_team <- function(name) {
    if (is.na(name) || name == "") return(NA_character_)
    name <- trimws(gsub("\u00a0", " ", name))
    switch(name,
      "Saint Mary's"       = "Saint Mary's (CA)",
      "Queens"             = "Queens (NC)",
      "Prairie View A&M"  = "Prairie View",
      "Northern Iowa"      = "UNI",
      "St. John's"         = "St. John's (NY)",
      "North Dakota State" = "North Dakota St.",
      "Tennessee State"    = "Tennessee St.",
      "Michigan State"     = "Michigan St.",
      "Ohio State"         = "Ohio St.",
      "South Florida"      = "South Fla.",
      "Utah State"         = "Utah St.",
      "Kennesaw State"     = "Kennesaw St.",
      "Iowa State"         = "Iowa St.",
      "Wright State"       = "Wright St.",
      name
    )
  }

  combined$MappedSchool <- sapply(combined$School, map_wiki_team)
  combined <- combined[!is.na(combined$MappedSchool), ]

  data.frame(
    School               = combined$MappedSchool,
    Actual_Tournament_Seed = combined$Seed,
    Actual_Overall_Seed  = combined$Overall.seed,
    Actual_Region        = combined$Region,
    stringsAsFactors     = FALSE
  )
}

cat("Fetching 2025-26 actual seeds from Wikipedia...\n")
actual_seeds_2026 <- get_actual_seeds_2026()
cat("  Fetched:", nrow(actual_seeds_2026), "teams.\n")

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

cat("Train rows after merging WAB:", nrow(train), "\n")
train <- subset(train, !is.na(NETNonConfSOS) & !is.na(Net.Rank))
cat("Train rows after subsetting NAs:", nrow(train), "\n")

cat("Train rows:", nrow(train), "| Test rows:", nrow(test), "\n")
cat("Qualified in train:", sum(ifelse(!is.na(train$Bid.Type), 1, 0)), "\n")
cat("Seeds available:", sum(!is.na(train$Overall.Seed)), "\n")

################################################################################
# QUALIFY
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

X <- as.matrix(train[, base_features])
train$Qualify <- ifelse(!is.na(train$Bid.Type), 1, 0)
y <- train$Qualify

good <- complete.cases(X, y)
X <- X[good, ]
y <- y[good]

dtrain <- xgb.DMatrix(data = X, label = y)

param_list <- list(
  objective = "binary:logistic",
  eta = 0.05,
  gamma = 1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.5
)

set.seed(112)
xgbcv <- xgb.cv(params = param_list,
               data = dtrain,
               nrounds = 1000,
               nfold = 5,
               print_every_n = 10,
               early_stopping_rounds = 30,
               maximize = FALSE)

best_nrounds <- xgbcv$niter
cat("Best qualify nrounds:", best_nrounds, "\n")

model_qualify <- xgb.train(
  data   = dtrain,
  params = param_list,
  nrounds = best_nrounds
)

################################################################################
# XGBOOST
################################################################################
trainXG <- subset(train, !is.na(Overall.Seed))

# Impute missing values with training medians
XTrain_raw   <- as.matrix(trainXG[, base_features])
XTest_raw    <- as.matrix(test[, base_features])
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

model_seed_xgb  <- xgb.train(data=dfull, params=best_params, nrounds=best_rounds)
predicted_seeds <- predict(model_seed_xgb, dtest)

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

lm_formula   <- as.formula(paste("Overall.Seed ~", paste(lm_features, collapse = " + ")))
model_lm     <- lm(lm_formula, data = lm_train)
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
  cat("Using: Linear Model\n"); lm_predicted
} else {
  cat("Using: XGBoost\n"); predicted_seeds
}

################################################################################
# RANKING LOGIC
################################################################################
train_copy      <- trainXG
train_preds_xgb <- predict(model_seed_xgb, xgb.DMatrix(XTrain))
train_preds_lm  <- predict(model_lm, lm_train)
train_copy$XGB_Pred_Raw      <- train_preds_xgb
train_copy$LM_Pred_Raw       <- train_preds_lm
train_copy$Ensemble_Pred_Raw <- (train_preds_xgb + train_preds_lm) / 2
train_copy$EnsembleSeed      <- train_copy$Ensemble_Pred_Raw

test_copy <- test
test_copy$XGB_Pred_Raw      <- predicted_seeds
test_copy$LM_Pred_Raw       <- lm_predicted
test_copy$Ensemble_Pred_Raw <- (predicted_seeds + lm_predicted) / 2
test_copy$EnsembleSeed      <- test_copy$Ensemble_Pred_Raw

pred_df_train <- train_copy
pred_df_test  <- test_copy

pred_df_train$Source <- "Train"
pred_df_test$Source  <- "Test"

# Drop columns that only exist in training (Overall.Seed etc.) before rbind
# We keep Overall.Seed as NA in test_copy
pred_df_test$Overall.Seed <- NA_integer_

# Ensure both have same columns for rbind (drop any extra training-only columns)
common_cols <- intersect(names(pred_df_train), names(pred_df_test))
pred_df_combined <- rbind(pred_df_train[, common_cols], pred_df_test[, common_cols])

pred_df_combined$Qualify   <- 0
pred_df_combined$FinalRank <- 0
# We'll also store prob_qualify here
pred_df_combined$Placement_Probability <- NA_real_

# Per-season ranking
for (s in unique(pred_df_combined$Season)) {
  season_mask <- pred_df_combined$Season == s

  X_season <- as.matrix(pred_df_combined[season_mask, base_features])
  for (j in seq_len(ncol(X_season))) {
    X_season[is.na(X_season[, j]), j] <- train_medians[j]
  }

  prob_qualify <- predict(model_qualify, X_season)
  pred_df_combined$Placement_Probability[season_mask] <- prob_qualify

  ranks_qualify      <- rank(-prob_qualify, ties.method = "first")
  qualified_in_season <- ranks_qualify <= 68

  pred_df_combined$Qualify[season_mask] <- ifelse(qualified_in_season, 1, 0)

  to_be_ranked_mask <- season_mask & pred_df_combined$Qualify == 1
  season_indices    <- which(to_be_ranked_mask)

  if (length(season_indices) > 0) {
    sorted_indices <- season_indices[order(pred_df_combined$EnsembleSeed[season_indices],
                                           pred_df_combined$Net.Rank[season_indices])]
    pred_df_combined$FinalRank[sorted_indices] <- seq_len(length(sorted_indices))
  }
}

# Per-season per-model ranked seeds (1-68) for ALL qualified teams
overall_to_tourn <- function(x) pmin(16L, ceiling(as.integer(round(x)) / 4L))

pred_df_combined$XGB_Overall_Seed      <- NA_integer_
pred_df_combined$LM_Overall_Seed       <- NA_integer_
pred_df_combined$Ensemble_Overall_Seed <- NA_integer_
pred_df_combined$Actual_Region        <- NA_character_

for (s in unique(pred_df_combined$Season)) {
  mask <- pred_df_combined$Season == s & pred_df_combined$Qualify == 1
  idx  <- which(mask)
  if (length(idx) == 0) next

  sub <- pred_df_combined[idx, ]

  # Rank each model's predictions (ascending = better seed)
  pred_df_combined$XGB_Overall_Seed[idx] <-
    rank(sub$XGB_Pred_Raw, ties.method = "first")
  pred_df_combined$LM_Overall_Seed[idx]  <-
    rank(sub$LM_Pred_Raw,  ties.method = "first")
  pred_df_combined$Ensemble_Overall_Seed[idx] <-
    rank(sub$Ensemble_Pred_Raw, ties.method = "first")
}

# Tournament seeds (1-16)
pred_df_combined$XGB_Tournament_Seed      <- overall_to_tourn(pred_df_combined$XGB_Overall_Seed)
pred_df_combined$LM_Tournament_Seed       <- overall_to_tourn(pred_df_combined$LM_Overall_Seed)
pred_df_combined$Ensemble_Tournament_Seed <- overall_to_tourn(pred_df_combined$Ensemble_Overall_Seed)
pred_df_combined$Actual_Tournament_Seed   <- overall_to_tourn(pred_df_combined$Overall.Seed)

# Errors (where actual seed is available)
pred_df_combined$XGB_Error_Overall      <- pred_df_combined$Overall.Seed - pred_df_combined$XGB_Overall_Seed
pred_df_combined$LM_Error_Overall       <- pred_df_combined$Overall.Seed - pred_df_combined$LM_Overall_Seed
pred_df_combined$Ensemble_Error_Overall <- pred_df_combined$Overall.Seed - pred_df_combined$Ensemble_Overall_Seed

pred_df_combined$XGB_Error_Tournament      <- pred_df_combined$Actual_Tournament_Seed - pred_df_combined$XGB_Tournament_Seed
pred_df_combined$LM_Error_Tournament       <- pred_df_combined$Actual_Tournament_Seed - pred_df_combined$LM_Tournament_Seed
pred_df_combined$Ensemble_Error_Tournament <- pred_df_combined$Actual_Tournament_Seed - pred_df_combined$Ensemble_Tournament_Seed

cat("\nGlobal Rank distribution (Tournament Teams Only):\n")
print(table(pred_df_combined$FinalRank[pred_df_combined$FinalRank > 0]))

################################################################################
# SUBMISSION
################################################################################
test_ranks <- subset(pred_df_combined, Source == "Test")

sub_template <- data.frame(
  RecordID = test_ranks$RecordID,
  `Overall Seed` = test_ranks$FinalRank,
  check.names = FALSE
)

out_path <- "submissions/submission.csv"
write.csv(sub_template, out_path, row.names = FALSE)
cat("Submission saved to:", out_path, "\n")

################################################################################
# MERGE ACTUAL 2025-26 SEEDS INTO PREDICTIONS
################################################################################
# Actual_Tournament_Seed and Actual_Overall_Seed for 2025-26 come from Wikipedia
if (nrow(actual_seeds_2026) > 0) {
  # Map to Season key (train uses first year, e.g. "2025" for 2025-26)
  # test Season column is "2025" (first year extracted in build_df)
  pred_df_combined <- merge(pred_df_combined, actual_seeds_2026,
                            by.x = "Team", by.y = "School", all.x = TRUE,
                            suffixes = c("", "_wiki"))

  # For Test rows where we just fetched actuals, populate
  is_test <- pred_df_combined$Source == "Test"
  pred_df_combined$Overall.Seed[is_test] <- pred_df_combined$Actual_Overall_Seed[is_test]
  pred_df_combined$Actual_Tournament_Seed[is_test] <- pred_df_combined$Actual_Tournament_Seed_wiki[is_test]

  # Recalculate Actual_Tournament_Seed from official Overall for all rows
  pred_df_combined$Actual_Tournament_Seed <- ifelse(
    !is.na(pred_df_combined$Actual_Tournament_Seed_wiki) & is_test,
    pred_df_combined$Actual_Tournament_Seed_wiki,
    pred_df_combined$Actual_Tournament_Seed
  )

  # Recalculate errors for 2025-26 now that we have actuals
  pred_df_combined$XGB_Error_Overall[is_test]      <- pred_df_combined$Overall.Seed[is_test] - pred_df_combined$XGB_Overall_Seed[is_test]
  pred_df_combined$LM_Error_Overall[is_test]        <- pred_df_combined$Overall.Seed[is_test] - pred_df_combined$LM_Overall_Seed[is_test]
  pred_df_combined$Ensemble_Error_Overall[is_test]  <- pred_df_combined$Overall.Seed[is_test] - pred_df_combined$Ensemble_Overall_Seed[is_test]
  pred_df_combined$XGB_Error_Tournament[is_test]    <- pred_df_combined$Actual_Tournament_Seed[is_test] - pred_df_combined$XGB_Tournament_Seed[is_test]
  pred_df_combined$LM_Error_Tournament[is_test]     <- pred_df_combined$Actual_Tournament_Seed[is_test] - pred_df_combined$LM_Tournament_Seed[is_test]
  pred_df_combined$Ensemble_Error_Tournament[is_test] <- pred_df_combined$Actual_Tournament_Seed[is_test] - pred_df_combined$Ensemble_Tournament_Seed[is_test]

  # Add region info
  pred_df_combined$Actual_Region[is_test] <- pred_df_combined$Actual_Region_wiki[is_test]
  # Drop the suffixed wiki column
  wiki_cols <- grep("_wiki$", names(pred_df_combined), value = TRUE)
  pred_df_combined <- pred_df_combined[, !names(pred_df_combined) %in% wiki_cols]
}

################################################################################
# SHEET 1: CLEANED DATA
################################################################################
cleaned_data <- pred_df_combined %>%
  select(
    RecordID,
    Season,
    Team,
    Conference,
    Source,
    NET_Rank       = Net.Rank,
    prevNET,
    AvgOppNETRank,
    AvgOppNET,
    NETSOS,
    NETNonConfSOS,
    Win,
    Loss,
    Conf_Win       = Conf.Win,
    Conf_Loss      = Conf.Loss,
    NonConf_Win    = NonConf.Win,
    NonConf_Loss   = NonConf.Loss,
    Road_Win       = Road.Win,
    Road_Loss      = Road.Loss,
    Q1_Win         = Q1.Win,
    Q1_Loss        = Q1.Loss,
    Q2_Win         = Q2.Win,
    Q2_Loss        = Q2.Loss,
    Q3_Win         = Q3.Win,
    Q3_Loss        = Q3.Loss,
    Q4_Win         = Q4.Win,
    Q4_Loss        = Q4.Loss,
    WL_Ratio       = WL.Ratio,
    Conf_Ratio     = Conf.Ratio,
    NonConf_Ratio  = NonConf.Ratio,
    Road_Ratio     = Road.Ratio,
    Q1_Ratio       = Q1.Ratio,
    Q2_Ratio       = Q2.Ratio,
    Q3_Ratio       = Q3.Ratio,
    Q4_Ratio       = Q4.Ratio,
    Q_Score        = Q.Score,
    WAB,
    Placement_Probability,
    Qualified_Status = Qualify,
    Bid_Type       = Bid.Type
  )

cat("Sheet 1 (Cleaned_Data) rows:", nrow(cleaned_data), "\n")

################################################################################
# SHEET 2: MODEL PREDICTIONS
################################################################################
predictions_df <- pred_df_combined %>%
  filter(Qualify == 1) %>%
  select(
    RecordID,
    Season,
    Team,
    Conference,
    Source,
    Actual_Overall_Seed      = Overall.Seed,
    Actual_Tournament_Seed,
    Actual_Region,
    LM_Pred_Raw,
    LM_Overall_Seed,
    LM_Tournament_Seed,
    LM_Error_Overall,
    LM_Error_Tournament,
    XGB_Pred_Raw,
    XGB_Overall_Seed,
    XGB_Tournament_Seed,
    XGB_Error_Overall,
    XGB_Error_Tournament,
    Ensemble_Pred_Raw,
    Ensemble_Overall_Seed,
    Ensemble_Tournament_Seed,
    Ensemble_Error_Overall,
    Ensemble_Error_Tournament
  )

cat("Sheet 2 (Model_Predictions) rows:", nrow(predictions_df), "\n")

################################################################################
# SHEET 3: MATCHUP PROBABILITIES
################################################################################
# Get 68 predicted tournament teams for 2025-26
tourn_teams_2026 <- pred_df_combined %>%
  filter(Source == "Test", Qualify == 1) %>%
  select(Team, NET_Rank = Net.Rank, Ensemble_Tournament_Seed)

cat("Tournament teams for matchup sheet:", nrow(tourn_teams_2026), "\n")

# Extract WAB model coefficients
b0 <- coef(wab_model)[1]  # Intercept
b1 <- coef(wab_model)[2]  # Slope on (away - home rating)

logistic <- function(x) 1 / (1 + exp(-x))

# All ordered pairs (A, B) where A != B
team_pairs <- expand.grid(
  Team_A = tourn_teams_2026$Team,
  Team_B = tourn_teams_2026$Team,
  stringsAsFactors = FALSE
) %>%
  filter(Team_A != Team_B)

# Merge team info
team_pairs <- team_pairs %>%
  left_join(tourn_teams_2026, by = c("Team_A" = "Team")) %>%
  rename(Team_A_NET = NET_Rank, Team_A_Seed = Ensemble_Tournament_Seed) %>%
  left_join(tourn_teams_2026, by = c("Team_B" = "Team")) %>%
  rename(Team_B_NET = NET_Rank, Team_B_Seed = Ensemble_Tournament_Seed)

team_pairs <- team_pairs %>%
  mutate(
    Win_Prob_Neutral    = logistic(b1 * (Team_B_NET - Team_A_NET)),
    Win_Prob_TeamA_Home = logistic(b0 + b1 * (Team_B_NET - Team_A_NET)),
    Win_Prob_TeamA_Away = 1 - logistic(b0 + b1 * (Team_A_NET - Team_B_NET))
  )

matchup_df <- team_pairs %>%
  select(Team_A, Team_B, Team_A_NET, Team_B_NET, Team_A_Seed, Team_B_Seed,
         Win_Prob_Neutral, Win_Prob_TeamA_Home, Win_Prob_TeamA_Away)

cat("Sheet 3 (Matchup_Probabilities) rows:", nrow(matchup_df), "\n")

################################################################################
# SHEET 4: VARIABLE DESCRIPTIONS
################################################################################
var_descriptions <- data.frame(
  Variable_Name = c(
    "RecordID", "Season", "Team", "Conference", "Source",
    "NET_Rank", "prevNET", "AvgOppNETRank", "AvgOppNET", "NETSOS", "NETNonConfSOS",
    "Win", "Loss",
    "Conf_Win", "Conf_Loss", "NonConf_Win", "NonConf_Loss",
    "Road_Win", "Road_Loss",
    "Q1_Win", "Q1_Loss", "Q2_Win", "Q2_Loss",
    "Q3_Win", "Q3_Loss", "Q4_Win", "Q4_Loss",
    "WL_Ratio", "Conf_Ratio", "NonConf_Ratio", "Road_Ratio",
    "Q1_Ratio", "Q2_Ratio", "Q3_Ratio", "Q4_Ratio",
    "Q_Score", "WAB",
    "Placement_Probability", "Qualified_Status", "Bid_Type",
    "Actual_Overall_Seed", "Actual_Tournament_Seed", "Actual_Region",
    "LM_Pred_Raw", "LM_Overall_Seed", "LM_Tournament_Seed", "LM_Error_Overall", "LM_Error_Tournament",
    "XGB_Pred_Raw", "XGB_Overall_Seed", "XGB_Tournament_Seed", "XGB_Error_Overall", "XGB_Error_Tournament",
    "Ensemble_Pred_Raw", "Ensemble_Overall_Seed", "Ensemble_Tournament_Seed", "Ensemble_Error_Overall", "Ensemble_Error_Tournament",
    "Team_A", "Team_B", "Team_A_NET", "Team_B_NET", "Team_A_Seed", "Team_B_Seed",
    "Win_Prob_Neutral", "Win_Prob_TeamA_Home", "Win_Prob_TeamA_Away"
  ),
  Sheet = c(
    "Cleaned_Data, Model_Predictions", "Cleaned_Data, Model_Predictions",
    "Cleaned_Data, Model_Predictions", "Cleaned_Data, Model_Predictions", "Cleaned_Data, Model_Predictions",
    "Cleaned_Data", "Cleaned_Data", "Cleaned_Data", "Cleaned_Data", "Cleaned_Data", "Cleaned_Data",
    "Cleaned_Data", "Cleaned_Data",
    "Cleaned_Data", "Cleaned_Data", "Cleaned_Data", "Cleaned_Data",
    "Cleaned_Data", "Cleaned_Data",
    "Cleaned_Data", "Cleaned_Data", "Cleaned_Data", "Cleaned_Data",
    "Cleaned_Data", "Cleaned_Data", "Cleaned_Data", "Cleaned_Data",
    "Cleaned_Data", "Cleaned_Data", "Cleaned_Data", "Cleaned_Data",
    "Cleaned_Data", "Cleaned_Data", "Cleaned_Data", "Cleaned_Data",
    "Cleaned_Data", "Cleaned_Data",
    "Cleaned_Data", "Cleaned_Data", "Cleaned_Data",
    "Model_Predictions", "Model_Predictions", "Model_Predictions",
    "Model_Predictions", "Model_Predictions", "Model_Predictions", "Model_Predictions", "Model_Predictions",
    "Model_Predictions", "Model_Predictions", "Model_Predictions", "Model_Predictions", "Model_Predictions",
    "Model_Predictions", "Model_Predictions", "Model_Predictions", "Model_Predictions", "Model_Predictions",
    "Matchup_Probabilities", "Matchup_Probabilities",
    "Matchup_Probabilities", "Matchup_Probabilities",
    "Matchup_Probabilities", "Matchup_Probabilities",
    "Matchup_Probabilities", "Matchup_Probabilities", "Matchup_Probabilities"
  ),
  Category = c(
    "Identifier", "Identifier", "Identifier", "Identifier", "Identifier",
    "Feature", "Feature", "Feature", "Feature", "Feature", "Feature",
    "Feature", "Feature",
    "Feature", "Feature", "Feature", "Feature",
    "Feature", "Feature",
    "Feature", "Feature", "Feature", "Feature",
    "Feature", "Feature", "Feature", "Feature",
    "Derived Metric", "Derived Metric", "Derived Metric", "Derived Metric",
    "Derived Metric", "Derived Metric", "Derived Metric", "Derived Metric",
    "Derived Metric", "Derived Metric",
    "Prediction", "Prediction", "Feature",
    "Actual Value", "Actual Value", "Actual Value",
    "Prediction", "Prediction", "Prediction", "Error Metric", "Error Metric",
    "Prediction", "Prediction", "Prediction", "Error Metric", "Error Metric",
    "Prediction", "Prediction", "Prediction", "Error Metric", "Error Metric",
    "Identifier", "Identifier",
    "Feature", "Feature",
    "Prediction", "Prediction",
    "Prediction", "Prediction", "Prediction"
  ),
  Data_Type = c(
    "Character", "Character", "Character", "Character", "Character",
    "Integer", "Integer", "Integer", "Numeric", "Integer", "Integer",
    "Integer", "Integer",
    "Integer", "Integer", "Integer", "Integer",
    "Integer", "Integer",
    "Integer", "Integer", "Integer", "Integer",
    "Integer", "Integer", "Integer", "Integer",
    "Numeric", "Numeric", "Numeric", "Numeric",
    "Numeric", "Numeric", "Numeric", "Numeric",
    "Numeric", "Numeric",
    "Numeric (0-1)", "Integer (0/1)", "Character",
    "Integer", "Integer", "Character",
    "Numeric", "Integer", "Integer", "Integer", "Integer",
    "Numeric", "Integer", "Integer", "Integer", "Integer",
    "Numeric", "Integer", "Integer", "Integer", "Integer",
    "Character", "Character",
    "Integer", "Integer",
    "Integer", "Integer",
    "Numeric (0-1)", "Numeric (0-1)", "Numeric (0-1)"
  ),
  Description = c(
    "Unique key identifying each team-season record (e.g., '2025-26-Alabama').",
    "Season year string — first year of the academic year (e.g., '2020' for 2020-21, '2025' for 2025-26).",
    "Team name as provided in the dataset.",
    "Conference the team belongs to.",
    "'Train' for historical seasons 2020-21 through 2024-25; 'Test' for the predicted 2025-26 season.",
    "NCAA Evaluation Tool (NET) Rank at the end of the regular season. Lower is better.",
    "Previous season's final NET Rank. Lower is better.",
    "Average NET Rank of all opponents played. Lower means stronger schedule.",
    "Average NET Rating of opponents played (continuous). Lower means stronger schedule.",
    "Strength of Schedule based on opponent NET Ranks.",
    "Non-conference Strength of Schedule based on NET.",
    "Total regular season wins.",
    "Total regular season losses.",
    "Conference wins.",
    "Conference losses.",
    "Non-conference wins.",
    "Non-conference losses.",
    "Road game wins.",
    "Road game losses.",
    "Wins vs. Quadrant 1 opponents (best competition).",
    "Losses vs. Quadrant 1 opponents.",
    "Wins vs. Quadrant 2 opponents.",
    "Losses vs. Quadrant 2 opponents.",
    "Wins vs. Quadrant 3 opponents.",
    "Losses vs. Quadrant 3 opponents.",
    "Wins vs. Quadrant 4 opponents (weakest competition).",
    "Losses vs. Quadrant 4 opponents.",
    "Overall Win/Loss Ratio: Win / (Loss + 1). The +1 avoids division by zero for undefeated teams.",
    "Conference Win/Loss Ratio: Conf_Win / (Conf_Loss + 1).",
    "Non-conference Win/Loss Ratio: NonConf_Win / (NonConf_Loss + 1).",
    "Road Win/Loss Ratio: Road_Win / (Road_Loss + 1).",
    "Quadrant 1 Win/Loss Ratio: Q1_Win / (Q1_Loss + 1).",
    "Quadrant 2 Win/Loss Ratio: Q2_Win / (Q2_Loss + 1).",
    "Quadrant 3 Win/Loss Ratio: Q3_Win / (Q3_Loss + 1).",
    "Quadrant 4 Win/Loss Ratio: Q4_Win / (Q4_Loss + 1).",
    "Weighted quadrant score: (Q1W-Q1L)*0.4 + (Q2W-Q2L)*0.3 + (Q3W-Q3L)*0.2 + (Q4W-Q4L)*0.1.",
    "Wins Above Bubble: total wins above an average bubble team (NET Rank 85) playing the same schedule.",
    "Predicted probability of qualifying for the tournament (0.0-1.0) from the XGBoost binary classification model.",
    "Binary tournament qualification: 1 if team is in the predicted top 68, 0 otherwise.",
    "Actual bid type for historical data: 'Auto' (conference champion), 'At-Large', or NA (did not qualify).",
    "Official NCAA Selection Committee overall seed rank (1-68). From Wikipedia for 2025-26; from dataset for historical seasons.",
    "Official tournament bracket seed (1-16). Derived from Actual_Overall_Seed as ceiling(Actual_Overall_Seed/4).",
    "Official NCAA tournament region (East, West, South, Midwest). Scraped from Wikipedia for 2025-26; NA for historical years.",
    "Continuous raw predicted seed from the Linear Regression model (not rounded or ranked).",
    "Overall seed rank (1-68) assigned by the Linear Model, ranked within each season's qualified teams.",
    "Tournament seed (1-16) derived from LM_Overall_Seed as ceiling(LM_Overall_Seed/4).",
    "Actual_Overall_Seed minus LM_Overall_Seed. Positive = model underseeded; negative = overseeded.",
    "Actual_Tournament_Seed minus LM_Tournament_Seed.",
    "Continuous raw predicted seed from the XGBoost Regression model (not rounded or ranked).",
    "Overall seed rank (1-68) assigned by the XGBoost model, ranked within each season's qualified teams.",
    "Tournament seed (1-16) derived from XGB_Overall_Seed as ceiling(XGB_Overall_Seed/4).",
    "Actual_Overall_Seed minus XGB_Overall_Seed.",
    "Actual_Tournament_Seed minus XGB_Tournament_Seed.",
    "Continuous raw predicted seed from the Ensemble model: average of LM_Pred_Raw and XGB_Pred_Raw.",
    "Overall seed rank (1-68) assigned by the Ensemble model, ranked within each season's qualified teams.",
    "Tournament seed (1-16) derived from Ensemble_Overall_Seed as ceiling(Ensemble_Overall_Seed/4).",
    "Actual_Overall_Seed minus Ensemble_Overall_Seed.",
    "Actual_Tournament_Seed minus Ensemble_Tournament_Seed.",
    "Team A in the simulated matchup.",
    "Team B (opponent) in the simulated matchup.",
    "Team A's NET Rank. Used as input to the logistic win probability model.",
    "Team B's NET Rank. Used as input to the logistic win probability model.",
    "Team A's predicted tournament seed (1-16) from the Ensemble model.",
    "Team B's predicted tournament seed (1-16) from the Ensemble model.",
    "Probability Team A beats Team B on a neutral court. Formula: logistic(b1 * (NET_B - NET_A)).",
    "Probability Team A beats Team B with Team A at home. Formula: logistic(b0 + b1*(NET_B - NET_A)).",
    "Probability Team A beats Team B with Team A away. Formula: 1 - logistic(b0 + b1*(NET_A - NET_B))."
  ),
  stringsAsFactors = FALSE
)

cat("Sheet 4 (Variable_Descriptions) rows:", nrow(var_descriptions), "\n")

################################################################################
# WRITE EXCEL FILE
################################################################################
cat("\nWriting Excel file...\n")
wb <- createWorkbook()

# ---- Sheet 1: Cleaned_Data ----
addWorksheet(wb, "Cleaned_Data")
writeData(wb, "Cleaned_Data", cleaned_data)

# Header style
header_style <- createStyle(fontColour = "#FFFFFF", fgFill = "#1F3864",
                             halign = "center", textDecoration = "bold",
                             border = "Bottom", borderColour = "#FFFFFF")
addStyle(wb, "Cleaned_Data", header_style, rows = 1, cols = 1:ncol(cleaned_data), gridExpand = TRUE)
setColWidths(wb, "Cleaned_Data", cols = 1:ncol(cleaned_data), widths = "auto")

# ---- Sheet 2: Model_Predictions ----
addWorksheet(wb, "Model_Predictions")
writeData(wb, "Model_Predictions", predictions_df)

addStyle(wb, "Model_Predictions", header_style, rows = 1, cols = 1:ncol(predictions_df), gridExpand = TRUE)
setColWidths(wb, "Model_Predictions", cols = 1:ncol(predictions_df), widths = "auto")

# ---- Sheet 3: Matchup_Probabilities ----
addWorksheet(wb, "Matchup_Probabilities")
writeData(wb, "Matchup_Probabilities", matchup_df)

addStyle(wb, "Matchup_Probabilities", header_style, rows = 1, cols = 1:ncol(matchup_df), gridExpand = TRUE)
setColWidths(wb, "Matchup_Probabilities", cols = 1:ncol(matchup_df), widths = "auto")

# ---- Sheet 4: Variable_Descriptions ----
addWorksheet(wb, "Variable_Descriptions")
writeData(wb, "Variable_Descriptions", var_descriptions)

desc_header_style <- createStyle(fontColour = "#FFFFFF", fgFill = "#14532D",
                                  halign = "center", textDecoration = "bold",
                                  border = "Bottom", borderColour = "#FFFFFF")
addStyle(wb, "Variable_Descriptions", desc_header_style, rows = 1,
         cols = 1:ncol(var_descriptions), gridExpand = TRUE)
# Wrap description column
wrap_style <- createStyle(wrapText = TRUE)
addStyle(wb, "Variable_Descriptions", wrap_style, rows = 2:(nrow(var_descriptions)+1),
         cols = ncol(var_descriptions), gridExpand = TRUE)
setColWidths(wb, "Variable_Descriptions",
             cols = 1:(ncol(var_descriptions)-1), widths = "auto")
setColWidths(wb, "Variable_Descriptions",
             cols = ncol(var_descriptions), widths = 60)

# Alternate row shading for readability
alt_row_style <- createStyle(fgFill = "#F0F7F0")
for (r in seq(2, nrow(var_descriptions)+1, by = 2)) {
  addStyle(wb, "Variable_Descriptions", alt_row_style, rows = r,
           cols = 1:ncol(var_descriptions), gridExpand = TRUE, stack = TRUE)
}

# Save
excel_path <- "data/NCAA_Tournament_Dashboard_Data.xlsx"
saveWorkbook(wb, excel_path, overwrite = TRUE)
cat("Excel file saved to:", excel_path, "\n")
cat("  Sheet 1 (Cleaned_Data):          ", nrow(cleaned_data),     "rows x", ncol(cleaned_data),     "cols\n")
cat("  Sheet 2 (Model_Predictions):     ", nrow(predictions_df),   "rows x", ncol(predictions_df),   "cols\n")
cat("  Sheet 3 (Matchup_Probabilities): ", nrow(matchup_df),        "rows x", ncol(matchup_df),        "cols\n")
cat("  Sheet 4 (Variable_Descriptions):", nrow(var_descriptions),  "rows x", ncol(var_descriptions),  "cols\n")