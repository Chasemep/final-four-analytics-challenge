# Diagnostic script to check qualification model performance
library(xgboost)
library(dplyr)

# Load data as in FinalModel.R (simplified)
test_input       <- read.csv("data/NCAA_Seed_Test_Set2.0.csv",      na.strings = "", stringsAsFactors = FALSE)
train_input      <- read.csv("data/NCAA_Seed_Training_Set2.0.csv",  na.strings = "", stringsAsFactors = FALSE)
match_data       <- read.csv("data/college_basketball_games_2020_2025.csv")

# ... (Include necessary functions from FinalModel.R) ...
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

build_df <- function(raw, rec, include_seed = FALSE) {
  df <- data.frame(
    RecordID      = raw$RecordID,
    Season        = sapply(strsplit(raw$Season, "-"), `[`, 1),
    Team          = raw$Team,
    Conference    = raw$Conference,
    Bid.Type      = raw$Bid.Type,
    Net.Rank      = raw$NET.Rank,
    prevNET       = if("PrevNET" %in% names(raw)) raw$PrevNET else if("prevNET" %in% names(raw)) raw$prevNET else NA,
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

add_ratios <- function(df, rec) {
  df$WL.Ratio      <- rec$wl$Win      / (rec$wl$Loss      + 1.1)
  df$Conf.Ratio    <- rec$conf$Win    / (rec$conf$Loss    + 1.1)
  df$NonConf.Ratio <- rec$nonconf$Win / (rec$nonconf$Loss + 1.1)
  df$Road.Ratio    <- rec$road$Win    / (rec$road$Loss    + 1.1)
  df$Q1.Ratio      <- rec$q1$Win      / (rec$q1$Loss      + 1.1)
  df$Q2.Ratio      <- rec$q2$Win      / (rec$q2$Loss      + 1.1)
  df$Q3.Ratio      <- rec$q3$Win      / (rec$q3$Loss      + 1.1)
  df$Q4.Ratio      <- rec$q4$Win      / (rec$q4$Loss      + 1.1)
  df
}

names(train_input) <- gsub(" ", ".", names(train_input))
names(test_input)  <- gsub(" ", ".", names(test_input))
names(train_input) <- gsub("-", ".", names(train_input))
names(test_input)  <- gsub("-", ".", names(test_input))

train_rec <- parse_records(train_input)
test_rec  <- parse_records(test_input)

train <- build_df(train_input, train_rec, include_seed = TRUE)
test  <- build_df(test_input,  test_rec,  include_seed = FALSE)

train <- add_ratios(train, train_rec)
test  <- add_ratios(test,  test_rec)

train$Q.Score <- (train$Q1.Win - train$Q1.Loss) * 0.4 + (train$Q2.Win - train$Q2.Loss) * 0.3 + (train$Q3.Win - train$Q3.Loss) * 0.2 + (train$Q4.Win - train$Q4.Loss) * 0.1

# Simplified WAB for check
train$WAB <- 0 

base_features <- c(
  "Net.Rank", "prevNET", "AvgOppNETRank", "AvgOppNET", "NETSOS", "NETNonConfSOS",
  "Win", "Loss", "Conf.Win", "Conf.Loss", "NonConf.Win", "NonConf.Loss",
  "Road.Win", "Road.Loss", "Q1.Win", "Q1.Loss", "Q2.Win", "Q2.Loss",
  "Q3.Win", "Q3.Loss", "Q4.Win", "Q4.Loss",
  "WL.Ratio", "Conf.Ratio", "NonConf.Ratio", "Road.Ratio",
  "Q1.Ratio", "Q2.Ratio", "Q3.Ratio", "Q4.Ratio",
  "Q.Score", "WAB"
)

train$Qualify <- ifelse(!is.na(train$Bid.Type), 1, 0)

cat("Total Train Rows:", nrow(train), "\n")
cat("Qualified Teams:", sum(train$Qualify == 1), "\n")
cat("Non-Qualified Teams:", sum(train$Qualify == 0), "\n")

X <- as.matrix(train[, base_features])
y <- train$Qualify

cat("Rows with missing values:\n")
print(colSums(is.na(X)))

# Check complete cases
cat("Rows dropped by complete.cases:", sum(!complete.cases(X)), "\n")

good <- complete.cases(X, y)
X_clean <- X[good, ]
y_clean <- y[good]

cat("Clean X rows:", nrow(X_clean), "\n")
cat("Clean Qualified:", sum(y_clean == 1), "\n")
cat("Clean Non-Qualified:", sum(y_clean == 0), "\n")

# Run simple XGBoost and check training accuracy
dtrain <- xgb.DMatrix(X_clean, label = y_clean)
model <- xgb.train(params = list(objective="binary:logistic"), data=dtrain, nrounds=50)

preds <- predict(model, dtrain)
pred_labels <- ifelse(preds > 0.5, 1, 0)
cat("\nConfusion Matrix (Training Set):\n")
print(table(Actual = y_clean, Predicted = pred_labels))

# Feature importance
importance <- xgb.importance(model = model)
cat("\nTop Features:\n")
print(head(importance))
