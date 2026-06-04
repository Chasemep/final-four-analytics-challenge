################################################################################
# IMPORTS
################################################################################
library(xgboost)
library(dplyr)
library(openxlsx)
library(rvest)

################################################################################
# TEAM NAME NORMALIZATION DICTIONARY
################################################################################
normalize_team_name <- function(name) {
  if (is.na(name) || name == "") return(NA_character_)
  name <- gsub("\\[[a-z]\\]", "", name) # Remove trailing Wikipedia footnotes
  name <- trimws(gsub("\u00a0", " ", name))
  
  switch(name,
    "Saint Mary's"        = "Saint Mary's (CA)",
    "Saint Mary's (CA)"   = "Saint Mary's (CA)",
    "Queens"              = "Queens (NC)",
    "Queens (NC)"         = "Queens (NC)",
    "Prairie View A&M"   = "Prairie View",
    "Prairie View"        = "Prairie View",
    "Northern Iowa"       = "UNI",
    "UNI"                 = "UNI",
    "St. John's"          = "St. John's (NY)",
    "St. John's (NY)"     = "St. John's (NY)",
    "North Dakota State"  = "North Dakota St.",
    "North Dakota St."    = "North Dakota St.",
    "Tennessee State"     = "Tennessee St.",
    "Tennessee St."       = "Tennessee St.",
    "Michigan State"      = "Michigan St.",
    "Michigan St."        = "Michigan St.",
    "Ohio State"          = "Ohio St.",
    "Ohio St."            = "Ohio St.",
    "South Florida"       = "South Fla.",
    "South Fla."          = "South Fla.",
    "Utah State"          = "Utah St.",
    "Utah St."            = "Utah St.",
    "Kennesaw State"      = "Kennesaw St.",
    "Kennesaw St."        = "Kennesaw St.",
    "Iowa State"          = "Iowa St.",
    "Iowa St."            = "Iowa St.",
    "Wright State"        = "Wright St.",
    "Wright St."          = "Wright St.",
    "San Diego State"     = "San Diego St.",
    "San Diego St."       = "San Diego St.",
    "Washington State"    = "Washington St.",
    "Washington St."      = "Washington St.",
    "Florida Atlantic"    = "Fla. Atlantic",
    "Fla. Atlantic"       = "Fla. Atlantic",
    "Morehead State"      = "Morehead St.",
    "Morehead St."        = "Morehead St.",
    "South Dakota State"  = "South Dakota St.",
    "South Dakota St."    = "South Dakota St.",
    "Mississippi State"   = "Mississippi St.",
    "Mississippi St."     = "Mississippi St.",
    "Grand Canyon"        = "Grand Canyon",
    "Long Beach State"    = "Long Beach St.",
    "Long Beach St."      = "Long Beach St.",
    "Boise State"         = "Boise St.",
    "Boise St."           = "Boise St.",
    "NC State"            = "NC State",
    "Western Kentucky"    = "Western Ky.",
    "Western Ky."         = "Western Ky.",
    "Colorado State"      = "Colorado St.",
    "Colorado St."        = "Colorado St.",
    "Saint Peter's"       = "Saint Peter's",
    "Montana State"       = "Montana St.",
    "Montana St."         = "Montana St.",
    "Grambling State"     = "Grambling",
    "Grambling"           = "Grambling",
    "Mount St. Mary's"    = "Mount St. Mary's",
    "Norfolk State"       = "Norfolk St.",
    "Norfolk St."         = "Norfolk St.",
    "Alabama State"       = "Alabama St.",
    "Alabama St."         = "Alabama St.",
    "Col. of Charleston"  = "Col. of Charleston",
    "Charleston"          = "Col. of Charleston",
    "College of Charleston" = "Col. of Charleston",
    "Texas Southern"      = "Texas Southern",
    "Fairleigh Dickinson" = "FDU",
    "FDU"                 = "FDU",
    "Loyola Chicago"      = "Loyola Chicago",
    "Loyola (IL)"         = "Loyola Chicago",
    "Miami (FL)"          = "Miami (FL)",
    "Miami"               = "Miami (FL)",
    "Texas A&M-Corpus Christi" = "A&M-Corpus Christi",
    "Texas A&M–Corpus Christi" = "A&M-Corpus Christi",
    "A&M-Corpus Christi"  = "A&M-Corpus Christi",
    "St. Bonaventure"     = "St. Bonaventure",
    "New Mexico State"    = "New Mexico St.",
    "New Mexico St."      = "New Mexico St.",
    "Cal State Fullerton" = "Cal St. Fullerton",
    "Cal St. Fullerton"   = "Cal St. Fullerton",
    "Georgia State"       = "Georgia St.",
    "Georgia St."         = "Georgia St.",
    "Murray State"        = "Murray St.",
    "Murray St."          = "Murray St.",
    "Jacksonville State"  = "Jacksonville St.",
    "Jacksonville St."    = "Jacksonville St.",
    "Texas A&M"           = "Texas A&M",
    "Penn State"          = "Penn St.",
    "Penn St."            = "Penn St.",
    "Northern Kentucky"   = "Northern Ky.",
    "Northern Ky."        = "Northern Ky.",
    "Arizona State"       = "Arizona St.",
    "Arizona St."         = "Arizona St.",
    "Oregon State"        = "Oregon St.",
    "Oregon St."          = "Oregon St.",
    "Cleveland State"     = "Cleveland St.",
    "Cleveland St."       = "Cleveland St.",
    "Virginia Tech"       = "Virginia Tech",
    "Appalachian State"   = "App State",
    "Appalachian St."     = "App State",
    "App State"           = "App State",
    "Wichita State"       = "Wichita St.",
    "Wichita St."         = "Wichita St.",
    "Eastern Washington"  = "Eastern Wash.",
    "Eastern Wash."       = "Eastern Wash.",
    "UNC Greensboro"      = "UNC Greensboro",
    "Abilene Christian"   = "Abilene Christian",
    "Hartford"            = "Hartford",
    "Southeast Missouri State" = "Southeast Mo. St.",
    "Southeast Missouri St."   = "Southeast Mo. St.",
    "Southeast Mo. St."        = "Southeast Mo. St.",
    "Louisiana"           = "Louisiana",
    "Oral Roberts"        = "Oral Roberts",
    "Iona"                = "Iona",
    "UNC Asheville"       = "UNC Asheville",
    "Saint Joseph's"      = "Saint Joseph's",
    "St. Thomas (MN)"     = "St. Thomas (MN)",
    "UC San Diego"        = "UC San Diego",
    "SIU Edwardsville"    = "SIUE",
    "SIUE"                = "SIUE",
    "USC"                 = "Southern California",
    "Southern California" = "Southern California",
    "Florida State"       = "Florida St.",
    "Florida St."         = "Florida St.",
    "Oklahoma State"      = "Oklahoma St.",
    "Oklahoma St."        = "Oklahoma St.",
    "Kansas State"        = "Kansas St.",
    "Kansas St."          = "Kansas St.",
    "Kent State"          = "Kent St.",
    "Kent St."            = "Kent St.",
    "UNC Wilmington"      = "UNCW",
    "UNCW"                = "UNCW",
    "Uconn"               = "UConn",
    "UConn"               = "UConn",
    "Connecticut"         = "UConn",
    name
  )
}

################################################################################
# WIKIPEDIA SEEDS SCRAPER
################################################################################
get_actual_seeds_wiki <- function(yr) {
  url <- paste0("https://en.wikipedia.org/wiki/", yr, "_NCAA_Division_I_men%27s_basketball_tournament")
  
  webpage <- NULL
  tryCatch({
    webpage <- read_html(url)
  }, error = function(e) {
    warning(paste("Could not load actual seeds for year", yr, "from Wikipedia."))
  })
  
  if (is.null(webpage)) {
    return(data.frame(School = character(), Actual_Tournament_Seed = integer(),
                      Actual_Overall_Seed = integer(), Actual_Region = character(),
                      Season = character(), stringsAsFactors = FALSE))
  }
  
  tables <- html_nodes(webpage, "table")
  extracted_list <- list()
  regions <- c("East", "West", "South", "Midwest")
  region_count <- 1
  
  for (i in seq_along(tables)) {
    headers <- trimws(html_text(html_nodes(tables[i], "th")))
    has_seed <- any(grepl("seed", headers, ignore.case = TRUE))
    has_school <- any(grepl("school|team", headers, ignore.case = TRUE))
    
    if (has_seed && has_school) {
      df <- html_table(tables[i], fill = TRUE)[[1]]
      # Filter for typical regional seeding tables: 16-20 rows, 5-10 cols
      if (nrow(df) >= 15 && nrow(df) <= 20 && ncol(df) >= 5 && ncol(df) <= 10) {
        df[] <- lapply(df, as.character)
        df$Region <- if (region_count <= 4) regions[region_count] else paste0("Region_", region_count)
        region_count <- region_count + 1
        extracted_list[[length(extracted_list) + 1]] <- df
      }
    }
  }
  
  if (length(extracted_list) == 0) {
    warning(paste("No seed tables found in Wikipedia page for year", yr))
    return(data.frame(School = character(), Actual_Tournament_Seed = integer(),
                      Actual_Overall_Seed = integer(), Actual_Region = character(),
                      Season = character(), stringsAsFactors = FALSE))
  }
  
  combined <- bind_rows(extracted_list)
  names(combined) <- make.names(names(combined))
  
  # Identify key columns using grep
  school_col  <- grep("school|team", names(combined), ignore.case = TRUE, value = TRUE)[1]
  seed_col    <- grep("^seed", names(combined), ignore.case = TRUE, value = TRUE)[1]
  overall_col <- grep("overall.seed", names(combined), ignore.case = TRUE, value = TRUE)[1]
  
  combined$CleanSchool <- combined[[school_col]]
  combined$CleanSchool <- gsub("\\[[a-z]\\]", "", combined$CleanSchool)
  combined$CleanSchool <- trimws(gsub("\u00a0", " ", combined$CleanSchool))
  
  combined$CleanSeed <- as.integer(gsub("[*†]", "", combined[[seed_col]]))
  
  combined$CleanOverall <- NA_integer_
  if (!is.na(overall_col)) {
    clean_overall_val <- gsub("[^0-9]", "", combined[[overall_col]])
    combined$CleanOverall <- as.integer(clean_overall_val)
  }
  
  combined$MappedSchool <- sapply(combined$CleanSchool, normalize_team_name)
  combined <- combined[!is.na(combined$MappedSchool) & !is.na(combined$CleanSeed), ]
  combined <- combined[combined$CleanSchool != "" & !grepl("school|team", combined$CleanSchool, ignore.case = TRUE), ]
  
  data.frame(
    School                 = combined$MappedSchool,
    Actual_Tournament_Seed = combined$CleanSeed,
    Actual_Overall_Seed    = combined$CleanOverall,
    Actual_Region          = combined$Region,
    Season                 = as.character(yr - 1),
    stringsAsFactors       = FALSE
  )
}

################################################################################
# RECORD PARSING HELPER FUNCTIONS
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

build_df <- function(raw, rec) {
  data.frame(
    RecordID      = raw$RecordID,
    Season        = sapply(strsplit(raw$Season, "-"), `[`, 1),
    Team          = raw$Team,
    Conference    = raw$Conference,
    NET.Rank      = raw$NET.Rank,
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
    Q4.Loss       = rec$q4$Loss,
    stringsAsFactors = FALSE
  )
}

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

quad_score <- function(df) {
  (df$Q1.Win - df$Q1.Loss) * 0.4 +
    (df$Q2.Win - df$Q2.Loss) * 0.3 +
    (df$Q3.Win - df$Q3.Loss) * 0.2 +
    (df$Q4.Win - df$Q4.Loss) * 0.1
}

################################################################################
# LOAD DATA AND COMBINE
################################################################################
cat("Loading and processing local CSV datasets...\n")
train_raw      <- read.csv("data/NCAA_Seed_Training_Set2.0.csv", na.strings = "", stringsAsFactors = FALSE)
test_hist_raw  <- read.csv("data/NCAA_Seed_Test_Set2.0.csv", na.strings = "", stringsAsFactors = FALSE)
test_2026_raw  <- read.csv("data/NCAA_Seed_Test_Set_2026.csv", na.strings = "", stringsAsFactors = FALSE)
match_data     <- read.csv("data/college_basketball_games_2020_2025.csv")

# Standardize spaces and dashes in raw column names
names(train_raw)     <- gsub("[ -]", ".", names(train_raw))
names(test_hist_raw) <- gsub("[ -]", ".", names(test_hist_raw))
names(test_2026_raw) <- gsub("[ -]", ".", names(test_2026_raw))

# Parse Win/Loss records
train_rec     <- parse_records(train_raw)
test_hist_rec <- parse_records(test_hist_raw)
test_2026_rec <- parse_records(test_2026_raw)

# Build cleaned data frames
train_df     <- build_df(train_raw,     train_rec)
test_hist_df <- build_df(test_hist_raw, test_hist_rec)
test_2026_df <- build_df(test_2026_raw, test_2026_rec)

# Calculate win ratios and quadrant scores
train_df     <- add_ratios(train_df,     train_rec)
test_hist_df <- add_ratios(test_hist_df, test_hist_rec)
test_2026_df <- add_ratios(test_2026_df, test_2026_rec)

train_df$Q.Score     <- quad_score(train_df)
test_hist_df$Q.Score <- quad_score(test_hist_df)
test_2026_df$Q.Score <- quad_score(test_2026_df)

# Annotate source files
train_df$Source     <- "Train_Hist"
test_hist_df$Source <- "Test_Hist"
test_2026_df$Source <- "Test_2026"

# Assign Bid.Type
train_df$Bid.Type     <- train_raw$Bid.Type
test_hist_df$Bid.Type <- test_hist_raw$Bid.Type
test_2026_df$Bid.Type <- test_2026_raw$Bid.Type

# Combine all teams across all seasons
combined_all <- rbind(train_df, test_hist_df, test_2026_df)

# Standardize team names in combined dataset
combined_all$Normalized_Team <- sapply(combined_all$Team, normalize_team_name)

################################################################################
# WAB (Wins Above Bubble) CALCULATION
################################################################################
cat("Fitting WAB logistic model...\n")
all_teams <- combined_all[, c("Team", "Season", "NET.Rank")]
all_teams <- unique(all_teams)

match_data <- merge(match_data, all_teams,
                    by.x = c("homeTeam", "season"),
                    by.y = c("Team", "Season"), all.x = TRUE)
names(match_data)[names(match_data) == "NET.Rank"] <- "homeTeamRating"

match_data <- merge(match_data, all_teams,
                    by.x = c("awayTeam", "season"),
                    by.y = c("Team", "Season"), all.x = TRUE)
names(match_data)[names(match_data) == "NET.Rank"] <- "awayTeamRating"

# Impute missing ratings with default median (350)
match_data$homeTeamRating[is.na(match_data$homeTeamRating)] <- 350
match_data$awayTeamRating[is.na(match_data$awayTeamRating)] <- 350

# Fit logit WAB model
wab_model <- glm(homeWinner ~ I(awayTeamRating - homeTeamRating),
                 data = match_data, family = binomial)

# Wins Above Bubble calculations using rank 85 as the standard bubble cutoff
R_bubble <- 85
bubble_home_df <- match_data; bubble_home_df$homeTeamRating <- R_bubble
bubble_away_df <- match_data; bubble_away_df$awayTeamRating <- R_bubble

match_data$bubbleWinProb_Home <- predict(wab_model, bubble_home_df, type = "response")
match_data$bubbleWinProb_Away <- predict(wab_model, bubble_away_df, type = "response")
match_data$WAB_home           <- as.numeric(match_data$homeWinner) - match_data$bubbleWinProb_Home
match_data$WAB_away           <- as.numeric(match_data$awayWinner) - match_data$bubbleWinProb_Away

home_wab <- aggregate(WAB_home ~ homeTeam + season, data = match_data, sum)
away_wab <- aggregate(WAB_away ~ awayTeam + season, data = match_data, sum)
names(home_wab) <- c("Team", "Season", "WAB_Home")
names(away_wab) <- c("Team", "Season", "WAB_Away")

final_wab <- merge(home_wab, away_wab, by = c("Team", "Season"), all = TRUE)
final_wab[is.na(final_wab)] <- 0
final_wab$WAB <- final_wab$WAB_Home + final_wab$WAB_Away

combined_all <- merge(combined_all, final_wab[, c("Team", "Season", "WAB")],
                      by = c("Team", "Season"), all.x = TRUE)
combined_all$WAB[is.na(combined_all$WAB)] <- 0

################################################################################
# FETCH HISTORICAL AND CURRENT SEEDS FROM WIKIPEDIA
################################################################################
years <- 2021:2026
wiki_list <- list()
for (yr in years) {
  cat("Fetching actual seeds from Wikipedia for tournament year:", yr, "...\n")
  wiki_list[[length(wiki_list) + 1]] <- get_actual_seeds_wiki(yr)
}
all_actual_seeds <- bind_rows(wiki_list)
cat("Scraped", nrow(all_actual_seeds), "actual tournament seeds in total.\n")

# Merge actual seeds and regions into the combined dataset
combined_all <- merge(combined_all, all_actual_seeds,
                      by.x = c("Season", "Normalized_Team"),
                      by.y = c("Season", "School"),
                      all.x = TRUE)

# Qualification status is defined strictly by presence in Wikipedia seeding tables
combined_all$Qualified_Status <- ifelse(!is.na(combined_all$Actual_Tournament_Seed), 1, 0)

################################################################################
# TOURNAMENT QUALIFICATION PROBABILITY MODEL (XGBOOST CLASSIFIER)
################################################################################
cat("Training XGBoost tournament qualification probability model...\n")
base_features <- c(
  "WAB", "NET.Rank", "prevNET", "AvgOppNETRank", "AvgOppNET", "NETSOS", "NETNonConfSOS",
  "Win", "Loss", "Conf.Win", "Conf.Loss", "NonConf.Win", "NonConf.Loss", "Road.Win", "Road.Loss",
  "Q1.Win", "Q1.Loss", "Q2.Win", "Q2.Loss", "Q3.Win", "Q3.Loss", "Q4.Win", "Q4.Loss",
  "WL.Ratio", "Conf.Ratio", "NonConf.Ratio", "Road.Ratio",
  "Q1.Ratio", "Q2.Ratio", "Q3.Ratio", "Q4.Ratio", "Q.Score"
)

# Train on all historical data
train_qual_data <- subset(combined_all, Source != "Test_2026")
X_qual <- as.matrix(train_qual_data[, base_features])
y_qual <- train_qual_data$Qualified_Status

# Median imputation for training classifier
qual_medians <- apply(X_qual, 2, median, na.rm = TRUE)
for (j in seq_len(ncol(X_qual))) {
  X_qual[is.na(X_qual[, j]), j] <- qual_medians[j]
}

dtrain_qual <- xgb.DMatrix(data = X_qual, label = y_qual)

param_qual <- list(
  objective        = "binary:logistic",
  eta              = 0.05,
  gamma            = 1,
  max_depth        = 6,
  subsample        = 0.8,
  colsample_bytree = 0.5
)

set.seed(112)
xgbcv_qual <- xgb.cv(params = param_qual,
                     data = dtrain_qual,
                     nrounds = 1000,
                     nfold = 5,
                     print_every_n = 50,
                     early_stopping_rounds = 30,
                     maximize = FALSE,
                     verbose = 0)

best_nrounds_qual <- xgbcv_qual$niter
model_qualify <- xgb.train(data = dtrain_qual, params = param_qual, nrounds = best_nrounds_qual)

# Generate Placement_Probability for all teams
X_all <- as.matrix(combined_all[, base_features])
for (j in seq_len(ncol(X_all))) {
  X_all[is.na(X_all[, j]), j] <- qual_medians[j]
}
combined_all$Placement_Probability <- predict(model_qualify, X_all)

################################################################################
# REGRESSION SEED PREDICTIONS (TRAINED ON HISTORICAL QUALIFIERS)
################################################################################
cat("Training seed regression models on actual historical qualifiers...\n")
# Filter historical dataset to actual qualifiers only (excluding 2025-26)
train_reg_data <- subset(combined_all, Qualified_Status == 1 & Source != "Test_2026")

XTrain_raw <- as.matrix(train_reg_data[, base_features])
XTest_raw  <- as.matrix(combined_all[combined_all$Qualified_Status == 1, base_features])

# Impute missing values with training medians
train_medians <- apply(XTrain_raw, 2, median, na.rm = TRUE)
for (j in seq_len(ncol(XTrain_raw))) {
  XTrain_raw[is.na(XTrain_raw[, j]), j] <- train_medians[j]
  XTest_raw[is.na(XTest_raw[, j]), j]   <- train_medians[j]
}

XTrain <- XTrain_raw
XTest  <- XTest_raw
y      <- train_reg_data$Actual_Overall_Seed

# Ensure complete cases for regression training
good   <- complete.cases(XTrain, y)
XTrain <- XTrain[good, ]
y      <- y[good]
cat("  Regression training rows:", nrow(XTrain), "\n")

# Local validation split for XGBoost
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

# Train XGBoost
model_val  <- xgb.train(data=dtrain, params=best_params, nrounds=best_rounds, verbose=0)
val_preds  <- round(predict(model_val, dval))
local_rmse <- sqrt(mean((val_preds - y_val)^2))
cat("  XGBoost Local Validation RMSE:", round(local_rmse, 5), "\n")

model_seed_xgb  <- xgb.train(data=dfull, params=best_params, nrounds=best_rounds)
predicted_seeds <- predict(model_seed_xgb, dtest)

# Train Linear Model
lm_train <- train_reg_data
lm_test  <- combined_all[combined_all$Qualified_Status == 1, ]
for (col in base_features) {
  med <- median(lm_train[[col]], na.rm = TRUE)
  lm_train[[col]][is.na(lm_train[[col]])] <- med
  lm_test[[col]][is.na(lm_test[[col]])]   <- med
}

lm_formula   <- as.formula(paste("Actual_Overall_Seed ~", paste(base_features, collapse = " + ")))
model_lm     <- lm(lm_formula, data = lm_train)
lm_predicted <- predict(model_lm, lm_test)

lm_val_preds  <- round(predict(model_lm, lm_train[val_idx, ]))
lm_local_rmse <- sqrt(mean((lm_val_preds - y_val)^2))
cat("  Linear Model Local RMSE:", round(lm_local_rmse, 5), "\n")

# Ensemble predictions (simple average of raw predicted seed ratings)
ensemble_seeds <- (predicted_seeds + lm_predicted) / 2

# Log local diagnostics
xgb_val_raw   <- predict(model_val, dval)
lm_val_raw    <- predict(model_lm, lm_train[val_idx, ])
ens_val_preds <- (xgb_val_raw + lm_val_raw) / 2
ens_rmse      <- sqrt(mean((ens_val_preds - y_val)^2))
cat("  Ensemble Local RMSE:", round(ens_rmse, 5), "\n")

################################################################################
# RANKING AND SEED TRANSLATION
################################################################################
cat("Ranking and translating overall seeds...\n")
predictions_df <- combined_all[combined_all$Qualified_Status == 1, ]

predictions_df$XGB_Pred_Raw      <- predicted_seeds
predictions_df$LM_Pred_Raw       <- lm_predicted
predictions_df$Ensemble_Pred_Raw <- ensemble_seeds

predictions_df$XGB_Overall_Seed      <- NA_integer_
predictions_df$LM_Overall_Seed       <- NA_integer_
predictions_df$Ensemble_Overall_Seed <- NA_integer_

# Rank predictions 1-68 within each season
for (s in unique(predictions_df$Season)) {
  mask <- predictions_df$Season == s
  idx  <- which(mask)
  
  sub <- predictions_df[idx, ]
  
  predictions_df$XGB_Overall_Seed[idx] <- rank(sub$XGB_Pred_Raw, ties.method = "first")
  predictions_df$LM_Overall_Seed[idx]  <- rank(sub$LM_Pred_Raw,  ties.method = "first")
  predictions_df$Ensemble_Overall_Seed[idx] <- rank(sub$Ensemble_Pred_Raw, ties.method = "first")
}

# Translate overall seeds (1-68) to tournament bracket seeds (1-16)
overall_to_tourn <- function(x) pmin(16L, ceiling(as.integer(round(x)) / 4L))

predictions_df$XGB_Tournament_Seed      <- overall_to_tourn(predictions_df$XGB_Overall_Seed)
predictions_df$LM_Tournament_Seed       <- overall_to_tourn(predictions_df$LM_Overall_Seed)
predictions_df$Ensemble_Tournament_Seed <- overall_to_tourn(predictions_df$Ensemble_Overall_Seed)

# Calculate errors
predictions_df$XGB_Error_Overall      <- predictions_df$Actual_Overall_Seed - predictions_df$XGB_Overall_Seed
predictions_df$LM_Error_Overall       <- predictions_df$Actual_Overall_Seed - predictions_df$LM_Overall_Seed
predictions_df$Ensemble_Error_Overall <- predictions_df$Actual_Overall_Seed - predictions_df$Ensemble_Overall_Seed

predictions_df$XGB_Error_Tournament      <- predictions_df$Actual_Tournament_Seed - predictions_df$XGB_Tournament_Seed
predictions_df$LM_Error_Tournament       <- predictions_df$Actual_Tournament_Seed - predictions_df$LM_Tournament_Seed
predictions_df$Ensemble_Error_Tournament <- predictions_df$Actual_Tournament_Seed - predictions_df$Ensemble_Tournament_Seed

################################################################################
# EXCEL SHEET PREPARATION
################################################################################

# ---- SHEET 1: CLEANED DATA (ALL TEAMS) ----
cleaned_data <- combined_all %>%
  select(
    RecordID,
    Season,
    Team,
    Conference,
    Source,
    NET_Rank       = NET.Rank,
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
    Qualified_Status,
    Bid_Type       = Bid.Type
  )

# ---- SHEET 2: MODEL PREDICTIONS (TOURNAMENT TEAMS) ----
predictions_export_df <- predictions_df %>%
  select(
    RecordID,
    Season,
    Team,
    Conference,
    Source,
    Actual_Overall_Seed,
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

# ---- SHEET 3: MATCHUP PROBABILITIES (2025-26 PAIRWISE MATCHUPS) ----
cat("Preparing pairwise matchups for 2025-26 tournament teams...\n")
tourn_teams_2026 <- predictions_df %>%
  filter(Season == "2025") %>%
  select(Team, NET_Rank = NET.Rank, Ensemble_Tournament_Seed)

b0 <- coef(wab_model)[1]
b1 <- coef(wab_model)[2]
logistic <- function(x) 1 / (1 + exp(-x))

# Generate directed pairs (A, B) where A != B
team_pairs <- expand.grid(
  Team_A = tourn_teams_2026$Team,
  Team_B = tourn_teams_2026$Team,
  stringsAsFactors = FALSE
) %>%
  filter(Team_A != Team_B)

# Merge team stats
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

# ---- SHEET 4: VARIABLE DESCRIPTIONS ----
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
    "Data source: 'Train_Hist' (training CSV), 'Test_Hist' (historical test CSV), or 'Test_2026' (current 2025-26 test CSV).",
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
    "Binary tournament qualification: 1 if team is in the actual tournament, 0 otherwise.",
    "Actual bid type for historical data: 'Auto' (conference champion), 'At-Large', or NA (did not qualify).",
    "Official NCAA Selection Committee overall seed rank (1-68). From Wikipedia.",
    "Official tournament bracket seed (1-16). From Wikipedia.",
    "Official NCAA tournament region (East, West, South, Midwest). From Wikipedia.",
    "Continuous raw predicted seed rating from the Linear Regression model (not rounded or ranked).",
    "Overall seed rank (1-68) assigned by the Linear Model, ranked within each season's qualified teams.",
    "Tournament seed (1-16) derived from LM_Overall_Seed as ceiling(LM_Overall_Seed/4).",
    "Actual_Overall_Seed minus LM_Overall_Seed. Positive = model underseeded; negative = overseeded.",
    "Actual_Tournament_Seed minus LM_Tournament_Seed.",
    "Continuous raw predicted seed rating from the XGBoost Regression model (not rounded or ranked).",
    "Overall seed rank (1-68) assigned by the XGBoost model, ranked within each season's qualified teams.",
    "Tournament seed (1-16) derived from XGB_Overall_Seed as ceiling(XGB_Overall_Seed/4).",
    "Actual_Overall_Seed minus XGB_Overall_Seed.",
    "Actual_Tournament_Seed minus XGB_Tournament_Seed.",
    "Continuous raw predicted seed rating from the Ensemble model: average of LM_Pred_Raw and XGB_Pred_Raw.",
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

################################################################################
# WRITE EXCEL WORKBOOK
################################################################################
cat("\nWriting final multi-sheet Excel file...\n")
wb <- createWorkbook()

# Setup header styles
header_style <- createStyle(fontColour = "#FFFFFF", fgFill = "#1F3864",
                             halign = "center", textDecoration = "bold",
                             border = "Bottom", borderColour = "#FFFFFF")

desc_header_style <- createStyle(fontColour = "#FFFFFF", fgFill = "#14532D",
                                  halign = "center", textDecoration = "bold",
                                  border = "Bottom", borderColour = "#FFFFFF")

# Sheet 1: Cleaned_Data
addWorksheet(wb, "Cleaned_Data")
writeData(wb, "Cleaned_Data", cleaned_data)
addStyle(wb, "Cleaned_Data", header_style, rows = 1, cols = 1:ncol(cleaned_data), gridExpand = TRUE)
setColWidths(wb, "Cleaned_Data", cols = 1:ncol(cleaned_data), widths = "auto")

# Sheet 2: Model_Predictions
addWorksheet(wb, "Model_Predictions")
writeData(wb, "Model_Predictions", predictions_export_df)
addStyle(wb, "Model_Predictions", header_style, rows = 1, cols = 1:ncol(predictions_export_df), gridExpand = TRUE)
setColWidths(wb, "Model_Predictions", cols = 1:ncol(predictions_export_df), widths = "auto")

# Sheet 3: Matchup_Probabilities
addWorksheet(wb, "Matchup_Probabilities")
writeData(wb, "Matchup_Probabilities", matchup_df)
addStyle(wb, "Matchup_Probabilities", header_style, rows = 1, cols = 1:ncol(matchup_df), gridExpand = TRUE)
setColWidths(wb, "Matchup_Probabilities", cols = 1:ncol(matchup_df), widths = "auto")

# Sheet 4: Variable_Descriptions
addWorksheet(wb, "Variable_Descriptions")
writeData(wb, "Variable_Descriptions", var_descriptions)
addStyle(wb, "Variable_Descriptions", desc_header_style, rows = 1, cols = 1:ncol(var_descriptions), gridExpand = TRUE)

wrap_style <- createStyle(wrapText = TRUE)
addStyle(wb, "Variable_Descriptions", wrap_style, rows = 2:(nrow(var_descriptions)+1), cols = ncol(var_descriptions), gridExpand = TRUE)
setColWidths(wb, "Variable_Descriptions", cols = 1:(ncol(var_descriptions)-1), widths = "auto")
setColWidths(wb, "Variable_Descriptions", cols = ncol(var_descriptions), widths = 60)

# Alternate shading
alt_row_style <- createStyle(fgFill = "#F0F7F0")
for (r in seq(2, nrow(var_descriptions)+1, by = 2)) {
  addStyle(wb, "Variable_Descriptions", alt_row_style, rows = r, cols = 1:ncol(var_descriptions), gridExpand = TRUE, stack = TRUE)
}

# Save workbook
excel_path <- "data/NCAA_Tournament_Dashboard_Data.xlsx"
saveWorkbook(wb, excel_path, overwrite = TRUE)

cat("Excel workbook saved to:", excel_path, "\n")
cat("  Sheet 1 (Cleaned_Data):          ", nrow(cleaned_data),           "rows x", ncol(cleaned_data),           "cols\n")
cat("  Sheet 2 (Model_Predictions):     ", nrow(predictions_export_df), "rows x", ncol(predictions_export_df), "cols\n")
cat("  Sheet 3 (Matchup_Probabilities): ", nrow(matchup_df),              "rows x", ncol(matchup_df),              "cols\n")
cat("  Sheet 4 (Variable_Descriptions): ", nrow(var_descriptions),        "rows x", ncol(var_descriptions),        "cols\n")