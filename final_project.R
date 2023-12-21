# Load CSV Files
project78.df <- as.data.frame(read.csv("data_1718.csv"))
project89.df <- as.data.frame(read.csv("data_1819.csv"))
project90.df <- as.data.frame(read.csv("data_1920.csv"))
options(scipen=100)

# Load Library to use
library(dplyr)
library(stringr)
library(ggplot2)
library(forecast)
library( caret )
library(corrplot)
library( party )
library(e1071)
library(randomForest)
library(ggplot2)

################################################################################
### Visualization about Value
options(scipen=100)
# Check Value's Range
ggplot(project78.df, aes(x = value)) +
  geom_histogram(binwidth = 1000000, fill = "skyblue", color = "black") +
  labs(title = "Distribution of 'value'", x = "Value", y = "Frequency")
# Visualize Value's OUtliers
ggplot(project78.df, aes(y = value)) +
  geom_boxplot() +
  labs(y = "Value", title = "Boxplot of 'Value' in project78.df")
################################################################################
# Check Correlation
cor_matrix <- cor(DF_78.df)
cor_matrix
corrplot(cor_matrix, method="color", addCoef.col = 1,number.cex = 0.3, tl.cex = 0.3)

cor_matrix <- cor(MF_78.df)
cor_matrix
corrplot(cor_matrix, method="color", addCoef.col = 1,number.cex = 0.3, tl.cex = 0.3)

cor_matrix <- cor(FW_78.df)
cor_matrix
corrplot(cor_matrix, method="color", addCoef.col = 1,number.cex = 0.3, tl.cex = 0.3)

cor_matrix <- cor(GK_78.df)
cor_matrix
corrplot(cor_matrix, method="color", addCoef.col = 1,number.cex = 0.3, tl.cex = 0.3)
################################################################################
# Data 17-18
################################################################################
project78.df <- na.omit(project78.df)
project78.df$Attendance <- as.numeric(gsub(",", "", project78.df$Attendance))
project78.df <- project78.df %>% filter(value>=1000000 & value<=30000000) %>% select(-c(X, player, nationality, squad, position2, league, Season, CLBestScorer))

# Categorical Variable to Numerical Variable
project78.df$left <- ifelse( project78.df$foot == 'left', 1, 0 )
project78.df$right <- ifelse( project78.df$foot == 'right', 1, 0 )
project78.df <- subset(project78.df, select=-foot)
project78.df$value <- log(project78.df$value)

GK_78.df <- project78.df %>% filter(grepl("GK", position)) %>% select(value, LgRk, Attendance, MP, L, games, dribbles_completed, dribbled_past, crosses_stopped_pct_gk, pens_concededm, GA, xGA, height, birth_year, left, right, fouls, games_starts, minutes, miscontrols, passes, passes_pct, W, passes_into_final_third, passes_into_penalty_area, progressive_passes)
DF_78.df <- project78.df %>% filter(grepl("DF", position)) %>% select(value, height, LgRk, birth_year, Attendance, goals_assists_per90, MP, npxg_per_shot, L, games, dribbles_completed, dribbled_past, shots_total, pens_concededm, GA, xGA, aerials_lost, aerials_won, assists, dribbles_completed_pct, left, right, fouls, games_starts, minutes, gca, miscontrols, passes, passes_pct, passes_received_pct, pressure_regain_pct, pressures, sca, sca_dribbles, sca_fouled, sca_passes_live, sca_per90, tackles, tackles_won, through_balls, throw_ins, touches, touches_live_ball, W, passes_into_final_third, passes_into_penalty_area, progressive_passes, crosses, blocked_passes, clearances, dribbled_past, interceptions)
MF_78.df <- project78.df %>% filter(grepl("MF", position)) %>% select(value, height, LgRk, birth_year, Attendance, goals_assists_per90, MP, npxg_per_shot, L, games, dribbles_completed, dribbled_past, shots_total, pens_concededm, GA, xGA, aerials_lost, aerials_won, assists, dribbles_completed_pct, left, right, fouls, games_starts, minutes, gca, miscontrols, passes, passes_pct, passes_received_pct, pressure_regain_pct, pressures, sca, sca_dribbles, sca_fouled, sca_passes_live, sca_per90, tackles, tackles_won, through_balls, throw_ins, touches, touches_live_ball, W, passes_into_final_third, passes_into_penalty_area, progressive_passes, crosses, carry_progressive_distance, assisted_shots, clearances, dribbled_past, goals, interceptions, offsides, pass_targets, sca_shots, shots_on_target, shots_total, shots_total_per90, goals_per90, goals_per_shot, goals_per_shot_on_target)
FW_78.df <- project78.df %>% filter(grepl("FW", position)) %>% select(value, height, LgRk, birth_year, Attendance, goals_assists_per90, MP, npxg_per_shot, L, games, dribbles_completed, dribbled_past, shots_total, pens_concededm, GA, xGA, aerials_lost, aerials_won, assists, dribbles_completed_pct, left, right, fouls, games_starts, minutes, gca, miscontrols, passes, passes_pct, passes_received_pct, pressure_regain_pct, pressures, sca, sca_dribbles, sca_fouled, sca_passes_live, sca_per90, tackles, tackles_won, through_balls, throw_ins, touches, touches_live_ball, W, passes_into_final_third, passes_into_penalty_area, progressive_passes, crosses, carry_progressive_distance, assisted_shots, goals, offsides, pass_targets, sca_shots, shots_on_target, shots_total, shots_total_per90, goals_per90, goals_per_shot, goals_per_shot_on_target)

# Delete Outlier
z_scores <- scale(GK_78.df)
outlier_indices <- which(abs(z_scores) > 3, arr.ind = TRUE)
GK_78.df <- GK_78.df[-unique(outlier_indices[, 1]), ]

z_scores <- scale(DF_78.df)
outlier_indices <- which(abs(z_scores) > 3, arr.ind = TRUE)
DF_78.df <- DF_78.df[-unique(outlier_indices[, 1]), ]

z_scores <- scale(MF_78.df)
outlier_indices <- which(abs(z_scores) > 3, arr.ind = TRUE)
MF_78.df <- MF_78.df[-unique(outlier_indices[, 1]), ]

z_scores <- scale(FW_78.df)
outlier_indices <- which(abs(z_scores) > 3, arr.ind = TRUE)
FW_78.df <- FW_78.df[-unique(outlier_indices[, 1]), ]

# Normalization
norm.values <- preProcess(GK_78.df[,-1], method=c("center", "scale"))
GK_78.norm.df <- predict(norm.values, GK_78.df[, -1])
GK_78.norm.df <- cbind(GK_78.norm.df, value = GK_78.df$value)

norm.values <- preProcess(DF_78.df[,-1], method=c("center", "scale"))
DF_78.norm.df <- predict(norm.values, DF_78.df[, -1])
DF_78.norm.df <- cbind(DF_78.norm.df, value = DF_78.df$value)

norm.values <- preProcess(MF_78.df[,-1], method=c("center", "scale"))
MF_78.norm.df <- predict(norm.values, MF_78.df[, -1])
MF_78.norm.df <- cbind(MF_78.norm.df, value = MF_78.df$value)

norm.values <- preProcess(FW_78.df[,-1], method=c("center", "scale"))
FW_78.norm.df <- predict(norm.values, FW_78.df[, -1])
FW_78.norm.df <- cbind(FW_78.norm.df, value = FW_78.df$value)

# Stepwise
GK_78.lm <- lm(value ~ ., data = GK_78.norm.df)
DF_78.lm <- lm(value ~ ., data = DF_78.norm.df)
MF_78.lm <- lm(value ~ ., data = MF_78.norm.df)
FW_78.lm <- lm(value ~ ., data = FW_78.norm.df)
GK_78.lm.step <- step(GK_78.lm, direction = "both")
DF_78.lm.step <- step(DF_78.lm, direction = "both")
MF_78.lm.step <- step(MF_78.lm, direction = "both")
FW_78.lm.step <- step(FW_78.lm, direction = "both")

# Select Variable to use
GK_78_final.df <- GK_78.norm.df %>% select(value, Attendance, L, birth_year, left, minutes, passes, passes_into_penalty_area, progressive_passes)

DF_78_final.df <- DF_78.norm.df %>% select(value, LgRk, birth_year, Attendance, MP, L, games, dribbles_completed, shots_total, aerials_lost, miscontrols, touches, W, passes_into_penalty_area, assists)

MF_78_final.df <- MF_78.norm.df %>% select(value, birth_year, Attendance, MP, L, games, dribbles_completed, shots_total, GA, xGA, aerials_won, fouls, gca, sca_fouled, tackles, passes_into_final_third, passes_into_penalty_area, progressive_passes, crosses, goals, goals_per90)

FW_78_final.df <- FW_78.norm.df %>% select(value, birth_year, Attendance, goals_assists_per90, MP, L, games, dribbled_past, shots_total, GA, xGA, aerials_lost, dribbles_completed_pct, left, right, fouls, passes_received_pct,  sca_passes_live, through_balls, touches, goals, pass_targets, goals_per90)

################################################################################
# Data 18-19
################################################################################
project89.df <- na.omit(project89.df)
project89.df$Attendance <- as.numeric(gsub(",", "", project89.df$Attendance))
project89.df <- project89.df %>% filter(value>=1000000 & value<=30000000) %>% select(-c( player, nationality, squad, position2, league, Season, CLBestScorer))

# Categorical Variable to Numerical Variable
project89.df$left <- ifelse( project89.df$foot == 'left', 1, 0 )
project89.df$right <- ifelse( project89.df$foot == 'right', 1, 0 )
project89.df <- subset(project89.df, select=-foot)
project89.df$value <- log(project89.df$value)

GK_89.df <- project89.df %>% filter(grepl("GK", position)) %>% select(value, LgRk, Attendance, MP, L, games, dribbles_completed, dribbled_past, crosses_stopped_pct_gk, pens_concededm, GA, xGA, height, birth_year, left, right, fouls, games_starts, minutes, miscontrols, passes, passes_pct, W, passes_into_final_third, passes_into_penalty_area, progressive_passes)
DF_89.df <- project89.df %>% filter(grepl("DF", position)) %>% select(value, height, LgRk, birth_year, Attendance, goals_assists_per90, MP, npxg_per_shot, L, games, dribbles_completed, dribbled_past, shots_total, pens_concededm, GA, xGA, aerials_lost, aerials_won, assists, dribbles_completed_pct, left, right, fouls, games_starts, minutes, gca, miscontrols, passes, passes_pct, passes_received_pct, pressure_regain_pct, pressures, sca, sca_dribbles, sca_fouled, sca_passes_live, sca_per90, tackles, tackles_won, through_balls, throw_ins, touches, touches_live_ball, W, passes_into_final_third, passes_into_penalty_area, progressive_passes, crosses, blocked_passes, clearances, dribbled_past, interceptions)
MF_89.df <- project89.df %>% filter(grepl("MF", position)) %>% select(value, height, LgRk, birth_year, Attendance, goals_assists_per90, MP, npxg_per_shot, L, games, dribbles_completed, dribbled_past, shots_total, pens_concededm, GA, xGA, aerials_lost, aerials_won, assists, dribbles_completed_pct, left, right, fouls, games_starts, minutes, gca, miscontrols, passes, passes_pct, passes_received_pct, pressure_regain_pct, pressures, sca, sca_dribbles, sca_fouled, sca_passes_live, sca_per90, tackles, tackles_won, through_balls, throw_ins, touches, touches_live_ball, W, passes_into_final_third, passes_into_penalty_area, progressive_passes, crosses, carry_progressive_distance, assisted_shots, clearances, dribbled_past, goals, interceptions, offsides, pass_targets, sca_shots, shots_on_target, shots_total, shots_total_per90, goals_per90, goals_per_shot, goals_per_shot_on_target)
FW_89.df <- project89.df %>% filter(grepl("FW", position)) %>% select(value, height, LgRk, birth_year, Attendance, goals_assists_per90, MP, npxg_per_shot, L, games, dribbles_completed, dribbled_past, shots_total, pens_concededm, GA, xGA, aerials_lost, aerials_won, assists, dribbles_completed_pct, left, right, fouls, games_starts, minutes, gca, miscontrols, passes, passes_pct, passes_received_pct, pressure_regain_pct, pressures, sca, sca_dribbles, sca_fouled, sca_passes_live, sca_per90, tackles, tackles_won, through_balls, throw_ins, touches, touches_live_ball, W, passes_into_final_third, passes_into_penalty_area, progressive_passes, crosses, carry_progressive_distance, assisted_shots, goals, offsides, pass_targets, sca_shots, shots_on_target, shots_total, shots_total_per90, goals_per90, goals_per_shot, goals_per_shot_on_target)

# Delete outlier
z_scores <- scale(GK_89.df)
outlier_indices <- which(abs(z_scores) > 3, arr.ind = TRUE)
GK_89.df <- GK_89.df[-unique(outlier_indices[, 1]), ]

z_scores <- scale(DF_89.df)
outlier_indices <- which(abs(z_scores) > 3, arr.ind = TRUE)
DF_89.df <- DF_89.df[-unique(outlier_indices[, 1]), ]

z_scores <- scale(MF_89.df)
outlier_indices <- which(abs(z_scores) > 3, arr.ind = TRUE)
MF_89.df <- MF_89.df[-unique(outlier_indices[, 1]), ]

z_scores <- scale(FW_89.df)
outlier_indices <- which(abs(z_scores) > 3, arr.ind = TRUE)
FW_89.df <- FW_89.df[-unique(outlier_indices[, 1]), ]

# Normalization
norm.values <- preProcess(GK_89.df[,-1], method=c("center", "scale"))
GK_89.norm.df <- predict(norm.values, GK_89.df[, -1])
GK_89.norm.df <- cbind(GK_89.norm.df, value = GK_89.df$value)

norm.values <- preProcess(DF_89.df[,-1], method=c("center", "scale"))
DF_89.norm.df <- predict(norm.values, DF_89.df[, -1])
DF_89.norm.df <- cbind(DF_89.norm.df, value = DF_89.df$value)

norm.values <- preProcess(MF_89.df[,-1], method=c("center", "scale"))
MF_89.norm.df <- predict(norm.values, MF_89.df[, -1])
MF_89.norm.df <- cbind(MF_89.norm.df, value = MF_89.df$value)

norm.values <- preProcess(FW_89.df[,-1], method=c("center", "scale"))
FW_89.norm.df <- predict(norm.values, FW_89.df[, -1])
FW_89.norm.df <- cbind(FW_89.norm.df, value = FW_89.df$value)

# Stepwise
GK_89.lm <- lm(value ~ ., data = GK_89.norm.df)
DF_89.lm <- lm(value ~ ., data = DF_89.norm.df)
MF_89.lm <- lm(value ~ ., data = MF_89.norm.df)
FW_89.lm <- lm(value ~ ., data = FW_89.norm.df)
GK_89.lm.step <- step(GK_89.lm, direction = "both")
DF_89.lm.step <- step(DF_89.lm, direction = "both")
MF_89.lm.step <- step(MF_89.lm, direction = "both")
FW_89.lm.step <- step(FW_89.lm, direction = "both")

# Select Valiable to use
GK_89_final.df <- GK_89.norm.df %>% select(value, Attendance, L, birth_year, left, minutes, passes, passes_into_penalty_area, progressive_passes)

DF_89_final.df <- DF_89.norm.df %>% select(value, LgRk, birth_year, Attendance, MP, L, games, dribbles_completed, shots_total, aerials_lost, miscontrols, touches, W, passes_into_penalty_area, assists)

MF_89_final.df <- MF_89.norm.df %>% select(value, birth_year, Attendance, MP, L, games, dribbles_completed, shots_total, GA, xGA, aerials_won, fouls, gca, sca_fouled, tackles, passes_into_final_third, passes_into_penalty_area, progressive_passes, crosses, goals, goals_per90)

FW_89_final.df <- FW_89.norm.df %>% select(value, birth_year, Attendance, goals_assists_per90, MP, L, games, dribbled_past, shots_total, GA, xGA, aerials_lost, dribbles_completed_pct, left, right, fouls, passes_received_pct,  sca_passes_live, through_balls, touches, goals, pass_targets, goals_per90)

################################################################################
# About 19-20 Data
################################################################################
project90.df <- na.omit(project90.df)
project90.df$Attendance <- as.numeric(gsub(",", "", project90.df$Attendance))
project90.df <- project90.df %>% filter(value>=1000000 & value<=30000000) %>% select(-c( player, nationality, squad, position2, league, Season, CLBestScorer))

# Categorical Variable to Numerical Variable
project90.df$left <- ifelse( project90.df$foot == 'left', 1, 0 )
project90.df$right <- ifelse( project90.df$foot == 'right', 1, 0 )
project90.df <- subset(project90.df, select=-foot)
project90.df$value <- log(project90.df$value)

GK_90.df <- project90.df %>% filter(grepl("GK", position)) %>% select(value, LgRk, Attendance, MP, L, games, dribbles_completed, dribbled_past, crosses_stopped_pct_gk, pens_concededm, GA, xGA, height, birth_year, left, right, fouls, games_starts, minutes, miscontrols, passes, passes_pct, W, passes_into_final_third, passes_into_penalty_area, progressive_passes)
DF_90.df <- project90.df %>% filter(grepl("DF", position)) %>% select(value, height, LgRk, birth_year, Attendance, goals_assists_per90, MP, npxg_per_shot, L, games, dribbles_completed, dribbled_past, shots_total, pens_concededm, GA, xGA, aerials_lost, aerials_won, assists, dribbles_completed_pct, left, right, fouls, games_starts, minutes, gca, miscontrols, passes, passes_pct, passes_received_pct, pressure_regain_pct, pressures, sca, sca_dribbles, sca_fouled, sca_passes_live, sca_per90, tackles, tackles_won, through_balls, throw_ins, touches, touches_live_ball, W, passes_into_final_third, passes_into_penalty_area, progressive_passes, crosses, blocked_passes, clearances, dribbled_past, interceptions)
MF_90.df <- project90.df %>% filter(grepl("MF", position)) %>% select(value, height, LgRk, birth_year, Attendance, goals_assists_per90, MP, npxg_per_shot, L, games, dribbles_completed, dribbled_past, shots_total, pens_concededm, GA, xGA, aerials_lost, aerials_won, assists, dribbles_completed_pct, left, right, fouls, games_starts, minutes, gca, miscontrols, passes, passes_pct, passes_received_pct, pressure_regain_pct, pressures, sca, sca_dribbles, sca_fouled, sca_passes_live, sca_per90, tackles, tackles_won, through_balls, throw_ins, touches, touches_live_ball, W, passes_into_final_third, passes_into_penalty_area, progressive_passes, crosses, carry_progressive_distance, assisted_shots, clearances, dribbled_past, goals, interceptions, offsides, pass_targets, sca_shots, shots_on_target, shots_total, shots_total_per90, goals_per90, goals_per_shot, goals_per_shot_on_target)
FW_90.df <- project90.df %>% filter(grepl("FW", position)) %>% select(value, height, LgRk, birth_year, Attendance, goals_assists_per90, MP, npxg_per_shot, L, games, dribbles_completed, dribbled_past, shots_total, pens_concededm, GA, xGA, aerials_lost, aerials_won, assists, dribbles_completed_pct, left, right, fouls, games_starts, minutes, gca, miscontrols, passes, passes_pct, passes_received_pct, pressure_regain_pct, pressures, sca, sca_dribbles, sca_fouled, sca_passes_live, sca_per90, tackles, tackles_won, through_balls, throw_ins, touches, touches_live_ball, W, passes_into_final_third, passes_into_penalty_area, progressive_passes, crosses, carry_progressive_distance, assisted_shots, goals, offsides, pass_targets, sca_shots, shots_on_target, shots_total, shots_total_per90, goals_per90, goals_per_shot, goals_per_shot_on_target)

# 이상치 빼기
z_scores <- scale(GK_90.df)
outlier_indices <- which(abs(z_scores) > 3, arr.ind = TRUE)
GK_90.df <- GK_90.df[-unique(outlier_indices[, 1]), ]

z_scores <- scale(DF_90.df)
outlier_indices <- which(abs(z_scores) > 3, arr.ind = TRUE)
DF_90.df <- DF_90.df[-unique(outlier_indices[, 1]), ]

z_scores <- scale(MF_90.df)
outlier_indices <- which(abs(z_scores) > 3, arr.ind = TRUE)
MF_90.df <- MF_90.df[-unique(outlier_indices[, 1]), ]

z_scores <- scale(FW_90.df)
outlier_indices <- which(abs(z_scores) > 3, arr.ind = TRUE)
FW_90.df <- FW_90.df[-unique(outlier_indices[, 1]), ]

# 정규화
norm.values <- preProcess(GK_90.df[,-1], method=c("center", "scale"))
GK_90.norm.df <- predict(norm.values, GK_90.df[, -1])
GK_90.norm.df <- cbind(GK_90.norm.df, value = GK_90.df$value)

norm.values <- preProcess(DF_90.df[,-1], method=c("center", "scale"))
DF_90.norm.df <- predict(norm.values, DF_90.df[, -1])
DF_90.norm.df <- cbind(DF_90.norm.df, value = DF_90.df$value)

norm.values <- preProcess(MF_90.df[,-1], method=c("center", "scale"))
MF_90.norm.df <- predict(norm.values, MF_90.df[, -1])
MF_90.norm.df <- cbind(MF_90.norm.df, value = MF_90.df$value)

norm.values <- preProcess(FW_90.df[,-1], method=c("center", "scale"))
FW_90.norm.df <- predict(norm.values, FW_90.df[, -1])
FW_90.norm.df <- cbind(FW_90.norm.df, value = FW_90.df$value)

# Stepwise
GK_90.lm <- lm(value ~ ., data = GK_90.norm.df)
DF_90.lm <- lm(value ~ ., data = DF_90.norm.df)
MF_90.lm <- lm(value ~ ., data = MF_90.norm.df)
FW_90.lm <- lm(value ~ ., data = FW_90.norm.df)
GK_90.lm.step <- step(GK_90.lm, direction = "both")
DF_90.lm.step <- step(DF_90.lm, direction = "both")
MF_90.lm.step <- step(MF_90.lm, direction = "both")
FW_90.lm.step <- step(FW_90.lm, direction = "both")

#변수 최종 선택
GK_90_final.df <- GK_90.norm.df %>% select(value, Attendance, L, birth_year, left, minutes, passes, passes_into_penalty_area, progressive_passes)

DF_90_final.df <- DF_90.norm.df %>% select(value, LgRk, birth_year, Attendance, MP, L, games, dribbles_completed, shots_total, aerials_lost, miscontrols, touches, W, passes_into_penalty_area, assists)

MF_90_final.df <- MF_90.norm.df %>% select(value, birth_year, Attendance, MP, L, games, dribbles_completed, shots_total, GA, xGA, aerials_won, fouls, gca, sca_fouled, tackles, passes_into_final_third, passes_into_penalty_area, progressive_passes, crosses, goals, goals_per90)

FW_90_final.df <- FW_90.norm.df %>% select(value, birth_year, Attendance, goals_assists_per90, MP, L, games, dribbled_past, shots_total, GA, xGA, aerials_lost, dribbles_completed_pct, left, right, fouls, passes_received_pct,  sca_passes_live, through_balls, touches, goals, pass_targets, goals_per90)

################################################################################
################################################################################
################################################################################
#### Regresssion Model
train.index <- sample(dim(GK_78_final.df)[1], round(dim(GK_78_final.df)[1]*0.7))
train.df <- GK_78_final.df[train.index, ]
validate.df <- GK_78_final.df[-train.index, ]
GK_78.lm <- lm(value ~ ., data = train.df)
GK_78.lm.pred <- predict(GK_78.lm, validate.df)
accuracy(exp(GK_78.lm.pred), exp(validate.df$value))

train.index <- sample(dim(DF_78_final.df)[1], round(dim(DF_78_final.df)[1]*0.7))
train.df <- DF_78_final.df[train.index, ]
validate.df <- DF_78_final.df[-train.index, ]
DF_78.lm <- lm(value ~ ., data = train.df)
DF_78.lm.pred <- predict(DF_78.lm, validate.df)
accuracy(exp(DF_78.lm.pred), exp(validate.df$value))

train.index <- sample(dim(MF_78_final.df)[1], round(dim(MF_78_final.df)[1]*0.7))
train.df <- MF_78_final.df[train.index, ]
validate.df <- MF_78_final.df[-train.index, ]
MF_78.lm <- lm(value ~ ., data = train.df)
MF_78.lm.pred <- predict(MF_78.lm, validate.df)
accuracy(exp(MF_78.lm.pred), exp(validate.df$value))

train.index <- sample(dim(FW_78_final.df)[1], round(dim(FW_78_final.df)[1]*0.7))
train.df <- FW_78_final.df[train.index, ]
validate.df <- FW_78_final.df[-train.index, ]
FW_78.lm <- lm(value ~ ., data = train.df)
FW_78.lm.pred <- predict(FW_78.lm, validate.df)
accuracy(exp(FW_78.lm.pred), exp(validate.df$value))

#### Regresssion Model base PCA
pca <- prcomp(GK_78_final.df)
summary(pca)
GK_78.norm.df_pca <- as.data.frame(pca$x[,1:6])
GK_78.norm.df_pca$value <- GK_78_final.df$value

train.index <- sample(dim(GK_78.norm.df_pca)[1], round(dim(GK_78.norm.df_pca)[1]*0.7))
train.df <- GK_78.norm.df_pca[train.index, ]
validate.df <- GK_78.norm.df_pca[-train.index, ]

GK_78.pca.lm <- lm(value ~ ., data = train.df)  # 선형회귀분석 수행

GK_78.pca.lm.pred <- predict(GK_78.pca.lm, newdata = validate.df)

accuracy(exp(GK_78.pca.lm.pred),exp(validate.df$value))

#==DF_78
pca <- prcomp(DF_78_final.df)
summary(pca)
DF_78.norm.df_pca <- as.data.frame(pca$x[,1:8])
DF_78.norm.df_pca$value <- DF_78_final.df$value

train.index <- sample(dim(DF_78.norm.df_pca)[1], round(dim(DF_78.norm.df_pca)[1]*0.7))
train.df <- DF_78.norm.df_pca[train.index, ]
validate.df <- DF_78.norm.df_pca[-train.index, ]

DF_78.pca.lm <- lm(value ~ ., data = train.df)  # 선형회귀분석 수행

DF_78.pca.lm.pred <- predict(DF_78.pca.lm, newdata = validate.df)

accuracy(exp(DF_78.pca.lm.pred),exp(validate.df$value))

#==MF_78
pca <- prcomp(MF_78_final.df)
summary(pca)
MF_78.norm.df_pca <- as.data.frame(pca$x[,1:11])
MF_78.norm.df_pca$value <- MF_78_final.df$value

train.index <- sample(dim(MF_78.norm.df_pca)[1], round(dim(MF_78.norm.df_pca)[1]*0.7))
train.df <- MF_78.norm.df_pca[train.index, ]
validate.df <- MF_78.norm.df_pca[-train.index, ]

MF_78.pca.lm <- lm(value ~ ., data = train.df)  # 선형회귀분석 수행

MF_78.pca.lm.pred <- predict(MF_78.pca.lm, newdata = validate.df)

accuracy(exp(MF_78.pca.lm.pred),exp(validate.df$value))

#==FW_78
pca <- prcomp(FW_78_final.df)
summary(pca)
FW_78.norm.df_pca <- as.data.frame(pca$x[,1:11])
FW_78.norm.df_pca$value <- FW_78_final.df$value

train.index <- sample(dim(FW_78.norm.df_pca)[1], round(dim(FW_78.norm.df_pca)[1]*0.7))
train.df <- FW_78.norm.df_pca[train.index, ]
validate.df <- FW_78.norm.df_pca[-train.index, ]

FW_78.pca.lm <- lm(value ~ ., data = train.df)  # 선형회귀분석 수행

FW_78.pca.lm.pred <- predict(FW_78.pca.lm, newdata = validate.df)

accuracy(exp(FW_78.pca.lm.pred),exp(validate.df$value))


#==== Validate by data_1819
#==GK_89 validate
pca <- prcomp(GK_89_final.df)
summary(pca)
GK_89.norm.df_pca <- as.data.frame(pca$x[,1:6])
GK_89.norm.df_pca$value <- GK_89_final.df$value

GK_78.pca.lm.pred <- predict(GK_78.pca.lm, newdata = GK_89.norm.df_pca)

accuracy(exp(GK_78.pca.lm.pred),exp(GK_89.norm.df_pca$value))
#==DF_89 validate
pca <- prcomp(DF_89_final.df)
summary(pca)
DF_89.norm.df_pca <- as.data.frame(pca$x[,1:8])
DF_89.norm.df_pca$value <- DF_89_final.df$value

DF_78.pca.lm.pred <- predict(DF_78.pca.lm, newdata = DF_89.norm.df_pca)

accuracy(exp(DF_78.pca.lm.pred),exp(DF_89.norm.df_pca$value))
#==MF_89 validate
pca <- prcomp(MF_89_final.df)
summary(pca)
MF_89.norm.df_pca <- as.data.frame(pca$x[,1:11])
MF_89.norm.df_pca$value <- MF_89_final.df$value

MF_78.pca.lm.pred <- predict(MF_78.pca.lm, newdata = MF_89.norm.df_pca)

accuracy(exp(MF_78.pca.lm.pred),exp(MF_89.norm.df_pca$value))
#==FW_89 validate
pca <- prcomp(FW_89_final.df)
summary(pca)
FW_89.norm.df_pca <- as.data.frame(pca$x[,1:11])
FW_89.norm.df_pca$value <- FW_89_final.df$value

FW_78.pca.lm.pred <- predict(FW_78.pca.lm, newdata = FW_89.norm.df_pca)

accuracy(exp(FW_78.pca.lm.pred),exp(FW_89.norm.df_pca$value))
#==== Validate by data_1920
#==GK_90 validate
pca <- prcomp(GK_90_final.df)
summary(pca)
GK_90.norm.df_pca <- as.data.frame(pca$x[,1:6])
GK_90.norm.df_pca$value <- GK_90_final.df$value

GK_78.pca.lm.pred <- predict(GK_78.pca.lm, newdata = GK_90.norm.df_pca)

accuracy(exp(GK_78.pca.lm.pred),exp(GK_90.norm.df_pca$value))
#==DF_90 validate
pca <- prcomp(DF_90_final.df)
summary(pca)
DF_90.norm.df_pca <- as.data.frame(pca$x[,1:8])
DF_90.norm.df_pca$value <- DF_90_final.df$value

DF_78.pca.lm.pred <- predict(DF_78.pca.lm, newdata = DF_90.norm.df_pca)

accuracy(exp(DF_78.pca.lm.pred),exp(DF_90.norm.df_pca$value))
#==MF_90 validate
pca <- prcomp(MF_90_final.df)
summary(pca)
MF_90.norm.df_pca <- as.data.frame(pca$x[,1:11])
MF_90.norm.df_pca$value <- MF_90_final.df$value

MF_78.pca.lm.pred <- predict(MF_78.pca.lm, newdata = MF_90.norm.df_pca)

accuracy(exp(MF_78.pca.lm.pred),exp(MF_90.norm.df_pca$value))
#==FW_90 validate
pca <- prcomp(FW_90_final.df)
summary(pca)
FW_90.norm.df_pca <- as.data.frame(pca$x[,1:11])
FW_90.norm.df_pca$value <- FW_90_final.df$value

FW_78.pca.lm.pred <- predict(FW_78.pca.lm, newdata = FW_90.norm.df_pca)

accuracy(exp(FW_78.pca.lm.pred),exp(FW_90.norm.df_pca$value))


##### Decision Tree Regression
### Create DF Model & Check Accuracy
train.index <- sample(dim(DF_78_final.df)[1], round( nrow( DF_78_final.df )* 0.6 ) )
train.df <- DF_78_final.df[ train.index, ]
validate.df <- DF_78_final.df[ -train.index, ]

DF78.lm <- ctree( value ~., data=train.df )
DF78.lm.pred <- predict( DF78.lm, validate.df )
plot( DF78.lm.pred )
temp.df <- data.frame( DF78.lm.pred, validate.df$value )
accuracy( exp( temp.df$value ), exp( validate.df$value ) )


### Create MF Model & Check Accuracy
train.index <- sample(dim(MF_78_final.df)[1], round( nrow(MF_78_final.df)*0.6 ) )
train.df <- MF_78_final.df[ train.index, ]
validate.df <- MF_78_final.df[ -train.index, ]

MF78.lm <- ctree( value ~., data=train.df )
MF78.lm.pred <- predict( MF78.lm, validate.df )
plot( MF78.lm.pred )
temp.df <- data.frame( MF78.lm.pred, validate.df$value )
accuracy( exp( temp.df$value ), exp( validate.df$value ) )

### Create FW Model & Check Accuracy
train.index <- sample(dim(FW_78_final.df)[1], round( nrow(FW_78_final.df)*0.6 ) )
train.df <- FW_78_final.df[ train.index, ]
validate.df <- FW_78_final.df[ -train.index, ]

FW78.lm <- ctree( value ~., data=train.df )
FW78.lm.pred <- predict( FW78.lm, validate.df )
plot( FW78.lm.pred )
temp.df <- data.frame( FW78.lm.pred, validate.df$value )
accuracy( exp( temp.df$value ), exp( validate.df$value ) )

### Create GK Model & Check Accuracy
train.index <- sample(dim(GK_78_final.df)[1], round( nrow(GK_78_final.df)*0.6) )
train.df <- GK_78_final.df[ train.index, ]
validate.df <- GK_78_final.df[ -train.index, ]

GK78.lm <- ctree( value ~., data=train.df )
GK78.lm.pred <- predict( GK78.lm, validate.df )
plot( GK78.lm.pred )
temp.df <- data.frame( GK78.lm.pred, validate.df$value )
accuracy( exp( temp.df$value ), exp( validate.df$value ) )

### Predict by Model
# DF
DF89.lm.pred <- predict( DF78.lm, DF_89_final.df )
DF89_TEMP.df <- data.frame( DF89.lm.pred, DF_89_final.df$value )
accuracy( exp( DF89_TEMP.df$value ), exp( DF_89_final.df$value ) )

DF90.lm.pred <- predict( DF78.lm, DF_90_final.df )
DF90_TEMP.df <- data.frame( DF90.lm.pred, DF_90_final.df$value )
accuracy( exp( DF90_TEMP.df$value ), exp( DF_90_final.df$value ) )

# MF
MF89.lm.pred <- predict( MF78.lm, MF_89_final.df )
MF89_TEMP.df <- data.frame( MF89.lm.pred, MF_89_final.df$value )
accuracy( exp( MF89_TEMP.df$value ), exp( MF_89_final.df$value ) )

MF90.lm.pred <- predict( MF78.lm, MF_90_final.df )
MF90_TEMP.df <- data.frame( MF90.lm.pred, MF_90_final.df$value )
accuracy( exp( MF90_TEMP.df$value ), exp( MF_90_final.df$value ) )

# FW
FW89.lm.pred <- predict( FW78.lm, FW_89_final.df )
FW89_TEMP.df <- data.frame( FW89.lm.pred, FW_89_final.df$value )
accuracy( exp( FW89_TEMP.df$value ), exp( FW_89_final.df$value ) )

FW90.lm.pred <- predict( FW78.lm, FW_90_final.df )
FW90_TEMP.df <- data.frame( FW90.lm.pred, FW_90_final.df$value )
accuracy( exp( FW90_TEMP.df$value ), exp( FW_90_final.df$value ) )

# GK
GK89.lm.pred <- predict( GK78.lm, GK_89_final.df )
GK89_TEMP.df <- data.frame( GK89.lm.pred, GK_89_final.df$value )
accuracy( exp( GK89_TEMP.df$value ), exp( GK_89_final.df$value ) )

GK90.lm.pred <- predict( GK78.lm, GK_90_final.df )
GK90_TEMP.df <- data.frame( GK90.lm.pred, GK_90_final.df$value )
accuracy( exp( GK90_TEMP.df$value ), exp( GK_90_final.df$value ) )

################################################################################
################################################################################
################################################################################

### Support Vector Machine Regression
# Create GK Model & Check Accuracy
train.index <- sample(dim(GK_78_final.df)[1], round(dim(GK_78_final.df)[1]*0.7))
train.df <- GK_78_final.df[train.index, ]
validate.df <- GK_78_final.df[-train.index, ]
GK_78.svm <- svm(value ~ ., data = train.df, kernel = "linear")
GK_78.svm.pred <- predict(GK_78.svm, newdata = validate.df)
accuracy(exp(GK_78.svm.pred), exp(validate.df$value))

# Create DF Model & Check Accuracy
train.index <- sample(dim(DF_78_final.df)[1], round(dim(DF_78_final.df)[1]*0.7))
train.df <- DF_78_final.df[train.index, ]
validate.df <- DF_78_final.df[-train.index, ]
DF_78.svm <- svm(value ~ ., data = train.df, kernel = "linear")
DF_78.svm.pred <- predict(DF_78.svm, newdata = validate.df)
accuracy(exp(DF_78.svm.pred), exp(validate.df$value))

# Create MF Model & Check Accuracy
train.index <- sample(dim(MF_78_final.df)[1], round(dim(MF_78_final.df)[1]*0.7))
train.df <- MF_78_final.df[train.index, ]
validate.df <- MF_78_final.df[-train.index, ]
MF_78.svm <- svm(value ~ ., data = train.df, kernel = "linear")
MF_78.svm.pred <- predict(MF_78.svm, newdata = validate.df)
accuracy(exp(MF_78.svm.pred), exp(validate.df$value))

# Create FW Model & Check Accuracy
train.index <- sample(dim(FW_78_final.df)[1], round(dim(FW_78_final.df)[1]*0.7))
train.df <- FW_78_final.df[train.index, ]
validate.df <- FW_78_final.df[-train.index, ]
FW_78.svm <- svm(value ~ ., data = train.df, kernel = "linear")
FW_78.svm.pred <- predict(FW_78.svm, newdata = validate.df)
accuracy(exp(FW_78.svm.pred), exp(validate.df$value))

### Predict by SVM Model
# About GK Data
GK_89.svm.pred <- predict(GK_78.svm, newdata = GK_89_final.df)
accuracy(exp(GK_89.svm.pred), exp(GK_89_final.df$value))
GK_90.svm.pred <- predict(GK_78.svm, newdata = GK_90_final.df)
accuracy(exp(GK_90.svm.pred), exp(GK_90_final.df$value))

# About DF Data
DF_89.svm.pred <- predict(DF_78.svm, newdata = DF_89_final.df)
accuracy(exp(DF_89.svm.pred), exp(DF_89_final.df$value))
DF_90.svm.pred <- predict(DF_78.svm, newdata = DF_90_final.df)
accuracy(exp(DF_90.svm.pred), exp(DF_90_final.df$value))

# About MF Data
MF_89.svm.pred <- predict(MF_78.svm, newdata = MF_89_final.df)
accuracy(exp(MF_89.svm.pred), exp(MF_89_final.df$value))
MF_90.svm.pred <- predict(MF_78.svm, newdata = MF_90_final.df)
accuracy(exp(MF_90.svm.pred), exp(MF_90_final.df$value))

# About FW Data
FW_89.svm.pred <- predict(FW_78.svm, newdata = FW_89_final.df)
accuracy(exp(FW_89.svm.pred), exp(FW_89_final.df$value))
FW_90.svm.pred <- predict(FW_78.svm, newdata = FW_90_final.df)
accuracy(exp(FW_90.svm.pred), exp(FW_90_final.df$value))

################################################################################
################################################################################
################################################################################

##### Random Forest Model
#####################################################################################################
#  Use Different Data Set - Need to Reset Base Dataframe After Executing R Code to use other Model  # 
#####################################################################################################
project78.df <- as.data.frame(read.csv("data_1718.csv"))
options(scipen=100)
project78.df <- na.omit(project78.df)
GK_78.df <- project78.df %>% filter(grepl("GK", position)) %>% filter(value >= 1000000 & value <= 30000000) %>% select(value, height, age, foot, fouls, games, games_starts, minutes, miscontrols, passes, passes_pct, through_balls, W, passes_into_final_third, passes_into_penalty_area, progressive_passes)
DF_78.df <- project78.df %>% filter(grepl("DF", position)) %>% filter(value >= 1000000 & value <= 30000000) %>% select(value, height, age, aerials_lost, aerials_won, assists, dribbles_completed_pct, foot, fouls, games, games_starts, minutes, gca, miscontrols, passes, passes_pct, passes_received_pct, pressure_regain_pct, pressures, sca, sca_dribbles, sca_fouled, sca_passes_live, sca_per90, tackles, tackles_won, through_balls, throw_ins, touches, touches_live_ball, W, passes_into_final_third, passes_into_penalty_area, progressive_passes, crosses, blocked_passes, clearances, dribbled_past, interceptions)
MF_78.df <- project78.df %>% filter(grepl("MF", position)) %>% filter(value >= 1000000 & value <= 30000000) %>% select(value, height, age, aerials_lost, aerials_won, assists, dribbles_completed_pct, foot, fouls, games, games_starts, minutes, gca, miscontrols, passes, passes_pct, passes_received_pct, pressure_regain_pct, pressures, sca, sca_dribbles, sca_fouled, sca_passes_live, sca_per90, tackles, tackles_won, through_balls, throw_ins, touches, touches_live_ball, W, passes_into_final_third, passes_into_penalty_area, progressive_passes, crosses, carry_progressive_distance, assisted_shots, clearances, dribbled_past, goals, interceptions, offsides, pass_targets, sca_shots, shots_on_target, shots_total, shots_total_per90, goals_per90, goals_per_shot, goals_per_shot_on_target)
FW_78.df <- project78.df %>% filter(grepl("FW", position)) %>% filter(value >= 1000000 & value <= 30000000) %>% select(value, height, age, aerials_lost, aerials_won, assists, dribbles_completed_pct, foot, fouls, games, games_starts, minutes, gca, miscontrols, passes, passes_pct, passes_received_pct, pressure_regain_pct, pressures, sca, sca_dribbles, sca_fouled, sca_passes_live, sca_per90, tackles, tackles_won, through_balls, throw_ins, touches, touches_live_ball, W, passes_into_final_third, passes_into_penalty_area, progressive_passes, crosses, carry_progressive_distance, assisted_shots, goals, offsides, pass_targets, sca_shots, shots_on_target, shots_total, shots_total_per90, goals_per90, goals_per_shot, goals_per_shot_on_target)

### Sampling
# GK
GK_Sample.df <- GK_78.df
GK_Sample.df$value <- log(GK_Sample.df$value)
GK_Train.index <- sample(dim(GK_Sample.df)[1], round( nrow( GK_Sample.df )* 0.7 ) )
GK_Train.df <- as.data.frame(GK_Sample.df[GK_Train.index, ])
GK_Validate.df <- as.data.frame(GK_Sample.df[-GK_Train.index, ])

# DF
DF_Sample.df <- DF_78.df
DF_Sample.df$value <- log(DF_Sample.df$value)
DF_Train.index <- sample(dim(DF_Sample.df)[1], round( nrow( DF_Sample.df )* 0.7 ) )
DF_Train.df <- as.data.frame(DF_Sample.df[DF_Train.index, ])
DF_Validate.df <- as.data.frame(DF_Sample.df[-DF_Train.index, ])

# MF
MF_Sample.df <- MF_78.df
MF_Sample.df$value <- log(MF_Sample.df$value)
MF_Train.index <- sample(dim(MF_Sample.df)[1], round( nrow( MF_Sample.df )* 0.7 ) )
MF_Train.df <- as.data.frame(MF_Sample.df[MF_Train.index, ])
MF_Validate.df <- as.data.frame(MF_Sample.df[-MF_Train.index, ])

# FW
FW_Sample.df <- FW_78.df
FW_Sample.df$value <- log(FW_Sample.df$value)
FW_Train.index <- sample(dim(FW_Sample.df)[1], round( nrow( FW_Sample.df )* 0.7 ) )
FW_Train.df <- as.data.frame(FW_Sample.df[FW_Train.index, ])
FW_Validate.df <- as.data.frame(FW_Sample.df[-FW_Train.index, ])

### Create Random Forest Model
# GK
GK_78.rf <- randomForest(value ~ ., data = GK_Train.df, ntree=500)
GK_78.rf.pred <- predict(GK_78.rf, GK_Validate.df)
accuracy(GK_78.rf.pred, GK_Validate.df$value)
accuracy(exp(GK_78.rf.pred), exp(GK_Validate.df$value))

# DF
DF_78.rf <- randomForest(value ~ ., data = DF_Train.df, ntree=500)
DF_78.rf.pred <- predict(DF_78.rf, DF_Validate.df)
accuracy(DF_78.rf.pred, DF_Validate.df$value)
accuracy(exp(DF_78.rf.pred), exp(DF_Validate.df$value))

# MF
MF_78.rf <- randomForest(value ~ ., data = MF_Train.df, ntree=500)
MF_78.rf.pred <- predict(MF_78.rf, MF_Validate.df)
accuracy(MF_78.rf.pred, MF_Validate.df$value)
accuracy(exp(MF_78.rf.pred), exp(MF_Validate.df$value))

# FW
FW_78.rf <- randomForest(value ~ ., data = FW_Train.df, ntree=500)
FW_78.rf.pred <- predict(FW_78.rf, FW_Validate.df)
accuracy(FW_78.rf.pred, FW_Validate.df$value)
accuracy(exp(FW_78.rf.pred), exp(FW_Validate.df$value))

### Validate by 18-19 Data
project89.df <- as.data.frame(read.csv("data_1819.csv"))
project89.df <- na.omit(project89.df)
GK_89.df <- project89.df %>% filter(grepl("GK", position)) %>% filter(value>=1000000 & value<=30000000) %>% select(value, height, age, foot, fouls, games, games_starts, minutes, miscontrols, passes, passes_pct, through_balls, W, passes_into_final_third, passes_into_penalty_area, progressive_passes)
GK_89.df$value <- log(GK_89.df$value)
DF_89.df <- project89.df %>% filter(grepl("DF", position)) %>% filter(value>=1000000 & value<=30000000) %>% select(value, height, age, aerials_lost, aerials_won, assists, dribbles_completed_pct, foot, fouls, games, games_starts, minutes, gca, miscontrols, passes, passes_pct, passes_received_pct, pressure_regain_pct, pressures, sca, sca_dribbles, sca_fouled, sca_passes_live, sca_per90, tackles, tackles_won, through_balls, throw_ins, touches, touches_live_ball, W, passes_into_final_third, passes_into_penalty_area, progressive_passes, crosses, blocked_passes, clearances, dribbled_past, interceptions)
DF_89.df$value <- log(DF_89.df$value)
MF_89.df <- project89.df %>% filter(grepl("MF", position)) %>% filter(value>=1000000 & value<=30000000) %>% select(value, height, age, aerials_lost, aerials_won, assists, dribbles_completed_pct, foot, fouls, games, games_starts, minutes, gca, miscontrols, passes, passes_pct, passes_received_pct, pressure_regain_pct, pressures, sca, sca_dribbles, sca_fouled, sca_passes_live, sca_per90, tackles, tackles_won, through_balls, throw_ins, touches, touches_live_ball, W, passes_into_final_third, passes_into_penalty_area, progressive_passes, crosses, carry_progressive_distance, assisted_shots, clearances, dribbled_past, goals, interceptions, offsides, pass_targets, sca_shots, shots_on_target, shots_total, shots_total_per90, goals_per90, goals_per_shot, goals_per_shot_on_target)
MF_89.df$value <- log(MF_89.df$value)
FW_89.df <- project89.df %>% filter(grepl("FW", position)) %>% filter(value>=1000000 & value<=30000000) %>% select(value, height, age, aerials_lost, aerials_won, assists, dribbles_completed_pct, foot, fouls, games, games_starts, minutes, gca, miscontrols, passes, passes_pct, passes_received_pct, pressure_regain_pct, pressures, sca, sca_dribbles, sca_fouled, sca_passes_live, sca_per90, tackles, tackles_won, through_balls, throw_ins, touches, touches_live_ball, W, passes_into_final_third, passes_into_penalty_area, progressive_passes, crosses, carry_progressive_distance, assisted_shots, goals, offsides, pass_targets, sca_shots, shots_on_target, shots_total, shots_total_per90, goals_per90, goals_per_shot, goals_per_shot_on_target)
FW_89.df$value <- log(FW_89.df$value)

# GK
GK_78.rf.pred.89 <- predict(GK_78.rf, GK_89.df)
accuracy(GK_78.rf.pred.89, GK_89.df$value)
View(data.frame(GK_78.rf.pred.89, GK_89.df$value))
accuracy(exp(GK_78.rf.pred.89), exp(GK_89.df$value))
View(data.frame(exp(GK_78.rf.pred.89), exp(GK_89.df$value)))

# DF
DF_78.rf.pred.89 <- predict(DF_78.rf, DF_89.df)
accuracy(DF_78.rf.pred.89, DF_89.df$value)
View(data.frame(DF_78.rf.pred.89, DF_89.df$value))
accuracy(exp(DF_78.rf.pred.89), exp(DF_89.df$value))
View(data.frame(exp(DF_78.rf.pred.89), exp(DF_89.df$value)))

# MF
MF_78.rf.pred.89 <- predict(MF_78.rf, MF_89.df)
accuracy(MF_78.rf.pred.89, MF_89.df$value)
View(data.frame(MF_78.rf.pred.89, MF_89.df$value))
accuracy(exp(MF_78.rf.pred.89), exp(MF_89.df$value))
View(data.frame(exp(MF_78.rf.pred.89), exp(MF_89.df$value)))

# FW
FW_78.rf.pred.89 <- predict(FW_78.rf, FW_89.df)
accuracy(FW_78.rf.pred.89, FW_89.df$value)
View(data.frame(FW_78.rf.pred.89, FW_89.df$value))
accuracy(exp(FW_78.rf.pred.89), exp(FW_89.df$value))
View(data.frame(exp(FW_78.rf.pred.89), exp(FW_89.df$value)))

### Validate by 19-20 Data
project90.df <- as.data.frame(read.csv("data_1920.csv"))
project90.df <- na.omit(project90.df)
GK_90.df <- project90.df %>% filter(grepl("GK", position)) %>% filter(value>=1000000 & value<=30000000) %>% select(value, height, age, foot, fouls, games, games_starts, minutes, miscontrols, passes, passes_pct, through_balls, W, passes_into_final_third, passes_into_penalty_area, progressive_passes)
GK_90.df$value <- log(GK_90.df$value)
DF_90.df <- project90.df %>% filter(grepl("DF", position)) %>% filter(value>=1000000 & value<=30000000) %>% select(value, height, age, aerials_lost, aerials_won, assists, dribbles_completed_pct, foot, fouls, games, games_starts, minutes, gca, miscontrols, passes, passes_pct, passes_received_pct, pressure_regain_pct, pressures, sca, sca_dribbles, sca_fouled, sca_passes_live, sca_per90, tackles, tackles_won, through_balls, throw_ins, touches, touches_live_ball, W, passes_into_final_third, passes_into_penalty_area, progressive_passes, crosses, blocked_passes, clearances, dribbled_past, interceptions)
DF_90.df$value <- log(DF_90.df$value)
MF_90.df <- project90.df %>% filter(grepl("MF", position)) %>% filter(value>=1000000 & value<=30000000) %>% select(value, height, age, aerials_lost, aerials_won, assists, dribbles_completed_pct, foot, fouls, games, games_starts, minutes, gca, miscontrols, passes, passes_pct, passes_received_pct, pressure_regain_pct, pressures, sca, sca_dribbles, sca_fouled, sca_passes_live, sca_per90, tackles, tackles_won, through_balls, throw_ins, touches, touches_live_ball, W, passes_into_final_third, passes_into_penalty_area, progressive_passes, crosses, carry_progressive_distance, assisted_shots, clearances, dribbled_past, goals, interceptions, offsides, pass_targets, sca_shots, shots_on_target, shots_total, shots_total_per90, goals_per90, goals_per_shot, goals_per_shot_on_target)
MF_90.df$value <- log(MF_90.df$value)
FW_90.df <- project90.df %>% filter(grepl("FW", position)) %>% filter(value>=1000000 & value<=30000000) %>% select(value, height, age, aerials_lost, aerials_won, assists, dribbles_completed_pct, foot, fouls, games, games_starts, minutes, gca, miscontrols, passes, passes_pct, passes_received_pct, pressure_regain_pct, pressures, sca, sca_dribbles, sca_fouled, sca_passes_live, sca_per90, tackles, tackles_won, through_balls, throw_ins, touches, touches_live_ball, W, passes_into_final_third, passes_into_penalty_area, progressive_passes, crosses, carry_progressive_distance, assisted_shots, goals, offsides, pass_targets, sca_shots, shots_on_target, shots_total, shots_total_per90, goals_per90, goals_per_shot, goals_per_shot_on_target)
FW_90.df$value <- log(FW_90.df$value)

# GK
GK_78.rf.pred.90 <- predict(GK_78.rf, GK_90.df)
accuracy(GK_78.rf.pred.90, GK_90.df$value)
View(data.frame(GK_78.rf.pred.90, GK_90.df$value))
accuracy(exp(GK_78.rf.pred.90), exp(GK_90.df$value))
View(data.frame(exp(GK_78.rf.pred.90), exp(GK_90.df$value)))

# DF
DF_78.rf.pred.90 <- predict(DF_78.rf, DF_90.df)
accuracy(DF_78.rf.pred.90, DF_90.df$value)
View(data.frame(DF_78.rf.pred.90, DF_90.df$value))
accuracy(exp(DF_78.rf.pred.90), exp(DF_90.df$value))
View(data.frame(exp(DF_78.rf.pred.90), exp(DF_90.df$value)))

# MF
MF_78.rf.pred.90 <- predict(MF_78.rf, MF_90.df)
accuracy(MF_78.rf.pred.90, MF_90.df$value)
View(data.frame(MF_78.rf.pred.90, MF_90.df$value))
accuracy(exp(MF_78.rf.pred.90), exp(MF_90.df$value))
View(data.frame(exp(MF_78.rf.pred.90), exp(MF_90.df$value)))

# FW
FW_78.rf.pred.90 <- predict(FW_78.rf, FW_90.df)
accuracy(FW_78.rf.pred.90, FW_90.df$value)
View(data.frame(FW_78.rf.pred.90, FW_90.df$value))
accuracy(exp(FW_78.rf.pred.90), exp(FW_90.df$value))
View(data.frame(exp(FW_78.rf.pred.90), exp(FW_90.df$value)))

################################################################################
################################################################################
################################################################################
##### K-NN Model
# About GK
result_GK78 <- GK_78_final.df[, 1]
predict_GK78 <- GK_78_final.df[, 2:9]

result_GK89 <- GK_89_final.df[, 1]
predict_GK89 <- GK_89_final.df[, 2:9]

result_GK90 <- GK_90_final.df[, 1]
predict_GK90 <- GK_90_final.df[, 2:9]

pca_GK78 <- prcomp(predict_GK78, scale = TRUE)
pca_GK89 <- prcomp(predict_GK89, scale = TRUE)
pca_GK90 <- prcomp(predict_GK90, scale = TRUE)
summary(pca_GK78)
summary(pca_GK89)
summary(pca_GK90)

# PC 6: 0.94717
num_components <- 6
predict_pca78 <- predict(pca_GK78, newdata = predict_GK78)[, 1:num_components]
predict_pca89 <- predict(pca_GK89, newdata = predict_GK89)[, 1:num_components]
predict_pca90 <- predict(pca_GK90, newdata = predict_GK90)[, 1:num_components]

set.seed(123)
train78.index <- sample(1:nrow(GK_78_final.df), 0.7*nrow(GK_78_final.df))
train_GK78.data <- predict_pca78[train78.index, ]
test_GK78.data <- predict_pca78[-train78.index, ]

train_GK78_target <- result_GK78[train78.index]
test_GK78_target <- result_GK78[-train78.index]

model_knn <- class::knn(train=train_GK78.data, test=test_GK78.data, cl=train_GK78_target, k = 5)
accuracy(as.numeric(model_knn), exp(test_GK78_target))

train89.index <- sample(1:nrow(GK_89_final.df), 0.7*nrow(GK_89_final.df))
train_GK89.data <- predict_pca89[train89.index, ]
test_GK89.data <- predict_pca89[-train89.index, ]

train_GK89_target <- result_GK89[train89.index]
test_GK89.target <- result_GK89[-train89.index]

model_knn <- class::knn(train=train_GK78.data, test=test_GK89.data, cl=train_GK78_target, k = 5)
accuracy(as.numeric(model_knn), exp(test_GK89.target))

train90.index <- sample(1:nrow(GK_90_final.df), 0.7*nrow(GK_90_final.df))
train_GK90.data <- predict_pca90[train90.index, ]
test_GK90.data <- predict_pca90[-train90.index, ]

train_GK90_target <- result_GK90[train90.index]
test_GK90.target <- result_GK90[-train90.index]

model_knn <- class::knn(train=train_GK78.data, test=test_GK90.data, cl=train_GK78_target, k = 5)
accuracy(as.numeric(model_knn), exp(test_GK90.target))
#################################################################################################################################
# About DF
result_DF78 <- DF_78_final.df[, 1]
predict_DF78 <- DF_78_final.df[, 2:15]

result_DF89 <- DF_89_final.df[, 1]
predict_DF89 <- DF_89_final.df[, 2:15]

result_DF90 <- DF_90_final.df[, 1]
predict_DF90 <- DF_90_final.df[, 2:15]

pca_DF78 <- prcomp(predict_DF78, scale = TRUE)
summary(pca_DF78)
num_components <- 8
predict_pca78 <- predict(pca_DF78, newdata = predict_DF78)[, 1:num_components]

pca_DF89 <- prcomp(predict_DF89, scale = TRUE)
summary(pca_DF89)
num_components <- 8
predict_pca89 <- predict(pca_DF89, newdata = predict_DF89)[, 1:num_components]

pca_DF90 <- prcomp(predict_DF908, scale = TRUE)
summary(pca_DF90)
num_components <- 8
predict_pca90 <- predict(pca_DF90, newdata = predict_DF90)[, 1:num_components]

set.seed(123)
train.index <- sample(1:nrow(DF_78_final.df), 0.7*nrow(DF_78_final.df))
train_DF78.data <- predict_pca78[train.index, ]
test_DF78.data <- predict_pca78[-train.index, ]

train_DF78_target <- result_DF78[train.index]
test_DF78_target <- result_DF78[-train.index]

model_knn <- class::knn(train=train_DF78.data, test=test_DF78.data, cl=train_DF78_target, k = 5)
accuracy(as.numeric(model_knn), exp(test_DF78_target))

set.seed(456)
train.index <- sample(1:nrow(DF_89_final.df), 0.7*nrow(DF_89_final.df))
train_DF89.data <- predict_pca89[train.index, ]
test_DF89.data <- predict_pca89[-train.index, ]

train_DF89_target <- result_DF89[train.index]
test_DF89_target <- result_DF89[-train.index]

model_knn <- class::knn(train=train_DF78.data, test=test_DF89.data, cl=train_DF78_target, k = 5)
accuracy(as.numeric(model_knn), exp(test_DF89_target))

set.seed(789)
train.index <- sample(1:nrow(DF_90_final.df), 0.7*nrow(DF_90_final.df))
train_DF90.data <- predict_pca90[train.index, ]
test_DF90.data <- predict_pca90[-train.index, ]

train_DF90_target <- result_DF90[train.index]
test_DF90_target <- result_DF90[-train.index]

model_knn <- class::knn(train=train_DF78.data, test=test_DF90.data, cl=train_DF78_target, k = 5)
accuracy(as.numeric(model_knn), exp(test_DF90_target))
#################################################################################################################################
# About MF
result_MF78 <- MF_78_final.df[, 1]
predict_MF78 <- MF_78_final.df[, 2:21]

result_MF89 <- MF_89_final.df[, 1]
predict_MF89 <- MF_89_final.df[, 2:21]

result_MF90 <- MF_90_final.df[, 1]
predict_MF90 <- MF_90_final.df[, 2:21]

pca_MF78 <- prcomp(predict_MF78, scale = TRUE)
summary(pca_MF78)
num_components <- 10
predict_pca78 <- predict(pca_MF78, newdata = predict_MF78)[, 1:num_components]

pca_MF89 <- prcomp(predict_MF89, scale = TRUE)
summary(pca_MF89)
num_components <- 10
predict_pca89 <- predict(pca_MF89, newdata = predict_MF89)[, 1:num_components]

pca_MF90 <- prcomp(predict_MF90, scale = TRUE)
summary(pca_MF90)
num_components <- 10
predict_pca90 <- predict(pca_MF90, newdata = predict_MF90)[, 1:num_components]

set.seed(123)
train.index <- sample(1:nrow(MF_78_final.df), 0.7*nrow(MF_78_final.df))
train_MF78.data <- predict_pca78[train.index, ]
test_MF78.data <- predict_pca78[-train.index, ]

train_MF78_target <- result_MF78[train.index]
test_MF78_target <- result_MF78[-train.index]

model_knn <- class::knn(train=train_MF78.data, test=test_MF78.data, cl=train_MF78_target, k = 5)
accuracy(as.numeric(model_knn), exp(test_MF78_target))

set.seed(456)
train.index <- sample(1:nrow(MF_89_final.df), 0.7*nrow(MF_89_final.df))
train_MF89.data <- predict_pca89[train.index, ]
test_MF89.data <- predict_pca89[-train.index, ]

train_MF89_target <- result_MF89[train.index]
test_MF89_target <- result_MF89[-train.index]

model_knn <- class::knn(train=train_MF78.data, test=test_MF89.data, cl=train_MF78_target, k = 5)
accuracy(as.numeric(model_knn), exp(test_MF89_target))

set.seed(789)
train.index <- sample(1:nrow(MF_90_final.df), 0.7*nrow(MF_90_final.df))
train_MF90.data <- predict_pca90[train.index, ]
test_MF90.data <- predict_pca90[-train.index, ]

train_MF90_target <- result_MF90[train.index]
test_MF90_target <- result_MF90[-train.index]

model_knn <- class::knn(train=train_MF78.data, test=test_MF90.data, cl=train_MF78_target, k = 5)
accuracy(as.numeric(model_knn), exp(test_MF90_target))
#################################################################################################################################
# About FW
result_FW78 <- FW_78_final.df[, 1]
predict_FW78 <- FW_78_final.df[, 2:23]

result_FW89 <- FW_89_final.df[, 1]
predict_FW89 <- FW_89_final.df[, 2:23]

result_FW90 <- FW_90_final.df[, 1]
predict_FW90 <- FW_90_final.df[, 2:23]

pca_FW78 <- prcomp(predict_FW78, scale = TRUE)
summary(pca_FW78)
num_components <- 10
predict_pca78 <- predict(pca_FW78, newdata = predict_FW78)[, 1:num_components]

pca_FW89 <- prcomp(predict_FW89, scale = TRUE)
summary(pca_FW89)
num_components <- 10
predict_pca89 <- predict(pca_FW89, newdata = predict_FW89)[, 1:num_components]

pca_FW90 <- prcomp(predict_FW90, scale = TRUE)
summary(pca_FW90)
num_components <- 10
predict_pca90 <- predict(pca_FW90, newdata = predict_FW90)[, 1:num_components]

set.seed(123)
train.index <- sample(1:nrow(FW_78_final.df), 0.7*nrow(FW_78_final.df))
train_FW78.data <- predict_pca78[train.index, ]
test_FW78.data <- predict_pca78[-train.index, ]

train_FW78_target <- result_FW78[train.index]
test_FW78_target <- result_FW78[-train.index]

model_knn <- class::knn(train=train_FW78.data, test=test_FW78.data, cl=train_FW78_target, k = 5)
accuracy(as.numeric(model_knn), exp(test_FW78_target))

set.seed(456)
train.index <- sample(1:nrow(FW_89_final.df), 0.7*nrow(FW_89_final.df))
train_FW89.data <- predict_pca89[train.index, ]
test_FW89.data <- predict_pca89[-train.index, ]

train_FW89_target <- result_FW89[train.index]
test_FW89_target <- result_FW89[-train.index]

model_knn <- class::knn(train=train_FW78.data, test=test_FW89.data, cl=train_FW78_target, k = 5)
accuracy(as.numeric(model_knn), exp(test_FW89_target))

set.seed(789)
train.index <- sample(1:nrow(FW_90_final.df), 0.7*nrow(FW_90_final.df))
train_FW90.data <- predict_pca90[train.index, ]
test_FW90.data <- predict_pca90[-train.index, ]

train_FW90_target <- result_FW90[train.index]
test_FW90_target <- result_FW90[-train.index]

model_knn <- class::knn(train=train_FW78.data, test=test_FW90.data, cl=train_FW78_target, k = 5)
accuracy(as.numeric(model_knn), exp(test_FW90_target))
################################################################################
################################################################################
################################################################################