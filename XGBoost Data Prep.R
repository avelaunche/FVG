library(tidyquant)
library(randomForest)
library(ranger)
library(xgboost)
library(caret)
library(forcats)
library(ROCR)

dim(hi_fvg)

fvg_dataframe_construct = hi_fvg |>
  mutate(
    lag_high = lag(high, 1),
    lag_low = lag(low, 1), 
    lag_open = lag(open, 1), 
    lag_close = lag(close, 1),
    lead_high = lead(high, 1), 
    lead_low = lead(low, 1),
    lead_open = lead(open, 1), 
    lead_close = lead(close, 1), 
    lead_volume = lead(volume, 1),
    lag_volume = lag(volume, 1)
  ) |>
  select(
    timestamp, 
    high, 
    low, 
    close, 
    volume, 
    bullish_fvg_size, 
    fvg_size, 
    fvg_size_p,
    lag_high,
    lag_low, 
    lag_open, 
    lag_close, 
    lead_close,
    lead_high,
    lead_low,
    lead_open,
    lead_volume, 
    lag_volume,
    bullish_fvg
  )

fvg_dataframe_construct = fvg_dataframe_construct |>
  mutate(
    weekday = weekdays.Date(timestamp),
    hour = hour(timestamp),
    month = month(timestamp)
  )

fvg_dataframe_construct = fvg_dataframe_construct |>
  mutate(
    sma_20 = SMA(close, n = 20),
    sma_14 = SMA(close, n = 14),
    sma_7 = SMA(close, n = 7),
    sma_50 = SMA(close, n = 50),
    sma_200 = SMA(close, n = 200)
  )

fvg_dataframe_construct = fvg_dataframe_construct |>
  mutate(
    ema_20 = EMA(close, n = 20),
    ema_14 = EMA(close, n = 14),
    ema_9 = EMA(close, n = 9),
    ema_50 = EMA(close, n = 50),
    ema_200 = EMA(close, n = 200)
  )



fvg_dataframe_construct = fvg_dataframe_construct |>
  mutate(
    rsi_20 = RSI(close, n = 20),
    rsi_14 = RSI(close, n = 14),
    rsi_7 = RSI(close, n = 7),
    rsi_50 = RSI(close, n = 50)
  )

fvg_dataframe_construct = fvg_dataframe_construct |>
  mutate(
    min_20 = runMin(close, n = 20),
    min_14 = runMin(close, n = 14),
    min_7 = runMin(close, n = 7),
    max_7 = runMax(close, n = 7),
    max_14 = runMax(close, n = 14),
    max_20 = runMax(close, n = 20)
  )

dim(fvg_dataframe_construct)

fvg_dataframe_construct

colnames(fvg_dataframe_construct)

bullish_fvg_dataframe_xgboost = filter(fvg_dataframe_construct, bullish_fvg)

bullish_fvg_dataframe_xgboost$y = as.factor(res)

bullish_fvg_dataframe_xgboost <- bullish_fvg_dataframe_xgboost[complete.cases(bullish_fvg_dataframe_xgboost), ]

rf_model <- randomForest(
  y ~ .,
  data = bullish_fvg_dataframe_xgboost,
  ntree = 200,
  importance = TRUE
)



n_features = ncol(bullish_fvg_dataframe_xgboost)

ames_rf1 <- ranger(
  y ~ ., 
  data = bullish_fvg_dataframe_xgboost,
  mtry = floor(n_features / 3),
  respect.unordered.factors = "order",
  seed = 123
)

(default_rmse <- sqrt(ames_rf1$prediction.error))


hyper_grid <- expand.grid(
  mtry = floor(n_features * c(.05, .15, .25, .333, .4)),
  min.node.size = c(1, 3, 5, 10), 
  replace = c(TRUE, FALSE),                               
  sample.fraction = c(.5, .63, .8),                       
  rmse = NA                                               
)

# execute full cartesian grid search
for(i in seq_len(nrow(hyper_grid))) {
  # fit model for ith hyperparameter combination
  fit <- ranger(
    formula         = y ~ ., 
    data            = bullish_fvg_dataframe_xgboost, 
    num.trees       = 200,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = hyper_grid$replace[i],
    sample.fraction = hyper_grid$sample.fraction[i],
    verbose         = FALSE,
    seed            = 123,
    respect.unordered.factors = 'order'
  )
  # export OOB error 
  hyper_grid$rmse[i] <- sqrt(fit$prediction.error)
}


top_ten_models = hyper_grid %>%
  arrange(rmse) %>%
  mutate(perc_gain = (default_rmse - rmse) / default_rmse * 100) %>%
  head(10)

top_ten_models

rf_model_hypertuned = randomForest(
  data = bullish_fvg_dataframe_xgboost, 
  y ~ ., 
  mtry = 2,
  ntree = 500,
  importance = TRUE,
  nodesize = 5
)

train_index <- createDataPartition(bullish_fvg_dataframe_xgboost$y, p = 0.8, list = FALSE)
train_index

X_train <- bullish_fvg_dataframe_xgboost[train_index,]
y_train <- bullish_fvg_dataframe_xgboost$y[train_index]
X_test  <- bullish_fvg_dataframe_xgboost[-train_index,]
y_test  <- bullish_fvg_dataframe_xgboost$y[-train_index]

X_train = dplyr::select(X_train, -y)
X_test = dplyr::select(X_test, -y)

class(X_train)

X_train <- X_train %>%
  mutate(across(everything(), as.numeric))

X_test <- X_test %>%
  mutate(across(everything(), as.numeric))

X_train <- as.matrix(X_train)
X_test  <- as.matrix(X_test)

xgb_train <- xgb.DMatrix(data = X_train, label = y_train)
xgb_test  <- xgb.DMatrix(data = X_test, label = y_test)



params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,             
  max_depth = 6,         
  subsample = 0.8,     
  colsample_bytree = 0.8 
)

#Basic xgb_model

xgb_model <- xgb.train(
  params = params,
  data = xgb_train,
  nrounds = 200,
  watchlist = list(train = xgb_train, test = xgb_test),
  early_stopping_rounds = 10,
  verbose = 0
)

sum(round(predict(xgb_model, X_test)) == as.numeric(y_test))/length(y_test)

importance_matrix <- xgb.importance(model = xgb_model)

importance_matrix = importance_matrix |>
  arrange(desc(Gain))

best_importance = importance_matrix |>
  arrange(desc(Gain)) |>
  head(30)

ggplot(best_importance, aes(Gain, fct_reorder(as.factor(Feature), Gain), fill = Feature)) + 
  geom_col() + 
  labs(
    title = "XGBoost Classification Feature Importance",
    subtitle = "C. Difficile with various gut microbiota",
    y = "Species",
    x = "Feature Importance"
  ) + 
  theme_bw() + 
  theme(legend.position = "off")



confusionMatrix(
  factor(pred_class, levels = c(1, 2)),
  factor(as.numeric(y_test), levels = c(1, 2))
)


pred_prob <- predict(xgb_model, X_test)

pred_prob

pred_rocr <- prediction(pred_prob, y_test)

# Calculate TPR/FPR for ROC
perf_roc <- performance(pred_rocr, measure = "tpr", x.measure = "fpr")

# Plot the ROC curve
plot(perf_roc, col = "blue", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")
title("ROC Curve for XGBoost Model")

# Calculate AUC
auc_perf <- performance(pred_rocr, measure = "auc")
auc_value <- auc_perf@y.values[[1]]
auc_value





dim(bullish_fvg_dataframe_xgboost)

ggplot(bullish_fvg_dataframe_xgboost, aes(fvg_size_p)) + 
  facet_grid(rows = vars(y)) + 
  geom_histogram()

ggplot(bullish_fvg_dataframe_xgboost, aes(fvg_size_p)) + 
  facet_grid(rows = vars(y)) + 
  geom_boxplot()

ggplot(bullish_fvg_dataframe_xgboost, aes(lead_volume)) + 
  facet_grid(rows = vars(y)) + 
  geom_histogram()

ggplot(bullish_fvg_dataframe_xgboost, aes(lead_volume)) + 
  facet_grid(rows = vars(y)) + 
  geom_boxplot()

ggplot(bullish_fvg_dataframe_xgboost, aes(month, fill = y)) + 
  geom_bar(position = "fill")

ggplot(bullish_fvg_dataframe_xgboost, aes(hour)) + 
  facet_grid(rows = vars(y)) + 
  geom_boxplot()

ggplot(bullish_fvg_dataframe_xgboost, aes(hour, fill = y)) + 
  geom_bar(position = "fill")

