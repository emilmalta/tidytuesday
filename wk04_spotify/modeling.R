# Load libraries ---------------------------------------------------------------

library(randomForest)
library(rpart.plot)
library(rsample)
library(glmnet)
library(rpart)
library(caret)

# Import / tidy / transform ----------------------------------------------------

source("wk04_spotify/import_tidy_transform.R")

# Visualise --------------------------------------------------------------------

tracks %>% 
  ggplot(aes(x = track_popularity)) +
  geom_histogram(binwidth = 1)

tracks %>% 
  pull(track_popularity) %>% 
  mean()

# Partition data into test and training ----------------------------------------

split <- tracks %>% 
  initial_split(prop = 0.8)

training_tracks_df <- split %>% 
  training()

test_tracks_df <- split %>% 
  training()

# Training matrices ------------------------------------------------------------

train_y_matrix <- training_tracks_df %>% 
  pull(track_popularity)

train_x_matrix <- training_tracks_df %>% 
  select(-track_popularity) %>% 
  data.matrix()

# Test matrices ----------------------------------------------------------------

test_y_matrix <- test_tracks_df %>% 
  pull(track_popularity)

test_x_matrix <- test_tracks_df %>% 
  select(-track_popularity) %>% 
  data.matrix()

# Uhm, model time I guess? -----------------------------------------------------

# I want to try a ridge regression model first
glmnet(train_x_matrix, train_y_matrix, alpha = 0) %>% 
  tidy() %>% 
  filter(term != "(Intercept)") %>% 
  mutate(term = str_to_title(term)) %>% 
  ggplot(aes(x = lambda, y = estimate, color = term)) +
  geom_line(size = 1) +
  lims(y = c(-10, 6)) +
  scale_x_log10() +
  scale_color_statgl () +
  theme_statgl() +
  labs(x = expression(lambda), y = "Estimate", color = "Predictor")

# What is the best lambda value?
ridge_cv <- 
  cv.glmnet(train_x_matrix, train_y_matrix, alpha = 0) 

# Visualise
ridge_cv %>% 
  tidy() %>% 
  ggplot(aes(x = lambda, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              fill = "salmon", alpha = .5) +
  geom_vline(xintercept = ridge_cv$lambda.1se, lty = 2) +
  geom_point(color = "red") +
  theme_statgl() +
  scale_x_log10() +
  labs(x = expression(lambda))

# Final ridge model
ridge_final <- glmnet(
  train_x_matrix, train_y_matrix,
  alpha = 0, 
  lambda = ridge_cv$lambda.min
)

# Try predicting
ridge_preds <- predict(ridge_final, test_x_matrix)

# Yeah, this ain't that great, is it?...
sqrt(mean((ridge_preds - test_y_matrix) ^ 2))

# Visualise Ridge Regression ---------------------------------------------------

ridge_final %>% 
  tidy() %>% 
  filter(!str_detect(term, "Intercept")) %>% 
  mutate(term = term %>% 
           str_to_title() %>% 
           fct_reorder(abs(estimate))) %>% 
  ggplot(aes(x = term, y = abs(estimate), fill = factor(sign(estimate)))) +
  geom_col() +
  coord_flip() +
  theme_statgl() 

ridge_preds %>% cbind(test_y_matrix) %>% 
  as_tibble() %>% 
  rename(prediction = 1, actual = test_y_matrix) %>% 
  ggplot(aes(x = prediction, y = actual)) +
  geom_point(shape = ".")

# Let's just jump straight into elastic net regression -------------------------

# Hyperparameter tuing for elastic net regresseion
elastic_net_hypers <- 
  train(
    track_popularity ~ ., data = training_tracks_df, method = "glmnet",
    trControl = trainControl("cv", number = 10),
    tuneLength = 10
  ) %>% 
  `$`("bestTune")

elastic_net_final <- glmnet(
  train_x_matrix, train_y_matrix, 
  alpha = elastic_net_hypers$alpha,
  lambda = elastic_net_hypers$lambda
)

# Predict on test set
preds <- predict(elastic_net_final, test_x_matrix)

# Still very bad no good horrible RMSE
RMSE(preds, test_y_matrix)

# Look, it's almost the same as just guessing by mean :'()
RMSE(rep(mean(train_y_matrix), length(test_y_matrix)), test_y_matrix)

# Let's just try random forests ------------------------------------------------

# Random forests, baby (not tuned yet)
rf_tree <- randomForest(
  formula = track_popularity ~ ., 
  data = training_tracks_df, 
  ntree = 100, 
  mtry = 4)

# Hoo boy this is much closer!
predict(rf_tree, test_tracks_df) %>% 
  RMSE(test_y_matrix)

# How to interpret, tho?
rf_tree$importance %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Feature") %>% 
  mutate(Feature = Feature %>% fct_reorder(IncNodePurity)) %>% 
  ggplot(aes(x = Feature, y = IncNodePurity)) +
  geom_point() +
  coord_flip()

# TODO: Parameter tune the RF
# TODO: Try boosting