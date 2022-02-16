rand_spec <- rand_forest() %>% 
  set_mode("classification") %>% 
  set_engine("randomForest")

# Create nb_wf which we will fit to each of the resamples.
tfidf_rf_wf <- tfidf_wf %>% 
  add_model(rand_spec)

# Fit to each of the folds, allowing 1 fold to be the test set 
# each time
# Using `save_pred = TRUE` allows us to save predictions from
# each of the folds. This will allow us to create a ROC curve, 
# without yet touching the holdout set.
tfidf_rf_fit <- fit_resamples(
  tfidf_rf_wf,
  lyrics_folds,
  control = control_resamples(save_pred = TRUE) 
)

# Collect metrics from our fitted model
tfidf_rf_metrics <- collect_metrics(tfidf_rf_fit)
tfidf_rf_predictions <- collect_predictions(tfidf_rf_fit)

# Check metrics
tfidf_rf_metrics

# Get a confusion matrix averaged over each of the folds
conf_mat_resampled(tfidf_rf_fit, tidy = FALSE) %>%
  autoplot(type = "heatmap")
