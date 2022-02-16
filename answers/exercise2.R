# New recipe using stopwords
tfidf_sw_stem_rec <- recipe(genre ~ lyrics, data = lyrics_train) %>%
  step_tokenize(lyrics) %>%
  step_stopwords(lyrics, custom_stopword_source = sw_vec) %>% 
  step_stem(lyrics) %>% 
  step_tokenfilter(lyrics, max_tokens = 250) %>% # referring to lyrics column
  step_tfidf(lyrics)

# We now follow the same steps as before, but with the new recipe

# Create a workflow - allows for neater code when working in
# tidymodels
tfidf_sw_stem_wf <- workflow() %>%
  add_recipe(tfidf_sw_stem_rec)

# We will use the random forest spec from the exercises, as this 
# was superior to the naive Bayes model
rand_spec <- rand_forest() %>% 
  set_mode("classification") %>% 
  set_engine("randomForest")

tfidf_sw_stem_wf <- tfidf_sw_stem_wf %>% 
  add_model(rand_spec)

# Fit to each of the folds, allowing 1 fold to be the test set 
# each time
# Using `save_pred = TRUE` allows us to save predictions from
# each of the folds. 
tfidf_sw_stem_fit <- fit_resamples(
  tfidf_sw_stem_wf,
  lyrics_folds,
  control = control_resamples(save_pred = TRUE) 
)

# Collect metrics from our fitted model
tfidf_sw_stem_metrics <- collect_metrics(tfidf_sw_stem_fit)
tfidf_sw_stem_predictions <- collect_predictions(tfidf_sw_stem_fit)

# Compare to previous accuracy
tfidf_sw_stem_metrics
tfidf_rf_metrics
